open Opam_build__Opam_benchmark

open Core
open Async

(* These types need to be synchronized with Tactician's types in benchmark.ml *)
type bench_request =
  { lemmas : string list }

type bench_result =
  | Started of string
  | Found of
      { lemma : string
      ; trace : int list
      ; time : float
      ; witness : string
      ; inferences : int }
[@@deriving bin_io]

type bench_stats =
  { trace : int list
  ; time : float
  ; witness : string
  ; inferences : int }
type bench_result_merged =
  { lemma : string
  ; result : bench_stats option }

module Cmd_worker = struct
  module T = struct

    module Cmd = struct
      type t = pre_bench_info [@@deriving bin_io]
    end
    module Response = struct
      type t =
        [ `Error of Error.t
        | `Stderr of string
        | `Stdout of string
        | `Result of bench_result ]
      [@@deriving bin_io]
    end
    type 'worker functions =
      { hostname : ('worker, unit, string) Rpc_parallel.Function.t
      ; process  : ('worker, Cmd.t, Response.t Pipe.Reader.t) Rpc_parallel.Function.t }

    type worker_state = { name : string } [@@deriving bin_io]
    module Worker_state = struct
      type init_arg = worker_state [@@deriving bin_io]
      type t = worker_state
    end

    module Connection_state = struct
      type init_arg = unit [@@deriving bin_io]
      type t = unit
    end

    module Functions
        (C : Rpc_parallel.Creator
         with type worker_state := Worker_state.t
          and type connection_state := Connection_state.t) =
    struct
      let hostname_impl ~worker_state:_ ~conn_state:() () =
        Unix.gethostname () |> return

      let hostname =
        C.create_rpc ~f:hostname_impl ~bin_input:Unit.bin_t ~bin_output:String.bin_t ()

      let make_process { exec; args; env; dir; lemmas = _ } =
        let args = Array.to_list args in
        let vo_files =
          Sys.getcwd () >>= fun old_pwd ->
          Sys.chdir dir >>= fun () ->
          let rec detect = function
            | [] -> Deferred.return []
            | "-o"::arg::args ->
              let cont = detect args in
              (Sys.file_exists arg >>= function
                | `Unknown | `No -> cont
                | `Yes -> cont >>| fun res -> arg::res)
            | "-l"::_::args -> detect args
            | arg::args ->
              let cont = detect args in
              let arg = arg ^ "o" in
              (Sys.file_exists arg >>= function
                | `Unknown | `No -> cont
                | `Yes -> cont >>| fun res -> arg::res)
          in
          detect args >>= fun vo_files ->
          let vo_files = List.map ~f:Filename.realpath vo_files in
          Deferred.all_unit
            (List.map ~f:(fun f -> Writer.with_file (f^".bench") ~f:(fun _ -> Deferred.unit)) vo_files) >>= fun () ->
          Sys.chdir old_pwd >>| fun () -> vo_files
        in
        vo_files >>= fun vo_files ->
        let bargs = ["--dev-bind"; "/"; "/"] in
        let vo_map = List.map ~f:(fun f -> ["--bind"; f^".bench"; f]) vo_files in
        let bargs = bargs @ List.concat vo_map in
        let (/) = Filename.concat in
        let exec =
          String.chop_suffix_exn exec ~suffix:("lib"/"coq-tactician"/"coqc.real") ^ "bin"/"coqc" in
        let args = bargs @ [exec] @ args in
        let str = "(cd " ^ dir ^ " && " ^ String.concat ~sep:" " ("bwrap"::args) ^ ")" in
        Pipe.write w (`Stdout str) >>= fun () ->
        Process.run
          ~working_dir:dir
          ~prog:"chmod"
          ~args:["+rw"; "-R"; dir] () >>=? fun _ ->
        Spawn_with_socket.create
          ~env:(`Replace_raw (Array.to_list env))
          ~working_dir:dir
          ~prog:"bwrap"
          ~args
          () >>|? fun p -> str, p

      let process_impl ~worker_state:_ ~conn_state:() ({ lemmas; _ } as info : pre_bench_info ) =
        Deferred.return @@ Pipe.create_reader ~close_on_exception:false @@ fun w ->
        make_process info >>= function
        | Error e -> Pipe.write w (`Error e)
        | Ok (str, { stdout; stderr; sock_in; sock_out; wait; _ }) ->
          let request = Marshal.to_bytes ({ lemmas } : bench_request) [] in
          Writer.write_bytes sock_in request;
          let pipes =
            [ Pipe.transfer ~f:(fun m -> `Stdout m) (Reader.pipe stdout) w
            ; Pipe.transfer ~f:(fun m -> `Stderr m) (Reader.pipe stderr) w
            ; Pipe.transfer ~f:(fun m -> `Result m) (Reader.read_all sock_out Reader.read_marshal) w] in
          Deferred.all_unit pipes >>= fun () ->
          force wait >>= function
          | Ok _ -> Deferred.unit
          | Error (`Exit_non_zero i) ->
            Pipe.write w (`Error (Error.createf "Abnormal exit code for coqc: %d\n Invocation:\n%s" i str))
          | Error (`Signal s) ->
            Pipe.write w (`Error (Error.createf "Abnormal exit signal for coqc: %s\n Invocation:\n%s"
                                    (Signal.to_string s) str))

      let process =
        C.create_pipe ~f:process_impl ~bin_input:Cmd.bin_t ~bin_output:Response.bin_t ()

      let functions = { hostname; process }
      let init_worker_state s = return s
      let init_connection_state ~connection:_ ~worker_state:_ = return
    end
  end

  include Rpc_parallel.Make (T)
end

let run_processor
  ~error_writer ~error_occurred
  ~reporter ~coq_out ~coq_err ~processor_out ~processor_err ~info_stream name =
  let stderr = Writer.pipe @@ Lazy.force Writer.stderr in
  (Cmd_worker.spawn_in_foreground
     ~on_failure:(fun e -> don't_wait_for (Pipe.write error_writer e))
     ~shutdown_on:Connection_closed
     { name }
     ~connection_state_init_arg:()
   >>=? fun (conn, process) ->
   don't_wait_for (error_occurred >>= fun () -> Cmd_worker.Connection.close conn);
   let perr1, perr2 = Pipe.fork ~pushback_uses:`Both_consumers (Reader.pipe @@ Process.stderr process) in
   let pipes =
     [ Reader.transfer (Process.stdout process) processor_out
     ; Pipe.transfer_id perr1 processor_err
     ; Pipe.transfer_id perr2 stderr ] in
   Cmd_worker.Connection.run conn
     ~f:Cmd_worker.functions.hostname
     ~arg:() >>=? fun hostname ->
   let rec loop () =
     Pipe.read info_stream >>= function
     | `Eof -> Deferred.Or_error.ok_unit
     | `Ok (cmd : pre_bench_info) ->
       Cmd_worker.Connection.run conn
         ~f:Cmd_worker.functions.process
         ~arg:cmd >>=? fun r ->
       Pipe.fold r ~init:(Ok (None, [])) ~f:(fun acc -> function
           | `Result r ->
             (match acc, r with
              | Ok (None, all), Started lemma ->
                Deferred.Or_error.return @@ (Some lemma, all)
              | Ok (Some lemma', all), Found { lemma; trace; time; witness; inferences } ->
                if String.equal lemma lemma' then
                  Pipe.write reporter { lemma; result = Some { trace; time; witness; inferences } } >>| fun () ->
                  Or_error.return (None, lemma::all)
                else
                  Deferred.Or_error.fail (Error.of_string "Coq benchmark protocol error")
              | Ok (Some lemma, all), Started lemma' ->
                Pipe.write reporter { lemma; result = None } >>| fun () -> Ok (Some lemma', lemma::all)
              | Error _ as err, _ -> Deferred.return err
              | _, _ ->
                Deferred.Or_error.fail (Error.of_string "Coq benchmark protocol error")
             )
           | `Error e ->
             Pipe.write coq_err (Error.to_string_hum e) >>= fun () -> Deferred.Or_error.fail e
           | `Stdout str -> Pipe.write coq_out str >>| fun () -> acc
           | `Stderr str -> Pipe.write coq_err str >>= fun () -> Pipe.write stderr str >>| fun () -> acc
         ) >>=? fun (final, processed_lemmas) ->
       (match final with
        | None -> Deferred.Or_error.return processed_lemmas
        | Some lemma ->
          Pipe.write reporter { lemma; result = None } >>| fun () -> Ok (lemma::processed_lemmas))
       >>=? fun processed_lemmas ->
       let processed_lemmas = String.Set.of_list processed_lemmas in
       let requested_lemmas = String.Set.of_list cmd.lemmas in
       if Set.equal requested_lemmas processed_lemmas then
         loop ()
       else (
         (* String.Set.iter requested_lemmas ~f:print_endline; *)
         (* print_endline "\n"; *)
         (* String.Set.iter processed_lemmas ~f:print_endline; *)
         Deferred.Or_error.fail (Error.of_string "Bench request and bench result mismatch")) in
   loop () >>=? fun () ->
   Cmd_worker.Connection.close conn >>= fun () ->
   Deferred.all_unit pipes >>= fun () ->
   Process.wait process >>= function
   | Ok () -> Deferred.Or_error.ok_unit
   | Error (`Exit_non_zero i) ->
     let err = "Abnormal exit code for command worker: " ^ name ^ " on host " ^ hostname ^ ". Code: " ^ string_of_int i in
     Pipe.write processor_err err >>= fun () ->
     Deferred.Or_error.fail (Error.of_string err)
   | Error (`Signal s) ->
     let err = "Abnormal exit signal for command worker: " ^ name ^ " on host " ^ hostname ^ ". Signal: " ^ Signal.to_string s in
     Pipe.write processor_err err >>= fun () ->
     Deferred.Or_error.fail (Error.of_string err))
  >>= function
  | Ok () -> Deferred.unit
  | Error e -> Pipe.write error_writer e

module Build_worker = struct
  module T = struct

    module Cmd = struct
      type t = { root_dir : string
               ; benchmark_target : string
               ; benchmark_url : string
               ; packages : string list } [@@deriving bin_io]
    end
    module Response = struct
      type t =
        [ `Info of pre_bench_info
        | `Timings of
             [ `Total_install_time of Core_kernel.Time_ns.Span.t ] *
             [ `Target_install_time of Core_kernel.Time_ns.Span.t ] *
             [ `Deps_install_time of Core_kernel.Time_ns.Span.t ] *
             [ `Subject_install_time of Core_kernel.Time_ns.Span.t ] ]
      [@@deriving bin_io]
    end
    type 'worker functions =
      { hostname : ('worker, unit, string) Rpc_parallel.Function.t
      ; build    : ('worker, Cmd.t, Response.t Pipe.Reader.t) Rpc_parallel.Function.t }

    module Worker_state = struct
      type init_arg = unit [@@deriving bin_io]
      type t = unit
    end

    module Connection_state = struct
      type init_arg = unit [@@deriving bin_io]
      type t = unit
    end

    module Functions
        (C : Rpc_parallel.Creator
         with type worker_state := Worker_state.t
          and type connection_state := Connection_state.t) =
    struct
      let hostname_impl ~worker_state:_ ~conn_state:() () =
        Unix.gethostname () |> return

      let hostname =
        C.create_rpc ~f:hostname_impl ~bin_input:Unit.bin_t ~bin_output:String.bin_t ()

      let build_impl ~worker_state:() ~conn_state:() Cmd.{ root_dir; benchmark_target; benchmark_url; packages } =
        compile_and_retrieve_benchmark_info
            ~root_dir
            ~benchmark_target
            ~benchmark_url
            ~packages >>| fun (info, cont) ->
        Pipe.create_reader ~close_on_exception:true @@ fun w ->
        Pipe.transfer info w ~f:(fun info -> `Info info) >>= fun () ->
        cont >>= fun timings ->
        (* Wait until the info pipe is closed so that the timings are guaranteed to be last *)
        Pipe.closed info >>= fun () -> Pipe.write w (`Timings timings)

      let build =
        C.create_pipe ~f:build_impl ~bin_input:Cmd.bin_t ~bin_output:Response.bin_t ()

      let functions = { hostname; build }
      let init_worker_state s = return s
      let init_connection_state ~connection:_ ~worker_state:_ = return
    end
  end

  include Rpc_parallel.Make (T)
end

let compile_and_retrieve_benchmark_info
  ~error_writer ~error_occurred
  ~opam_out ~opam_err ~opam_timings
  ~root_dir
  ~benchmark_target
  ~benchmark_url
  ~packages =
  let stderr = Writer.pipe @@ Lazy.force Writer.stderr in
  let stdout = Writer.pipe @@ Lazy.force Writer.stdout in
  Build_worker.spawn_in_foreground
    ~on_failure:(fun e -> don't_wait_for (Pipe.write error_writer e))
    ~shutdown_on:Connection_closed
    ()
    ~connection_state_init_arg:()
  >>=? fun (conn, process) ->
  don't_wait_for (error_occurred >>= fun () -> Build_worker.Connection.close conn);
  let perr1, perr2 = Pipe.fork ~pushback_uses:`Both_consumers (Reader.pipe @@ Process.stderr process) in
  let pout1, pout2 = Pipe.fork ~pushback_uses:`Both_consumers (Reader.pipe @@ Process.stdout process) in
  let pipes =
    [ Pipe.transfer_id pout1 opam_out
    ; Pipe.transfer_id pout2 stdout
    ; Pipe.transfer_id perr1 opam_err
    ; Pipe.transfer_id perr2 stderr ] in
  Build_worker.Connection.run conn
    ~f:Build_worker.functions.hostname
    ~arg:() >>=? fun hostname ->
  Build_worker.Connection.run conn
        ~f:Build_worker.functions.build
        ~arg:{ root_dir; benchmark_target; benchmark_url; packages } >>|? fun r ->
  let r1, r2 = Pipe.fork ~pushback_uses:`Fast_consumer_only r in
  let r1 = Pipe.filter_map r1 ~f:(function | `Info info -> Some info | `Timings _ -> None) in
  let r2 = Pipe.filter_map r2 ~f:(function | `Info _ -> None | `Timings timings -> Some timings) in
  r1,
  Pipe.read_all r2 >>= fun timings ->
  let finish =
    Build_worker.Connection.close conn >>= fun () ->
    Deferred.all_unit pipes >>= fun () ->
    Process.wait process >>= (function
        | Ok () -> Deferred.unit
        | Error (`Exit_non_zero i) ->
          let err = "Abnormal exit code for build worker on host " ^ hostname ^ ". Code: " ^ string_of_int i in
          Pipe.write opam_err err >>= fun () ->
          Pipe.write error_writer (Error.of_string err)
        | Error (`Signal s) ->
          let err = "Abnormal exit signal for build worker on host " ^ hostname ^ ". Signal: " ^ Signal.to_string s in
          Pipe.write opam_err err >>= fun () ->
          Pipe.write error_writer (Error.of_string err)) in
  match Base.Queue.to_list timings with
  | [`Total_install_time total_install_time,
     `Target_install_time target_install_time,
     `Deps_install_time deps_install_time,
     `Subject_install_time subject_install_time] ->
    let str =
      "Total install time: " ^ Time_ns.Span.to_string_hum total_install_time ^ "\n" ^
      "Target install time: " ^ Time_ns.Span.to_string_hum target_install_time ^ "\n" ^
      "Deps install time: " ^ Time_ns.Span.to_string_hum deps_install_time ^ "\n" ^
      "Subject install time: " ^ Time_ns.Span.to_string_hum subject_install_time ^ "\n"
    in
    Pipe.write opam_timings str >>= fun () ->
    finish
  | _ ->
    Pipe.write error_writer (Error.of_string "Initial build did not fully complete") >>= fun () ->
    finish

let write_bench_params ~scratch ~time =
  let (/) = Filename.concat in
  let file_name = scratch/"BenchParams.v" in
  Writer.with_file file_name ~f:(fun w ->
      Writer.write w ("Set Tactician Benchmark " ^ string_of_int time ^ "."); Deferred.unit)
  >>| fun () ->
  [| "-l"; file_name |]

let prepare_data_dir ~benchmark_data ~benchmark_commit ~time =
  let (/) = Filename.concat in
  (* This directory structure is for legacy reasons *)
  let data_dir = benchmark_data/benchmark_commit/("Set-Tactician-Benchmark-"^string_of_int time^".") in
  Sys.file_exists (benchmark_data/benchmark_commit) >>= (function
  | `Unknown | `No -> Unix.mkdir (benchmark_data/benchmark_commit)
  | `Yes -> Deferred.unit) >>= fun () ->
  Sys.file_exists data_dir >>= (function
      | `Unknown | `No -> Unix.mkdir data_dir
      | `Yes -> Deferred.unit) >>| fun () ->
  data_dir

let with_log_writer file f =
  Writer.with_file file ~f:(fun w ->
      f w)

let with_log_pipe file f =
  Writer.with_file file ~f:(fun w ->
    let w = Writer.pipe w in
    f w >>= fun res ->
    Pipe.upstream_flushed w >>| fun _ -> res)

let reporter time_limit info_stream log =
  let total = ref 0 in
  let processed = ref 0 in
  let synthesized = ref 0 in
  let complete = ref false in
  let summarize () =
    Print.printf "Synthesized %d out of %d lemmas, with %d remaining, initial compilation %s\n"
      !synthesized !processed (!total - !processed) (if !complete then "complete" else "incomplete") in
  Clock.every (Time.Span.of_min 1.) summarize;
  don't_wait_for @@ Pipe.iter info_stream ~f:(fun ({ lemmas; _ } : pre_bench_info) ->
      total := !total + List.length lemmas; Deferred.unit);
  Deferred.upon (Pipe.closed info_stream) (fun () -> complete := true);
  let writer = Pipe.create_writer (fun r ->
      Pipe.iter r ~f:(fun { lemma; result } ->
          processed := !processed + 1;
          (match result with
           | None ->
             Print.fprintf log
               "%s\t%d\n"
               lemma time_limit
           | Some { trace; time; witness; inferences } ->
             synthesized := !synthesized + 1;
             let trace = String.concat ~sep:"." @@ List.map ~f:string_of_int trace in
             Print.fprintf log
               "%s\t%d\t%s\t%s\t%f\t%d\n"
               lemma time_limit trace witness time inferences);
          Writer.flushed log
        )) in
  writer, fun () -> Pipe.upstream_flushed writer >>| fun _ -> summarize ()

let error_handler error_log =
  let stderr = Lazy.force Writer.stderr in
  let error_occurred = Ivar.create () in
  let process_error e =
    if Ivar.is_empty error_occurred then
      (let str = "\n\n------------------ Benchmark terminated -----------------------\n\n" in
        Writer.write stderr str;
        Writer.write error_log str);
    Ivar.fill_if_empty error_occurred ();
    let str = "Fatal benchmarking error: " ^ Error.to_string_hum e ^ "\n" in
    Writer.write stderr str;
    Writer.write error_log str in
  let writer = Pipe.create_writer @@ fun r ->
    Pipe.iter r ~f:(fun e ->
        process_error e;
        Deferred.unit) in
  let term_request = ref false in
  let terminate_handler s =
    match !term_request with
    | false ->
      Writer.write stderr "Termination request received. Repeat within a second to confirm.\n";
      term_request := true;
      Clock.run_after Time.Span.second (fun () -> term_request := false) ()
    | true ->
      process_error (Error.createf "Termination request received: %s" (Signal.to_string s)) in
  Signal.handle [Signal.int] ~f:terminate_handler;
  writer, Ivar.read error_occurred

let commit
    ~error_writer
    ~data_dir ~benchmark_repo ~benchmark_commit =
  let stdout = Lazy.force Writer.stdout in
  Writer.write stdout "\n\nUploading benchmark results\n\n";
  (Process.run
    ~working_dir:data_dir
    ~prog:"git"
    ~args:["add"; "."] () >>=? fun out ->
  Writer.write stdout out;
  Process.run
    ~working_dir:data_dir
    ~prog:"git"
    ~args:["commit"; "-m"; ("benchmark data for " ^ benchmark_repo ^ "#" ^ benchmark_commit)] () >>=? fun out ->
  Writer.write stdout out;
  Process.run
    ~working_dir:data_dir
    ~prog:"git"
    ~args:["push"] () >>=? fun out ->
  Writer.write stdout out;
  Writer.flushed stdout >>= fun () ->
  Deferred.Or_error.ok_unit) >>= function
  | Ok () -> Deferred.unit
  | Error e -> Pipe.write error_writer e

let main
    ~scratch
    ~delay_benchmark
    ~processors
    ~benchmark_data
    ~benchmark_target
    ~benchmark_repo
    ~benchmark_commit
    ~time
    ~packages
  =
  let (/) = Filename.concat in
  prepare_data_dir ~benchmark_data ~benchmark_commit ~time >>= fun data_dir ->
  with_log_writer (data_dir/"error.log") @@ fun error_log ->
  let error_writer, error_occurred = error_handler error_log in
  (with_log_pipe (data_dir/"opam-out.log") @@ fun opam_out ->
  with_log_pipe (data_dir/"opam-err.log") @@ fun opam_err ->
  with_log_pipe (data_dir/"opam-timings.log") @@ fun opam_timings ->
  with_log_pipe (data_dir/"coq-out.log") @@ fun coq_out ->
  with_log_pipe (data_dir/"coq-err.log") @@ fun coq_err ->
  with_log_pipe (data_dir/"processor-out.log") @@ fun processor_out ->
  with_log_pipe (data_dir/"processor-err.log") @@ fun processor_err ->
  with_log_writer (data_dir/"combined.bench") @@ fun bench_log ->
  compile_and_retrieve_benchmark_info
    ~error_writer ~error_occurred
    ~opam_out ~opam_err ~opam_timings
    ~root_dir:(scratch/"opam-root")
    ~benchmark_target
    ~benchmark_url:(benchmark_repo ^ "#" ^ benchmark_commit)
    ~packages
  >>= function
  | Error e ->
    Pipe.write error_writer e
  | Ok (info_stream, cont) ->
    write_bench_params ~scratch ~time >>= fun extra_args ->
    let info_stream = Pipe.map info_stream ~f:(fun ({ args; _ } as info) -> { info with args = Array.append args extra_args }) in
    let info_stream, reporter_stream = Pipe.fork ~pushback_uses:`Fast_consumer_only info_stream in
    let reporter, summarize = reporter time reporter_stream bench_log in
    (if delay_benchmark then cont else Deferred.unit) >>= fun () ->
    let run_processor = run_processor
        ~error_writer ~error_occurred
        ~reporter ~coq_out ~coq_err ~processor_out ~processor_err ~info_stream in
    let processors = List.init processors ~f:(fun i -> run_processor (string_of_int i)) in
    Deferred.all_unit processors >>= fun () ->
    cont >>= summarize)
  >>= fun () ->
  commit ~error_writer ~data_dir ~benchmark_repo ~benchmark_commit >>= fun () ->
  if Deferred.is_determined error_occurred then
    Deferred.Or_error.error_string "Benching errors occurred"
  else
    Deferred.Or_error.ok_unit

module CommandLetSyntax = struct
  include Command.Param
  let (let+) = (>>|)
  let (and+) = both
end

let rec _rmrf path =
  Sys.is_directory path >>= function
  | `Yes ->
    Sys.readdir path >>= fun dir ->
    Deferred.Array.all_unit @@ Array.map dir ~f:(fun name -> _rmrf (Filename.concat path name)) >>= fun () ->
    Unix.rmdir path
  | `No -> Sys.remove path
  | `Unknown -> Deferred.unit
(* TODO: this is a hack; the solution above is not reliable *)
let rmrf path =
  Process.run
    ~prog:"rm"
    ~args:["-rf"; path] () >>| function
  | Ok _ -> ()
  | Error e -> raise (Error.to_exn e)

(* TODO: This can be much better, look at the bos package. *)
let with_temp parent cont =
  let (/) = Filename.concat in
  Unix.mkdtemp (parent/"tactician-benchmark") >>= fun d ->
  try_with (fun () -> cont d) >>= fun res ->
  rmrf d >>| fun () -> match res with
  | Ok x -> x
  | Error e -> raise e

let command =
  Log.Global.set_level `Error;
  let open CommandLetSyntax in
  Command.async_or_error
    ~summary:"Benchmark Tactician"
    (let tmp_dir = flag "tmp-dir" (map_flag (optional string) ~f:(Option.map ~f:(fun x -> `Tmp x)))
         ~doc:"dir Location in which a temporary directory will be created to store the build. If not supplied, it is taken from $TMPDIR. \
               After the benchmark is finished, the directory is cleaned up. Mutually exclusive with -build-dir." in
     let build_dir = flag "build-dir" (map_flag (optional string) ~f:(Option.map ~f:(fun x -> `Build x)))
         ~doc:"dir Location of the build. This directory will not be cleaned up after the benchmark finishes. Mutually exclusive with -tmp-dir." in
     let+ loc = choose_one [tmp_dir; build_dir] ~if_nothing_chosen:If_nothing_chosen.Return_none
     and+ delay_benchmark = flag "delay-benchmark" no_arg
         ~doc:"Delay the benchmark until the initial build is fully complete. Useful when the build process may interfere with the benchmark timings."
     and+ benchmark_data = anon ("benchmark-data" %: string)
     and+ benchmark_target = anon ("benchmark-target" %: string)
     and+ benchmark_repo = anon ("benchmark-repo" %: string)
     (* TODO: Convert branches to commits *)
     and+ benchmark_commit = anon ("benchmark-commit" %: string)
     and+ time = anon ("benchmark-time" %: int)
     and+ processors = anon ("processors" %: int)
     and+ packages = anon (non_empty_sequence_as_list ("package" %: string))
     in fun () ->
       Sys.file_exists benchmark_data >>= function
       | `Unknown | `No -> Deferred.Or_error.error_string "Benchmark data directory does not exist"
       | `Yes ->
         (match loc with
          | None ->
            Deferred.return @@ with_temp Filename.temp_dir_name
          | Some (`Tmp tmp) ->
            (Sys.file_exists tmp >>= function
              | `No | `Unknown -> Deferred.return @@ fun _ -> Deferred.Or_error.error_string "Supplied tmp directory does not exist"
              | `Yes -> Deferred.return @@ with_temp (Filename.realpath tmp)
            )
          | Some (`Build build) ->
            (Sys.file_exists build >>= function
              | `No | `Unknown -> Deferred.return @@ fun _ -> Deferred.Or_error.error_string "Supplied build directory does not exist"
              | `Yes -> Deferred.return @@ fun cont -> cont (Filename.realpath build)))
         >>= fun with_scratch -> with_scratch @@ fun scratch ->
         print_endline ("Scratch directory: " ^ scratch);
         List.iter ~f:print_endline packages;
         main
           ~scratch ~delay_benchmark ~processors
           ~benchmark_data ~benchmark_target ~benchmark_repo ~benchmark_commit ~time ~packages)

(* TODO: Properly catch CTRL+C for cleanup *)
(* TODO: Use brwap to sandbox to the scratch directory *)
let () =
  (match Core.Unix.fork () with
   | `In_the_child ->
     ExtUnix.Specific.setpgid 0 0;
     Rpc_parallel.start_app command
   | `In_the_parent child ->
     let handler s =
       match Core.Signal.send s (`Pid child) with
        | `Ok -> ()
        | `No_such_process -> () in
     let signals_to_forward = [Core.Signal.int; Core.Signal.hup; Core.Signal.term; Core.Signal.quit] in
     List.iter ~f:(fun s -> Core.Signal.Expert.handle s handler) signals_to_forward;
     match Core.Unix.waitpid child with
     | Ok () -> ()
     | Error (`Exit_non_zero i) -> Core.exit i
     | Error (`Signal _) -> Core.exit 1 (* TODO: Proper way of handling this? *)
  );

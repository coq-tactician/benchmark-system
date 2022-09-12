open Opam_build__Opam_benchmark

open Core
open Async

(* These types need to be synchronized with Tactician's types in benchmark.ml *)
type bench_request =
  { lemmas : string list }

type bench_result =
  | Should of string
  | Found of
      { lemma : string
      ; trace : int list
      ; time : float
      ; witness : string
      ; inferences : int }
[@@deriving bin_io]

type bench_response =
  | Skip
  | Bench of int
[@@deriving bin_io]

type bench_stats =
  { trace : int list
  ; time : float
  ; witness : string
  ; inferences : int }
type bench_result_merged =
  { lemma : string
  ; result : bench_stats option }

type exec_info =
  { exec   : string
  ; args   : string array
  ; env    : string array
  ; dir    : string }
[@@deriving bin_io]

let enable_debug = ref false
let debug_output =
  let stderr = Lazy.force Writer.stderr in
  fun s ->
    if !enable_debug then
      Writer.write_line stderr s

module Counter : sig
  type t
  val make : int -> t
  val increase : t -> unit
  val decrease : t -> unit
  val count : t -> int
end = struct
  type t = int ref
  let make i = ref i
  let increase c = c := !c + 1
  let decrease c =
    assert (!c > 0);
    c := !c - 1
  let count c = !c
end

module Cmd_worker = struct
  module T = struct

    module Cmd = struct
      type t = exec_info [@@deriving bin_io]
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
      ; process  : ('worker, Cmd.t, Response.t Pipe.Reader.t) Rpc_parallel.Function.t
      ; continue : ('worker, bench_response, unit) Rpc_parallel.Function.t }

    module Worker_state = struct
      type t = { name : string } [@@deriving bin_io]
      type init_arg = t [@@deriving bin_io]
    end

    module Connection_state = struct
      type t = bench_response Pipe.Reader.t * bench_response Pipe.Writer.t
      type init_arg = unit [@@deriving bin_io]
    end

    module Functions
        (C : Rpc_parallel.Creator
         with type worker_state := Worker_state.t
          and type connection_state := Connection_state.t) =
    struct
      let hostname_impl ~worker_state:_ ~conn_state:_ () =
        Unix.gethostname () |> return

      let hostname =
        C.create_rpc ~f:hostname_impl ~bin_input:Unit.bin_t ~bin_output:String.bin_t ()

      let make_process { exec; args; env; dir } =
        let args = List.tl_exn @@ Array.to_list args in
        let vo_postfix = ".bench-" ^ Pid.to_string @@ Unix.getpid () in
        let vo_files =
          Sys.getcwd () >>= fun old_pwd ->
          Sys.chdir dir >>= fun () ->
          let check_file cont f =
            if String.length f > 255 then cont else
              (Sys.file_exists f >>= function
                | `Unknown | `No -> cont
                | `Yes -> cont >>| fun res -> f::res) in
          let rec detect = function
            | [] -> Deferred.return []
            | "-o"::arg::args ->
              let cont = detect args in
              check_file cont arg
            | "-l"::_::args -> detect args
            | arg::args ->
              let cont = detect args in
              let arg = arg ^ "o" in
              check_file cont arg
          in
          detect args >>= fun vo_files ->
          let vo_files = List.map ~f:Filename.realpath vo_files in
          Deferred.all_unit
            (List.map ~f:(fun f -> Writer.with_file (f^vo_postfix) ~f:(fun _ -> Deferred.unit)) vo_files) >>= fun () ->
          Sys.chdir old_pwd >>| fun () -> vo_files
        in
        vo_files >>= fun vo_files ->
        let bargs = ["--dev-bind"; "/"; "/"] in
        let vo_map = List.map ~f:(fun f -> ["--bind"; f^vo_postfix; f]) vo_files in
        let bargs = bargs @ List.concat vo_map in
        let (/) = Filename.concat in
        let exec =
          String.chop_suffix_exn exec ~suffix:("lib"/"coq-tactician"/"coqc.real") ^ "bin"/"coqc" in
        let args = bargs @ [exec] @ args in
        let env =
          List.map ~f:(fun s ->
              match OpamStd.String.cut_at s '=' with
              | None   -> s, None
              | Some (key, value) ->
                if String.equal key "PATH" then
                  key, Some (Option.value ~default:""
                               (Option.map ~f:(fun p -> p^":") @@ Unix.getenv "PATH_EXTRA") ^ value)
                else key, Some value
            ) @@ Array.to_list env in
        let str = String.concat ~sep:"\n" @@ List.map ~f:(function
            | (key, None) -> key ^ "=; export " ^ key ^ ";"
            | (key, Some value) -> key ^ "=" ^ value ^ "; export " ^ key ^ ";") env in
        let str = str ^ "\n(cd " ^ dir ^ " && " ^ String.concat ~sep:" " ("bwrap"::args) ^ ")" in
        Process.run
          ~working_dir:dir
          ~prog:"chmod"
          ~args:["+rw"; "-R"; dir] () >>=? fun _ ->
        Spawn_with_socket.create
          ~env:(`Override env)
          ~working_dir:dir
          ~prog:"bwrap"
          ~args
          () >>|? fun p -> str, p

      let process_impl ~worker_state:_ ~conn_state (info : exec_info) =
        Deferred.return @@ Pipe.create_reader ~close_on_exception:false @@ fun w ->
        make_process info >>= function
        | Error e -> Pipe.write w (`Error e)
        | Ok (str, { stdout; stderr; sock_in; sock_out; wait; _ }) ->
          let pipes =
            [ Pipe.transfer ~f:(fun m -> `Stdout m) (Reader.pipe stdout) w
            ; Pipe.transfer ~f:(fun m -> `Stderr m) (Reader.pipe stderr) w ] in
          let messages : bench_result Pipe.Reader.t = Reader.read_all sock_out Reader.read_marshal in
          let rec loop () =
            Pipe.read messages >>= function
            | `Eof -> Deferred.unit
            | `Ok ((Should _) as msg) ->
              Pipe.write w (`Result msg) >>= fun () ->
              Pipe.read (fst conn_state) >>= (function
                  | `Eof -> assert false
                  | `Ok res ->
                    let res = Marshal.to_bytes (res : bench_response) [] in
                    Writer.write_bytes sock_in res; loop ())
            | `Ok ((Found _) as msg) -> Pipe.write w (`Result msg) >>= fun () -> loop () in
          loop () >>= fun () ->
          Deferred.all_unit pipes >>= fun () ->
          Writer.close sock_in >>= fun () ->
          Reader.close sock_out >>= fun () ->
          force wait >>= function
          | Ok _ -> Deferred.unit
          | Error (`Exit_non_zero i) ->
            Pipe.write w (`Error (Error.createf "Abnormal exit code for coqc: %d\n Invocation:\n%s" i str))
          | Error (`Signal s) ->
            Pipe.write w (`Error (Error.createf "Abnormal exit signal for coqc: %s\n Invocation:\n%s"
                                    (Signal.to_string s) str))

      let process =
        C.create_pipe ~f:process_impl ~bin_input:Cmd.bin_t ~bin_output:Response.bin_t ()

      let continue_impl ~worker_state:_ ~conn_state res =
        Pipe.write (snd conn_state) res

      let continue =
        C.create_rpc ~f:continue_impl ~bin_input:bin_bench_response ~bin_output:Unit.bin_t ()

      let functions = { hostname; process; continue }
      let init_worker_state s = return s
      let init_connection_state ~connection:_ ~worker_state:_ () = return (Pipe.create ())
    end
  end

  include Rpc_parallel.Make (T)
end

let prefix_invocation prefix cmd =
  let prefix = Arg_parser.split prefix in
  let cmd = prefix @ cmd in
  let prog = List.hd_exn cmd in
  let args = List.tl_exn cmd in
  prog, args

let remote_how prefix =
  let open Rpc_parallel in
  How_to_run.wrap How_to_run.local ~f:(fun { prog=_; args } ->
      (* A bit a a hack to get the right executable name into the invocation *)
      let prog, args = prefix_invocation prefix ((Sys.get_argv ()).(0) :: args) in
      debug_output ("Invocation: " ^ prog ^ " " ^ String.concat ~sep:" " args);
      { prog; args })

let run_processor
    ~prefix
    ~error_writer ~error_occurred
    ~task_allocator
    ~reporter ~coq_out ~coq_err ~processor_out ~processor_err ~job_time ~job_name with_job =
  let deadline = Time_ns.add (Time_ns.now ()) job_time in
  let stderr = Writer.pipe @@ Lazy.force Writer.stderr in
  (Cmd_worker.spawn_in_foreground
     ~how:(remote_how prefix)
     ~on_failure:(fun e -> don't_wait_for (Pipe.write error_writer e))
     ~shutdown_on:Connection_closed
     { name = job_name }
     ~connection_state_init_arg:()
   >>=? fun (conn, process) ->
   let perr1, perr2 = Pipe.fork ~pushback_uses:`Both_consumers (Reader.pipe @@ Process.stderr process) in
   let pipes =
     [ Pipe.transfer_id (Reader.pipe @@ Process.stdout process) processor_out
     ; Pipe.transfer_id perr1 processor_err
     ; Pipe.transfer_id perr2 stderr ] in
   don't_wait_for (error_occurred >>= fun () -> Cmd_worker.Connection.close conn);
   (Cmd_worker.Connection.run conn
     ~f:Cmd_worker.functions.hostname
     ~arg:() >>=? fun hostname ->
   let rec loop () =
     task_allocator ~prefix ~hostname deadline >>= function
     | `Stop -> Deferred.Or_error.ok_unit
     | `Task (relinquish, exec_info, lemma_disseminator) ->
       let continue lemma =
         let ivar = Ivar.create () in
         Pipe.write lemma_disseminator (deadline, lemma, ivar) >>= fun () ->
         Ivar.read ivar >>= fun res ->
         Cmd_worker.Connection.run conn
           ~f:Cmd_worker.functions.continue
           ~arg:res >>|? fun () -> match res with
         | Skip -> None
         | Bench _ -> Some lemma in
       Cmd_worker.Connection.run conn
         ~f:Cmd_worker.functions.process
         ~arg:exec_info >>=? fun r ->
       Pipe.fold r ~init:(Ok (None, [])) ~f:(fun acc -> function
           | `Result r ->
             (match acc, r with
              | Ok (None, all), Should lemma ->
                continue lemma >>=? fun res ->
                Deferred.Or_error.return @@ (res, all)
              | Ok (Some lemma', all), Found { lemma; trace; time; witness; inferences } ->
                if String.equal lemma lemma' then
                  Pipe.write reporter { lemma; result = Some { trace; time; witness; inferences } } >>| fun () ->
                  Or_error.return (None, lemma::all)
                else
                  Deferred.Or_error.fail (Error.of_string "Coq benchmark protocol error")
              | Ok (Some lemma, all), Should lemma' ->
                continue lemma' >>=? fun res ->
                Pipe.write reporter { lemma; result = None } >>| fun () -> Ok (res, lemma::all)
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
          (if Deferred.is_determined error_occurred then
             Deferred.unit
           else
             Pipe.write reporter { lemma; result = None }) >>| fun () ->
          Ok (lemma::processed_lemmas))
       >>=? fun _processed_lemmas ->
       relinquish ();
       loop () in
   with_job ~prefix ~job_name ~hostname loop) >>= fun res ->
   Cmd_worker.Connection.close conn >>= fun () ->
   Deferred.all_unit pipes >>= fun () ->
   Writer.close (Process.stdin process) >>= fun () ->
   (Process.wait process >>= function
     | Ok () -> Deferred.unit
     | Error (`Exit_non_zero i) ->
       let err = "Abnormal exit code for command worker: " ^ job_name ^ " with prefix " ^
                 prefix ^ ". Code: " ^ string_of_int i in
       Pipe.write processor_err err >>= fun () ->
       Pipe.write error_writer (Error.of_string err)
     | Error (`Signal s) ->
       let err = "Abnormal exit signal for command worker: " ^ job_name ^ " with prefix " ^
                 prefix ^ ". Signal: " ^ Signal.to_string s in
       Pipe.write processor_err err >>= fun () ->
       Pipe.write error_writer (Error.of_string err)) >>| fun () -> res)
  >>= function
  | Ok () -> Deferred.unit
  | Error e -> if Deferred.is_determined error_occurred then Deferred.unit else Pipe.write error_writer e

module Build_worker = struct
  module T = struct

    module Cmd = struct
      type t = { root_dir : string
               ; benchmark_target : string
               ; benchmark_url : string
               ; packages : string list
               ; injections_extra : string list } [@@deriving bin_io]
    end
    module Prereq = struct
      type t = { dir : string
               ; repo : string
               ; commit : string } [@@deriving bin_io]
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
      ; build    : ('worker, Cmd.t, Response.t Pipe.Reader.t) Rpc_parallel.Function.t
      ; prereq    : ('worker, Prereq.t, string) Rpc_parallel.Function.t }

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

      let prereq_impl ~worker_state:() ~conn_state:()
          Prereq.{ dir; repo; commit} =
        let stderr = Writer.pipe @@ Lazy.force Writer.stderr in
        let stdout = Writer.pipe @@ Lazy.force Writer.stdout in
        (Sys.file_exists dir >>= fun exists ->
         (match exists with
          | `No | `Unknown -> Deferred.Or_error.return ()
          | `Yes -> Process.run ~prog:"rm" ~args:["-rf"; dir] () >>|? fun _ -> ()) >>=? fun () ->
         Unix.mkdir dir >>= fun () ->
         let repo = String.strip repo in
         let repo = if String.is_prefix ~prefix:"git+http" repo then
             String.chop_prefix_exn ~prefix:"git+" repo else repo in
         let cmds =
           [ "git", ["init"]
           ; "git", ["remote"; "add"; "origin"; repo]
           ; "git", ["fetch"; "--depth"; "1"; "origin"; commit]
           ; "git", ["checkout"; "FETCH_HEAD"]] in
         (Deferred.Or_error.List.iter cmds ~f:(fun (prog, args) ->
              Process.run ~working_dir:dir ~prog ~args () >>|? fun _ -> ())) >>=? fun () ->
         let prereq_file = Filename.concat dir "prerequisites" in
         (Sys.file_exists prereq_file >>= function
           | `No | `Unknown -> Deferred.Or_error.return ()
           | `Yes ->
             Process.create ~working_dir:dir ~prog:prereq_file ~args:[] () >>=? fun p ->
             let pipes = [ Pipe.transfer_id (Reader.pipe @@ Process.stderr p) stderr
                         ; Pipe.transfer_id (Reader.pipe @@ Process.stdout p) stdout ] in
             (Deferred.all_unit pipes >>= fun () ->
              Writer.close (Process.stdin p) >>= fun () ->
              Process.wait p >>= function
               | Ok () -> Deferred.Or_error.return ()
               | Error _ -> Deferred.Or_error.errorf "Prerequisites file error")) >>=? fun () ->
         let update_env_file = Filename.concat dir "update-env" in
         (Sys.file_exists update_env_file >>= function
           | `No | `Unknown -> Deferred.Or_error.return ""
           | `Yes -> Process.run ~working_dir:dir ~prog:update_env_file ~args:[] ())) >>= function
        | Ok env -> Deferred.return env
        | Error e -> raise (Error.to_exn e)

      let prereq =
        C.create_rpc ~f:prereq_impl ~bin_input:Prereq.bin_t ~bin_output:bin_string ()

      let build_impl ~worker_state:() ~conn_state:()
          Cmd.{ root_dir; benchmark_target; benchmark_url; packages; injections_extra } =
        compile_and_retrieve_benchmark_info
            ~root_dir
            ~benchmark_target
            ~benchmark_url
            ~packages
            ~injections_extra
        >>| fun (info, cont) ->
        Pipe.create_reader ~close_on_exception:true @@ fun w ->
        Pipe.transfer info w ~f:(fun info -> `Info info) >>= fun () ->
        cont >>= fun timings ->
        (* Wait until the info pipe is closed so that the timings are guaranteed to be last *)
        Pipe.closed info >>= fun () -> Pipe.write w (`Timings timings)

      let build =
        C.create_pipe ~f:build_impl ~bin_input:Cmd.bin_t ~bin_output:Response.bin_t ()

      let functions = { hostname; build; prereq }
      let init_worker_state s = return s
      let init_connection_state ~connection:_ ~worker_state:_ = return
    end
  end

  include Rpc_parallel.Make (T)
end

let compile_and_retrieve_benchmark_info
    ~error_writer ~error_occurred
    ~compile_allocator
    ~opam_out ~opam_err ~opam_timings
    ~scratch
    ~benchmark_target
    ~benchmark_repo
    ~benchmark_commit
    ~packages
    ~injections_extra
    ~add_job ~remove_job ~switch_data_host
    ~final_data_prefix ~final_data_host =
  let stderr = Writer.pipe @@ Lazy.force Writer.stderr in
  let stdout = Writer.pipe @@ Lazy.force Writer.stdout in
  Process.create
    ~prog:"setsid"
    ~args:["-w"; compile_allocator; benchmark_commit]
    () >>=? fun p ->
  let pipe = Pipe.transfer_id (Reader.pipe @@ Process.stderr p) stderr in
  let pstdout = Process.stdout p in
  let stop_clock = Ivar.create () in
  Clock_ns.every ~start:(Clock_ns.after Time_ns.Span.second)
    ~stop:(Ivar.read stop_clock) Time_ns.Span.minute (fun () ->
      Print.printf "\nWaiting for initial compilation resources\n");
  Deferred.choose [ Deferred.choice (Reader.read_line pstdout) (fun t -> `Allocated t)
                  ; Deferred.choice error_occurred (fun () -> `Aborted) ] >>= fun line ->
  Ivar.fill stop_clock ();
  match line with
  | `Aborted ->
    Signal.send_i Signal.int (`Group (Process.pid p));
    pipe >>= fun () -> Writer.close (Process.stdin p) >>= fun () -> Reader.close (Process.stdout p) >>= fun () ->
    Process.wait p >>| fun _ -> Or_error.errorf "Aborted before initial compilation could start"
  | `Allocated `Eof ->
    Signal.send_i Signal.int (`Group (Process.pid p));
    pipe >>= fun () -> Writer.close (Process.stdin p) >>= fun () -> Reader.close (Process.stdout p) >>= fun () ->
    Process.wait p >>| fun _ -> Or_error.errorf "Compile alloc protocol error: Unexpected eof"
  | `Allocated `Ok prefix ->
    Build_worker.spawn_in_foreground
      ~how:(remote_how prefix)
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
    let job_name = "compile_job" in
    add_job ~job_name ~hostname;
    (* Make sure the initial data is copied over in case the build directory is being reused *)
    switch_data_host ~new_prefix:prefix ~new_host:hostname >>= fun () ->
    Build_worker.Connection.run conn
      ~f:Build_worker.functions.prereq
      ~arg:{ dir = Filename.concat scratch "target-source"; repo = benchmark_repo; commit = benchmark_commit } >>=? fun env ->
    let env = String.split_lines env in
    List.iter ~f:(fun s ->
        let key, data = match OpamStd.String.cut_at s '=' with
          | None   -> s, ""
          | Some p -> p in
        if String.equal "PATH" key then
          let new_paths = String.split data ~on:':' in
          let old_paths = String.split (Unix.getenv_exn "PATH") ~on:':' in
          let new_paths = String.concat ~sep:":" @@
            List.filter ~f:(fun p -> not @@ List.exists old_paths ~f:(String.equal p)) new_paths in
          Unix.putenv ~key:"PATH_EXTRA" ~data:new_paths
        else
          Unix.putenv ~key ~data

      ) env;
    Build_worker.Connection.run conn
      ~f:Build_worker.functions.build
      ~arg:{ root_dir = Filename.concat scratch "opam-root"; benchmark_target
           ; benchmark_url = benchmark_repo^"#"^benchmark_commit; packages; injections_extra } >>|? fun r ->
    let r1, r2 = Pipe.fork ~pushback_uses:`Fast_consumer_only r in
    let r1 = Pipe.filter_map r1 ~f:(function | `Info info -> Some info | `Timings _ -> None) in
    let r2 = Pipe.filter_map r2 ~f:(function | `Info _ -> None | `Timings timings -> Some timings) in
    r1,
    Pipe.read_all r2 >>= fun timings ->
    let finish =
      switch_data_host ~new_prefix:final_data_prefix ~new_host:final_data_host >>= fun () ->
      remove_job ~prefix ~job_name ~hostname >>= fun () ->
      Build_worker.Connection.close conn >>= fun () ->
      Deferred.all_unit pipes >>= fun () ->
      Writer.close (Process.stdin process) >>= fun () ->
      Process.wait process >>= (function
          | Ok () -> Deferred.unit
          | Error (`Exit_non_zero i) ->
            let err = "Abnormal exit code for build worker on host " ^ hostname ^ ". Code: " ^ string_of_int i in
            Pipe.write opam_err err >>= fun () ->
            Pipe.write error_writer (Error.of_string err)
          | Error (`Signal s) ->
            let err = "Abnormal exit signal for build worker on host " ^ hostname ^
                      ". Signal: " ^ Signal.to_string s in
            Pipe.write opam_err err >>= fun () ->
            Pipe.write error_writer (Error.of_string err)) >>= fun () ->
      let pstdin = Process.stdin p in
      Monitor.detach (Writer.monitor pstdin);
      Writer.write pstdin "done\n";
      pipe >>= fun () ->
      Pipe.transfer_id (Reader.pipe @@ Process.stdout p) stdout >>= fun () ->
      Writer.close (Process.stdin p) >>= fun () ->
      Process.wait p >>= function
      | Ok () -> Deferred.unit
      | Error (`Exit_non_zero i) ->
        Pipe.write error_writer @@ Error.createf "Compile alloc protocol error: Abnormal exit code: %d" i
      | Error (`Signal s) ->
        Pipe.write error_writer @@ Error.createf "Compile alloc protocol error: Abnormal signal: %s" @@
        Signal.to_string s
    in
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

let write_bench_params ~scratch =
  let (/) = Filename.concat in
  let file_name = scratch/"BenchParams.v" in
  Writer.with_file file_name ~f:(fun w ->
      Writer.write w "Set Tactician Benchmark."; Deferred.unit)
  >>| fun () ->
  [| "-l"; file_name |]

let write_injections ~data_dir ~injections_extra =
  let (/) = Filename.concat in
  let write fn =
    Writer.with_file fn ~f:(fun w ->
        List.iter ~f:(Writer.write_line w) injections_extra; Deferred.unit) in
  write (data_dir/"Injections.v")

let prepare_data_dir ~benchmark_data ~benchmark_commit ~lemma_time =
  let (/) = Filename.concat in
  (* This directory structure is for legacy reasons *)
  let data_dir = benchmark_data/benchmark_commit/("Set-Tactician-Benchmark-"^string_of_int lemma_time^".") in
  Sys.file_exists (benchmark_data/benchmark_commit) >>= (function
  | `Unknown | `No -> Unix.mkdir (benchmark_data/benchmark_commit)
  | `Yes -> Deferred.unit) >>= fun () ->
  Sys.file_exists data_dir >>= (function
      | `Unknown | `No -> Unix.mkdir data_dir
      | `Yes -> Deferred.unit) >>| fun () ->
  print_endline ("Data directory:\n"^data_dir);
  data_dir

let with_log_writer ~append file f =
  Writer.with_file ~append file ~f:(fun w ->
      f w)

let with_log_pipe ~append file f =
  Writer.with_file ~append file ~f:(fun w ->
    let w = Writer.pipe w in
    f w >>= fun res ->
    Pipe.upstream_flushed w >>| fun _ -> res)

let reporter ~lemma_time ~info_stream ~bench_log ~resources_requested ~resources_total ~jobs_running =
  let total = ref 0 in
  let processed = ref 0 in
  let synthesized = ref 0 in
  let complete = ref false in
  let summarize () =
    Print.printf "\nSynthesized %d out of %d lemmas, with %d remaining, %d total.\n\
                  Initial compilation %s.\n\
                  Resource requests running: %d; Resources allocated: %d; Total jobs running: %d\n"
      !synthesized !processed (!total - !processed) !total
      (if !complete then "complete" else "incomplete")
      (resources_requested ()) (resources_total () - resources_requested ()) (jobs_running ())
      in
  Clock_ns.every Time_ns.Span.minute summarize;
  don't_wait_for @@ Pipe.iter info_stream ~f:(fun ({ lemmas; _ } : pre_bench_info) ->
      total := !total + List.length lemmas; Deferred.unit);
  Deferred.upon (Pipe.closed info_stream) (fun () -> complete := true);
  let writer = Pipe.create_writer (fun r ->
      Pipe.iter r ~f:(fun { lemma; result } ->
          processed := !processed + 1;
          (match result with
           | None ->
             Print.fprintf bench_log
               "%s\t%d\n"
               lemma lemma_time
           | Some { trace; time; witness; inferences } ->
             synthesized := !synthesized + 1;
             let trace = String.concat ~sep:"." @@ List.map ~f:string_of_int trace in
             Print.fprintf bench_log
               "%s\t%d\t%s\t%s\t%f\t%d\n"
               lemma lemma_time trace witness time inferences);
          Writer.flushed bench_log
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
      ~args:["pull"] () >>=? fun out ->
    Writer.write stdout out;
    Process.run
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
  Writer.write stdout ("Data directory:\n"^data_dir^"\n");
  let dir = List.last_exn (Filename.parts data_dir) in
  (* TODO: In the future, we should not hardcode this link *)
  Writer.write stdout ("Online data location:\nhttps://github.com/coq-tactician/benchmark-data/tree/master/"^
                       benchmark_commit^"/"^dir^"/\n");
  Writer.flushed stdout >>= fun () ->
  Deferred.Or_error.ok_unit) >>= function
  | Ok () -> Deferred.unit
  | Error e -> Pipe.write error_writer e

let alloc_benchers =
  let mk_id =
    let id = Counter.make 0 in
    fun () ->
      let res = Counter.count id in
      Counter.increase id;
      res in
  fun ~task_allocator
    ~relinquish_alloc_token ~relinquish_running_token
    ~abort ~error_writer
    ~bench_allocator
    ~job_starter
    ~benchmark_commit
    ~processor_err ->
    let stderr = Writer.pipe @@ Lazy.force Writer.stderr in
    let error_if_not_aborted e =
      if Deferred.is_determined abort then Deferred.Or_error.ok_unit else
        Deferred.Or_error.fail e in
    (Process.create
       ~prog:"setsid"
       ~args:["-w"; bench_allocator; benchmark_commit]
       () >>=? fun p ->
     let perr1, perr2 = Pipe.fork ~pushback_uses:`Both_consumers (Reader.pipe @@ Process.stderr p) in
     let pipe = [ Pipe.transfer_id perr1 stderr
                ; Pipe.transfer_id perr2 processor_err ] in
     let p_stdout = Process.stdout p in
     Deferred.choose [ Deferred.choice (Reader.read_line p_stdout) (fun t -> `Allocated t)
                     ; Deferred.choice abort (fun () -> `Aborted) ] >>= function
     | `Aborted ->
       relinquish_alloc_token ();
       relinquish_running_token ();
       Signal.send_i Signal.int (`Group (Process.pid p));
       Deferred.List.all_unit pipe >>= fun () ->
       Writer.close (Process.stdin p) >>= fun () -> Reader.close (Process.stdout p) >>= fun () ->
       Process.wait p >>| fun _ -> Or_error.return ()
     | `Allocated `Eof ->
       relinquish_alloc_token ();
       relinquish_running_token ();
       Signal.send_i Signal.int (`Group (Process.pid p));
       error_if_not_aborted @@ Error.createf "Alloc protocol error: Unexpected eof"
     | `Allocated `Ok time ->
       (match int_of_string_opt time with
        | None -> Deferred.Or_error.errorf "Alloc protocol error: Int expected, got %s" time
        | Some time -> Deferred.Or_error.return time) >>=? fun time ->
       let rec loop cont =
         Reader.read_line p_stdout >>= function
         | `Eof -> error_if_not_aborted @@ Error.createf "Alloc protocol error: Unexpected eof"
         | `Ok prefix ->
           if String.equal "done" prefix then
             cont
           else
             let job = job_starter
                 ~task_allocator
                 ~job_time:(Time_ns.Span.of_min (float_of_int time))
                 ~prefix ~job_name:("job" ^ string_of_int (mk_id ())) >>| fun () -> Or_error.return () in
             loop (cont >>=? fun () -> job) in
       let cont = loop Deferred.Or_error.ok_unit in
       relinquish_alloc_token ();
       cont >>=? fun () ->
       let stdin = Process.stdin p in
       Monitor.detach (Writer.monitor stdin);
       Writer.write stdin "done\n";
       Deferred.List.all_unit pipe >>= fun () ->
       Writer.close (Process.stdin p) >>= fun () -> Reader.close (Process.stdout p) >>= fun () ->
       Process.wait p >>= function
       | Ok () ->
         relinquish_running_token ();
         Deferred.Or_error.ok_unit
       | Error (`Exit_non_zero i) ->
         Deferred.Or_error.errorf "Alloc protocol error: Abnormal exit code: %d" i
       | Error (`Signal s) ->
         Deferred.Or_error.errorf "Alloc protocol error: Abnormal signal: %s" @@ Signal.to_string s) >>= function
    | Ok () -> Deferred.unit
    | Error e ->
      Pipe.write processor_err (Error.to_string_hum e) >>= fun () ->
      Pipe.write error_writer e

module ResourceManager : sig
  type ('size, 'taken, 'release) t
  val make_queue : unit -> (int, unit, unit) t
  val make_resource : 'a -> int -> (int, int, 'a -> unit) t
  val make_resource' : 'a -> int option -> (int option, int, 'a -> unit) t
  val allocate : (_, _, 'a) t -> [ `Eof | `Ok of 'a | `Race ] Deferred.Choice.t
  val add_work : (int, unit, unit) t -> (unit -> unit) Staged.t
  val taken : (_, 'a, _) t -> 'a
  val size : ('a, _, _) t -> 'a
  val merge : ('a1, 'b1, 'c1) t -> ('a2, 'b2, 'c2) t -> ('a1 * 'a2, 'b1 * 'b2, 'c1 * 'c2) t
  val finish : (int, unit, unit) t -> unit
  val finished : (_, _, _) t -> unit Deferred.t
end = struct
  type ('size, 'taken, 'release) t =
    { available : unit -> [ `Eof | `Ok ] Deferred.t
    ; take_now : unit -> [ `Eof | `Ok of 'release | `Race ]
    ; add_work : unit -> (unit -> unit) Staged.t
    ; taken : unit -> 'taken
    ; size : unit -> 'size
    ; finish : unit -> unit
    ; finished : unit -> unit Deferred.t
    ; call_release : 'release -> unit }
  let call_once f =
    let called = ref false in
    fun x ->
      if !called then
        raise (Error.to_exn @@ Error.createf "function called twice")
      else begin
        called := true;
        f x
      end
  let make_queue () =
    let r, w = Pipe.create () in
    { available = (fun () -> Pipe.values_available r)
    ; take_now = (fun () -> match Pipe.is_closed r, Pipe.is_empty r with
          | true, true -> `Eof
          | false, true -> `Race
          | _, false -> `Ok ())
    ; add_work = (fun () ->
          Pipe.write_without_pushback w ();
          Staged.stage @@ call_once @@ fun () ->
            match Pipe.read_now r with
            | `Eof | `Nothing_available -> assert false
            | `Ok () -> ())
    ; taken = (fun () -> ())
    ; size = (fun () -> Pipe.length w)
    ; finish = (fun () -> Pipe.close w)
    ; finished = (fun () -> Pipe.closed r >>= fun () -> Pipe.upstream_flushed r >>| fun _ -> ())
    ; call_release = (fun () -> ()) }
  let make_resource e f size =
    let r, w = Pipe.create () in
    List.iter ~f:(Pipe.write_without_pushback w) @@ List.init size ~f:(fun _ -> ());
    { available = (fun () -> Pipe.values_available r)
    ; take_now = (fun () -> match Pipe.read_now r with
          | `Eof -> `Eof
          | `Nothing_available -> `Race
          | `Ok () -> `Ok (call_once @@ fun _ -> Pipe.write_without_pushback w ()))
    ; add_work = (fun () -> assert false)
    ; taken = (fun () -> size - Pipe.length r)
    ; size = (fun () -> f size)
    ; finish = (fun () -> assert false)
    ; finished = (fun () -> Deferred.unit)
    ; call_release = (fun r -> r e)}
  let make_resource' e size =
    match size with
    | None ->
      let taken = ref 0 in
      { available = (fun () -> Deferred.return `Ok)
      ; take_now = (fun () ->
            taken := !taken + 1;
            `Ok (call_once @@ fun _ -> taken := !taken - 1))
      ; add_work = (fun () -> assert false)
      ; taken = (fun () -> !taken)
      ; size = (fun () -> None)
      ; finish = (fun () -> assert false)
      ; finished = (fun () -> Deferred.unit)
      ; call_release = (fun r -> r e) }
    | Some size -> make_resource e (fun x -> Some x) size
  let make_resource e size = make_resource e (fun x -> x) size
  let allocate { available; take_now; _ } =
    Deferred.choice (available ()) @@ function
    | `Eof -> `Eof
    | `Ok -> take_now ()
  let add_work { add_work; _ } = add_work ()
  let taken { taken; _ } = taken ()
  let  size { size; _ } = size ()
  let merge tq1 tq2 =
    { available = (fun () -> Deferred.both (tq1.available ()) (tq2.available ()) >>| function
        | (`Eof, _) | (_, `Eof) -> `Eof
        | (`Ok, `Ok) -> `Ok)
    ; take_now = (fun () ->
          match tq1.take_now () with
          | `Ok r1 -> (match tq2.take_now () with
            | `Ok r2 -> `Ok (r1, r2)
            | `Race -> tq1.call_release r1; `Race
            | `Eof -> tq1.call_release r1;  `Eof)
          | `Race -> `Race
          | `Eof -> `Eof)
    ; add_work = (fun () -> assert false)
    ; taken = (fun () -> tq1.taken (), tq2.taken ())
    ; size = (fun () -> tq1.size (), tq2.size ())
    ; finish = (fun () -> assert false)
    ; finished = (fun () -> Deferred.both (tq1.finished ()) (tq2.finished ()) >>| fun ((), ()) -> ())
    ; call_release = (fun (r1, r2) -> tq1.call_release r1; tq2.call_release r2) }
  let finish { finish; _ } = finish ()
  let finished { finished; _ } = finished ()
end

type host_data =
  { jobs : String.Set.t
  ; wait_for_data : string -> (bool * int) option -> unit Deferred.t }

type compile_unit_data =
  { exec_info          : exec_info
  ; lemma_disseminator : (Time_ns.t * string * bench_response Ivar.t) Pipe.Writer.t
  ; executors          : Counter.t
  ; lemma_count        : unit -> int
  ; abstract_time      : int }

let task_disseminator
    ~alloc_benchers ~request_allocate
    ~error_occurred ~info_stream ~lemma_time
    ~wait_for_data
    ~last_abstract_time =
  let stderr = Writer.pipe @@ Lazy.force Writer.stderr in
  let lemma_time' = Time_ns.Span.of_sec @@ float_of_int lemma_time in
  let data = ref [] in

  let lemma_token_queue = ResourceManager.make_queue () in
  let time_remaining deadline =
    Time_ns.Span.(Time_ns.diff deadline (Time_ns.now ()) > lemma_time') in
  let lemma_disseminator lemmas =
    let lemmas = ref lemmas in
    (Pipe.create_writer @@ fun r ->
     let rec loop () =
       Pipe.read r >>= function
       | `Eof -> assert false
       | `Ok (deadline, lemma, ivar) ->
         match time_remaining deadline, String.Map.find !lemmas lemma with
         | _, None | false, _ ->
           Ivar.fill ivar Skip;
           loop ()
         | true, Some release ->
           Ivar.fill ivar (Bench lemma_time);
           lemmas := String.Map.remove !lemmas lemma;
           release ();
           loop () in
     loop ()
    ), (fun () -> String.Map.length !lemmas) in
  let task_receiver = Pipe.iter_without_pushback info_stream
    ~f:(fun ({exec; args; env; dir; lemmas; time=_ } : pre_bench_info) ->
         let lemmas = List.map lemmas ~f:(fun l ->
             l, Staged.unstage @@ ResourceManager.add_work lemma_token_queue) in
         let lemmas = String.Map.of_alist_exn lemmas in
         if not @@ String.Map.is_empty lemmas then
           let lemma_disseminator, lemma_count = lemma_disseminator lemmas in
           Counter.increase last_abstract_time;
           data := { exec_info = { exec; args; env; dir }
                   ; lemma_disseminator
                   ; executors = Counter.make 0
                   ; lemma_count
                   ; abstract_time = Counter.count last_abstract_time } :: !data) >>| fun () ->
    ResourceManager.finish lemma_token_queue in
  let task_allocator ~prefix ~hostname deadline =
    let rec loop () =
      Deferred.choose
        [ Deferred.Choice.map (ResourceManager.allocate lemma_token_queue) ~f:(fun x -> `Available x)
        ; Deferred.choice (Clock_ns.at (Time_ns.sub deadline lemma_time')) (fun () -> `Out_of_time)
        ; Deferred.choice error_occurred (fun () -> `Error) ] >>= function
      | `Out_of_time | `Available `Eof | `Error ->
        Deferred.return `Stop
      | `Available `Race ->
        Pipe.write stderr "Race condition during task dissemination. Trying again.\n" >>= fun () ->
        loop ()
      | `Available (`Ok ()) ->
        (* TODO: Use a more clever priority queue? *)
        let compare cud1 cud2 = Int.compare (* executors1 / lemma_count1 < executors2 / lemma_count2 *)
            (Counter.count cud1.executors * cud2.lemma_count ())
            (Counter.count cud2.executors * cud1.lemma_count ()) in
        let task = List.min_elt ~compare @@ List.filter !data ~f:(fun { lemma_count; _ } ->
            lemma_count () > 0 && time_remaining deadline) in
        match task with
        | None ->
          Deferred.return `Stop
        | Some { exec_info; executors; lemma_disseminator; abstract_time; _ } ->
          Counter.increase executors;
          wait_for_data ~prefix ~full:false ~hostname ~time:abstract_time >>| fun () ->
          `Task ((fun () -> Counter.decrease executors), exec_info, lemma_disseminator) in
    loop () in
  let allocator =
    let q = ResourceManager.merge request_allocate lemma_token_queue in
    let rec loop cont =
      Deferred.choose
        [ Deferred.Choice.map (ResourceManager.allocate q) ~f:(fun x -> `Go x)
        ; Deferred.choice error_occurred (fun () -> `Error) ] >>= function
      | `Error | `Go `Eof -> cont
      | `Go (`Ok ((ra, rr), ())) ->
        let bench_alloc =
          alloc_benchers
            ~task_allocator
            ~abort:(Deferred.any [error_occurred; ResourceManager.finished lemma_token_queue])
            ~relinquish_alloc_token:(fun () -> ra `Requested)
            ~relinquish_running_token:(fun () -> rr `Total) in
        loop (cont >>= fun () -> bench_alloc)
      | `Go `Race ->
        Pipe.write stderr "Race condition during task dissemination. Trying again.\n" >>= fun () ->
        loop cont
    in
    loop Deferred.unit in
  Deferred.all_unit [task_receiver; allocator]

let filter_lemmas lemma_filter info_stream =
  let mk_lemma_set file =
    Reader.file_lines file >>| fun lemmas ->
    let lemmas = List.map lemmas ~f:String.strip in
    String.Set.of_list lemmas in
  (match lemma_filter with
   | None -> Deferred.return (fun _ -> true)
   | Some (`Include file) ->
     mk_lemma_set file >>| fun lemmas ->
     fun lemma -> String.Set.mem lemmas lemma
   | Some (`Include_list lemmas) ->
     let lemmas = String.Set.of_list lemmas in
     return (fun lemma -> String.Set.mem lemmas lemma)
   | Some (`Exclude file) ->
     mk_lemma_set file >>| fun lemmas ->
     fun lemma -> not @@ String.Set.mem lemmas lemma
   | Some (`Exclude_list lemmas) ->
     let lemmas = String.Set.of_list lemmas in
     return (fun lemma -> not @@ String.Set.mem lemmas lemma)
   | Some (`IncludeRegexp regexp) ->
     let regexp = Str.regexp regexp in
     return (fun lemma -> Str.string_match regexp lemma 0)
   | Some (`ExcludeRegexp regexp) ->
     let regexp = Str.regexp regexp in
     return (fun lemma -> not @@ Str.string_match regexp lemma 0))
  >>| fun filter ->
  Pipe.map info_stream ~f:(fun ({lemmas; _ } as info : pre_bench_info) ->
      { info with lemmas = List.filter lemmas ~f:filter })

let main
    ~lemma_filter
    ~injections_extra
    ~scratch
    ~delay_benchmark
    ~bench_allocator ~compile_allocator ~max_requests ~max_running
    ~benchmark_data
    ~benchmark_target
    ~benchmark_repo
    ~benchmark_commit
    ~lemma_time
    ~packages
    ~shared_filesystem
    ~resume
  =
  let (/) = Filename.concat in
  Process.run
    ~working_dir:benchmark_data
    ~prog:"git"
    ~args:["pull"] () >>=? fun out ->
  let stdout = Lazy.force Writer.stdout in
  Writer.write stdout out;
  prepare_data_dir ~benchmark_data ~benchmark_commit ~lemma_time >>= fun data_dir ->
  with_log_writer ~append:resume (data_dir/"error.log") @@ fun error_log ->
  let error_writer, error_occurred = error_handler error_log in
  (if resume then
     Sys.file_exists (data_dir/"combined.bench") >>= function
     | `No | `Unknown ->
       Pipe.write error_writer (Error.of_string "No existing benchmark could be found to resume") >>| fun () ->
       None
     | `Yes ->
       Reader.file_lines (data_dir/"combined.bench") >>= fun lines ->
       let already_done = List.filter_map ~f:(fun line -> List.hd @@ String.split ~on:'\t' line) lines in
       (if List.is_empty already_done then
         Pipe.write error_writer (Error.of_string "The previous benchmark was empty, nothing to resume")
         else Deferred.unit) >>| fun () ->
       Some (`Exclude_list already_done)
   else
     Deferred.return None) >>= fun resume_filter ->
  (with_log_pipe ~append:resume (data_dir/"opam-out.log") @@ fun opam_out ->
   with_log_pipe ~append:resume (data_dir/"opam-err.log") @@ fun opam_err ->
   with_log_pipe ~append:resume (data_dir/"opam-timings.log") @@ fun opam_timings ->
   with_log_pipe ~append:resume (data_dir/"coq-out.log") @@ fun coq_out ->
   with_log_pipe ~append:resume (data_dir/"coq-err.log") @@ fun coq_err ->
   with_log_pipe ~append:resume (data_dir/"processor-out.log") @@ fun processor_out ->
   with_log_pipe ~append:resume (data_dir/"processor-err.log") @@ fun processor_err ->
   with_log_writer ~append:resume (data_dir/"combined.bench") @@ fun bench_log ->
   write_injections ~data_dir ~injections_extra >>= fun () ->
   write_bench_params ~scratch >>= fun extra_args ->

   let last_abstract_time = Counter.make 1 in
   let self = Unix.gethostname () in
   let data_host = ref @@ self in
   let data_host_prefix = ref "" in
   let data_host_rsyncs_active = ref @@ String.Map.singleton !data_host Deferred.unit in
   let hosts = ref String.Map.empty in
   let copier target =
     let reqs = Mvar.create () in
     let update = Bvar.create () in
     let rec wait_for_time t =
       Bvar.wait update >>= fun tcurr ->
       if tcurr >= t then Deferred.unit else wait_for_time t in
     let host_abstract_time = ref 0 in
     let rec loop () =
       (Mvar.take reqs >>= fun (prefix, time) ->
        match time with
        | None ->
          (host_abstract_time := 0;
           (* We have to run this in a loop because in error conditions, sometimes Coq processes are
              still running on remote nodes that prevent files from being deleted. *)
           let prog, args = prefix_invocation prefix ["rm"; "-rf"; scratch] in
           debug_output ("removing data from node " ^ target ^ "\nInvocation: " ^ prog ^ " " ^
                         String.concat ~sep:" " args);
           let rec loop i =
             Process.run ~prog ~args () >>= function
             | Error e -> if i = 0 then Pipe.write error_writer e else begin
                 Writer.write stdout ("Could not delete " ^ target ^ ":" ^ scratch ^ ". Trying again");
                 Clock.after (Time.Span.of_sec 10.) >>= fun () -> loop (i - 1)
               end
             | Ok _out -> Deferred.unit in
           loop 10)
        | Some (full, t) ->
          if not full && !host_abstract_time >= t then Deferred.unit else
            let synced_time = Counter.count last_abstract_time in
            let data_host = !data_host in
            let data_host_prefix = !data_host_prefix in
            (if not @@ String.equal target data_host then begin
                let exclude =
                  if full then [ "*.vo.bench-*" ] else
                    [ "opam-root/bench/.opam-switch/sources"
                    ; "opam-root/bench/.opam-switch/build/coq.*"
                    ; "opam-root/bench/.opam-switch/build/ocaml-base-compiler.*"
                    ; "opam-root/bench/.opam-switch/build/dune.*"
                    ; "opam-root/bench/.opam-switch/build/dose3.*"
                    ; "opam-root/download-cache"
                    ; "opam-root/repo"
                    ; "*.vo.bench-*"
                    ; "*.glob"
                    ; "*.aux"] in
                let exclude = List.concat @@ List.map ~f:(fun d -> ["--exclude"; d]) exclude in
                let copydir = scratch^"/" in
                let prog, args =
                  if String.equal target self then
                    let rsync_remote_shell = "rsync-preprocess " ^ data_host_prefix ^ " rsync-preprocess-marker" in
                    prefix_invocation ""
                      ([ "rsync"; "-e"; rsync_remote_shell; "-qa"] @ exclude @
                       [ data_host^":"^copydir; copydir])
                  else
                    let rsync_remote_shell = "rsync-preprocess " ^ prefix ^ " rsync-preprocess-marker" in
                    prefix_invocation data_host_prefix
                      ([ "rsync"; "-e"; rsync_remote_shell; "-qa"] @ exclude @
                       [ copydir; target^":"^copydir]) in
                debug_output ("Rsyncing from data host " ^ data_host ^ " to " ^ target ^
                              " at time " ^ string_of_int t ^ "/" ^
                              string_of_int (Counter.count last_abstract_time) ^ "\nInvocation: " ^ prog ^ " " ^
                              String.concat ~sep:" " args);
                let prsync = Process.create ~prog ~args () >>= fun p ->
                  match p with
                  | Error e ->
                    if Deferred.is_determined error_occurred then Deferred.unit else Pipe.write error_writer e
                  | Ok p ->
                    Deferred.upon error_occurred (fun () -> Signal.send_i Signal.int (`Pid (Process.pid p)));
                    Process.collect_output_and_wait p >>= fun Process.Output.{ stderr; exit_status; _ } ->
                    match exit_status with
                    | Ok () -> return ()
                    | Error (`Exit_non_zero 24) ->
                      Pipe.write processor_err stderr
                    | Error _ ->
                      if Deferred.is_determined error_occurred then Deferred.unit else Pipe.write error_writer @@
                        Error.createf "Rsync failed: %s" stderr
                in
                data_host_rsyncs_active := String.Map.update !data_host_rsyncs_active data_host ~f:(function
                    | None -> assert false
                    | Some m -> prsync >>= fun _ -> m);
                prsync
              end
             else Deferred.unit) >>| fun () ->
            host_abstract_time := synced_time)
       >>= fun () ->
       Bvar.broadcast update !host_abstract_time;
       loop ()
     in
     (* TODO: This is most likely a memory leak, because the loop never stops *)
     don't_wait_for (loop ());
     fun prefix time' ->
       Mvar.update reqs ~f:(fun x ->
           match x, time' with
           | None, _ | Some (_, None), _ -> prefix, time'
           | Some (_, Some _), None -> assert false
           | Some (_, Some (full, time)), Some (full', time') -> prefix, Some (full || full', Int.max time time'));
       match time' with
       | None ->
         Bvar.wait update >>| fun _ -> ()
       | Some (_, t) -> wait_for_time t in
   let copier = if shared_filesystem then fun _ _ _ -> Deferred.unit else copier in
   let add_job ~job_name ~hostname =
     hosts := String.Map.update !hosts hostname ~f:(function
         | None -> { jobs = String.Set.singleton job_name
                   ; wait_for_data = copier hostname }
         | Some ({ jobs; _ } as data) -> { data with jobs = String.Set.add jobs job_name });
   in
   let remove_job ~prefix ~job_name ~hostname =
     let { jobs; wait_for_data } = String.Map.find_exn !hosts hostname in
     let jobs = String.Set.remove jobs job_name in
     hosts := String.Map.set !hosts ~key:hostname ~data:{ jobs; wait_for_data };
     if String.Set.is_empty jobs then wait_for_data prefix None else Deferred.unit in
   let with_job ~prefix ~job_name ~hostname f =
     add_job ~job_name ~hostname;
     Monitor.protect ~finally:(fun () ->
         remove_job ~prefix ~job_name ~hostname)
       f in
   let wait_for_data ~prefix ~full ~hostname ~time =
     (String.Map.find_exn !hosts hostname).wait_for_data prefix (Some (full, time)) in
   let switch_data_host ~new_prefix ~new_host =
     let old_host = !data_host in
     wait_for_data ~prefix:new_prefix ~full:true ~hostname:new_host ~time:(Counter.count last_abstract_time)
     >>= fun () ->
     data_host_rsyncs_active := String.Map.set !data_host_rsyncs_active ~key:new_host ~data:Deferred.unit;
     data_host := new_host;
     data_host_prefix := new_prefix;
     String.Map.find_exn !data_host_rsyncs_active old_host in

   (* This job is running the entire session *)
   let main_job = "main_job" in
   add_job ~job_name:main_job ~hostname:self;

   compile_and_retrieve_benchmark_info
     ~error_writer ~error_occurred
     ~compile_allocator
     ~opam_out ~opam_err ~opam_timings
     ~scratch
     ~benchmark_target
     ~benchmark_repo
     ~benchmark_commit
     ~packages
     ~injections_extra
     ~add_job ~remove_job ~switch_data_host
     ~final_data_prefix:!data_host_prefix ~final_data_host:!data_host
   >>= function
   | Error e ->
     Pipe.write error_writer e
   | Ok (info_stream, cont) ->
     (* Filter out a super-annoying special case in coq-gappa where a Coq file is being generated and promptly
        deleted again. *)
     let info_stream = Pipe.filter ~f:(fun { dir; args; _ } ->
         not (String.is_substring dir ~substring:"coq-gappa") &&
         Array.exists ~f:(String.equal "conftest.v") args) info_stream in
     let info_stream = Pipe.map info_stream ~f:(fun ({ args; _ } as info) ->
         { info with args = Array.append args extra_args }) in
     filter_lemmas lemma_filter info_stream >>= fun info_stream ->
     filter_lemmas resume_filter info_stream >>= fun info_stream ->
     let info_stream, reporter_stream = Pipe.fork ~pushback_uses:`Fast_consumer_only info_stream in
     let resources_requested_queue = ResourceManager.make_resource `Requested max_requests in
     let resources_total_queue = ResourceManager.make_resource' `Total max_running in
     let jobs_running = Counter.make 0 in
     let reporter, summarize = reporter ~lemma_time ~info_stream:reporter_stream ~bench_log
         ~resources_requested:(fun () -> ResourceManager.taken resources_requested_queue)
         ~resources_total:(fun () -> ResourceManager.taken resources_total_queue)
         ~jobs_running:(fun () -> Counter.count jobs_running) in
     (if delay_benchmark then cont else Deferred.unit) >>= fun () ->
     let job_starter ~task_allocator ~job_time ~prefix ~job_name =
       Counter.increase jobs_running;
       run_processor
         ~prefix
         ~error_writer ~error_occurred
         ~task_allocator
         ~reporter ~coq_out ~coq_err ~processor_out ~processor_err
         ~job_time ~job_name with_job >>| fun () ->
       Counter.decrease jobs_running in
     let alloc_benchers ~task_allocator ~abort =
       alloc_benchers
         ~task_allocator
         ~abort ~error_writer
         ~bench_allocator ~job_starter
         ~benchmark_commit
         ~processor_err in
     let request_allocate = ResourceManager.merge resources_requested_queue resources_total_queue in
     task_disseminator
       ~alloc_benchers
       ~request_allocate
       ~error_occurred
       ~info_stream ~lemma_time
       ~wait_for_data
       ~last_abstract_time >>= fun () ->
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
  Unix.mkdtemp (parent/"tactician") >>= fun d ->
  try_with (fun () -> cont d) >>= fun res ->
  rmrf d >>| fun () -> match res with
  | Ok x -> x
  | Error e -> raise e

let compile_injection_string ~injection_strings ~injection_files =
  Deferred.List.concat_map ~f:Reader.file_lines injection_files >>| fun file_lines ->
  file_lines@injection_strings

let command =
  Log.Global.set_level `Error;
  let open CommandLetSyntax in
  Command.async_or_error
    ~readme:(fun () -> {|
The distributed benchmarking system of Tactician.
Input:
  - The repository and commit hash of a particular version of Tactician (or one of it's model plugins).
  - A set of Opam packages to benchmark on that particular commit.
  - Parameters governing which lemmas to benchmark and how long.
  - A way to allocate resources either locally, remotely through ssh, or through a HPC cluster.
Output:
  - Every requested lemma will be benchmarked, and the results will be uploaded to the 'benchmark-data'
    repository of the coq-tactician Github organization.
Usage Details:
  For details, please read the description of the flags below.
Examples:
  - Running a benchmark of coq-tactician-stdlib on a single computer with a time limit of 40 seconds.
    A maximum of 16 cores are used for benchmarking (as ensured by '-maxrequests 16' and '-max-running 16').
    Additionally, the flag '-delay-benchmark' ensures that benchmarking only starts after the initial
    compilation of Opam packages has finished. This ensures hat the CPU cycles taken by the initial
    compilation does not interfere with the benchmarking.
    tactician-benchmark \
        -benchmark-data ./benchmark-data/ \
        -compile-allocator ./benchmark-system/local/compile_allocator \
        -bench-allocator ./benchmark-system/local/bench_allocator \
        -max-requests 16 \
        -max-running 16 \
        -delay-benchmark \
        -benchmark-target coq-tactician \
        -benchmark-repo git+ssh://git@github.com/coq-tactician/coq-tactician \
        -benchmark-commit f267afe31ab82b071d3f03d08bc4a6595c30b1d2 \
        -benchmark-time 40 \
        coq-tactician-stdlib
  - Running a benchmark of coq-tactician-stdlib on a SLURM cluster with a time limit of 40 seconds. A maximum of
    28 resource requests are allowed to be submitted to the SLURM queue at the same time. There is no limit on
    the number of simultaneous allocated resources.
    tactician-benchmark \
        -benchmark-data ./benchmark-data/ \
        -compile-allocator ./benchmark-system/slurm/compile_allocator \
        -bench-allocator ./benchmark-system/slurm/bench_allocator \
        -max-requests 28 \
        -benchmark-target coq-tactician \
        -benchmark-repo git+ssh://git@github.com/coq-tactician/coq-tactician \
        -benchmark-commit f267afe31ab82b071d3f03d08bc4a6595c30b1d2 \
        -benchmark-time 40 \
        coq-tactician-stdlib
|})
    ~summary:"Benchmark Tactician"
    (let tmp_dir = flag "tmp-dir" (map_flag (optional string) ~f:(Option.map ~f:(fun x -> `Tmp x)))
         ~doc:"dir Location in which a temporary directory will be created to store the build. If not supplied, it is taken from $TMPDIR. \
               After the benchmark is finished, the directory is cleaned up. Mutually exclusive with -build-dir."
     and build_dir = flag "build-dir" (map_flag (optional string) ~f:(Option.map ~f:(fun x -> `Build x)))
         ~doc:"dir Location of the build. This directory will not be cleaned up after the benchmark finishes. \
               Mutually exclusive with -tmp-dir."
     and lemmas_include = flag "include" (map_flag (optional string) ~f:(Option.map ~f:(fun x -> `Include x)))
         ~doc:"file A file containing a line-separated list of lemma names that should be benchmarked. Other lemmas \ 
              are ignored."
     and lemmas_exclude = flag "exclude" (map_flag (optional string) ~f:(Option.map ~f:(fun x -> `Exclude x)))
         ~doc:"file A file containing a line-separated list of lemma names that should not benchmarked. Thos lemmas \
               are ignored."
     and lemmas_include_regexp = flag "include-regexp"
         (map_flag (optional string) ~f:(Option.map ~f:(fun x -> `IncludeRegexp x)))
         ~doc:"regexp A regular expression matching lemmas. Only lemmas matching the expression are benchmarked."
     and lemmas_exclude_regexp = flag "exclude-regexp"
         (map_flag (optional string) ~f:(Option.map ~f:(fun x -> `ExcludeRegexp x)))
         ~doc:"regexp A regular expression matching lemmas. Only lemmas not matching the expression are benchmarked."
     in
     let+ loc = choose_one [tmp_dir; build_dir] ~if_nothing_chosen:If_nothing_chosen.Return_none
     and+ lemma_filter = choose_one [lemmas_include; lemmas_exclude; lemmas_include_regexp; lemmas_exclude_regexp]
         ~if_nothing_chosen:If_nothing_chosen.Return_none
     and+ delay_benchmark = flag "delay-benchmark" no_arg
         ~doc:"Delay the benchmark until the initial build is fully complete. Useful when the build process may interfere with the benchmark timings."
     and+ injection_strings = flag "inject" (listed string)
         ~doc:"vernacular Inject Coq vernacular into the compilation and benchmarking process. \
               Typically used to specify options. Can be repeated multiple times and combined with -inject-file."
     and+ injection_files = flag "inject-file" (listed string)
         ~doc:"file Inject a file containing Coq vernacular into the compilation and benchmarking process. \
               Typically used to specify options. Can be repeated multiple times and combined with -inject-file."
     and+ debug = flag "debug" no_arg
         ~doc:"Show additional debug info on stderr."
     and+ benchmark_data = flag "benchmark-data" (required string)
         ~doc:"dir Location of the benchmark-data storage repository."
     and+ benchmark_target = flag "benchmark-target" (required string)
         ~doc:"string Name of the Opam package containing the model that should be benchmarked. This can be any \
              package that depends on coq-tactician."
     and+ benchmark_repo = flag "benchmark-repo" (required string)
         ~doc:"URI The repository where the benchmark target can be found. Accepts any URI that is accepted by Opam."
     (* TODO: Convert branches to commits *)
     and+ benchmark_commit = flag "benchmark-commit" (required string)
         ~doc:"hash The hash of the commit that should be benchmarked."
     and+ lemma_time = flag "benchmark-time" (required int)
         ~doc:"int Time limit in seconds for synthesizing an individual lemma."
     and+ compile_allocator = flag "compile-allocator" (required string)
         ~doc:"executable An executable file that allocates computational resources for the initial compilation \
               of Opam packages. When executed, this file should request the needed resources, which can come either \
               from the local machine, from a remote machine through ssh, or from a HPC cluster scheduler. When \
               the resources have been allocated, the executable should output the prefix of a command that can be \
               executed in order to access the allocated resources on stdout.
               Example prefixes:
               - For local resources, this prefix would be the empty string
               - For a remote node accessed through ssh, the prefix would be 'ssh remote-node'
               - For a SLURM cluster, where a job id has been allocated, the prefix would be \
               'srun --ntasks=1 --jobid $SLURM_JOB_ID'
               After benchmarking system is finished with the resources, it will print 'done' to the stdin of the \
               executable. At that point, the executable should relinquish the allocated resources and exit."
     and+ bench_allocator = flag "bench-allocator" (required string)
         ~doc:"executable An executable file that allocates computational resources for benchmarking. This file \
              may be executed many times, as governed by the flags '-max-requests' and '-max-running'. When \
              executed, this file should request the needed resources to run one or more Coq processes that \
              benchmark lemmas for a given amount of time. Similar to '-compile-allocator', these resources can be \
              allocated local, remote through ssh, or through a HPC cluster scheduler. When the resources have \
              been allocated, the executable should first output a line to stdout indicating the number of \
              minutes the resources can be used. The benchmarking system may not be able to relinquish the \
              resources exactly within that time. A safety margin of ~15 minutes is recommended. Then the \
              executable should output the prefixes of commands that can be executed in order to access the \
              allocated resources (see '-compile-allocator' for examples). A single outputted prefix will be \
              used to run a single Coq benchmarking process. This series of prefixes should be followed by the \
              string 'done'. After the bencharking system is finished with the resources it will print 'done' to \
              the stdin of the executable. At that point the executable should relinquish the allocated resources \
              and exit."
     and+ max_requests = flag "max-requests" (required int)
         ~doc:"int The maximum number of benchmark resource allocations that may be running at the same time. \
               Related to '-bench-allocator'. This governs how much load may be applied to queueing mechanism \
               of the underlying HPC cluster. The higher this number, the more resources will be requested at \
               the same time. When this number is low, the benchmarking system may be starved of resources on \
              busy clusters. When this number is high, this may flood the underlying queue, which may annoy other \
              users of the cluster. NOTE: This flag does not limit the total number of resources allocated. For \
              that, see '-max-running'."
     and+ max_running = flag "max-running" (optional int)
         ~doc:"int The maximum number of benchmark resources requests that can be requested and running at the same \
               time. See also '-max-requests'. WARNING: This number is infinite by default, which is likely not \
               appropriate for local resources."
     and+ shared_filesystem = flag "shared-filesystem" no_arg
         ~doc:"Assume that the scratch directory is hosted on a shared filesystem. No copying of files between hosts \
               is performed."
     and+ resume = flag "resume" no_arg
         ~doc:"Look up the previous (partial) benchmark with the same parameters as specified in the current \
               invocation, and finish that benchmark appending old the existing log files."
     and+ packages = anon (non_empty_sequence_as_list ("package" %: string))
     in fun () ->
       enable_debug := debug;
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
         compile_injection_string ~injection_strings ~injection_files >>= fun injections_extra ->
         main
           ~lemma_filter
           ~injections_extra
           ~scratch ~delay_benchmark
           ~bench_allocator ~compile_allocator ~max_requests ~max_running
           ~benchmark_data ~benchmark_target ~benchmark_repo ~benchmark_commit ~lemma_time ~packages
           ~shared_filesystem ~resume)

(* TODO: Use brwap to sandbox to the scratch directory *)
let () =
  match Rpc_parallel__.Utils.whoami () with
  | `Master ->
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
  | `Worker ->
    Rpc_parallel.start_app command

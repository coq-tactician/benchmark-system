
open Core
open Async

(**
   Stupid async library does not let us spawn a process with a socket as stdin, so we have to write it ourselves.
   Most of this code is adapted from the Async and Core libraries *)

let create_process_internal
  :  working_dir : string option
    -> prog        : string
    -> argv        : string list
    -> env         : string list
    -> Core.Unix.Process_info.t
  =
  fun ~working_dir ~prog ~argv ~env ->
  let close_on_err = ref [] in
  let safe_pipe () =
    let (fd_read, fd_write) as result = Spawn.safe_pipe () in
    close_on_err := fd_read :: fd_write :: !close_on_err;
    result
  in
  try
    let in_read, in_write  = Caml_unix.(socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true)
    in
    close_on_err := in_read :: in_write :: !close_on_err;
    let out_read, out_write = safe_pipe () in
    let err_read, err_write = safe_pipe () in
    let pid =
      Spawn.spawn
        ?cwd:(Option.map working_dir ~f:(fun x -> Spawn.Working_dir.Path x))
        ~prog
        ~argv
        ~env:(Spawn.Env.of_list env)
        ~stdin:in_read
        ~stdout:out_write
        ~stderr:err_write
        ()
      |> Pid.of_int
    in
    Core.Unix.close in_read; Core.Unix.close out_write; Core.Unix.close err_write;
    { pid; stdin = in_write; stdout = out_read; stderr = err_read; }
  with exn ->
    List.iter !close_on_err ~f:(fun x -> try Core.Unix.close x with _ -> ());
    raise exn

module Execvp_emulation : sig
  (* This is a reimplementation of execvp semantics with two main differences:
     - it does [spawn] instead of [execve] and returns its result on success
     - it checks file existence and access rights before trying to spawn.
       This optimization is valuable because a failed [spawn] is much more expensive than a
       failed [execve]. *)
  val run
    :  working_dir : string option
    -> spawn       : (prog:string -> argv:string list -> 'a)
    -> prog        : string
    -> args        : string list
    -> ?prog_search_path : string list
    -> ?argv0      : string
    -> unit
    -> 'a

end = struct

  let get_path prog_search_path =
    (match prog_search_path with
     | Some [] -> invalid_arg "Core.Unix.create_process: empty prog_search_path"
     | Some dirs -> dirs
     | None -> Sys.getenv "PATH"
               |> Option.value_map ~f:(String.split ~on:':') ~default:["/bin"; "/usr/bin"]
               |> List.map ~f:(function
                 | "" -> "."
                 | x  -> x))
  ;;

  let candidate_paths ?prog_search_path prog =
    (* [assert] is to make bugs less subtle if we try to make this
       portable to non-POSIX in the future. *)
    assert (String.equal Filename.dir_sep "/");
    if String.contains prog '/' then
      [ prog ]
    else
      List.map (get_path prog_search_path) ~f:(fun h -> h ^/ prog)
  ;;

  type 'a spawn1_result =
    | Eaccess           of exn
    | Enoent_or_similar of exn
    | Ok                of 'a

  let run ~working_dir ~spawn ~prog ~args ?prog_search_path ?argv0 () =
    let argv = (Option.value argv0 ~default:prog)::args in
    let spawn1 candidate =
      match
        (try
           Caml_unix.access
             (if not (Filename.is_relative candidate) then
                candidate
              else
                match working_dir with
                | Some working_dir -> working_dir ^/ candidate
                | None -> candidate)
             [Caml_unix.X_OK]
         with Caml_unix.Unix_error (code, _, args) ->
           raise (Caml_unix.Unix_error (code, "Core.Unix.create_process", args)));
        spawn ~prog:candidate ~argv
      with
      | exception Caml_unix.Unix_error (ENOEXEC, _, _) -> Ok (
        (* As crazy as it looks, this is what execvp does. It's even documented in the man
           page. *)
        spawn
          ~prog:"/bin/sh"
          ~argv:("/bin/sh" :: candidate :: args))
      | exception (Caml_unix.Unix_error (EACCES, _, _) as exn) ->
        Eaccess exn
      | exception (Caml_unix.Unix_error (
        (* This list of nonfatal errors comes from glibc and openbsd implementations of
           execvpe, as collected in [execvpe_ml] function in ocaml (see
           otherlibs/unix/unix.ml in https://github.com/ocaml/ocaml/pull/1414). *)
        (EISDIR|ELOOP|ENAMETOOLONG|ENODEV|ENOENT|ENOTDIR|ETIMEDOUT), _, _) as exn) ->
        Enoent_or_similar exn
      | pid -> Ok pid
    in
    let rec go first_eaccess = function
      | [] -> assert false (* [candidate_paths] can't return an empty list *)
      | [ candidate ] ->
        (match spawn1 candidate with
         | Eaccess exn
         | Enoent_or_similar exn ->
           raise (Option.value first_eaccess ~default:exn)
         | Ok pid -> pid)
      | candidate :: (_ :: _ as candidates) ->
        match spawn1 candidate with
        | Eaccess exn ->
          let first_eaccess = Some (Option.value first_eaccess ~default:exn) in
          go first_eaccess candidates
        | Enoent_or_similar _exn ->
          go first_eaccess candidates
        | Ok pid -> pid
    in
    go None (candidate_paths ?prog_search_path prog)
  ;;
end


let create_process_env ?working_dir ?prog_search_path ?argv0 ~prog ~args ~env () =
  let env_assignments = Core.Unix.Env.expand env in
  Execvp_emulation.run
    ~prog
    ~args
    ?argv0
    ?prog_search_path
    ~working_dir
    ~spawn:(fun ~prog ~argv ->
      create_process_internal
        ~working_dir
        ~prog
        ~argv
        ~env:env_assignments)
    ()

type t =
  { pid : Pid.t
  ; stdout : Reader.t
  ; stderr : Reader.t
  ; sock_in : Writer.t
  ; sock_out : Reader.t
  ; prog : string
  ; args : string list
  ; working_dir : string option
  ; env : Process.env
  ; wait : Unix.Exit_or_signal.t Deferred.t Lazy.t
  }
[@@deriving fields, sexp_of]

let create
      ?buf_len
      ?(env = `Extend [])
      ?working_dir
      ~prog
      ~args
      ()
  =
  match%map
    In_thread.syscall ~name:"create_process_env" (fun () ->
      create_process_env
        ~prog
        ~args
        ~env
        ?working_dir
        ())
  with
  | Error exn -> Or_error.of_exn exn
  | Ok { pid; stdin; stdout; stderr } ->
    let create_fd name file_descr =
      Fd.create
        Fifo
        file_descr
        (Info.create
           "child process"
           ~here:[%here]
           (name, `pid pid, `prog prog, `args args)
           [%sexp_of:
             string * [ `pid of Pid.t ] * [ `prog of string ] * [ `args of string list ]])
    in
    let sock = create_fd "sock" stdin in
    let t =
      { pid
      ; stdout = Reader.create ?buf_len (create_fd "stdout" stdout)
      ; stderr = Reader.create ?buf_len (create_fd "stderr" stderr)
      ; sock_in = Writer.create ?buf_len sock
      ; sock_out = Reader.create ?buf_len sock
      ; prog
      ; args
      ; working_dir
      ; env
      ; wait = lazy (Unix.waitpid_prompt pid)
      }
    in
    Ok t

let collect_output_and_wait t =
  let stdout = Reader.contents t.stdout in
  let stderr = Reader.contents t.stderr in
  let%bind () = Reader.close t.sock_out in
  let%bind () = Writer.close t.sock_in ~force_close:(Deferred.never ()) in
  let%bind exit_status = force t.wait in
  let%bind stdout = stdout in
  let%bind stderr = stderr in
  return { Process.Output.stdout; stderr; exit_status }

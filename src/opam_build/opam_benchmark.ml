
let repos =
  [ "custom-archive", "git+ssh://git@github.com/LasseBlaauwbroek/custom-archive.git"
  ; "coq-core-dev", "https://coq.inria.fr/opam/core-dev"
  ; "coq-extra-dev", "https://coq.inria.fr/opam/extra-dev"
  ; "coq-released", "https://coq.inria.fr/opam/released" ]

let tactician_package = OpamPackage.Name.of_string "coq-tactician"
let tactician_stdlib_package = OpamPackage.Name.of_string "coq-tactician-stdlib"

let opam_init
    ~root_dir
    () =
  OpamClientConfig.opam_init
    ~root_dir
    ~yes:(Some true)
    ~no_env_notice:true
    ~keep_build_dir:true
    ()

let init_root () =
  let repos =
    List.map (fun (name, url) -> OpamRepositoryName.of_string name, (OpamUrl.of_string url, None)) repos @
    [ OpamRepositoryName.of_string "default", (OpamInitDefaults.repository_url, None) ] in
  let init_config =
    OpamFile.InitConfig.with_repositories repos @@
    OpamInitDefaults.init_config ~sandboxing:false () in
  OpamClient.init
      ~init_config
      ~interactive:false
      ~update_config:false
      ~env_hook:false
      ~completion:false
      ~check_sandbox:true
      OpamStd.Sys.SH_bash

let inject ~gt ~st =
  let wrap_command = "[\"tactician\" \"exec\" \"--\"] {coq-tactician:installed}" in
  let pre_build_command = "[\"%{coq-tactician:lib}%/tactician-patch\" name version] {coq-tactician:installed}" in
  let open OpamConfigCommand in
  let config_add_remove gt ?st name value =
    let st = set_opt_switch gt ?st name (`Remove value) in
    set_opt_switch gt ?st name (`Add value) in
  let st = config_add_remove gt ~st "wrap-build-commands" wrap_command in
  let st = config_add_remove gt ?st "pre-build-commands" pre_build_command in
  Option.get st

let enable_bench ~st ~port ~injections_extra =
  let nv = OpamSwitchState.get_package st tactician_package in
  let opam = OpamSwitchState.opam st nv in
  let env = OpamPackageVar.resolve ~opam st in
  let share_var = OpamVariable.(Full.create tactician_package (of_string "share")) in
  let share = OpamFilter.ident_string env (OpamFilter.ident_of_var share_var) in
  let (/) = Filename.concat in

  let bench_dir = share/"plugins"/"zzz-benchmark" in
  if not (Sys.file_exists bench_dir) then
    Sys.mkdir bench_dir 0o755;

  let prebench_file = bench_dir/"PreBench.v" in
  let prebench_contents = "Set Tactician Prebench Port " ^ string_of_int port ^ "." in
  OpamFilename.write (OpamFilename.of_string prebench_file) prebench_contents;

  let injections_extra_file = bench_dir/"Injections.v" in
  let injections_extra_contents = String.concat "\n" injections_extra in
  OpamFilename.write (OpamFilename.of_string injections_extra_file) injections_extra_contents;

  let injection_file = bench_dir/"injection-flags" in
  let injection_flags = ["-l"; prebench_file] in
  let injection_contents = String.concat " " (injection_flags @ ["-l"; injections_extra_file]) in
  OpamFilename.write (OpamFilename.of_string injection_file) injection_contents;
  injection_flags

let timing () =
  let open Core in
  let before = Time_ns.now () in
  fun () ->
    let after = Time_ns.now () in
    Time_ns.abs_diff before after

let build_switch_for_benchmark
    ~gt ~rt ~port ~benchmark_target ~benchmark_url ~packages ~injections_extra =
  let switch = OpamSwitch.of_string "bench" in
  let packages = List.map OpamFormula.atom_of_string packages in
  let do_actions st =
    let target_install_time = timing () in
    let name = OpamPackage.Name.of_string benchmark_target in
    let url = OpamUrl.of_string benchmark_url in
    let st = OpamPinCommand.source_pin st name (Some url) in
    let st = OpamClient.install st [tactician_package, None; name, None] in
    let target_install_time = target_install_time () in

    let st = inject ~gt ~st in

    let deps_install_time = timing () in
    (* If coq-tactician-stdlib is not requested for benchmarking, we check wether it can be installed.
       (Not so on newer versions of Coq, because the package is not needed). If so, it will be pre-installed. *)
    let st = if List.exists (OpamPackage.Name.equal tactician_stdlib_package) @@ List.map fst packages then st else
        let request = OpamSolver.request ~install:[tactician_stdlib_package, None] () in
        let stdlib_installable =
          OpamSolution.resolve st Install
            (* ~orphans:OpamPackage.Set.empty *)
            ~requested:(OpamPackage.Set.empty)
            request in
        match stdlib_installable with
        | OpamTypes.Success _ ->
          OpamClient.install st [tactician_stdlib_package, None]
        | OpamTypes.Conflicts _ -> st in

    let st = OpamClient.install st ~deps_only:true packages in
    let deps_install_time = deps_install_time () in

    enable_bench ~st ~port ~injections_extra, fun () ->
      let subject_install_time = timing () in
      let st = OpamClient.install st packages in
      let subject_install_time = subject_install_time () in

      let timings = `Target_install_time target_install_time, `Deps_install_time deps_install_time, `Subject_install_time subject_install_time in
      st, timings
  in
  if OpamGlobalState.switch_exists gt switch then
    OpamSwitchState.with_ `Lock_write ~rt ~switch gt @@ fun st ->
    let st = OpamClient.remove ~autoremove:false ~force:false st packages in
    do_actions st
  else
    let (), st = OpamSwitchCommand.create gt ~rt
      ~update_config:false
      ~invariant:OpamFormula.Empty
      switch
      (fun st ->
         (), st) in
    do_actions st

let compile_for_benchmark
    ~port
    ~root_dir
    ~benchmark_target
    ~benchmark_url
    ~packages
    ~injections_extra
  =
  let total_install_time = timing () in
  opam_init
    ~root_dir:(OpamFilename.Dir.of_string root_dir)
    ();
  let gt, rt, _ = init_root () in
  let inj_flags, cont = build_switch_for_benchmark ~gt ~rt ~port
    ~benchmark_target
    ~benchmark_url
    ~packages
    ~injections_extra in
  inj_flags, fun () ->
    let st, (a, b, c) = cont () in
    let total_install_time = total_install_time () in
    st, (`Total_install_time total_install_time, a, b, c)

open Core
open Async

type pre_bench_info =
  { exec   : string
  ; args   : string array
  ; env    : string array
  ; dir    : string
  ; lemmas : string list
  ; time   : float }
[@@deriving bin_io]

exception CompilationError

let rec remove_sequence ~seq ls =
  match ls with
  | [] -> raise_s (Async.Sexp.of_string "Sequence not found in list")
  | x::ls' ->
    if List.is_prefix ~prefix:seq ~equal:String.equal ls
    then List.drop ls (List.length seq)
    else x :: remove_sequence ~seq ls'

let post_process_info to_remove ({ args; _ } as info) =
  let args = Array.of_list @@ remove_sequence ~seq:to_remove @@ Array.to_list args in
  { info with args }

let compile_and_retrieve_benchmark_info
    ~root_dir
    ~benchmark_target
    ~benchmark_url
    ~packages
    ~injections_extra
  : (pre_bench_info Pipe.Reader.t * _ Deferred.t) Deferred.t =
  let r, w = Pipe.create () in
  Async_unix.Tcp.Server.create ~on_handler_error:`Raise
    Tcp.Where_to_listen.of_port_chosen_by_os
    (fun _ r _ ->
       Reader.read_marshal r >>= function
       | `Eof -> raise CompilationError
       | `Ok info ->
         (* We wait for the process that sent us the info to terminate (detected by waiting for the socket to close).
            This is needed, because otherwise the .vo files may not have been written before we start benchmarking *)
         Pipe.closed (Reader.pipe r) >>= fun () ->
         Pipe.write w info
    ) >>= fun server ->
  let `Inet (_addr, port) = Async_unix.Tcp.Server.listening_on_address server in
  In_thread.run (fun () ->
      compile_for_benchmark
        ~port ~root_dir ~benchmark_target ~benchmark_url ~packages ~injections_extra)
  >>| fun (to_remove, cont) ->
  let r = Pipe.map r ~f:(post_process_info to_remove) in
  let cont =
    In_thread.run cont >>= fun (_st, timings) ->
    Async_unix.Tcp.Server.close server >>| fun () -> timings in
  Deferred.upon cont
    (fun _ -> Pipe.close w);
  r, cont

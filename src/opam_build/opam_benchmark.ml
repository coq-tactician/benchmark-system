
let repos =
  [ "custom-archive", "git+ssh://git@github.com/LasseBlaauwbroek/custom-archive.git"
  ; "coq-core-dev", "https://coq.inria.fr/opam/core-dev"
  ; "coq-extra-dev", "https://coq.inria.fr/opam/extra-dev"
  ; "coq-released", "https://coq.inria.fr/opam/released" ]

let tactician_package = OpamPackage.Name.of_string "coq-tactician"
let tactician_stdlib_package = OpamPackage.Name.of_string "coq-tactician-stdlib"
let coq_package = OpamPackage.Name.of_string "coq"
let coq_core_package = OpamPackage.Name.of_string "coq-core"

let opam_init
    ~root_dir
    () =
  OpamClientConfig.opam_init
    ~root_dir
    (* ~fake:true *)
    ~yes:(Some true)
    ~no_env_notice:true
    ~keep_build_dir:true
    ~assume_depexts:true
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

let get_bench_dir ~st =
  let nv = OpamSwitchState.get_package st tactician_package in
  let opam = OpamSwitchState.opam st nv in
  let env = OpamPackageVar.resolve ~opam st in
  let share_var = OpamVariable.(Full.create tactician_package (of_string "share")) in
  let share = OpamFilter.ident_string env (OpamFilter.ident_of_var share_var) in
  let (/) = Filename.concat in
  share/"plugins"/"zzz-benchmark"

let enable_bench ~st ~port ~injections_extra =
  let (/) = Filename.concat in
  let bench_dir = get_bench_dir ~st in
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

let disable_bench ~st =
  let (/) = Filename.concat in
  let bench_dir = get_bench_dir ~st in
  let injection_file = bench_dir/"injection-flags" in
  if Sys.file_exists injection_file then
    Unix.unlink injection_file

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
    let target_name = OpamPackage.Name.of_string benchmark_target in
    let url = OpamUrl.of_string benchmark_url in
    let st = OpamPinCommand.source_pin st target_name (Some url) in

    disable_bench ~st; (* In case we are reusing the build dir of a previous benchmark *)

    (* Stage 0: Calculate sets of packages that we need to install *)

    let every_root_package = packages @ [target_name, None; tactician_package, None] in
    let every_package =
      let request = OpamSolver.request ~install:every_root_package () in
      let solution =
        OpamSolution.resolve st Install
          ~orphans:OpamPackage.Set.empty
          ~requested:(OpamPackage.Name.Set.of_list @@ List.map fst every_root_package)
          request in
      match solution with
      | OpamTypes.Success s -> OpamSolver.new_packages s
      | OpamTypes.Conflicts cs ->
        OpamConsole.errmsg "%s" @@
        OpamCudf.string_of_explanations (OpamSwitchState.unavailable_reason st) @@
        OpamCudf.conflict_explanations_raw st.packages cs;
        OpamStd.Sys.exit_because `No_solution
    in
    let coq_base_package = if OpamPackage.Set.exists
        (fun p -> OpamPackage.Name.equal coq_core_package @@ OpamPackage.name p) every_package then
        coq_core_package else coq_package in
    let coq_dependees =
      OpamPackage.Set.filter
        (fun p -> not @@ OpamPackage.Name.Set.mem (OpamPackage.name p)
            (OpamPackage.Name.Set.of_list [target_name; tactician_package])) @@
      OpamListCommand.filter ~base:every_package st @@
      Atom (OpamListCommand.(Depends_on ({ default_dependency_toggles with recursive = true },
                                         [coq_base_package, None]))) in
    let not_coq_dependees = OpamSolution.eq_atoms_of_packages @@
      OpamPackage.Set.Op.(every_package -- coq_dependees) in
    (* If coq-tactician-stdlib is not requested for benchmarking, we check wether it can be installed.
       (Not so on newer versions of Coq, because the package is not needed). If so, it will be pre-installed. *)
    let not_coq_dependees = not_coq_dependees @
      if List.exists (OpamPackage.Name.equal tactician_stdlib_package) @@ List.map fst packages then [] else
        let request = OpamSolver.request
            ~install:[target_name, None; tactician_package, None; tactician_stdlib_package, None] () in
        let stdlib_installable =
          OpamSolution.resolve st Install
            ~orphans:OpamPackage.Set.empty
            ~requested:(OpamPackage.Name.Set.of_list [target_name; tactician_package; tactician_stdlib_package])
            request in
        match stdlib_installable with
        | OpamTypes.Success _ ->
          [tactician_stdlib_package, None]
        | OpamTypes.Conflicts _ -> [] in

    (* Stage 1: Install Coq, Tactician, the targeted plugin (if different from Tactician) and any other
       packages that don't depend on Coq. The reason we install those is that the requested benchmark
       packages may need some specific version of a package that Tactician depends on, causing everything
       to be recompiled in a next step. *)
    let st = OpamClient.install st not_coq_dependees in
    let target_install_time = target_install_time () in

    let st = inject ~gt ~st in
    enable_bench ~st ~port ~injections_extra, fun () ->
      (* Stage 2: If coq-tactician-stdlib needs to be benchmarked, we install it now, while benchmarking
         is enabled. This needs to happen, because other packages have an implicit dependency on it. *)
      let subject_install_time1 = timing () in
      let st = match List.find_opt (fun p -> OpamPackage.Name.equal tactician_stdlib_package @@ fst p) @@
          packages with
      | None -> st
      | Some atom -> OpamClient.install st [atom] in
      let subject_install_time1 = subject_install_time1 () in

      (* Stage 3: Install any Coq-dependees that we do not wish to benchmark. For these, we do inject
         Tactician into the compilation, so that we can learn from them. For this, we temporarily disable
         benchmarking. *)
      let deps_install_time = timing () in
      disable_bench ~st;
      let st = OpamClient.install st ~deps_only:true packages in
      let deps_install_time = deps_install_time () in

      (* Stage 4: Benchmark remaining packages *)

      let subject_install_time2 = timing () in
      let _ : string list = enable_bench ~st ~port ~injections_extra in
      let st = OpamClient.install st packages in
      let subject_install_time = Core.Time_ns.Span.(subject_install_time1 + subject_install_time2 ()) in

      let timings = `Target_install_time target_install_time, `Deps_install_time deps_install_time,
                    `Subject_install_time subject_install_time in
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

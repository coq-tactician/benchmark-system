let calculate ~root_dir () =
  OpamClientConfig.opam_init
    ~root_dir
    (* ~debug_level:100 *)
    (* ~cudf_file:(Some "/home/lasse/Documents/Projects/Tactician/benchmark-system/tmp-scratch/graph") *)
    ~yes:(Some true)
    ~no_env_notice:true
    ~best_effort:true
    ~solver:(lazy (OpamCudfSolver.solver_of_string "aspcud --solver-option=--opt-strategy=bb,hier %{input}% %{output}% %{criteria}%"))
    ~solver_timeout:None
    (* ~solver_preferences_default:(lazy ( *)
    (*     let module S = OpamCudfSolver.Aspcud in *)
    (*     Some S.default_criteria.crit_default)) *)
    ~solver_preferences_upgrade:(lazy (
        let module S = OpamCudfSolver.Aspcud in
        Some S.default_criteria.crit_upgrade))
    ~solver_preferences_fixup:(lazy (
        let module S = OpamCudfSolver.Aspcud in
        Some S.default_criteria.crit_fixup))
    ~solver_preferences_best_effort_prefix:(lazy (
        let module S = OpamCudfSolver.Aspcud in
        S.default_criteria.crit_best_effort_prefix))
    ~solver_preferences_default:(lazy (Some "-count(down),-count(up),-count(removed)"))
    ~fake:true
    ();
  let gt, rt, _ = Opam_benchmark.init_root () in
  let pkgs = OpamPackage.keys @@
    OpamRepositoryName.Map.find (OpamRepositoryName.of_string "coq-released") rt.repo_opams in
  (* let st = OpamSwitchState.load_virtual ?repos_list:(Some repos) gt rt in *)
  let switch = OpamSwitch.of_string "calculation" in
  let gt =
    if OpamGlobalState.switch_exists gt switch then
      OpamSwitchCommand.remove ~confirm:false gt switch else
      gt in
  let (), st = OpamSwitchCommand.create gt ~rt ~update_config:false
      ~repos:(List.map OpamRepositoryName.of_string
                ["custom-archive"; "coq-extra-dev"; "coq-core-dev"; "coq-released"; "default"])
      ~invariant:OpamFormula.(
          ands
            [ Atom ((OpamPackage.Name.of_string "coq-tactician"), Empty)
            ; Atom ((OpamPackage.Name.of_string "coq"), Atom (`Eq, OpamPackage.Version.of_string "8.11.dev"))
            ; Atom ((OpamPackage.Name.of_string "ocaml-base-compiler"), Empty) ]
        )
      switch
      (fun st -> (), st) in
  let quniv = OpamSwitchState.universe st ~requested:OpamPackage.Set.empty Query in
  let pkgs = OpamSolver.coinstallable_subset quniv
      (OpamPackage.Set.singleton
         (OpamPackage.create
            (OpamPackage.Name.of_string "coq-tactician") (OpamPackage.Version.of_string "8.11.dev")))
      pkgs in
  (* let to_exclude = OpamSolver.reverse_dependencies *)
  (*     ~depopts:false ~build:true ~post:false ~installed:false *)
  (*     quniv (OpamPackage.packages_of_name pkgs @@ OpamPackage.Name.of_string "coq-tactician") in *)
  let pkgs = OpamPackage.Set.filter
      (fun p -> OpamPackage.equal p (OpamPackage.max_version pkgs @@ OpamPackage.name p)) pkgs in
  let pkgs = OpamPackage.Set.filter
      (fun p -> not @@ OpamPackage.Name.Set.mem (OpamPackage.name p) @@ OpamPackage.Name.Set.of_list @@
        List.map OpamPackage.Name.of_string
          ["coq-tactician"; "coq-tactician-stdlib"; "coq-tactician-dummy"; "coq-tactician-reinforce"; "coq-gaia"; "coq-hierarchy-builder-shim"; "coq-compcert-64"; "coq-vst-64"]) pkgs in
  print_endline "\n";
  OpamPackage.Set.iter (fun p -> print_string @@ (OpamPackage.to_string p ^ " ")) pkgs;
  print_endline "\n";
  let rec install_all stage st pkgs acc =
    if OpamPackage.Set.is_empty pkgs then acc else
      let st = OpamClient.install st (OpamSolution.eq_atoms_of_packages pkgs) in
      let installed = OpamPackage.Set.inter st.installed pkgs in
      print_endline "\n";
      OpamPackage.Set.iter (fun p -> print_string @@ (OpamPackage.to_string p ^ " ")) installed;
      print_endline "\n";
      let remaining = OpamPackage.Set.diff pkgs st.installed in
      if OpamPackage.Set.equal pkgs remaining then begin
        print_endline "remaining";
        OpamPackage.Set.iter (fun p -> print_endline @@ (OpamPackage.to_string p)) remaining;
        acc
      end else begin
        OpamSwitchCommand.export rt ~switch
          (Some (OpamFile.make (OpamFilename.of_string ("opam-stage" ^ string_of_int stage))));
        install_all (stage + 1) st remaining (OpamPackage.Set.union acc st.installed)
      end in
  let everything = install_all 1 st pkgs OpamPackage.Set.empty in
  print_endline "installing:";
  OpamPackage.Set.iter (fun p -> print_endline @@ OpamPackage.to_string p) everything

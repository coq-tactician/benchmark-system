let remove_create_switch gt rt c relax =
  let switch = OpamSwitch.of_string ("calculation" ^ string_of_int c) in
  let gt =
    if OpamGlobalState.switch_exists gt switch then
      OpamSwitchCommand.remove ~confirm:false gt switch else
      gt in
  let version = if relax then "8.10~" else "8.11~" in
  let (), st = OpamSwitchCommand.create gt ~rt ~update_config:false
      ~repos:(List.map OpamRepositoryName.of_string
                ["custom-archive"; "coq-extra-dev"; "coq-core-dev"; "coq-released"; "default"])
      ~invariant:OpamFormula.(
          ands
            [ Atom ((OpamPackage.Name.of_string "coq-tactician"), Empty)
            ; Atom ((OpamPackage.Name.of_string "coq"), Atom (`Geq, (OpamPackage.Version.of_string version)))
            ; Atom ((OpamPackage.Name.of_string "ocaml-base-compiler"), Empty) ]
        )
      switch
      (fun st -> (), st) in
  gt, st, switch

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
    ~solver_preferences_default:(lazy (
        let module S = OpamCudfSolver.Aspcud in
        Some S.default_criteria.crit_default))
    ~solver_preferences_upgrade:(lazy (
        let module S = OpamCudfSolver.Aspcud in
        Some S.default_criteria.crit_upgrade))
    ~solver_preferences_fixup:(lazy (
        let module S = OpamCudfSolver.Aspcud in
        Some S.default_criteria.crit_fixup))
    ~solver_preferences_best_effort_prefix:(lazy (
        let module S = OpamCudfSolver.Aspcud in
        S.default_criteria.crit_best_effort_prefix))
    (* ~solver_preferences_default:(lazy (Some "-count(down),-count(up),-count(removed)")) *)
    ~fake:true
    ();
  let gt, rt, _ = Opam_benchmark.init_root () in
  let pkgs = OpamPackage.keys @@
    OpamRepositoryName.Map.find (OpamRepositoryName.of_string "coq-released") rt.repo_opams in
  let dev_pkgs = OpamPackage.keys @@
    OpamRepositoryName.Map.find (OpamRepositoryName.of_string "coq-extra-dev") rt.repo_opams in
  let pkgs = OpamPackage.Set.union pkgs @@
    OpamPackage.Set.filter
      (fun p -> not @@ OpamPackage.Set.exists
          (fun p2 -> OpamPackage.Name.equal (OpamPackage.name p) (OpamPackage.name p2)) pkgs) dev_pkgs in
  (* let st = OpamSwitchState.load_virtual ?repos_list:(Some repos) gt rt in *)
  let pkgs = OpamPackage.Set.filter
      (fun p -> OpamPackage.equal p (OpamPackage.max_version pkgs @@ OpamPackage.name p)) pkgs in
  let pkgs = OpamPackage.Set.filter
      (fun p -> not @@ OpamPackage.Name.Set.mem (OpamPackage.name p) @@ OpamPackage.Name.Set.of_list @@
        List.map OpamPackage.Name.of_string
          ["coq-tactician"; "coq-tactician-stdlib"; "coq-tactician-dummy"; "coq-tactician-reinforce";
           "coq-gaia" (* has been split *);
           "coq-hierarchy-builder-shim" (* empty shim that conflicts with original *);
           "coq-compcert-64"; "coq-vst-64" (* these are old, coq-compcert is now 64b *);
           "gappa" (* renamed to coq-gappa *);
           "coq-engine-bench"; "coq-engine-bench-lite"; "coq-performance-tests"; "coq-performance-tests-lite" (* benchmarks *);
           "coq-fiat-crypto-legacy"; "coq-fiat-crypto-legacy-extra"; "coq-fiat-core"; "coq-fiat-crypto-with-bedrock"; "coq-fiat-parsers"; (* seems to all be part of coq-fiat-crypto *)
          "coq-flocq3"; (* legacy package *)
           "coq-rewriter-perf-Fast"; "coq-rewriter-perf-Medium"; "coq-rewriter-perf-Slow"; "coq-rewriter-perf-SuperFast"; "coq-rewriter-perf-VerySlow" (* Some benchmark *);
          "menhirLib"; "menhirSdk"; "menhir" (* coq-menhirlib should cover tihs *);
          "coq-albert" (* too old *)]) pkgs in
  print_endline "\n";
  OpamPackage.Set.iter (fun p -> print_string @@ (OpamPackage.to_string p ^ " ")) pkgs;
  print_endline "\n";
  let rec install_all stage gt pkgs acc relax =
    if OpamPackage.Set.is_empty pkgs then acc else
      let gt, st, switch = remove_create_switch gt rt stage relax in
      let st = OpamClient.install st (OpamSolution.eq_atoms_of_packages pkgs) in
      let installed = OpamPackage.Set.inter st.installed pkgs in
      print_endline "\n";
      OpamPackage.Set.iter (fun p -> print_string @@ (OpamPackage.to_string p ^ " ")) installed;
      print_endline "\n";
      let remaining = OpamPackage.Set.diff pkgs st.installed in
      if OpamPackage.Set.equal pkgs remaining then
        if not relax then
          install_all (stage + 1) gt remaining (OpamPackage.Set.union acc st.installed) true
        else begin
          print_endline "remaining";
        OpamPackage.Set.iter (fun p -> print_endline @@ (OpamPackage.to_string p)) remaining;
        acc
        end
      else begin
        OpamSwitchCommand.export rt ~switch
          (Some (OpamFile.make (OpamFilename.of_string ("opam-stage" ^ string_of_int stage))));
        install_all (stage + 1) gt remaining (OpamPackage.Set.union acc st.installed) relax
      end in
  let everything = install_all 1 gt pkgs OpamPackage.Set.empty false in
  print_endline "installing:";
  OpamPackage.Set.iter (fun p -> print_endline @@ OpamPackage.to_string p) everything

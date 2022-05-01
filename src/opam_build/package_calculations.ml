let calculate ~root_dir () =
  OpamClientConfig.opam_init
    ~root_dir
    ~yes:(Some true)
    ~no_env_notice:true
    ~best_effort:true
    (* ~solver:(lazy (module OpamCudfSolver.Mccs)) *)
    ~solver_timeout:None
    ();
  let gt, rt, _ = Opam_benchmark.init_root () in
  let repos =
    List.map (fun (name, _) -> OpamRepositoryName.of_string name) Opam_benchmark.repos @
    [ OpamRepositoryName.of_string "default" ] in
  let pkgs = OpamPackage.keys @@
    OpamRepositoryName.Map.find (OpamRepositoryName.of_string "coq-released") rt.repo_opams in
  let st = OpamSwitchState.load_virtual ?repos_list:(Some repos) gt rt in
  let pkgs = OpamSolver.coinstallable_subset
      (OpamSwitchState.universe st ~requested:OpamPackage.Set.empty Query)
      (OpamPackage.Set.singleton
         (OpamPackage.create
            (OpamPackage.Name.of_string "coq-tactician") (OpamPackage.Version.of_string "8.11.dev")))
      pkgs in
  let pkgs = OpamPackage.Set.filter
      (fun p -> OpamPackage.equal p (OpamPackage.max_version pkgs @@ OpamPackage.name p)) pkgs in
  print_endline "\n";
  OpamPackage.Set.iter (fun p -> print_string @@ (OpamPackage.to_string p ^ "\n")) pkgs;
  print_endline "\n";
  let base_compiler = OpamPackage.create (OpamPackage.Name.of_string "ocaml-base-compiler") (OpamPackage.Version.of_string "4.10.2") in
  let coq = OpamPackage.create (OpamPackage.Name.of_string "coq") (OpamPackage.Version.of_string "8.11.2") in
  let pkgs = OpamPackage.Set.add base_compiler @@ OpamPackage.Set.singleton coq in
  let request = OpamSolver.request
      ~install:(OpamSolution.eq_atoms_of_packages pkgs) () in
  let solution = OpamSolver.resolve
      (OpamSwitchState.universe st ~requested:pkgs Install) request in
  match solution with
  | OpamTypes.Success solution ->
    print_endline "installing:";
    let pkgs = OpamSolver.new_packages solution in
    OpamPackage.Set.iter (fun p -> print_endline @@ OpamPackage.to_string p) pkgs
  | OpamTypes.Conflicts _ -> print_endline "conflict"


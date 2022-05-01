open Opam_build__Package_calculations

let () =
  if Array.length Sys.argv != 2 then
    print_endline "First argument should be the opam root";
  calculate ~root_dir:(OpamFilename.Dir.of_string @@ Sys.argv.(1)) ()

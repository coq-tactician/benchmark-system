open Opam_build__Opam_benchmark

open Core
open Async

let main () =
  compile_and_retrieve_benchmark_info
      ~root_dir:"/home/lasse/Documents/Projects/Tactician/benchmark-system/tmp-root"
      ~benchmark_target:"coq-tactician"
      ~benchmark_url:
        "git+file:///home/lasse/Documents/Projects/Tactician/develop/coq-tactician#newbench"
      ~pins:[]
      ~packages:["coq-tactician-stdlib"]
      ~injections_extra:[]
  >>= fun (r, cont) ->
  Pipe.iter r ~f:(fun { exec; args; env; dir; lemmas; _ } ->
      print_endline exec;
      print_endline dir;
      Array.iter args ~f:print_endline;
      Array.iter env ~f:print_endline;
      List.iter lemmas ~f:print_endline;
      Deferred.unit
    ) >>= fun () -> cont >>| fun _ -> ()

let () =
  Command.async ~summary:"Benchmark"
    Command.Let_syntax.(return main)
  |> Command.run

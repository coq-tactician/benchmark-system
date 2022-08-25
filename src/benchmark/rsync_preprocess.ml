let () =
  let args = List.tl @@ Array.to_list Sys.argv in
  let rec process acc = function
    | [] -> Printf.eprintf "rsync-preprocess: error: No split point detected!\n"; exit 1
    | "rsync-preprocess-marker"::_::ls -> List.rev acc @ ls
    | x::ls -> process (x::acc) ls in
  let args = process [] args in
  Unix.execvp (List.hd args) @@ Array.of_list args

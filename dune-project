(lang dune 2.8)
(name tactician-benchmark)

(generate_opam_files true)

(source (github coq-tactician/benchmark-system))
(homepage "https://coq-tactician.github.io")
(authors "Lasse Blaauwbroek <lasse@blaauwbroek.eu>")
(maintainers "Lasse Blaauwbroek <lasse@blaauwbroek.eu>")

(package
 (name tactician-benchmark)
 (synopsis "Benchmarking for Tactician")
 (depends
  (ocaml (>= 4.12.0))
   dune
  (dune-site (>= 2.9.1))
  (opam-client (and (>= 2.2.0) (< 2.3.0)))
   conf-git
   extunix
   core
   async
  (rpc_parallel (= v0.16.0))))

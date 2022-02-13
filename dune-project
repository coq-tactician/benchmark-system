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
  (ocaml (>= 4.08))
   dune
  (dune-site (>= 2.9.1))
  (opam-client (>= 2.1.0))
   conf-git
   extunix
   core
   async
   rpc_parallel))

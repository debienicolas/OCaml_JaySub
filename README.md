## J< : Time reversible language

A time-reversible programming language is one that in addition to the usual interpretation of computation
as calculating a program’s output from some input, also allows computing a program’s inputs from its output.

```shell
opam install dune menhir ppx_deriving

dune build

dune exec jaysub forward examples/fib.jsub
```

## CLI usage

```
jaysub [forward|backward|invert|optimize] <filename.jsub>
```


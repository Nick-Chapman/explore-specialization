# explore-specialization

Explore the program specialization example described in Max Bernstein's [`weval` blog article](https://bernsteinbear.com/blog/weval) using the [_Normalization by Evaluation_](https://github.com/Nick-Chapman/barefun/blob/main/haskell/src/Stage2_NBE.hs) approach from my [barefun](https://github.com/Nick-Chapman/barefun) project.


Baseline C++ interpreter version:
```
$ time jenga run c++/peval
(C++)Result: 5000000050000000

real    0m1.247s
user    0m1.244s
sys     0m0.003s
```

First cut ocaml interpreter version:
```
$ time dune exec ocaml/peval.exe
(Ocaml)Result: 5000000050000000

real    0m2.304s
user    0m2.259s
sys     0m0.028s
```

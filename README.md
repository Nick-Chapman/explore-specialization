# explore-specialization

Explore the program specialization example described in Max Bernstein's [`weval` blog article](https://bernsteinbear.com/blog/weval) using the [_Normalization by Evaluation_](https://github.com/Nick-Chapman/barefun/blob/main/haskell/src/Stage2_NBE.hs) approach from my [barefun](https://github.com/Nick-Chapman/barefun) project.


Baseline C++ interpreter version:
```
$ time jenga run peval/c++/peval.exe
(C++)Result: 5000000050000000

real    0m1.247s
user    0m1.244s
sys     0m0.003s
```

Baseline C++ interpreter version (`-O2`)
```
$ time jenga run peval/c++/peval.exe
(C++)Result: 5000000050000000


real    0m0.328s
user    0m0.327s
sys     0m0.002s
```

First cut ocaml interpreter version:
```
# time jenga run peval/ocaml/peval.exe
(Ocaml)Result: 5000000050000000

real    0m2.304s
user    0m2.259s
sys     0m0.028s
```

Ocaml normalized by barefun:
```
$ time jenga run peval/fun/norm.exe
(Ocaml)Result: 5000000050000000

real    0m0.445s
user    0m0.436s
sys     0m0.010s
```

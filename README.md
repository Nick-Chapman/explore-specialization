# explore-specialization

Explore the program specialization example described in Max Bernstein's [`weval` blog article](https://bernsteinbear.com/blog/weval) using the [_Normalization by Evaluation_](https://github.com/Nick-Chapman/barefun/blob/main/haskell/src/Stage2_NBE.hs) approach from my [barefun](https://github.com/Nick-Chapman/barefun) project.


## peval example

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

## brainfuck example

Brainfuck interpreter specialized to prime-factorization example.

```
$ echo '123456' | time ,jenga/bf/orig_example1.exe
mode:ORIGINAL
123456: 2 2 2 2 2 2 3 643
#steps=2702449
3.60user 0.00system 0:03.60elapsed 99%CPU (0avgtext+0avgdata 5444maxresident)k
0inputs+0outputs (0major+843minor)pagefaults 0swaps

$ echo '123456' | time ,jenga/bf/norm_example1.exe
mode:NORMALIZED
123456: 2 2 2 2 2 2 3 643
#steps=2702449
0.13user 0.00system 0:00.13elapsed 100%CPU (0avgtext+0avgdata 3404maxresident)k
0inputs+0outputs (0major+265minor)pagefaults 0swaps
```

The compiled code runs about x27 times faster than the interpreted code. 0.13s vs 3.6s.

# Implementations of generic count with first-class control

This repository contains code associated with the following papers:

* [Effects for Efficiency: Asymptotic Speedup with First-Class Control](https://dl.acm.org/doi/pdf/10.1145/3408982), published in ICFP 2020.
* [Asymptotic Speedup with Effect Handlers](https://dhil.net/research/papers/asympeff-jfp2023-draft.pdf), under consideration for publication in JFP, 2023.

Note that the code in this repository is not part of the companion
artifact code published along side the papers. See the papers for
details on their artifacts.

<hr />

The code in this repository illustrates different implementations of
the generic count problem using delimited continuations (shift/reset),
undelimited continuations (callcc) and state, and effect handlers.
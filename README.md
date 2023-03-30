# Gen.clj

![tests](https://github.com/OpenIQL/inferenceql.query/workflows/tests/badge.svg)
![linter](https://github.com/OpenIQL/inferenceql.query/workflows/linter/badge.svg)
![Stability: Experimental](https://img.shields.io/badge/stability-experimental-orange.svg)

An open-source stack for generative modeling and probabilistic inference.

> **Warning**
>
> Gen.clj, the Clojure implementation of the Gen language, currently only supports a subset of Gen's features. For a complete implementation see [Gen.jl](https://github.com/probcomp/Gen.jl). If you would like to get involved with Gen.clj's development please [contact us](mailto:contributing@zane.io).

## Why Gen?

- Gen automates the implementation details of probabilistic inference algorithms
- Gen allows users to flexibly navigate performance trade-offs
- Gen supports custom hybrid inference algorithms
- Users write custom inference algorithms without extending the compiler
- Efficient inference in models with stochastic structure

## Getting started

### Install

Gen.clj is currently only available as a [git dependency](https://clojure.org/guides/deps_and_cli#_using_git_libraries). To install Gen.clj, add the following entry to your `deps.edn` under the `:deps` key:

``` clojure
io.github.inferenceql/gen.clj {:git/url "https://github.com/inferenceql/gen.clj"
                               :sha "3283de82c2b25870b530c22948ac3cdc1b6996d4"}
```

### Learn

There are tutorials available as [Clerk](https://github.com/nextjournal/clerk/) notebooks in the [`examples`](https://) directory. 

To view a notebook first clone this repository, then start a Clojure REPL that includes Clerk:

``` shell
clj -A:clerk
```

Require and start Clerk:

``` clojure
(require '[nextjournal.clerk :as clerk])
(clerk/serve! {:browse true})
```

This will open a tab in your web browser. You can then view a notebook in that tab:

``` clojure
(clerk/show! "examples/intro_to_modeling.clj")
```

For more information on Clerk see the [Book of Clerk](https://book.clerk.vision/) and the [Clerk repository](https://github.com/nextjournal/clerk).

## Contributors

### The Gen team

Gen.jl was created by [Marco Cusumano-Towner](https://www.mct.dev/) the [MIT Probabilistic Computing Project](http://probcomp.csail.mit.edu/), which is led by [Vikash Mansinghka](http://probcomp.csail.mit.edu/principal-investigator/). Gen.jl has grown and is maintained through the help of a core research and engineering team that includes [Alex Lew](http://alexlew.net/), [Tan Zhi-Xuan](https://github.com/ztangent/), [George Matheos](https://www.linkedin.com/in/george-matheos-429982160/), [McCoy Becker](https://femtomc.github.io/), and [Feras Saad](http://fsaad.mit.edu/), as well as a number of open-source contributors. Gen.jl was adapted to Clojure by [Zane Shelby](https://zane.io) with help from Ulrich Schaechtle. The Gen architecture is described in [Marco's PhD thesis](https://www.mct.dev/assets/mct-thesis.pdf).

### Citation

If you use Gen in your research, please cite our PLDI paper:

> Gen: A General-Purpose Probabilistic Programming System with Programmable Inference. Cusumano-Towner, M. F.; Saad, F. A.; Lew, A.; and Mansinghka, V. K. In Proceedings of the 40th ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI ‘19). ([pdf](https://dl.acm.org/doi/10.1145/3314221.3314642)) ([bibtex](https://www.gen.dev/assets/gen-pldi.txt))

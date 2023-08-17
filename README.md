# Gen.clj

<div align="center">

[![Build Status][build-status]][build-status-url]
[![Linter][linter]][linter-url]
![Stability: Experimental][experimental-badge]

</div>

An open-source stack for generative modeling and probabilistic inference.

> **Warning**
> Gen.clj, the Clojure implementation of the Gen language, currently only
> supports a subset of Gen's features. For a complete implementation see
> [Gen.jl](https://github.com/probcomp/Gen.jl). If you would like to get
> involved with Gen.clj's development please [contact
> us](mailto:contributing@zane.io).

## Why Gen?

- Gen automates the implementation details of probabilistic inference algorithms
- Gen allows users to flexibly navigate performance trade-offs
- Gen supports custom hybrid inference algorithms
- Users write custom inference algorithms without extending the compiler
- Efficient inference in models with stochastic structure

## Getting started

### Install

Gen.clj is currently only available as a [git
dependency](https://clojure.org/guides/deps_and_cli#_using_git_libraries). To
install Gen.clj, add the following entry to your `deps.edn` under the `:deps`
key:

``` clojure
io.github.inferenceql/gen.clj {:git/url "https://github.com/inferenceql/gen.clj"
                               :git/sha "3283de82c2b25870b530c22948ac3cdc1b6996d4"}
```

### Learn

The project's [interactive documentation][gen-clj-url] was generated from
the notebooks in the
[`examples`](https://github.com/InferenceQL/gen.clj/tree/main/examples)
directory using Nextjournal's [Clerk][clerk-url]. If you'd like to edit or play
with the documentation or demos, you'll need to install

- The [Clojure command line tool](https://clojure.org/guides/install_clojure)
- [Babashka](https://github.com/babashka/babashka#installation)

Next, clone the repository:

```bash
git clone git@github.com:InferenceQL/gen.clj.git
cd gen.clj
```

Run this command in the cloned repository:

```sh
bb clerk-watch
```

This will open a browser window to `http://localhost:7777` with the contents of
the ["Introduction to Modeling in
Gen.clj"](https://github.com/InferenceQL/gen.clj/blob/main/examples/intro_to_modeling.clj)
notebook loaded. Any edits you make to `examples/intro_to_modeling.clj` on your
filesystem will update this page, and editing any other file in `examples`, like
`examples/introduction.clj`, will load that file's namespace into the browser.

For more information on Clerk see the [Book of
Clerk](https://book.clerk.vision/) and the [Clerk
repository](https://github.com/nextjournal/clerk).

## Contributors

### The Gen team

Gen.jl was created by [Marco Cusumano-Towner](https://www.mct.dev/) the [MIT
Probabilistic Computing Project](http://probcomp.csail.mit.edu/), which is led
by [Vikash Mansinghka](http://probcomp.csail.mit.edu/principal-investigator/).
Gen.jl has grown and is maintained through the help of a core research and
engineering team that includes [Alex Lew](http://alexlew.net/), [Tan
Zhi-Xuan](https://github.com/ztangent/), [George
Matheos](https://www.linkedin.com/in/george-matheos-429982160/), [McCoy
Becker](https://femtomc.github.io/), [Feras Saad](http://fsaad.mit.edu/) and
[Sam Ritchie](https://samritchie.io), as well as a number of open-source
contributors. Gen.jl was adapted to Clojure by [Zane Shelby](https://zane.io)
with help from Ulrich Schaechtle. The Gen architecture is described in [Marco's
PhD thesis](https://www.mct.dev/assets/mct-thesis.pdf).

### Citation

If you use Gen in your research, please cite our PLDI paper:

> Gen: A General-Purpose Probabilistic Programming System with Programmable
> Inference. Cusumano-Towner, M. F.; Saad, F. A.; Lew, A.; and Mansinghka, V. K.
> In Proceedings of the 40th ACM SIGPLAN Conference on Programming Language
> Design and Implementation (PLDI ‘19).
> ([pdf](https://dl.acm.org/doi/10.1145/3314221.3314642))
> ([bibtex](https://www.gen.dev/assets/gen-pldi.txt))


## License

Distributed under the [Apache 2.0](LICENSE) license. See [LICENSE](LICENSE).

[build-status-url]: https://github.com/InferenceQL/gen.clj/actions/workflows/tests.yaml?query=branch%3Amain
[build-status]: https://github.com/InferenceQL/gen.clj/workflows/tests/badge.svg?branch=main
[clerk-url]: https://github.com/nextjournal/clerk
[experimental-badge]: https://img.shields.io/badge/stability-experimental-orange.svg
[gen-clj-url]: https://github.clerk.garden/inferenceql/gen.clj
[license-url]: LICENSE
[license]: https://img.shields.io/badge/License-Apache_2.0-brightgreen.svg
[linter-url]: https://github.com/InferenceQL/gen.clj/actions/workflows/linter.yaml?query=branch%3Amain
[linter]: https://github.com/InferenceQL/gen.clj/workflows/linter/badge.svg?branch=main

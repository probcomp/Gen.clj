## Dev dependencies

- [node.js](https://nodejs.org/en/)
- The [clojure command line tool](https://clojure.org/guides/install_clojure)
- [Babashka](https://github.com/babashka/babashka#installation)

## Github Pages, docs notebook

The project's [Github Pages site](https://inferenceql.github.io/gen.clj) hosts
an interactive [Clerk](https://github.com/nextjournal/clerk) notebook
demonstrating the library's use.

### Local notebook dev

Start a Clojure process however you like, and run `(user/serve!)` to run the
Clerk server. This command should open up `localhost:7777`.

Alternatively, run

```sh
bb clerk-watch
```

### Static build

To test the static build locally:

```
bb publish-local
```

This will generate the static site in `public/build`, start a development http
server and open up a browser window (http://127.0.0.1:8080/) with the production
build of the documentation notebook.

### GitHub Pages

To build and release to Github Pages:

```
bb release-gh-pages
```

This will ship the site to https://inferenceql.github.io/gen.clj.

## Publishing to Clojars

The template for the project's `pom.xml` lives at
[`template/pom.xml`](https://github.com/InferenceQL/gen.clj/blob/main/template/pom.xml).

To create a new release:

- Update the version in
  [build.clj](https://github.com/InferenceQL/gen.clj/blob/main/build.clj)
- Make a new [Github
  Release](https://github.com/InferenceQL/gen.clj/releases) with tag
  `v<the-new-version>`.

Submitting the release will create the new tag and trigger the following
command:

```
bb release
```

The new release will appear on Clojars.

## Publishing JS

> **Note**
> To publish `emmy-viewers` JS, you must be part of the [InferenceQL
> GitHub organization](https://github.com/inferenceql).

- Create a classic personal access token with `read:org` permissions.
- Add a line like this to your bash profile:

```bash
export GITHUB_TOKEN="<your_token>"
```

Run the following code with the `:nextjournal/clerk` alias activated:

```clojure
(require '[mentat.clerk-utils.build :as b])

(b/release->cas!
 {:cljs-namespaces '[gen.sci-extensions]
  :cas-namespace "inferenceql"
  :prefix "gen.clj"
  :token (System/getenv "GITHUB_TOKEN")})
```

- Take the resulting URL and replace the existing entry at
  `emmy.clerk/custom-js` with the new URL that prints.

## Linting

Code is linted with [`clj-kondo`](https://github.com/clj-kondo/clj-kondo):

```
bb lint
```

The first time you interact with the project, run the following command to lint
all dependencies and populate the `clj-kondo` cache:

```
bb lint-deps
```

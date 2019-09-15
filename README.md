# Language Server Index Format for OCaml

**Export OCaml type hovers and jump-to-definitions in LSIF**

Note: development is early work-in-progress; the tool is released in the interest of early adopters who may find it immediately useful for open source projects. 

> What is LSIF?

> The Language Server Index Format (LSIF) is a format for storing code intelligence data like type hovers and jump-to-definitions. See [LSIF](https://github.com/Microsoft/language-server-protocol/blob/master/indexFormat/specification.md) and how it relates to [LSP](https://microsoft.github.io/language-server-protocol/) for more details. Platforms like [Sourcegraph](https://sourcegraph.com) use LSIF to surface this information in your browser.

## Quickstart : generate LSIF dump

1. Pin and install `opam pin add merlin-lsif https://github.com/rvantonder/merlin.git\#lsif`. This installs an extension of merlin that dumps type and definition data. Installing `merlin-lsif` will not conflict with existing `merlin` installations.

1. Pin and install `opam pin add lsif-ocaml https://github.com/rvantonder/lsif-ocaml.git`

1. Compile your OCaml project. Note: `lsif-ocaml` depends on `.merlin` files to work

1. At the root of your directory, run `lsif-ocaml-dump`

1. Run `lsif-ocaml -only-type-hovers > data.lsif`

1. Optionally delete the `*.lsif.in` files in your repository with `find . -name *.lsif.in | xargs rm`, or keep them around and update your `data.lsif` for changed files in new commits.

## FAQ

#### Generating LSIF takes a while, how can I make it faster?

Initially generating an LSIF file can take some time (20 minutes on a Macbook for large projects). You can speed it up with `lsif-ocaml-dump -p` to generate `*.lsif.in` files in a (less stable) parallel frenzy. Note that once every `.ml*` file is processed, `*.lsif.in` files only need to be updated for changed files. Easily regenerate a smaller number of files by running `lsif-ocaml-dump` in subdirectories, or with, e.g.,:

```
cat updated-file.ml | ocamlmerlin-with-lsif server lsif update-file.ml -dot-merlin ./path/where/update-file/lives/.merlin > updated-file.ml.lsif.in
```

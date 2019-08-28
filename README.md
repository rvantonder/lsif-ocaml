# Language Server Index Format for OCaml

Surface OCaml type hovers and jump-to-definitions using LSIF (in your browser).


> What is LSIF?

> The Language Server Index Format (LSIF) is a format for storing code intelligence data like type hovers and jump-to-definitions. See [LSIF](https://github.com/Microsoft/language-server-protocol/blob/master/indexFormat/specification.md) and how it relates to [LSP](https://microsoft.github.io/language-server-protocol/) for more details.

Note: development is early work-in-progress; the tool is released in the interest of early adopters who may find it immediately useful for open source projects.

## Example

Try hovering on files in the `stdlib` folder for OCaml 4.08: 

- In Sourcegraph: 

- Or on GitHub if you have the [Sourcegraph browser extension](https://docs.sourcegraph.com/integration/browser_extension) installed:


## Quickstart : generate LSIF dump

1. Pin and install `opam pin add merlin-lsif https://github.com/rvantonder/merlin.git\#lsif`. This installs an extension of merlin that dumps type and definition data. Installing `merlin-lsif` will not conflict with existing `merlin` installations.

1. Pin and install `opam pin add lsif-ocaml https://github.com/rvantonder/lsif-ocaml.git`

1. Compile your OCaml project. Note: `lsif-ocaml` depends on `.merlin` files to work

1. At the root of your directory, run `lsif-ocaml-dump`

1. Run `lsif-ocaml -only-type-hovers > data.lsif`

## Quickstart : upload your LSIF dump

You can surface LSIF data in the browser with Sourcegraph. Type-on-hover is currently supported (cross-file and cross-repo jump to definition is planned).

1. Generate an upload token:
    1. Visit https://sourcegraph.com/github.com/your-username/your-repo. Wait a few seconds for your repo to be indexed if needed, and then refresh.
    
    - Scroll down and copy the LSIF code:
    
    <details>
      <summary>Expand example</summary>
  
    ![Screen Shot 2019-08-28 at 2 25 22 PM](https://user-images.githubusercontent.com/888624/63882339-c8f51a00-c99f-11e9-83c7-8a2df7ee8e5f.png)
    </details>
    
    - Temporarily add the code to your repository's GitHub topics:
    
     <details>
      <summary>Expand example</summary>
    
    ![Screen Shot 2019-08-28 at 2 28 28 PM](https://user-images.githubusercontent.com/888624/63882568-4751bc00-c9a0-11e9-929c-d20ad01f16cb.png)

    ![Screen Shot 2019-08-28 at 2 27 12 PM](https://user-images.githubusercontent.com/888624/63882638-68b2a800-c9a0-11e9-983e-fde0bd548a39.png)
    </details>
    
    - Go back to https://sourcegraph.com/github.com/your-username/your-repo and click `Check now` to recieve the upload token
    
     <details>
      <summary>Expand example</summary>
  
    ![Screen Shot 2019-08-28 at 2 28 05 PM](https://user-images.githubusercontent.com/888624/63882725-9861b000-c9a0-11e9-8ba0-6c2ed04ab69b.png)
    </details>
    
    - You should see your token. Copy it.

     <details>
      <summary>Expand example</summary>
  
    ![Screen Shot 2019-08-28 at 2 28 14 PM](https://user-images.githubusercontent.com/888624/63882810-c810b800-c9a0-11e9-9cad-815bc2abefd6.png)
     </details>
    
    - Delete the topic code from your repository.

1. In your project root, upload `data.lsif` using the token, and fill out your repository (and commit, if different from `HEAD`) as follows:

  ```
  env SRC_ENDPOINT="https://sourcegraph.com" \
      SRC_LSIF_UPLOAD_TOKEN="<upload-token>" \
      REPOSITORY="github.com/your-username/your-repo" \
      COMMIT=$(git rev-parse HEAD) lsif-ocaml-upload data.lsif
  ```

3. Optionally delete the `*.lsif.in` files in your repository with `find . -name *.lsif.in | xargs rm`, or keep them around and update your `data.lsif` for changed files in new commits.

4. Try hovering in the browser! You can do so on Sourcegraph at https://sourcegraph.com/github/your-username/your-repo, or on GitHub if you have the [Sourcegraph browser extension](https://docs.sourcegraph.com/integration/browser_extension) installed.

**Important notes:**

- The `lsif.data` file is connected to a specific commit. You can upload different `data.lsif` for each commit (in feature branches, etc.)
- Make sure you are browsing the repository at the same commit you uploaded the LSIF data for. Sourcegraph will fall back to heuristics for commits that do not have LSIF data.
- Upload size for a `data.lsif` file is currently limited to 100MB (limit increases and compression is work-in-progress)

## FAQ

#### Generating LSIF takes a while, how can I make it faster?

Initially generating an LSIF file can take some time (20 minutes or more for large projects). You can speed it up with `lsif-ocaml-dump -p` to generate `*.lsif.in` files in a parallel frenzy. Note that once every `.ml*` file is processed, `*.lsif.in` files only need to be updated for changed files. Easily regenerate a smaller number of files by running `lsif-ocaml-dump` in subdirectories, or with, e.g.,:

```
cat updated-file.ml | ocamlmerlin-with-lsif server lsif update-file.ml -dot-merlin ./path/where/update-file/lives/.merlin > updated-file.ml.lsif.in
```

#### How can I exclude directories?

See `lsif-ocaml-dump -h`, and the `-exclude` option when generating `lsif.in` files. See `lsif-ocaml -help` and `-exclude` to exclude `lsif.in` files from being generated in `data.lsif`. By default, `_build` and `*test*` directories are excluded.


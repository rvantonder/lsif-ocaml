#!/bin/bash

set -ex
# set -e

TMP=${TMPDIR:-/tmp}
OCAML_MERLIN_LSIF="ocaml-merlin-lsif-0.1.0"

if [ ! -f ocamlmerlin-with-lsif ]; then
    echo "LSIF support is not installed. Try opam pin https://github.com/rvantonder/merlin-lsif"
    install_merlin_with_lsif
    if [ $? -eq 0 ]; then
        echo "Installed LSIF support"
    else
        echo "Failed to install LSIF support. Please see manual install instructions at \
              https://github.com/rvantonder/merlin \
              and merlin in a "merlin" subdirectory in this folder."
    fi  
else  
    MERLIN_BINARY=ocamlmerlin-with-lsif
fi

EXCLUDE="_build\|test"

FILES=$(find . -type f \( -name "*.ml" -o -name "*.mli" \) | grep -v "$EXCLUDE")
N_FILES=$(echo "$FILES" | wc -l)
i=0

for f in $FILES; do
  FILE_DIR=$(dirname $f)
  if [[ -f "$FILE_DIR/.merlin" ]]; then
    ((i++))
    printf "(%4d/%4d) %s\n" "$i" "$N_FILES" "$f"
    cat "$f" | $MERLIN_BINARY server lsif "$f" "-dot-merlin" "$FILE_DIR/.merlin" > "$f.lsif.in" # &
  fi
done

# wait

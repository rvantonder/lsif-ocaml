#!/bin/bash

set -e

PARALLEL_FRENZY=$1

EXISTS=$(command -v ocamlmerlin-with-lsif || echo)

if [ ! -n "$EXISTS" ]; then
    echo "LSIF support is not installed. Try 'opam pin add merlin-lsif https://github.com/rvantonder/merlin.git\#dumper-lite'"
    exit 1
else  
    MERLIN_LSIF_BINARY=ocamlmerlin-with-lsif
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
    if [ -z "$PARALLEL_FRENZY" ]; then
      cat "$f" | $MERLIN_LSIF_BINARY server lsif "$f" "-dot-merlin" "$FILE_DIR/.merlin" > "$f.lsif.in" # &
    else
      cat "$f" | $MERLIN_LSIF_BINARY server lsif "$f" "-dot-merlin" "$FILE_DIR/.merlin" > "$f.lsif.in" &
    fi
  fi
done

if [-z "$PARALLEL_FRENZY" ]; then 
    wait
fi

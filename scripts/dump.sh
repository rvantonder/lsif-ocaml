#!/bin/bash

# set -ex
set -e

MERLIN_BINARY="/Users/rvt/merlin/ocamlmerlin"

EXCLUDE="_build\|test"

FILES=$(find . -type f \( -name "*.ml" -o -name "*.mli" \) | grep -v "$EXCLUDE")
N_FILES=$(echo "$FILES" | wc -l)
i=0

for f in $FILES; do
  FILE_DIR=$(dirname $f)
  if [[ -f "$FILE_DIR/.merlin" ]]; then
    ((i++))
    printf "(%4d/%4d) %s\n" "$i" "$N_FILES" "$f"
    cat "$f" | $MERLIN_BINARY server lsif "$f" "-dot-merlin" "$FILE_DIR/.merlin" > "$f.lsif" # &
  fi
done

# wait

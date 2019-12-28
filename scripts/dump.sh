#!/bin/bash

set -e

function ctrl_c() {
        pkill -9 merlin
        exit 1
}

trap ctrl_c INT

EXISTS=$(command -v ocamlmerlin-with-lsif || echo)

if [ ! -n "$EXISTS" ]; then
    echo "LSIF support is not installed. Try 'opam pin add merlin-lsif https://github.com/rvantonder/merlin.git\#lsif'"
    exit 1
else  
    MERLIN_LSIF_BINARY=ocamlmerlin-with-lsif
fi

EXCLUDE="_build\|test"

while test $# -gt 0; do
    case "$1" in
        "-p") 
            shift
            PARALLEL_FRENZY="1"
            ;;
        "-h"|"--help"|"-help") echo -e "-exclude \"pattern\": exclude paths containing this regex pattern (\"_build|test\" is the default)\n-p: parallel frenzy (spawn a bunch of processes in parallel)\n-h: this help menu"; exit 0;;
        "-exclude") 
            shift
            if test $# -gt 0; then
                EXCLUDE=$1
                EXCLUDE=$(echo "$EXCLUDE" | sed 's/|/\\|/g')
            else
                echo "No pattern to exclude"
                exit 1
            fi
            shift
            ;;
        *) 
            break
            ;;
    esac
done

FILES=$(find . -type f \( -name "*.ml" -o -name "*.mli" \) | grep -v "$EXCLUDE")
N_FILES=$(echo "$FILES" | wc -l)
i=0

for f in $FILES; do
  FILE_DIR=$(dirname $f)
  if [[ -f "$FILE_DIR/.merlin" ]]; then
    ((i++)) || true
    printf "(%4d/%4d) %s\n" "$i" "$N_FILES" "$f"
   if [ -z "$PARALLEL_FRENZY" ]; then
      cat "$f" | $MERLIN_LSIF_BINARY server lsif "$f" "-dot-merlin" "$FILE_DIR/.merlin" > "$f.lsif.in"
    else
      cat "$f" | $MERLIN_LSIF_BINARY server lsif "$f" "-dot-merlin" "$FILE_DIR/.merlin" > "$f.lsif.in" 2> /dev/null &
    fi
  else
    ((i++)) || true
    printf "(%4d/%4d) %s (skipped, no .merlin)\n" "$i" "$N_FILES" "$f"
  fi
done

if [ ! -z "$PARALLEL_FRENZY" ]; then 
    echo "Waiting for parallel processes to finish..."
    wait
fi

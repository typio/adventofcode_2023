#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 dayXX (Do not put .ml!)"
    exit 1
fi

DAY_FILE=$1

ocamlopt -O3 -o "$DAY_FILE" utils.ml "$DAY_FILE.ml"

./"$DAY_FILE"

rm *.cmx *.o *.cmi "$DAY_FILE"
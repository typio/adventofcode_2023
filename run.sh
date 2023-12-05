#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 dayXX"
    exit 1
fi

DAY_FILE=$1

if [[ ! $DAY_FILE =~ ^day[0-9][0-9]$ ]]; then
    echo "Error: Argument should be in the format 'dayXX' where 'X' is a digit."
    exit 1
fi

ocamlopt -O3 -o "$DAY_FILE" utils.ml "$DAY_FILE.ml"

./"$DAY_FILE"

rm *.cmx *.o *.cmi "$DAY_FILE"

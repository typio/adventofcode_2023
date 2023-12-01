#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 dayXX"
    exit 1
fi

DAY_FILE=$1

ocamlc -c utils.ml

ocamlc -o "$DAY_FILE" utils.cmo "$DAY_FILE.ml"

./"$DAY_FILE"

rm *.cmo *.cmi "$DAY_FILE"
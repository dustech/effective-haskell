#!/bin/bash

# make dirs if they don't exists
mkdir -p ./bin
mkdir -p ./obj

# clean build files
# Funzione per verificare se una directory è vuota
is_dir_empty() {
    [ -z "$(ls -A "$1")" ]
}

# Pulizia dei file in bin se non è vuota
if ! is_dir_empty "./bin"; then
    rm ./bin/*
fi

# Pulizia dei file in obj se non è vuota
if ! is_dir_empty "./obj"; then
    rm ./obj/*
fi

# compile haskell
ghc -Wincomplete-patterns Main.hs -outputdir ./obj -o ./bin/Main
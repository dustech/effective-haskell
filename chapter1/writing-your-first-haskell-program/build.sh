#!/bin/bash

rm ./bin/*
rm ./obj/*
ghc Main.hs -outputdir ./obj -o ./bin/Main
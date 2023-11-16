#!/bin/bash

if [ -z "$1" ]; then
  echo "Error: Please specify a directory path as an argument."
  exit 1
fi

ln build.sh "$1/build.sh"
ln run.sh "$1/run.sh"
ln exec.sh "$1/exec.sh"
touch "$1/Main.hs"

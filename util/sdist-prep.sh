#!/usr/bin/env sh

## Run this script from the top-level of the repository.

rm -f fits-parse.cabal
stack sdist . --pvp-bounds=both --resolver=nightly

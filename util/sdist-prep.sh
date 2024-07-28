#!/usr/bin/env sh

## Run this script from the top-level of the repository.

stack sdist . --pvp-bounds=lower --resolver=nightly

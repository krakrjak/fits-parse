[![Build Status](https://travis-ci.com/krakrjak/fits-parse.svg?branch=master)](https://travis-ci.com/krakrjak/fits-parse)
[![Documentation Status](https://readthedocs.org/projects/fits-parse/badge/?version=latest)](https://fits-parse.readthedocs.io/en/latest/?badge=latest)

# fits-parse

FITS - Flexible Image Transport System

This project focuses on building a Haskell native parser for FITS files. This file format is used widely by Astronomers and those analyzing astronomical data. There is currently a C library called `fitsio` and a Haskell wrapper for the library. However, this library can be cumbersome to use and the Haskell bindings are very thin and close to the metal of the `fitsio` library. This project focuses on providing good Haskell bindings and datatypes by implementing the FITS spec directly in Haskell, without the need for a C library. The reference for this work comes from NASA in the US. Conformance is currently limited to the Version 4.0 specification with no extensions. See [the NASA site](https://fits.gsfc.nasa.gov/fits_standard.html) for more details and to download the spec.

# Documentation Building

To build the docs you need `sphinx`. To install it locally to a single user use `pip` with the `--user` option to install `sphinx` and `sphinx-rtd-theme`. Alternatively, on Ubuntu you can install the `python3-sphinx` and `python3-sphinx-rtd-theme` package.

# Reading Documentation

There are two sites with pointers on using this code. The [readthedocs](https://fits-parse.readthedocs.org) pages should guide you on installation and usage of the `fits-render` program. The [Haddock](https://krakrjak.github.io/fits-parse/index.html) docs are an API reference, which you can build locally with `stack build --haddock` for offline viewing.

# Lucretia

*A type-checker for Python and an intermediate language*

## About

An aim is to type-check as much as possible from Python code. We use an intermediate abstract language called `Lucretia` to which Python code is (will be) compilled.

## Installing package

~~~~ {.bash}
$ cabal install
~~~~

## Running Lucretia interpreter

~~~~ {.bash}
$ ./lucre_interpreter < LUCRETIA_SOURCE_FILE
~~~~

## Running tests

~~~~ {.bash}
$ cabal tests
~~~~

## Type-checker documentation

[Documentation](dist/doc/html/lucretia/index.html) is describing how the implementation of the Type Checker is connected with the theory describing the type-checker in the white paper (to be published).

To compile this documentation, run:

~~~~ {.bash}
$ cabal haddock --hyperlink-source
~~~~


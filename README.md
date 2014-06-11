# Lucretia: A type-checker for Python

We use an intermediate abstract language called `Lucretia` to which Python code is (will be) compilled.

## How to run
### Install
~~~~ {.bash}
$ cabal install
~~~~

### Run Lucretia interpreter

~~~~ {.bash}
$ ./lucre_interpreter < LUCRETIA_SOURCE_FILE
~~~~

### Run tests

~~~~ {.bash}
$ cabal tests
~~~~

## Documentation

[Documentation](dist/doc/html/lucretia/index.html) is describing how the implementation is connected with the theory from the white paper about Lucretia (to be published).

To compile the documentation, run:

~~~~ {.bash}
$ cabal haddock --hyperlink-source
~~~~

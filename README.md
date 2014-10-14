# Lucretia: A type-checker for Python

We use an intermediate abstract language called `Lucretia` to which Python will be compilled (to be implemented). For now the package provides functionality of type-checking programmes in `Lucretia`.

## How to run
### Install
~~~~ {.bash}
$ cabal install
~~~~

### Run tests

~~~~ {.bash}
$ cabal test
~~~~

## Documentation

[Documentation](dist/doc/html/lucretia/index.html) is describing how the implementation is connected with the theory from the white paper about Lucretia (to be published).

To compile the documentation, run:

~~~~ {.bash}
$ cabal haddock --hyperlink-source
~~~~

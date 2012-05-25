# Lucretia

*A type-checker for Python*

## About

An aim is to type-check as much as possible from Python code. We use an intermediate abstract language called `Lucretia` to which Python code is (will be) compilled.

## Running Lucretia interpreter

~~~~ {.bash}
$ ./lucre < LUCRETIA_SOURCE_FILE
~~~~

## Type-checker documentation

[Documentation](dist/doc/html/lucre/Test/Lucretia-TypeChecker-Main.html) is describing how the implementation of the Type Checker is connected with the theory describing the type-checker in the white paper (to be published).

To (re)compile this documentation, run:

~~~~ {.bash}
$ make doc
~~~~
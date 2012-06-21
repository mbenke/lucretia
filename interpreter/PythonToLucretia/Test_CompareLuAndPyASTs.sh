#!/bin/bash

runhaskell -f ghc-7.0.4 PythonToLucretia/Test_CompareLuAndPyASTs.hs | sed \
  -e "s/[a-z]*_annot = Span[^}]*}//g" \
  -e "s/, }/}/g" 


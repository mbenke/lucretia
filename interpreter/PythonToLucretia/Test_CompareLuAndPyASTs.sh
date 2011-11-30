#!/bin/bash

runhaskell PythonToLucretia/Test_CompareLuAndPyASTs.hs | sed \
  -e "s/[a-z]*_annot = Span[^}]*}//g" \
  -e "s/, }/}/g" 


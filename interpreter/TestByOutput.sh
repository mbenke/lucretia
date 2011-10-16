#!/bin/bash

runhaskell TestByOutput.hs | sed \
  -e "s/[a-z]*_annot = Span[^}]*}//g" \
  -e "s/, }/}/g" 


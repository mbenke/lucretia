module PrettyPrintUtils where

import Text.PrettyPrint
import Data.List (intersperse)

class Pretty a where
  pretty :: a -> Doc

hSpace :: Doc
hSpace = text ""

hLine :: Doc
hLine = text "----------------------------------------"

hDoubleLine :: Doc
hDoubleLine = text "========================================"

indent :: Doc -> Doc
indent = nest 2

hintersperse :: Doc -> [Doc] -> Doc
hintersperse separator ds =
  hcat $ intersperse separator ds

hcommasep :: [Doc] -> Doc
hcommasep = hintersperse (text ", ")


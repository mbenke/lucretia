{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PrettyDecodablePrint where

import Control.Exception
import Data.List(intercalate)

instance Exception [Char]

class Show a => ShowDecodable a where
  pretty :: a -> String

  prettyDecodable :: a -> String
  prettyDecodable a = show a ++ " `prettyPrintedAs` " ++ pretty a

  prettyPrintedAs :: a -> String -> a
  prettyPrintedAs a prettyA = if (pretty a) == prettyA 
				then a 
				else throw $ "Error, the 2nd argument is not: pretty (1st). It should be: " ++ pretty a ++ " but is: " ++ prettyA ++ "."

instance ShowDecodable String where
  pretty str = str

--instance ShowDecodable a => ShowDecodable [a] where
--  pretty xs = "[" ++ intercalate ", " (map pretty xs) ++ "]"

instance (ShowDecodable a, ShowDecodable b) => ShowDecodable (Either a b) where
--  pretty (Left a) = "Left " ++ pretty a
--  pretty (Right b) = "Right " ++ pretty b
  pretty (Left a) = pretty a
  pretty (Right b) = pretty b

instance (ShowDecodable a, ShowDecodable b) => ShowDecodable (a, b) where
  pretty (a, b) = "(" ++ pretty a ++ ", " ++ pretty b ++ ")"

  --TODO Doc
  
  
  -- Why all that stuff?
    -- to allow easy saving of output of (runCheck exp)
    -- as a HUnit test of a form: (exp, output)
  -- Sidenote: How it could be done differently?
    --
    --   serialize = show :: a -> HumanReadableString
    -- deserialize = read :: HumanReadableString -> a
    --
    -- read . show = id
    --
    -- http://web.archiveorange.com/archive/v/V6skItH7Yao0uZ9VZpDK
    --
    -- One could even write a framework containing some "combinators" for defining serialize. Such "combinators" would allow to automatically derive deserialize without any additional programmer effort to write the deserialize function.

--instance ShowDecodable (Either String (Type, CheckState)) where


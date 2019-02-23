module Chiasma.Data.Text.Pretty(
  prettyS,
) where

import qualified Data.Text as T (pack)
import Data.Text.Prettyprint.Doc (Doc, pretty)

prettyS :: String -> Doc a
prettyS = pretty . T.pack

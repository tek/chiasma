{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}

module Chiasma.Codec.Query where

import Data.Char (isUpper, toLower)
import qualified Data.Text as Text (concatMap, singleton)
import GHC.Generics (C1, D1, S1, Selector, selName, (:*:))

class TmuxDataQuery f where
  query' :: [Text]

instance TmuxDataQuery f => (TmuxDataQuery (D1 c f)) where
  query' = query' @f

instance TmuxDataQuery f => (TmuxDataQuery (C1 c f)) where
  query' = query' @f

instance (TmuxDataQuery f, TmuxDataQuery g) => TmuxDataQuery (f :*: g) where
  query' = query' @f <> query' @g

trans :: Char -> Text
trans a | isUpper a = toText @String ['_', toLower a]
trans a = Text.singleton a

snakeCase :: Text -> Text
snakeCase =
  Text.concatMap trans

formatQuery :: Text -> Text
formatQuery q = "#{" <> snakeCase q <> "}"

instance Selector s => (TmuxDataQuery (S1 s f)) where
  query' =
    [formatQuery (toText query)]
    where
      query = selName (undefined :: t s f p)

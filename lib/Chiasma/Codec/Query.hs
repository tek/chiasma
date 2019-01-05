{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Chiasma.Codec.Query(
  TmuxDataQuery(..),
) where

import GHC.Generics ((:*:), D1, C1, S1, Selector, selName)
import GHC.Unicode (isUpper, toLower)

class TmuxDataQuery f where
  query' :: [String]

instance (TmuxDataQuery f, TmuxDataQuery g) => TmuxDataQuery (f :*: g) where
  query' = query' @f ++ query' @g

instance TmuxDataQuery f => (TmuxDataQuery (D1 c f)) where
  query' = query' @f

instance TmuxDataQuery f => (TmuxDataQuery (C1 c f)) where
  query' = query' @f

trans :: Char -> String
trans a | isUpper a = ['_', toLower a]
trans a = [a]

snakeCase :: String -> String
snakeCase = (>>= trans)

formatQuery :: String -> String
formatQuery q = "#{" ++ snakeCase q ++ "}"

instance Selector s => (TmuxDataQuery (S1 s f)) where
  query' =
    [formatQuery query]
    where
      query = selName (undefined :: t s f p)

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}

module Chiasma.Codec.Query(
  TmuxDataQuery(..),
  Query(..),
) where

import GHC.Generics ((:*:), D1, C1, S1, Selector, selName)
import GHC.Unicode (isUpper, toLower)

newtype Query a =
  Query { unQ :: [String] }
  deriving Functor

class TmuxDataQuery f where
  query' :: Query (f a)

instance (TmuxDataQuery f, TmuxDataQuery g) => TmuxDataQuery (f :*: g) where
  query' =
    Query (
      unQ (query' :: Query (f a)) ++
      unQ (query' :: Query (g a))
      )

instance TmuxDataQuery f => (TmuxDataQuery (D1 c f)) where
  query' = Query (unQ (query' :: Query (f a)))

instance TmuxDataQuery f => (TmuxDataQuery (C1 c f)) where
  query' = Query (unQ (query' :: Query (f a)))

trans :: Char -> String
trans a | isUpper a = ['_', toLower a]
trans a = [a]

snakeCase :: String -> String
snakeCase = (>>= trans)

formatQuery :: String -> String
formatQuery q = "#{" ++ snakeCase q ++ "}"

instance Selector s => (TmuxDataQuery (S1 s f)) where
  query' =
    Query [formatQuery query]
    where
      query = selName (undefined :: t s f p)

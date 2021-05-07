module Chiasma.Data.List(
  head',
  tail',
  last',
) where

head' :: [a] -> Maybe a
head' (a : _) = Just a
head' _ = Nothing

tail' :: [a] -> Maybe [a]
tail' (_ : as) = Just as
tail' _ = Nothing

last' :: [a] -> Maybe a
last' = head' . reverse

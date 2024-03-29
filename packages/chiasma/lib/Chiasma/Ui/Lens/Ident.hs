module Chiasma.Ui.Lens.Ident where

import Control.Lens (Prism', prism)

import Chiasma.Data.Ident (Ident, Identifiable (..), sameIdent)

matchIdent :: Identifiable a => Ident -> Traversal' a a
matchIdent i =
  filtered (sameIdent i)

matchIdentL :: Identifiable a => Ident -> Traversal' [a] a
matchIdentL ident =
  each . matchIdent ident

identEither :: Identifiable a => Ident -> a -> Either a a
identEither target a =
  if sameIdent target a then Right a else Left a

matchIdentP :: Identifiable a => Ident -> Prism' a a
matchIdentP ident =
  prism id (identEither ident)

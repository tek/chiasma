module Chiasma.Ui.Lens.Ident(
  matchIdent,
  matchIdentL,
  matchIdentP,
) where

import Control.Lens (Traversal', Prism', filtered, each, prism)
import Chiasma.Data.Ident (Ident, Identifiable(..), sameIdent)

matchIdent :: Identifiable a => Ident -> Traversal' a a
matchIdent = filtered . sameIdent

matchIdentL :: Identifiable a => Ident -> Traversal' [a] a
matchIdentL ident = each . matchIdent ident

identEither :: Identifiable a => Ident -> a -> Either a a
identEither target a =
  if sameIdent target a then Right a else Left a

matchIdentP :: Identifiable a => Ident -> Prism' a a
matchIdentP ident = prism id (identEither ident)

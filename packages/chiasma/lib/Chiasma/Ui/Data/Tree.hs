module Chiasma.Ui.Data.Tree where

import Control.Lens (makeClassy)
import Prettyprinter (Pretty (..), nest, vsep)

data Tree f l p =
  Tree {
    _treeData :: l,
    _forest :: f (Node f l p)
    }

deriving stock instance (Eq l, Eq (Node [] l p)) => Eq (Tree [] l p)
deriving stock instance (Show l, Show (Node [] l p)) => Show (Tree [] l p)

deriving stock instance (Eq l, Eq (Node NonEmpty l p)) => Eq (Tree NonEmpty l p)
deriving stock instance (Show l, Show (Node NonEmpty l p)) => Show (Tree NonEmpty l p)

data Node f l p =
  Sub { _subTree :: Tree f l p }
  |
  Leaf { _leafData :: p }

makeClassy ''Tree
makeClassy ''Node

deriving stock instance (Eq l, Eq p) => Eq (Node [] l p)
deriving stock instance (Show l, Show p) => Show (Node [] l p)

deriving stock instance (Eq l, Eq p) => Eq (Node NonEmpty l p)
deriving stock instance (Show l, Show p) => Show (Node NonEmpty l p)

type LTree l p = Tree [] l p
type LNode l p = Node [] l p

type NTree l p = Tree NonEmpty l p
type NNode l p = Node NonEmpty l p

instance (Foldable f, Pretty l, Pretty p) => Pretty (Tree f l p) where
  pretty (Tree l sub) =
    nest 2 $ vsep $ pretty l : (pretty <$> toList sub)

instance (Foldable f, Pretty l, Pretty p) => Pretty (Node f l p) where
  pretty (Sub tree') = pretty tree'
  pretty (Leaf a) = pretty a

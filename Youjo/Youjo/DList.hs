module Youjo.DList
( DList(..)
, toDList
, fromDList
) where

import Control.Applicative
import Control.Monad
import Data.Foldable

newtype DList a = DList { getDList :: [a] -> [a] }

instance Show a => Show (DList a) where
  show ds = "DList " ++ show (fromDList ds)

instance Monoid (DList a) where
  mempty = DList id
  (DList f) `mappend` (DList g) = DList (f . g)

cons :: a -> DList a -> DList a
cons x xs = DList ((x:) . getDList xs)

instance Functor DList where
  fmap f = foldr (cons . f) mempty

instance Applicative DList where
  pure = return
  (<*>) = ap

instance Alternative DList where
  empty = mempty
  (<|>) = mappend

instance Monad DList where
  return x = DList (x:)
  m >>= k = foldr (mappend . k) mempty m
  fail _ = mempty

instance Foldable DList where
  foldr f x = foldr f x . fromDList

-- instance Traversable DList where

toDList :: [a] -> DList a
toDList xs = DList (xs ++)

fromDList :: DList a -> [a]
fromDList d = getDList d []

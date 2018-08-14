{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Youjo.Nest
(
) where

-- import GHC.Show
import Control.Applicative
import Control.Monad

newtype Nest2 n a = Nest2 (n (n a))

instance Functor n => Functor (Nest2 n) where
  fmap f (Nest2 x) = Nest2 $ fmap (fmap f) x

instance Applicative n => Applicative (Nest2 n) where
  pure x = Nest2 $ pure (pure x)
  (Nest2 f) <*> (Nest2 x) = Nest2 $ (<*>) <$> f <*> x

{--
instance Monad n => Monad (Nest2 n) where
  return = pure
  -- m >>= f = Nest2 $ fromNest (fromNest m >>= fmap f) >>= fmap join
  (Nest2 x) >>= f = Nest2 $ fmap (join . fromNest) (x >>= fmap f)
--}

instance Show (n (n a)) => Show (Nest2 n a) where
  show (Nest2 x) = "Nest2 (" ++ show x ++ ")"
-- instance Show (n (n a)) => Show (Nest2 n a) where
  -- showsPrec p x s = (showParen (p > appPrec) $ showString "" . showsPrec appPrec1 x) s

fromNest :: Nest2 n a -> n (n a)
fromNest (Nest2 x) = x

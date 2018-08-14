module Youjo.Applicative
( guard
, when
, unless
, goldaxe
, meld
, sever
, (<|)
, (|>)
) where

import Control.Applicative
import Control.Monad (guard, when, unless)

import Youjo.Tuple

goldaxe :: Alternative f => Bool -> a -> f a
goldaxe pred x = if pred then pure x else empty

meld :: Applicative f => (f a, f b) -> f (a, b)
meld = uncurry $ liftA2 (,)

sever :: (Applicative f, Traversable f, Monoid (f a))
     => f (a, b) -> (f a, f b)
sever = sequenceA . fmap (first pure)

(<|) :: Alternative f => f a -> a -> f a
xs <| x = xs <|> pure x

(|>) :: Alternative f => a -> f a -> f a
x |> xs = pure x <|> xs

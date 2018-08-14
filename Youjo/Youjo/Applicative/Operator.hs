module Youjo.Applicative.Operator
( (&&)
, (||)
, (==)
, (/=)
, (<)
, (>)
, (>=)
, (<=)
, (+)
, (-)
, (*)
, (/)
, (**)
, (^)
, (^^)
, (>>=)
, (>>)
, (=<<)
, (.)
, ($)
, ($!)
, (++)
, (!!)
) where

import qualified Prelude as P
import Control.Applicative (Applicative, liftA2)

(&&) :: Applicative f => f P.Bool -> f P.Bool -> f P.Bool
(&&) = liftA2 (P.&&)

(||) :: Applicative f => f P.Bool -> f P.Bool -> f P.Bool
(||) = liftA2 (P.||)

(==) :: (P.Eq a, Applicative f) => f a -> f a -> f P.Bool
(==) = liftA2 (P.==)

(/=) :: (P.Eq a, Applicative f) => f a -> f a -> f P.Bool
(/=) = liftA2 (P./=)

(<) :: (P.Ord a, Applicative f) => f a -> f a -> f P.Bool
(<) = liftA2 (P.<)

(>) :: (P.Ord a, Applicative f) => f a -> f a -> f P.Bool
(>) = liftA2 (P.>)

(>=) :: (P.Ord a, Applicative f) => f a -> f a -> f P.Bool
(>=) = liftA2 (P.>=)

(<=) :: (P.Ord a, Applicative f) => f a -> f a -> f P.Bool
(<=) = liftA2 (P.<=)

(+) :: (P.Num a, Applicative f) => f a -> f a -> f a
(+) = liftA2 (P.+)

(-) :: (P.Num a, Applicative f) => f a -> f a -> f a
(-) = liftA2 (P.-)

(*) :: (P.Num a, Applicative f) => f a -> f a -> f a
(*) = liftA2 (P.*)

(/) :: (P.Fractional a, Applicative f) => f a -> f a -> f a
(/) = liftA2 (P./)

(**) :: (P.Floating a, Applicative f) => f a -> f a -> f a
(**) = liftA2 (P.**)

(^) :: (P.Num a, P.Integral b, Applicative f) => f a -> f b -> f a
(^) = liftA2 (P.^)

(^^) :: (P.Fractional a, P.Integral b, Applicative f) => f a -> f b -> f a
(^^) = liftA2 (P.^^)

(>>=) :: (P.Monad m, Applicative f) => f (m a) -> f (a -> m b) -> f (m b)
(>>=) = liftA2 (P.>>=)

(>>) :: (P.Monad m, Applicative f) => f (m a) -> f (m b) -> f (m b)
(>>) = liftA2 (P.>>)

(=<<) :: (P.Monad m, Applicative f) => f (a -> m b) -> f (m a) -> f (m b)
(=<<) = liftA2 (P.=<<)

(.) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(.) = liftA2 (P..)

($) :: Applicative f => f (a -> b) -> f a -> f b
($) = liftA2 (P.$)

($!) :: Applicative f => f (a -> b) -> f a -> f b
($!) = liftA2 (P.$!)

(++) :: Applicative f => f [a] -> f [a] -> f [a]
(++) = liftA2 (P.++)

(!!) :: Applicative f => f [a] -> f P.Int -> f a
(!!) = liftA2 (P.!!)

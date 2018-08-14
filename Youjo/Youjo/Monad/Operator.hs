module Youjo.Monad.Operator
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
import Control.Monad (Monad, liftM2)

(&&) :: Monad f => f P.Bool -> f P.Bool -> f P.Bool
(&&) = liftM2 (P.&&)

(||) :: Monad f => f P.Bool -> f P.Bool -> f P.Bool
(||) = liftM2 (P.||)

(==) :: (P.Eq a, Monad f) => f a -> f a -> f P.Bool
(==) = liftM2 (P.==)

(/=) :: (P.Eq a, Monad f) => f a -> f a -> f P.Bool
(/=) = liftM2 (P./=)

(<) :: (P.Ord a, Monad f) => f a -> f a -> f P.Bool
(<) = liftM2 (P.<)

(>) :: (P.Ord a, Monad f) => f a -> f a -> f P.Bool
(>) = liftM2 (P.>)

(>=) :: (P.Ord a, Monad f) => f a -> f a -> f P.Bool
(>=) = liftM2 (P.>=)

(<=) :: (P.Ord a, Monad f) => f a -> f a -> f P.Bool
(<=) = liftM2 (P.<=)

(+) :: (P.Num a, Monad f) => f a -> f a -> f a
(+) = liftM2 (P.+)

(-) :: (P.Num a, Monad f) => f a -> f a -> f a
(-) = liftM2 (P.-)

(*) :: (P.Num a, Monad f) => f a -> f a -> f a
(*) = liftM2 (P.*)

(/) :: (P.Fractional a, Monad f) => f a -> f a -> f a
(/) = liftM2 (P./)

(**) :: (P.Floating a, Monad f) => f a -> f a -> f a
(**) = liftM2 (P.**)

(^) :: (P.Num a, P.Integral b, Monad f) => f a -> f b -> f a
(^) = liftM2 (P.^)

(^^) :: (P.Fractional a, P.Integral b, Monad f) => f a -> f b -> f a
(^^) = liftM2 (P.^^)

(>>=) :: (P.Monad m, Monad f) => f (m a) -> f (a -> m b) -> f (m b)
(>>=) = liftM2 (P.>>=)

(>>) :: (P.Monad m, Monad f) => f (m a) -> f (m b) -> f (m b)
(>>) = liftM2 (P.>>)

(=<<) :: (P.Monad m, Monad f) => f (a -> m b) -> f (m a) -> f (m b)
(=<<) = liftM2 (P.=<<)

(.) :: Monad f => f (b -> c) -> f (a -> b) -> f (a -> c)
(.) = liftM2 (P..)

($) :: Monad f => f (a -> b) -> f a -> f b
($) = liftM2 (P.$)

($!) :: Monad f => f (a -> b) -> f a -> f b
($!) = liftM2 (P.$!)

(++) :: Monad f => f [a] -> f [a] -> f [a]
(++) = liftM2 (P.++)

(!!) :: Monad f => f [a] -> f P.Int -> f a
(!!) = liftM2 (P.!!)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Youjo.Power
( Powers
, Power1(..)
, Power2(..)
, Power3(..)
, Power4(..)
, Power5(..)
, Power6(..)
, Power7(..)
, Power8(..)
, Power9(..)
, Power10(..)
, Power11(..)
, Power12(..)
, Power13(..)
, Power14(..)
, Power15(..)
, Power
, power1
, power2
, power3
, power4
, power5
, power6
, power7
, power8
, power9
, power10
, power11
, power12
, power13
, power14
, power15
, fromPower1
, fromPower2
, fromPower3
, fromPower4
, fromPower5
, fromPower6
, fromPower7
, fromPower8
, fromPower9
, fromPower10
, fromPower11
, fromPower12
, fromPower13
, fromPower14
, fromPower15
, dotp
, PowerLex(..)
, PowerRevlex(..)
, PowerEuc(..)
) where

import Control.Applicative
import Data.Data
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Typeable
import GHC.Generics
import GHC.TypeLits

import Youjo.Foldable

class (Applicative p, Foldable p) => Powers (p :: * -> *)

newtype Power1 a = Power1 a
  deriving(Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power2 a = Power2 (a, a)
  deriving(Eq, Show, Read, Bounded, Data, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power3 a = Power3 (a, a, a)
  deriving(Eq, Show, Read, Bounded, Data, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power4 a = Power4 (a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Data, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power5 a = Power5 (a, a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Data, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power6 a = Power6 (a, a, a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Data, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power7 a = Power7 (a, a, a, a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Data, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power8 a = Power8 (a, a, a, a, a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power9 a = Power9 (a, a, a, a, a, a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power10 a = Power10 (a, a, a, a, a, a, a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power11 a = Power11 (a, a, a, a, a, a, a, a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power12 a = Power12 (a, a, a, a, a, a, a, a, a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power13 a = Power13 (a, a, a, a, a, a, a, a, a, a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power14 a = Power14 (a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Typeable, Generic,
           Functor, Foldable, Traversable)

newtype Power15 a = Power15 (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
  deriving(Eq, Show, Read, Bounded, Typeable, Generic,
           Functor, Foldable, Traversable)

type family Power (n :: Nat) :: * -> *
type instance Power 1 = Power1
type instance Power 2 = Power2
type instance Power 3 = Power3
type instance Power 4 = Power4
type instance Power 5 = Power5
type instance Power 6 = Power6
type instance Power 7 = Power7
type instance Power 8 = Power8
type instance Power 9 = Power9
type instance Power 10 = Power10
type instance Power 11 = Power11
type instance Power 12 = Power12
type instance Power 13 = Power13
type instance Power 14 = Power14
type instance Power 15 = Power15

power1 :: a -> Power1 a
power1 = Power1

power2 :: a -> a -> Power2 a
power2 = (Power2 .) .  (,)

power3 :: a -> a -> a -> Power3 a
power3 = ((Power3 .) .) . (,,)

power4 :: a -> a -> a -> a -> Power4 a
power4 = (((Power4 .) .) .) . (,,,)

power5 :: a -> a -> a -> a -> a -> Power5 a
power5 = ((((Power5 .) .) .) .) . (,,,,)

power6 :: a -> a -> a -> a -> a -> a -> Power6 a
power6 = (((((Power6 .) .) .) .) .) . (,,,,,)

power7 :: a -> a -> a -> a -> a -> a -> a -> Power7 a
power7 = ((((((Power7 .) .) .) .) .) .) . (,,,,,,)

power8 :: a -> a -> a -> a -> a -> a -> a -> a -> Power8 a
power8 = (((((((Power8 .) .) .) .) .) .) .) . (,,,,,,,)

power9 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> Power9 a
power9 = ((((((((Power9 .) .) .) .) .) .) .) .) . (,,,,,,,,)

power10 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Power10 a
power10 = (((((((((Power10 .) .) .) .) .) .) .) .) .) . (,,,,,,,,,)

power11 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> Power11 a
power11 = ((((((((((Power11 .) .) .) .) .) .) .) .) .) .) . (,,,,,,,,,,)

power12 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a
        -> Power12 a
power12 = (((((((((((Power12 .) .) .) .) .) .) .) .) .) .) .)
            . (,,,,,,,,,,,)

power13 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a
        -> Power13 a
power13 = ((((((((((((Power13 .) .) .) .) .) .) .) .) .) .) .) .)
            . (,,,,,,,,,,,,)

power14 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a
        -> a -> Power14 a
power14 = (((((((((((((Power14 .) .) .) .) .) .) .) .) .) .) .) .) .)
            . (,,,,,,,,,,,,,)

power15 :: a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> a
        -> a -> a -> Power15 a
power15 = ((((((((((((((Power15 .) .) .) .) .) .) .) .) .) .) .) .) .) .)
            . (,,,,,,,,,,,,,,)

fromPower1 :: Power1 a -> a
fromPower1 (Power1 x) = x

fromPower2 :: Power2 a -> (a, a)
fromPower2 (Power2 x) = x

fromPower3 :: Power3 a -> (a, a, a)
fromPower3 (Power3 x) = x

fromPower4 :: Power4 a -> (a, a, a, a)
fromPower4 (Power4 x) = x

fromPower5 :: Power5 a -> (a, a, a, a, a)
fromPower5 (Power5 x) = x

fromPower6 :: Power6 a -> (a, a, a, a, a, a)
fromPower6 (Power6 x) = x

fromPower7 :: Power7 a -> (a, a, a, a, a, a, a)
fromPower7 (Power7 x) = x

fromPower8 :: Power8 a -> (a, a, a, a, a, a, a, a)
fromPower8 (Power8 x) = x

fromPower9 :: Power9 a -> (a, a, a, a, a, a, a, a, a)
fromPower9 (Power9 x) = x

fromPower10 :: Power10 a -> (a, a, a, a, a, a, a, a, a, a)
fromPower10 (Power10 x) = x

fromPower11 :: Power11 a -> (a, a, a, a, a, a, a, a, a, a, a)
fromPower11 (Power11 x) = x

fromPower12 :: Power12 a -> (a, a, a, a, a, a, a, a, a, a, a, a)
fromPower12 (Power12 x) = x

fromPower13 :: Power13 a -> (a, a, a, a, a, a, a, a, a, a, a, a, a)
fromPower13 (Power13 x) = x

fromPower14 :: Power14 a -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a)
fromPower14 (Power14 x) = x

fromPower15 :: Power15 a -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
fromPower15 (Power15 x) = x

instance Powers Power1

instance Powers Power2

instance Powers Power3

instance Powers Power4

instance Powers Power5

instance Powers Power6

instance Powers Power7

instance Powers Power8

instance Powers Power9

instance Powers Power10

instance Powers Power11

instance Powers Power12

instance Powers Power13

instance Powers Power14

instance Powers Power15

instance Applicative Power1 where
  pure = Power1
  Power1 f <*> Power1 x = Power1 (f x)

instance Applicative Power2 where
  pure x = Power2 (x, x)
  Power2 (f1, f2) <*> Power2 (x1, x2) = Power2 (f1 x1, f2 x2)

instance Applicative Power3 where
  pure x = Power3 (x, x, x)
  Power3 (f1, f2, f3) <*> Power3 (x1, x2, x3)
    = Power3 (f1 x1, f2 x2, f3 x3)

instance Applicative Power4 where
  pure x = Power4 (x, x, x, x)
  Power4 (f1, f2, f3, f4) <*> Power4 (x1, x2, x3, x4)
    = Power4 (f1 x1, f2 x2, f3 x3, f4 x4)

instance Applicative Power5 where
  pure x = Power5 (x, x, x, x, x)
  Power5 (f1, f2, f3, f4, f5) <*> Power5 (x1, x2, x3, x4, x5)
    = Power5 (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5)

instance Applicative Power6 where
  pure x = Power6 (x, x, x, x, x, x)
  Power6 (f1, f2, f3, f4, f5, f6) <*> Power6 (x1, x2, x3, x4, x5, x6)
    = Power6 (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6)

instance Applicative Power7 where
  pure x = Power7 (x, x, x, x, x, x, x)
  Power7 (f1, f2, f3, f4, f5, f6, f7)
    <*> Power7 (x1, x2, x3, x4, x5, x6, x7)
      = Power7 (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6, f7 x7)

instance Applicative Power8 where
  pure x = Power8 (x, x, x, x, x, x, x, x)
  Power8 (f1, f2, f3, f4, f5, f6, f7, f8)
    <*> Power8 (x1, x2, x3, x4, x5, x6, x7, x8)
      = Power8 (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6, f7 x7, f8 x8)

instance Applicative Power9 where
  pure x = Power9 (x, x, x, x, x, x, x, x, x)
  Power9 (f1, f2, f3, f4, f5, f6, f7, f8, f9)
    <*> Power9 (x1, x2, x3, x4, x5, x6, x7, x8, x9)
      = Power9 (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6, f7 x7, f8 x8,
                f9 x9)

instance Applicative Power10 where
  pure x = Power10 (x, x, x, x, x, x, x, x, x, x)
  Power10 (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
    <*> Power10 (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
      = Power10 (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6, f7 x7, f8 x8,
                 f9 x9, f10 x10)

instance Applicative Power11 where
  pure x = Power11 (x, x, x, x, x, x, x, x, x, x, x)
  Power11 (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
    <*> Power11 (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
      = Power11 (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6, f7 x7, f8 x8,
                 f9 x9, f10 x10, f11 x11)

instance Applicative Power12 where
  pure x = Power12 (x, x, x, x, x, x, x, x, x, x, x, x)
  Power12 (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
    <*> Power12 (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
      = Power12 (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6, f7 x7, f8 x8,
                 f9 x9, f10 x10, f11 x11, f12 x12)

instance Applicative Power13 where
  pure x = Power13 (x, x, x, x, x, x, x, x, x, x, x, x, x)
  Power13 (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
    <*> Power13 (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
      = Power13 (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6, f7 x7, f8 x8,
                 f9 x9, f10 x10, f11 x11, f12 x12, f13 x13)

instance Applicative Power14 where
  pure x = Power14 (x, x, x, x, x, x, x, x, x, x, x, x, x, x)
  Power14 (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
    <*> Power14 (x1, x2, x3, x4, x5, x6, x7, x8, x9,
                 x10, x11, x12, x13, x14)
      = Power14 (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6, f7 x7, f8 x8,
                 f9 x9, f10 x10, f11 x11, f12 x12, f13 x13, f14 x14)

instance Applicative Power15 where
  pure x = Power15 (x, x, x, x, x, x, x, x, x, x, x, x, x, x, x)
  Power15 (f1, f2, f3, f4, f5, f6, f7, f8, f9,
           f10, f11, f12, f13, f14, f15)
    <*> Power15 (x1, x2, x3, x4, x5, x6, x7, x8, x9,
                 x10, x11, x12, x13, x14, x15)
      = Power15 (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5,
                 f6 x6, f7 x7, f8 x8, f9 x9, f10 x10,
                 f11 x11, f12 x12, f13 x13, f14 x14, f15 x15)

instance Monoid a => Monoid (Power1 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power2 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power3 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power4 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power5 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power6 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power7 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power8 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power9 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power10 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power11 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power12 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power13 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power14 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q

instance Monoid a => Monoid (Power15 a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q


dotp :: (Num a, Applicative p, Foldable p, Powers p) => p a -> p a -> a
dotp = (getSum . ) . appFold Sum (*)


newtype PowerLex p a = PowerLex { fromPowerLex :: p a }
  deriving(Show, Read, Bounded, Data, Typeable, Generic, Monoid,
           Functor, Applicative, Foldable, Traversable, Powers)

instance (Powers p, Eq a) => Eq (PowerLex p a) where
  p1 == p2 = getAll $ appFold All (==) p1 p2

instance (Powers p, Ord a) => Ord (PowerLex p a) where
  compare p1 p2 = fold $ compare <$> p1 <*> p2

newtype PowerRevlex p a = PowerRevlex { fromPowerRevlex :: p a }
  deriving(Show, Read, Bounded, Data, Typeable, Generic, Monoid,
           Functor, Applicative, Foldable, Traversable, Powers)

instance (Powers p, Eq a) => Eq (PowerRevlex p a) where
  p1 == p2 = getAll $ appFold All (==) p1 p2

instance (Powers p, Ord a) => Ord (PowerRevlex p a) where
  compare p1 p2 = getDual $ appFold Dual compare p1 p2

newtype PowerEuc p a = PowerEuc { fromPowerEuc :: p a }
  deriving(Show, Read, Bounded, Data, Typeable, Generic, Monoid,
           Functor, Applicative, Foldable, Traversable, Powers)

instance (Powers p, Eq a, Num a) => Eq (PowerEuc p a) where
  p1 == p2 = dotp p1 p1 == dotp p2 p2

instance (Powers p, Ord a, Num a) => Ord (PowerEuc p a) where
  compare p1 p2 = compare (dotp p1 p1) (dotp p2 p2)

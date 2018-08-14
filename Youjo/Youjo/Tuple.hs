module Youjo.Tuple
( module Data.Tuple
, first
, second
, (***)
, (&&&)
, assoc
, unassoc
, tmap
, tmap2
, tmap3
, tmap4
, tmap5
, tmap6
, tmap7
, tmap8
, tmap9
, tmap10
, tmap11
, tmap12
, tmap13
, tmap14
, tmap15
, tap
, tap2
, tap3
, tap4
, tap5
, tap6
, tap7
, tap8
, tap9
, tap10
, tap11
, tap12
, tap13
, tap14
, tap15
, dup
, dup2
, dup3
, dup4
, dup5
, dup6
, dup7
, dup8
, dup9
, dup10
, dup11
, dup12
, dup13
, dup14
, dup15
) where

import Control.Arrow (first, second, (***), (&&&))
import Data.Tuple

assoc :: ((a, b), c) -> (a, (b, c))
assoc ((x, y), z) = (x, (y, z))

unassoc :: (a, (b, c)) -> ((a, b), c)
unassoc (x, (y, z)) = ((x, y), z)

tmap :: (a -> b) -> (a, a) -> (b, b)
tmap f (x1, x2) = (f x1, f x2)

tmap2 :: (a -> b) -> (a, a) -> (b, b)
tmap2 = tmap

tmap3 :: (a -> b) -> (a, a, a) -> (b, b, b)
tmap3 f (x1, x2, x3) = (f x1, f x2, f x3)

tmap4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
tmap4 f (x1, x2, x3, x4) = (f x1, f x2, f x3, f x4)

tmap5 :: (a -> b) -> (a, a, a, a, a) -> (b, b, b, b, b)
tmap5 f (x1, x2, x3, x4, x5) = (f x1, f x2, f x3, f x4, f x5)

tmap6 :: (a -> b) -> (a, a, a, a, a, a) -> (b, b, b, b, b, b)
tmap6 f (x1, x2, x3, x4, x5, x6) = (f x1, f x2, f x3, f x4, f x5, f x6)

tmap7 :: (a -> b) -> (a, a, a, a, a, a, a) -> (b, b, b, b, b, b, b)
tmap7 f (x1, x2, x3, x4, x5, x6, x7)
  = (f x1, f x2, f x3, f x4, f x5, f x6, f x7)

tmap8 :: (a -> b) -> (a, a, a, a, a, a, a, a) -> (b, b, b, b, b, b, b, b)
tmap8 f (x1, x2, x3, x4, x5, x6, x7, x8)
  = (f x1, f x2, f x3, f x4, f x5, f x6, f x7, f x8)

tmap9 :: (a -> b) -> (a, a, a, a, a, a, a, a, a)
                  -> (b, b, b, b, b, b, b, b, b)
tmap9 f (x1, x2, x3, x4, x5, x6, x7, x8, x9)
  = (f x1, f x2, f x3, f x4, f x5, f x6, f x7, f x8, f x9)

tmap10 :: (a -> b) -> (a, a, a, a, a, a, a, a, a, a)
                   -> (b, b, b, b, b, b, b, b, b, b)
tmap10 f (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  = (f x1, f x2, f x3, f x4, f x5, f x6, f x7, f x8, f x9, f x10)

tmap11 :: (a -> b) -> (a, a, a, a, a, a, a, a, a, a, a)
                   -> (b, b, b, b, b, b, b, b, b, b, b)
tmap11 f (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  = (f x1, f x2, f x3, f x4, f x5, f x6, f x7, f x8, f x9, f x10, f x11)

tmap12 :: (a -> b) -> (a, a, a, a, a, a, a, a, a, a, a, a)
                   -> (b, b, b, b, b, b, b, b, b, b, b, b)
tmap12 f (x1, x2, x3, x4, x5, x6, x7, x8, x9,
        x10, x11, x12)
  = (f x1, f x2, f x3, f x4, f x5, f x6, f x7, f x8, f x9,
     f x10, f x11, f x12)

tmap13 :: (a -> b) -> (a, a, a, a, a, a, a, a, a, a, a, a, a)
                   -> (b, b, b, b, b, b, b, b, b, b, b, b, b)
tmap13 f (x1, x2, x3, x4, x5, x6, x7, x8, x9,
        x10, x11, x12, x13)
  = (f x1, f x2, f x3, f x4, f x5, f x6, f x7, f x8, f x9,
     f x10, f x11, f x12, f x13)

tmap14 :: (a -> b) -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a)
                   -> (b, b, b, b, b, b, b, b, b, b, b, b, b, b)
tmap14 f (x1, x2, x3, x4, x5, x6, x7, x8, x9,
          x10, x11, x12, x13, x14)
  = (f x1, f x2, f x3, f x4, f x5, f x6, f x7, f x8, f x9,
     f x10, f x11, f x12, f x13, f x14)

tmap15 :: (a -> b) -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
                   -> (b, b, b, b, b, b, b, b, b, b, b, b, b, b, b)
tmap15 f (x1, x2, x3, x4, x5, x6, x7, x8, x9,
          x10, x11, x12, x13, x14, x15)
  = (f x1, f x2, f x3, f x4, f x5, f x6, f x7, f x8, f x9,
     f x10, f x11, f x12, f x13, f x14, f x15)

tap :: (a1 -> b1, a2 -> b2) -> (a1, a2) -> (b1, b2)
tap (f1, f2) (x1, x2) = (f1 x1, f2 x2)

tap2 :: (a1 -> b1, a2 -> b2) -> (a1, a2) -> (b1, b2)
tap2 = tap

tap3 :: (a1 -> b1, a2 -> b2, a3 -> b3) -> (a1, a2, a3) -> (b1, b2, b3)
tap3 (f1, f2, f3) (x1, x2, x3) = (f1 x1, f2 x2, f3 x3)

tap4 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4)
     -> (a1, a2, a3, a4) -> (b1, b2, b3, b4)
tap4 (f1, f2, f3, f4) (x1, x2, x3, x4) = (f1 x1, f2 x2, f3 x3, f4 x4)

tap5 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4, a5 -> b5)
     -> (a1, a2, a3, a4, a5) -> (b1, b2, b3, b4, b5)
tap5 (f1, f2, f3, f4, f5) (x1, x2, x3, x4, x5)
  = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5)

tap6 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4, a5 -> b5, a6 -> b6)
     -> (a1, a2, a3, a4, a5, a6) -> (b1, b2, b3, b4, b5, b6)
tap6 (f1, f2, f3, f4, f5, f6) (x1, x2, x3, x4, x5, x6)
  = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6)

tap7 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4, a5 -> b5,
          a6 -> b6, a7 -> b7)
     -> (a1, a2, a3, a4, a5, a6, a7) -> (b1, b2, b3, b4, b5, b6, b7)
tap7 (f1, f2, f3, f4, f5, f6, f7) (x1, x2, x3, x4, x5, x6, x7)
  = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6, f7 x7)

tap8 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4, a5 -> b5,
          a6 -> b6, a7 -> b7, a8 -> b8)
     -> (a1, a2, a3, a4, a5, a6, a7, a8)
     -> (b1, b2, b3, b4, b5, b6, b7, b8)
tap8 (f1, f2, f3, f4, f5, f6, f7, f8) (x1, x2, x3, x4, x5, x6, x7, x8)
  = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6, f7 x7, f8 x8)

tap9 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4, a5 -> b5,
          a6 -> b6, a7 -> b7, a8 -> b8, a9 -> b9)
     -> (a1, a2, a3, a4, a5, a6, a7, a8, a9)
     -> (b1, b2, b3, b4, b5, b6, b7, b8, b9)
tap9 (f1, f2, f3, f4, f5, f6, f7, f8, f9)
     (x1, x2, x3, x4, x5, x6, x7, x8, x9)
  = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5, f6 x6, f7 x7, f8 x8, f9 x9)

tap10 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4, a5 -> b5,
          a6 -> b6, a7 -> b7, a8 -> b8, a9 -> b9, a10 -> b10)
      -> (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
      -> (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
tap10 (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
      (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5,
     f6 x6, f7 x7, f8 x8, f9 x9, f10 x10)

tap11 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4, a5 -> b5,
          a6 -> b6, a7 -> b7, a8 -> b8, a9 -> b9, a10 -> b10,
          a11 -> b11)
      -> (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
      -> (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11)
tap11 (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
      (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5,
     f6 x6, f7 x7, f8 x8, f9 x9, f10 x10, f11 x11)

tap12 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4, a5 -> b5,
          a6 -> b6, a7 -> b7, a8 -> b8, a9 -> b9, a10 -> b10,
          a11 -> b11, a12 -> b12)
      -> (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
      -> (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12)
tap12 (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
      (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
  = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5,
     f6 x6, f7 x7, f8 x8, f9 x9, f10 x10, f11 x11, f12 x12)

tap13 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4, a5 -> b5,
          a6 -> b6, a7 -> b7, a8 -> b8, a9 -> b9, a10 -> b10,
          a11 -> b11, a12 -> b12, a13 -> b13)
      -> (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
      -> (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13)
tap13 (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
      (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13)
  = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5,
     f6 x6, f7 x7, f8 x8, f9 x9, f10 x10, f11 x11, f12 x12, f13 x13)

tap14 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4, a5 -> b5,
          a6 -> b6, a7 -> b7, a8 -> b8, a9 -> b9, a10 -> b10,
          a11 -> b11, a12 -> b12, a13 -> b13, a14 -> b14)
      -> (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
      -> (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14)
tap14 (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
      (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)
  = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5,
     f6 x6, f7 x7, f8 x8, f9 x9, f10 x10,
     f11 x11, f12 x12, f13 x13, f14 x14)

tap15 :: (a1 -> b1, a2 -> b2, a3 -> b3, a4 -> b4, a5 -> b5,
          a6 -> b6, a7 -> b7, a8 -> b8, a9 -> b9, a10 -> b10,
          a11 -> b11, a12 -> b12, a13 -> b13, a14 -> b14, a15 -> b15)
      -> (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
      -> (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15)
tap15 (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
      (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  = (f1 x1, f2 x2, f3 x3, f4 x4, f5 x5,
     f6 x6, f7 x7, f8 x8, f9 x9, f10 x10,
     f11 x11, f12 x12, f13 x13, f14 x14, f15 x15)

dup :: a -> (a, a)
dup x = (,) x x

dup2 :: a -> (a, a)
dup2 = dup

dup3 :: a -> (a, a, a)
dup3 x = (,,) x x x

dup4 :: a -> (a, a, a, a)
dup4 x = (,,,) x x x x

dup5 :: a -> (a, a, a, a, a)
dup5 x = (,,,,) x x x x x

dup6 :: a -> (a, a, a, a, a, a)
dup6 x = (,,,,,) x x x x x x

dup7 :: a -> (a, a, a, a, a, a, a)
dup7 x = (,,,,,,) x x x x x x x

dup8 :: a -> (a, a, a, a, a, a, a, a)
dup8 x = (,,,,,,,) x x x x x x x x

dup9 :: a -> (a, a, a, a, a, a, a, a, a)
dup9 x = (,,,,,,,,) x x x x x x x x x

dup10 :: a -> (a, a, a, a, a, a, a, a, a, a)
dup10 x = (,,,,,,,,,) x x x x x x x x x x

dup11 :: a -> (a, a, a, a, a, a, a, a, a, a, a)
dup11 x = (,,,,,,,,,,) x x x x x x x x x x x

dup12 :: a -> (a, a, a, a, a, a, a, a, a, a, a, a)
dup12 x = (,,,,,,,,,,,) x x x x x x x x x x x x

dup13 :: a -> (a, a, a, a, a, a, a, a, a, a, a, a, a)
dup13 x = (,,,,,,,,,,,,) x x x x x x x x x x x x x

dup14 :: a -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a)
dup14 x = (,,,,,,,,,,,,,) x x x x x x x x x x x x x x

dup15 :: a -> (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
dup15 x = (,,,,,,,,,,,,,,) x x x x x x x x x x x x x x x

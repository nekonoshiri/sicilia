{-# LANGUAGE DeriveDataTypeable #-}

module Youjo.Math.Quaternion
( Quaternion(..)
, realPart
, imagPart
, conjugate
, mkPolar
, polar
, magnitude
, imagMagnitude
) where

import Data.Data
import Data.Typeable

newtype Quaternion a = Quaternion (a, a, a, a)
  deriving (Eq, Show, Read, Data, Typeable)

instance RealFloat a => Num (Quaternion a) where
  Quaternion (w, x, y, z) + Quaternion (w', x', y', z') =
    Quaternion (w + w', x + x', y + y', z + z')
  Quaternion (w, x, y, z) - Quaternion (w', x', y', z') =
    Quaternion (w - w', x - x', y - y', z - z')
  Quaternion (w, x, y, z) * Quaternion (w', x', y', z') =
    Quaternion ( w * w' - x * x' - y * y' - z * z'
               , w * x' + x * w' + y * z' - z * y'
               , w * y' - x * z' + y * w' + z * x'
               , w * z' + x * y' - y * x' + z * w'
               )
  negate (Quaternion (w, x, y, z)) =
    Quaternion (negate w, negate x, negate y, negate z)
  abs q = Quaternion (magnitude q, 0, 0, 0)
  signum (Quaternion (0, 0, 0, 0)) = 0
  signum q@(Quaternion (w, x, y, z)) = Quaternion (w/r, x/r, y/r, z/r)
    where r = magnitude q
  fromInteger n = Quaternion (fromInteger n, 0, 0, 0)

instance RealFloat a => Fractional (Quaternion a) where
  Quaternion (w, x, y, z) / Quaternion (w', x', y', z') =
    Quaternion ( (w * w'' + x * x'' + y * y'' + z * z'') / d
               , (x * w'' - w * x'' - y * z'' + z * y'') / d
               , (y * w'' - w * y'' - z * x'' + x * z'') / d
               , (z * w'' - w * z'' - x * y'' + y * x'') / d
               )
      where w'' = scaleFloat k w'
            x'' = scaleFloat k x'
            y'' = scaleFloat k y'
            z'' = scaleFloat k z'
            k = - maximum [exponent w', exponent x', exponent y', exponent z']
            d = w' * w'' + x' * x'' + y' * y'' + z' * z''
  fromRational a = Quaternion (fromRational a, 0, 0, 0)

{-
instance RealFloat a => Floating (Quaternion a) where
  pi = Quaternion (pi, 0, 0, 0)
  exp (Quaternion (w, 0, 0, 0)) = Quaternion (exp w, 0, 0, 0)
  exp q@(Quaternion (w, x, y, z)) =
    Quaternion (expw * cos v, expw_sincv * x, expw_sincv * y, expw_sincv * z)
      where expw = exp w
            v = imagMagnitude q
            sincv = (sin v) / v
            expw_sincv = expw * sincv
  log (Quaternion (w, 0, 0, 0)) = Quaternion (log w, 0, 0, 0)
  log q@(Quaternion (w, x, y, z)) = Quaternion (log r, t * x, t * y, t * z)
    where r = magnitude q
          v = imagMagnitude q
          t = (atan (v / w)) / v
-}

realPart :: Quaternion a -> a
realPart (Quaternion (w, _, _, _)) = w

imagPart :: Quaternion a -> (a, a, a)
imagPart (Quaternion (_, x, y, z)) = (x, y, z)

conjugate :: Num a => Quaternion a -> Quaternion a
conjugate (Quaternion (w, x, y, z)) = Quaternion (w, -x, -y, -z)

mkPolar :: Floating a => a -> a -> a -> a -> Quaternion a
mkPolar r t1 t2 t3 = Quaternion ( r * cos t1
                                , rsint1 * cos t2
                                , rsint1sint2 * cos t3
                                , rsint1sint2 * sin t3
                                )
  where rsint1 = r * sin t1
        rsint1sint2 = rsint1 * sin t2

polar :: RealFloat a => Quaternion a -> (a, a, a, a)
polar (Quaternion (w, 0, 0, 0)) = (w, 0, 0, 0)
polar q@(Quaternion (w, x, y, z)) = (r, t1, acos (x / (r * sin t1)), atan2 z y)
  where r = magnitude q
        t1 = acos (w / r)

magnitude :: RealFloat a => Quaternion a -> a
magnitude (Quaternion (w, x, y, z)) = scaleFloat k
  (sqrt (sqr (scaleFloat mk w) + sqr (scaleFloat mk x)
       + sqr (scaleFloat mk y) + sqr (scaleFloat mk z)))
    where k = maximum [exponent z, exponent x, exponent y, exponent z]
          mk = - k
          sqr t = t * t

imagMagnitude :: RealFloat a => Quaternion a -> a
imagMagnitude (Quaternion (_, x, y, z)) = magnitude (Quaternion (0, x, y, z))

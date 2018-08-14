{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Youjo.Matrix
( Matrix(..)
, MatrixP
, matrix
, fromMatrix
, thruMatrix
, raidMatrix
, matmul
, toListC
, toListR
, mapC
, mapC'
, mapR
, mapR'
, matseq
, pile
, rowVec
, colVec
, vecmat
, matvec
, zeroMx
, riverMx
, idMx
, diagMx
, rotMx2D
, rotMx3D
, rotMx3Dx
, rotMx3Dy
, rotMx3Dz
) where

import Control.Applicative
import Data.Bool (bool)
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Traversable
import GHC.TypeLits
import Youjo.Power
import Youjo.Traversable (itrmap)

newtype Matrix p q a = Matrix (p (q a))
  deriving (Eq, Show, Read, Bounded,
            Functor, Foldable, Traversable)

type family MatrixP (n1 :: Nat) (n2 :: Nat) :: * -> *
type instance MatrixP n m = Matrix (Power n) (Power m)

instance (Applicative p, Applicative q) => Applicative (Matrix p q) where
  pure = Matrix . pure . pure
  (Matrix f) <*> (Matrix x) = Matrix $ (<*>) <$> f <*> x

instance (Applicative p, Applicative q, Monoid a)
         => Monoid (Matrix p q a) where
  mempty = pure mempty
  p `mappend` q = mappend <$> p <*> q


matrix :: p (q a) -> Matrix p q a
matrix = Matrix

fromMatrix :: Matrix p q a -> p (q a)
fromMatrix (Matrix p1) = p1

thruMatrix :: (p1 (q1 a) -> p2 (q2 b)) -> Matrix p1 q1 a -> Matrix p2 q2 b
thruMatrix f (Matrix p) = Matrix (f p)

raidMatrix :: (p (q a) -> b) -> Matrix p q a -> b
raidMatrix f (Matrix p1) = f p1

matmul :: (Num a, Applicative p, Applicative q, Applicative r,
           Traversable p, Traversable q, Traversable r)
       => Matrix p q a -> Matrix q r a -> Matrix p r a
matmul (Matrix p1) (Matrix q1) = Matrix $ sequenceA $ for p1 f
  where q2 = sequenceA q1
        f p' = fmap (sum . liftA2 (*) p') q2

toListC :: (Foldable p, Foldable q) => Matrix p q a -> [[a]]
toListC = raidMatrix (fmap F.toList . F.toList)

toListR :: (Foldable p, Foldable q) => Matrix p q a -> [[a]]
toListR = L.transpose . toListC

mapC :: Functor p => (q a -> r b) -> Matrix p q a -> Matrix p r b
mapC = thruMatrix . fmap

mapC' :: Functor p => (q a -> b) -> Matrix p q a -> p b
mapC' = raidMatrix . fmap

mapR :: (Applicative q, Applicative r, Traversable p, Traversable q)
     => (p a -> r b) -> Matrix p q a -> Matrix r q b
mapR f = matseq . mapC f . matseq

mapR' :: (Applicative q, Traversable p) => (p a -> b) -> Matrix p q a -> q b
mapR' f = mapC' f . matseq

matseq :: (Traversable p, Applicative q) => Matrix p q a -> Matrix q p a
matseq = thruMatrix sequenceA

pile :: (Applicative p, Traversable q) => q a -> Matrix p q a
pile = Matrix . pure

rowVec :: Traversable p => p a -> Matrix Power1 p a
rowVec = pile

colVec :: (Applicative p, Traversable p) => p a -> Matrix p Power1 a
colVec = matseq . rowVec

vecmat :: (Num a, Applicative p, Applicative q,
           Traversable p, Traversable q)
       => p a -> Matrix p q a -> q a
vecmat = ((fromPower1 . fromMatrix) . ) . matmul . rowVec

matvec :: (Num a, Applicative p, Applicative q,
           Traversable p, Traversable q)
       => Matrix p q a -> q a -> p a
matvec m p1 = fmap fromPower1 $ fromMatrix $ matmul m (colVec p1)

zeroMx :: (Applicative p, Applicative q, Num a) => Matrix p q a
zeroMx = pure 0

riverMx :: (Applicative p, Traversable p) => a -> a -> Matrix p p a
riverMx x y = bool x y <$> d
  where p = itrmap succ (pure 0)
        m = (pile p)
        d = (==) <$> m <*> (matseq m)

idMx :: (Num a, Applicative p, Traversable p) => Matrix p p a
idMx = riverMx 0 1

diagMx :: (Num a, Applicative p, Traversable p) => p a -> Matrix p p a
diagMx q = bool 0 <$> (pile q) <*> d
  where p = itrmap succ (pure 0)
        m = (pile p)
        d = (==) <$> m <*> (matseq m)

rotMx2D :: Floating a => a -> Matrix Power2 Power2 a
rotMx2D t = Matrix $ power2 (power2 cost (-sint)) (power2 sint cost)
  where sint = sin t
        cost = cos t

rotMx3D :: Floating a => Power3 a -> a -> Matrix Power3 Power3 a
rotMx3D axis@(Power3 (x, y, z)) t = Matrix $ power3 p q r
  where (Power3 (nx, ny, nz)) = fmap (/ sqrt (x * x + y * y + z * z)) axis
        sint = sin t
        cost = cos t
        mcost = 1 - cost
        nxmcost = nx * mcost
        nymcost = ny * mcost
        nzmcost = nz * mcost
        nxnymcost = nx * nymcost
        nxnzmcost = nx * nzmcost
        nynzmcost = ny * nzmcost
        nxsint = nx * sint
        nysint = ny * sint
        nzsint = nz * sint
        p = power3 (nx * nxmcost + cost)
                   (nxnymcost + nzsint)
                   (nxnzmcost - nysint)
        q = power3 (nxnymcost - nzsint)
                   (ny * nymcost + cost)
                   (nynzmcost + nxsint)
        r = power3 (nxnzmcost + nysint)
                   (nynzmcost - nxsint)
                   (nz * nzmcost + cost)

{--
(nx * nx * (1 - cos t) + cos t) (nx * ny * (1 - cos t) + nz * sin t) (nx * nz * (1 - cos t) - ny * sin t)
(nx * ny * (1 - cos t) - nz * sin t) (ny * ny * (1 - cos t) + cos t) (ny * nz * (1 - cos t) + nx * sin t)
(nx * nz * (1 - cos t) + ny * sin t) (ny * nz * (1 - cos t) - nx * sin t) (nz * nz * (1 - cos t) + cos t)
--}

rotMx3Dx :: Floating a => a -> Matrix Power3 Power3 a
rotMx3Dx t = Matrix $ power3 p q r
  where sint = sin t
        cost = cos t
        p = power3 1 0 0
        q = power3 0 cost sint
        r = power3 0 (-sint) cost

rotMx3Dy :: Floating a => a -> Matrix Power3 Power3 a
rotMx3Dy t = Matrix $ power3 p q r
  where sint = sin t
        cost = cos t
        p = power3 cost 0 (-sint)
        q = power3 0 1 0
        r = power3 sint 0 cost

rotMx3Dz :: Floating a => a -> Matrix Power3 Power3 a
rotMx3Dz t = Matrix $ power3 p q r
  where sint = sin t
        cost = cos t
        p = power3 cost sint 0
        q = power3 (-sint) cost 0
        r = power3 0 0 1

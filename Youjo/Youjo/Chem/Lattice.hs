{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Youjo.Chem.Lattice
( Lattice
, lattice
, latticePt
, latticePtInt
, latticePts
, latticePtsInt
, latticeBound
, latticeBoundInt
) where

-- import qualified Youjo.Applicative.Operator as AO
import Control.Applicative
import Text.Read.Lex as L
import Youjo.Power

data Lattice p a = Lattice (p a) (p (p a))

instance (Bounded (p a), Bounded (p (p a))) => Bounded (Lattice p a) where
  minBound = Lattice minBound minBound
  maxBound = Lattice maxBound maxBound

instance (Eq (p a), Eq (p (p a))) => Eq (Lattice p a) where
  (Lattice r1 as1) == (Lattice r2 as2) = r1 == r2 && as1 == as2

-- instance (Data (p a), Data (p (p a))) => Data (Lattice p a) where

{--
instance (Read (p a), Read (p (p a))) => Read (Lattice p a) where
  readPrec =
    parens
      (do expectP (L.Ident "
--}

instance (Show (p a), Show (p (p a))) => Show (Lattice p a) where
  show (Lattice r as) =
    showString "Lattice (" .  shows r . showString ") (" . shows as $ ")"

-- instance (Typeable (p a), Typeable (p (p a))) => Typeable (Lattice p a) where

-- instance (Generic (p a), Generic (p (p a))) => Generic (Lattice p a) where

instance Functor p => Functor (Lattice p) where
  fmap f (Lattice r as) = Lattice (fmap f r) ((fmap . fmap) f as)

{--
instance Applicative p => Applicative (Lattice p) where
  pure x = Lattice (pure x) ((pure . pure) x)
  f <*> (Lattice r as) = Lattice (f <*> r) (((<*>) . (<*>)) f as)
--}

-- instance (Foldable (p a), Foldable (p (p a))) => Foldable (Lattice p a) where
-- instance (Traversable (p a), Traversable (p (p a))) => Traversable (Lattice p a) where

lattice :: p a -> p (p a) -> Lattice p a
lattice = Lattice

latticePt :: (Num a, Applicative p, Foldable p)
          => Lattice p a -> p a -> p a
latticePt (Lattice r as) us =
  foldr (liftA2 (+)) r $ fmap . (*) <$> us <*> as

latticePtInt :: (Num a, Integral b, Applicative p, Foldable p)
             => Lattice p a -> p b -> p a
latticePtInt lat = latticePt lat . liftA fromIntegral

latticePts :: (Num a, Applicative p, Foldable p)
           => Lattice p a -> [p a] -> [p a]
latticePts lat uss = [ latticePt lat us | us <- uss ]

latticePtsInt :: (Num a, Integral b, Applicative p, Foldable p)
              => Lattice p a -> [p b] -> [p a]
latticePtsInt lat = latticePtsInt lat . liftA (liftA fromIntegral)

latticeBound :: (Enum a, Num a, Applicative p, Traversable p)
             => Lattice p a -> p (Power2 a) -> [p a]
latticeBound lat ubounds =
  [ latticePt lat us | us <- sequenceA $ (\(Power2(x, y)) -> [x..y]) <$> ubounds ]

latticeBoundInt :: (Enum a, Num a, Integral b, Applicative p, Traversable p)
                => Lattice p a -> p (Power2 b) -> [p a]
latticeBoundInt lat = latticeBound lat . liftA (liftA fromIntegral)

-- reciproLat :: Lattice p a -> Lattice p a
-- reciproLat (Lattice r as) = 

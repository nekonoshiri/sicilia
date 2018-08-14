{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Youjo.Math.Metric
( Metric
) where

class Real b => Metric a b where
  dist :: a -> a -> b

newtype AbsDist a = AbsDist a

instance Real a => Metric (AbsDist a) a where
  dist (AbsDist x) (AbsDist y) = abs (x - y)

{--
newtype 

instance Num a => Metric a where
  dist x y = abs (x - y)
  --}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

class Power t p | t -> p, p -> t where
  power :: t -> p
  fromPower :: p -> t

data Power2 a = Power2 (a, a)
  deriving (Show)


instance Power (a, a) (Power2 a) where
  power (x, y) = Power2 (x, y)
  fromPower (Power2 p) = p

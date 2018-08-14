{-# LANGUAGE FlexibleInstances #-}

class Buildable f where
  build :: f

instance Buildable (a -> Maybe a) where
  build = Just

test :: a -> Maybe a
test = build

instance Buildable (a -> b -> (a, b)) where
  build = (,)

test2 :: a -> b -> (a, b)
test2 = build


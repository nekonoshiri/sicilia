{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Youjo.Nest.HNest
( HNest1(..)
, HNest2(..)
) where

import Control.Monad

app :: Applicative f => f (a -> b) -> f a -> f b
app = (<*>)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = x >>= f

unnest1 :: HNest1 p a -> p a
unnest1 (HNest1 x) = x

newtype HNest1 p a = HNest1 (p a)
  deriving (Show, Functor, Applicative, Monad)

{--
instance (Functor p) => Functor (HNest1 p) where
  fmap f (HNest1 x) = HNest1 $ fmap f x

instance (Applicative p) => Applicative (HNest1 p) where
  pure = HNest1 . pure
  (HNest1 f) <*> (HNest1 x) = HNest1 $ app f x

instance (Monad p) => Monad (HNest1 p) where
  return = pure
  (HNest1 x) >>= f = HNest1 $ bind (unnest1 . f) x
--}

newtype HNest2' p q a = HNest2' (p (HNest1 q a))

instance (Functor p, Functor q) => Functor (HNest2' p q) where
  fmap f (HNest2' x) = HNest2' $ (fmap . fmap) f x





newtype HNest2 p q a = HNest2 (p (q a))
  deriving (Show)

instance (Functor p, Functor q) => Functor (HNest2 p q) where
  fmap f (HNest2 x) = HNest2 $ (fmap . fmap) f x

instance (Applicative p, Applicative q) => Applicative (HNest2 p q) where
  pure = HNest2 . (pure . pure)
  (HNest2 f) <*> (HNest2 x) = HNest2 $ (app . fmap app) f x
                                      -- (<*>) <$> f <*> x

instance (Monad p, Monad q, Traversable q) => Monad (HNest2 p q) where
  return = pure
  (HNest2 x) >>= f =
    HNest2 $ x >>= liftM join . sequence . liftM (unnest . f)
      where unnest (HNest2 t) = t

data P :: * -> *

instance Functor P

instance Applicative P

instance Monad P

data Q :: * -> *

instance Functor Q

instance Applicative Q

instance Monad Q

data A

data B

unnest2 :: HNest2 p q a -> p (q a)
unnest2 (HNest2 x) = x

x :: P (Q A)
x = undefined

f :: A -> HNest2 P Q B
f = undefined

test1 :: A -> P (Q B)
test1 = unnest2 . f

test10 :: P (A -> P (Q B))
test10 = return test1

test100 :: P (A -> Q B)
test100 = undefined

test11 :: Q a -> (a -> Q b) -> Q b
test11 = (>>=)

test110 :: (a -> Q b) -> Q a -> Q b
test110 = flip (>>=)

test1100 :: (P a -> P (Q b)) -> Q a -> Q b
test1100 = undefined

test111 :: Q a -> P (a -> Q b) -> P (Q b)
test111 y g = (fmap . test11) y g


test112 :: P (a -> Q b) -> Q a -> P (Q b)
test112 g y = (fmap . test11) y g

test113 :: P A -> P (Q B)
test113 = join . fmap test1

-- test112 を変形してつくれないか
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- (>>= test1) :: P A -> P (Q B)
-- これを P (A -> Q B) にすればよい


test114 :: (P a -> P (Q b)) -> Q a -> P (Q b)
test114 g y = undefined


test115 :: (P a -> P (Q b)) -> P (Q a) -> P (P (Q b))
test115 g = fmap (test114 g)

test116 :: (P a -> P (Q b)) -> P (Q a) -> P (Q b)
test116 g y = join $ fmap (test114 g) y

test117 :: P (Q A) -> P (Q B)
test117 = test116 test113

test118 :: P (Q B)
test118 = test117 x

test119 :: P (Q B)
-- test119 = x >>= (test114 (>>= test1))
test119 = x >>= \y -> fmap (y >>=) test100


instance Foldable P

instance Traversable P

test120 :: A -> Q (P B)
test120 = sequence . unnest2 . f

test12 :: P A -> P (Q B)
test12 y = y >>= (unnest2 . f)

test121 :: P A -> Q (P B)
test121 y = sequence $ y >>= (unnest2 . f)


test13 :: Q (P A) -> (P A -> Q b) -> Q b
test13 = (>>=)

test2 :: Q A -> Q (P B)
test2 x = x >>= (sequence . unnest2 . f)

test20 :: P (Q A) -> P (Q (P B))
test20 = fmap (\x -> x >>= (sequence . unnest2 . f))

test200 :: Q A -> Q (P (Q B))
test200 = fmap (unnest2 . f)

instance Foldable Q

instance Traversable Q

test201 :: P (Q A) -> P (Q B)
test201 x = x >>= fmap join . sequence . fmap (unnest2 . f)


test21 :: Q A -> P (Q B)
test21 = undefined

test22 :: P (Q B)
test22 = x >>= test21

test3 :: P A -> P (P (Q B))
test3 = fmap (unnest2 . f)

test31 :: P A -> P (Q B)
test31 = join . fmap (unnest2 . f)





test4 :: Q A -> P (Q (P (Q B)))
test4 = return . fmap (unnest2 . f)

test41 :: Q A -> Q (Q (P (Q B)))
test41 = return . fmap (unnest2 . f)

test42 :: P A -> P (P (P (Q B)))
test42 = return . fmap (unnest2 . f)

test43 :: P A -> Q (P (Q B))
test43 x = return $ x >>= (unnest2 . f)

-- test44 :: P (Q A) -> P (Q B)
test44 x = sequence x >>= test43

test5 :: P (Q A) -> P (Q (P (Q B)))
test5 y = fmap (fmap (unnest2 . f)) y

test6 :: Q (Q A) -> Q (Q (P (Q B)))
test6 y = fmap (fmap (unnest2 . f)) y

test7 :: P (Q (P (Q B)))
test7 = test5 x

{--
test5 :: (Monad p, Monad q) => p (p (q b))
test5 = x >>= test3

test6 :: (Monad p, Monad q) => p (q b)
test6 = join test5
--}

--test3 :: (Monad p, Monad q) => p (q a)
--test3 = x >>= test1


{--
test2 :: (Monad p, Monad q) => p a -> p (HNest2 p q b)
test2 = fmap f

fmap' :: (Monad p, Monad q) => (a -> b) -> HNest2 p q a -> HNest2 p q b
fmap' = fmap

nest :: (Monad p, Monad q) => p (q (p (q a)))
nest = (unnest2 . fmap (unnest2 . f)) x
--}

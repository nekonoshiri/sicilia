module Youjo.Foldable
( count
, pick
, pickDft
, pickhead
, headDft
, picklast
, lastDft
, appFold
, groupL
, groupL'
) where

import Control.Applicative
import Data.Foldable
import Data.Monoid

import Youjo.Applicative
import Youjo.DList

count :: (Enum b, Foldable t) => (a -> Bool) -> t a -> b -> b
count pred xs z = foldr (\x -> if pred x then succ else id) z xs

pick :: Foldable f => Int -> f a -> Maybe a
pick n t
  | n < 0     = Nothing
  | otherwise = pickL n (toList t)
  where pickL _ [] = Nothing
        pickL 0 (x:_) = Just x
        pickL n (x:xs) = pickL (n - 1) xs

pickDft :: Foldable f => Int -> b -> (a -> b) -> f a -> b
pickDft n d f = maybe d f . pick n

pickhead :: Foldable f => f a -> Maybe a
pickhead = pick 0

headDft :: Foldable f => b -> (a -> b) -> f a -> b
headDft = pickDft 0

picklast :: Foldable f => f a -> Maybe a
picklast t = pick (length t - 1) t

lastDft :: Foldable f => b -> (a -> b) -> f a -> b
lastDft d f t = pickDft (length t - 1) d f t

appFold :: (Applicative f, Foldable f, Monoid m)
        => (a -> m) -> (b -> c -> a) -> f b -> f c -> m
appFold fm g x y = foldMap fm $ g <$> x <*> y

groupL :: Foldable t => Int -> t a -> [[a]]
groupL n ts
  | n <= 0    = groupL 1 ts
  | otherwise = let (rest, xss, _) = foldl' g (empty, empty, 0) ts
                in  fromDList (xss <| fromDList rest)
  where g (buf, res, ct) x
          | ct >= n   = (pure x, res <| fromDList buf, 1)
          | otherwise = (buf <| x, res, ct + 1)

groupL' :: Foldable t => Int -> t a -> [[a]]
groupL' n ts
  | n <= 0    = groupL' 1 ts
  | otherwise = let (_, xss, _) = foldl' g (empty, empty, 1) ts
                in  fromDList xss
  where g (buf, res, ct) x
          | ct >= n   = (empty, res <| fromDList (buf <| x), 1)
          | otherwise = (buf <| x, res, ct + 1)

-- groupR

-- groupR'

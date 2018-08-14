module Youjo.Traversable
( gizel
, gizer
, gizelMaybe
, gizerMaybe
, gizelAlter
, gizerAlter
, itrmap
, itrmap0
, tscanl
, tscanl1
, tscanr
, tscanr1
, scanl
, scanl1
, scanr
, scanr1
, accumlateL
, accumlateR
, reverse
, sort
, sortBy
) where

import Prelude hiding (reverse, scanl, scanl1, scanr, scanr1)
import Control.Applicative
import Data.Foldable
import Data.Traversable
import qualified Data.List as L

import Youjo.Applicative
import Youjo.Foldable
import Youjo.Tuple

gizel :: Traversable t => ([a] -> [a]) -> (t a -> t a)
gizel f t = snd $ accumlateL (\(x:xs) -> (xs, x)) (f $ toList t) t

gizer :: Traversable t => ([a] -> [a]) -> (t a -> t a)
gizer f t = snd $ accumlateR (\(x:xs) -> (xs, x)) (f $ toList t) t

gizelMaybe :: Traversable t => ([a] -> [a]) -> (t a -> Maybe (t a))
gizelMaybe f t = sequence $ snd $ accumlateL g (Just (f $ toList t)) t
  where g (Just (x:xs)) = (Just xs, Just x)
        g _ = (Nothing, Nothing)

gizerMaybe :: Traversable t => ([a] -> [a]) -> (t a -> Maybe (t a))
gizerMaybe f t = sequence $ snd $ accumlateR g (Just (f $ toList t)) t
  where g (Just (x:xs)) = (Just xs, Just x)
        g _ = (Nothing, Nothing)

gizelAlter :: (Traversable t, Alternative t) => ([a] -> [a]) -> (t a -> t a)
gizelAlter = (maybe empty id . ) . gizelMaybe

gizerAlter :: (Traversable t, Alternative t) => ([a] -> [a]) -> (t a -> t a)
gizerAlter = (maybe empty id .) . gizerMaybe

itrmap :: Traversable t => (a -> a) -> t a -> t a
itrmap f = snd . mapAccumL (\g y -> (f . g, g y)) id

itrmap0 :: Traversable t => (a -> a) -> t a -> t a
itrmap0 f = snd . mapAccumL (\g y -> (f . g, g y)) f

tscanl :: Traversable t => (b -> a -> b) -> b -> t a -> t b
tscanl f z = snd . mapAccumL (\acc x -> (f acc x, f acc x)) z

tscanl1 :: Traversable t => (a -> a -> a) -> t a -> Maybe (t a)
tscanl1 f t = return $ snd $ mapAccumL g Nothing t
  where g Nothing x    = (Just x, x)
        g (Just acc) x = (Just $ f acc x, f acc x)

tscanr :: Traversable t => (a -> b -> b) -> b -> t a -> t b
tscanr f z = snd . mapAccumR (\acc x -> (f x acc, f x acc)) z

tscanr1 :: Traversable t => (a -> a -> a) -> t a -> Maybe (t a)
tscanr1 f t = return $ snd $ mapAccumR g Nothing t
    where g Nothing x    = (Just x, x)
          g (Just acc) x = (Just $ f x acc , f x acc)

scanl :: (Traversable t, Alternative t) => (b -> a -> b) -> b -> t a -> t b
scanl f z t = pure z <|> (tscanl f z t)

scanl1 :: (Traversable t, Alternative t) => (a -> a -> a) -> t a -> t a
scanl1 = (maybe empty id . ) . tscanl1

scanr :: (Traversable t, Alternative t) => (a -> b -> b) -> b -> t a -> t b
scanr f z = (\(x, y) -> pure x <|> y) . mapAccumR (\ac x -> (f x ac, ac)) z

scanr1 :: (Traversable t, Alternative t) => (a -> a -> a) -> t a -> t a
scanr1 = (maybe empty id .) . tscanr1


-- tscan
-- scan
-- tscanMap
-- scanMap

-- tscanrM
-- tscanlM
-- scanrM
-- scanlM

{--
mapAccumLM :: (Traversable t, Monad m)
           => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumLM f z = meld . second sequence . mapAccumL f' (return z)
  where f' :: m a -> b -> (m a, m c)
        f' = undefined
  -- acc x = acc >>= (flip f) x
--}

-- mapAccumLM
-- mapAccumRM


accumlateL :: Traversable t => (a -> (a, c)) -> a -> t b -> (a, t c)
accumlateL f = mapAccumL ((f . ) . const)

accumlateR :: Traversable t => (a -> (a, c)) -> a -> t b -> (a, t c)
accumlateR f = mapAccumR ((f . ) . const)



-- modify n

reverse :: Traversable t => t a -> t a
reverse = gizel L.reverse

-- isPrefixOf

-- isSuffixOf

-- isInfixOf

-- isSubsequenceOf

-- elemindex
-- elemindices
--findindex
--findindices


-- zip
-- zipwith

-- unzip

sort :: (Ord a, Traversable t) => t a -> t a
sort = gizel L.sort

sortBy :: Traversable t => (a -> a -> Ordering) -> t a -> t a
sortBy = gizel . L.sortBy

--sort
-- etc


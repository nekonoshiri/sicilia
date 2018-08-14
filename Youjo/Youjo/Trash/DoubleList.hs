
newtype DoubleList a = DoubleList [[a]]
  deriving (Show)

instance Functor DoubleList where
  fmap f (DoubleList xss) = DoubleList $ fmap (fmap f) xss
  --   fmap f (DoubleList xss) = DoubleList $ map (map f) xss


{--
functor 則
1. fmap id = id
2. fmap (f . g) = fmap f . fmap g
の確認

1.
リストに対しての fmap は functor 則 fmap id = id を満たしているので

fmap id (DoubleList xss)
= DoubleList $ fmap (fmap id) xss
= DoubleList $ fmap id xss
= DoubleList $ id xss
= DoubleList xss
= id (DoubleList xss)

2.
リストに対しての fmap は functor 則 fmap (f . g) = fmap f . fmap g
を満たしている

また (f . g) x = f (g (x)) より

fmap (f . g) (DoubleList xss)
= DoubleList $ fmap (fmap (f . g)) xss
= DoubleList $ fmap (fmap f . fmap g) xss
= DoubleList $ (fmap (fmap f)) . (fmap (fmap g)) $ xss
= DoubleList $ fmap (fmap f) ((fmap (fmap g)) xss)
= DoubleList $ fmap (fmap f) (fmap (fmap g) xss)
= fmap f (DoubleList $ fmap (fmap g) xss)
= fmap f (fmap g (DoubleList xss))
= (fmap f . fmap g) (DoubleList xss)

下から読んだ方がわかりやすい
--}

instance Applicative DoubleList where
  pure x = DoubleList [[x]]
  (DoubleList f) <*> (DoubleList x) = DoubleList $ (fmap f) <*> x

{--
g :: [[a -> b]]
x :: [[a]]
目的 :: DoubleList [[b]]

(<*>) :: Applicative f => f (a -> b) -> f a -> f b

リストは Applicative なので f を [] とすると
(<*>) :: [a -> b] -> [a] -> [b]

Control.Applicative.liftA
  :: Applicative f => (s -> t) -> f s -> f t

s = [a -> b], t = ([a] -> [b]) と考えれば

(<*>) :: s -> t より liftA でリフトできて

liftA (<*>) :: Applicative f => f s -> f t

つまり


liftA (<*>) :: Applicative f => f [a -> b] -> f ([a] -> [b])

リストは Applicative なので f を [] とすると

liftA (<*>) :: [[a -> b]] -> [[a] -> [b]]

よって g が当てられて

liftA (<*>) g :: [[a] -> [b]]

a' = [a], b' = [b] とおけば

liftA (<*>) g :: [a' -> b']


これより <*> :: [a' -> b'] -> [a'] -> [b'] が適用できて

(<*>) (liftA (<*>) g) :: [a'] -> [b']

つまり
(<*>) (liftA (<*>) g) :: [[a]] -> [[b]]

よって x を渡すことができて

(liftA (<*>) g) <*> x:: [[b]]

DoubleList でくるんで

DoubleList $ (liftA (<*>) g) <*> x:: DoubleList [[b]]


ちなみに
liftA = fmap = (<$>) より

DoubleList $ (<*>) <$> g <*> x:: DoubleList [[b]]
--}

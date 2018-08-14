

type Loop = Either

next :: a -> Loop a b
next = Left

loop :: (a -> Loop a b) -> a -> b
loop f x = case f x of
             Left y  -> loop f y
             Right z -> z

test2 x = loop test x

test x = do
  if x < 10 then return x else next (x `div` 10)

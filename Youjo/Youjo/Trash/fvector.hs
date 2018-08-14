import Control.Arrow
import Youjo.Power

data Fector p a = Fector (p a -> a)

instance Functor (Fector p) where
  fmap f (Fector g) = Fector ()


-- app (FVector f) p = f p

{--
fv1 :: FVector Power3 Double
fv1 = FVector $ \(Power3 (x, y, z)) -> x + y + z

fv2 :: FVector Power3 Double
fv2 = FVector $ \(Power3 (x, y, z)) -> x - y + z

fv3 :: FVector Power3 Double
fv3 = FVector $ \p -> app fv1 p + app fv2 p


data Function a b = Function (a -> b)

type FVector p a = Function (p a -> a)



type FVector p a = p a -> a

fv1 :: FVector Power3 Double
fv1 = \(Power3 (x, y, z)) -> x + y + z

fv2 :: FVector Power3 Double
fv2 = \(Power3 (x, y, z)) -> x - y + z

test p = uncurry (+) $ (fv1 &&& fv2) p
--}


-- data FVector p a b = p a -> b

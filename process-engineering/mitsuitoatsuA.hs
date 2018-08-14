{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import qualified Data.Foldable as F

newtype Vtr4D a = Vtr4D (a, a, a, a)
  deriving (Show, Functor)

instance Applicative Vtr4D where
  pure x = Vtr4D (x, x, x, x)
  Vtr4D (f1, f2, f3, f4) <*> Vtr4D (x1, x2, x3, x4)
    = Vtr4D (f1 x1, f2 x2, f3 x3, f4 x4)

instance F.Foldable Vtr4D where
  foldr f a (Vtr4D (x1, x2, x3, x4)) = foldr f a [x1, x2, x3, x4]

-- treated as (x, y, z, 0)
type Vtr3D a = Vtr4D a

vtr3D :: Num a => (a, a, a) -> Vtr3D a
vtr3D (x, y, z) = Vtr4D (x, y, z, 0)

dif :: Num a => a -> a -> a
dif x y = abs(x - y)

printIx :: (Show a, Show c, Enum c) => c -> [a] -> IO ()
printIx _ []     = return ()
printIx c (x:xs) = putStr (shows c ": ") >> print x >> printIx (succ c) xs

-- Mol / second
type Mol_s = Double

-- temperature (Celsius)
type TmpC = Double

-- conversion vector
type ConV = Vtr4D Double

type X = Vtr4D Mol_s

type Pres = Double

-- pressure vector
type PresV = Vtr3D Pres

type Reactor = X -> X

type Flasher = X -> (X, X)

type Mixer = [X] -> X

data FlasState = FlasState { pf :: Pres, ηf :: ConV }
  deriving (Show)

class Vacant a where
  vacant :: a

instance Vacant FlasState where
  vacant = FlasState { pf = 0, ηf = Vtr4D (0, 0, 0, 0) }

main :: IO()
main = do
  let (result, fh, fl) = process ε ts x1 x1
  printIx 1 result
  putStrLn . showString "P (high) = " . shows (pf fh) $ " Pa"
  putStrLn . showString "P (low)  = " . shows (pf fl) $ " Pa"
  putStrLn . showString "η (high) = " $ show (ηf fh)
  putStrLn . showString "η (low)  = " $ show (ηf fl)
    where ε  = 1e-6
          ts = 200
          x1 = feedToX 2.051 1.002 0 0

process :: Double -> TmpC -> X -> X -> ([X], FlasState, FlasState)
process ε ts x1 = proc x2 -> do
  x3 <- reactor ts -< x2
  ((x4, x5), fh) <- runFlasher flasher_HP -< x3
  ((x6, x7), fl) <- runFlasher flasher_LP -< x5
  x2_new <- mixer -< [x4, x6, x1]
  if F.all (< ε) (dif <$> x2 <*> x2_new)
    then returnA -< ([x1, x2, x3, x4, x5, x6, x7], fh, fl)
    else process ε ts x1 -< x2_new

runFlasher :: (X -> State FlasState Flasher) -> X -> ((X, X), FlasState)
runFlasher fm x = let (f, s) = runState (fm x) vacant
                  in  (f x, s)

flasher_HP :: X -> State FlasState Flasher
flasher_HP x = makeFlasher δ x t η_assume
  where δ = 1e-6
        t = 133.0
        η_assume = Vtr4D (0.5, 0.8228, 0.5, 0)

flasher_LP :: X -> State FlasState Flasher
flasher_LP x = makeFlasher δ x t η_assume
  where δ = 1e-6
        t = 82.3
        η_assume = Vtr4D (0.5, 0.9737, 0.5, 0)

flasher :: ConV -> Flasher
flasher η x = (xg, (-) <$> x <*> xg)
  where xg = (*) <$> η <*> x

makeFlasher :: Double -> X -> TmpC -> ConV -> State FlasState Flasher
makeFlasher δ x_in t η_assume = do
  find_η δ x_in t η_assume
  fmap ηf get >>= return . flasher

-- assume η1, η3
-- η2 was already known, η4 = 0
find_η :: Double -> X -> TmpC -> ConV -> State FlasState ()
find_η δ x_in t η = do
  let x_l   = snd $ flasher η x_in
      p0    = calc_p0 t
      x     = calc_x x_l
      γ     = calc_γ x
      η_new = calc_η p0 γ η
  if F.all (< δ) (dif <$> η <*> η_new)
    then put FlasState { pf = calc_pf p0 x γ, ηf = η_new }
    else find_η δ x_in t η_new

calc_pf :: PresV -> Vtr3D Double -> Vtr3D Double -> Pres
calc_pf p0 x γ = F.sum $ (\a b c -> a * b * c) <$> γ <*> p0 <*> x

calc_x :: X -> Vtr3D Double
calc_x (Vtr4D (x1, x2, x3, _))
  = let x = vtr3D (x1, x2, x3)
    in  fmap (/ F.sum x) x

calc_γ :: Vtr3D Double -> Vtr3D Double
calc_γ (Vtr4D (x1, x2, x3, x4))
  = vtr3D ( 10 ** ((-0.796 * x3 * x3 - 2.155 * x2 * x3) / (x1 + 1.220 * x2 + 1.038 * x3) ** 2)
          , 10 ** ((0.820 * x3 * x3 + 1.237 * x1 * x3) / (0.819 * x1 + x2 + 0.835 * x3) ** 2)
          , 10 ** ((-0.755 * x1 * x1 + 1.411 * x2 * x2 - 0.236 * x1 * x2) / (0.938 * x1 + 1.198 * x2 + x3) ** 2)
          )

calc_η :: PresV -> Vtr3D Double -> ConV -> ConV
calc_η (Vtr4D (p1, p2, p3, _)) (Vtr4D (γ1, γ2, γ3, _)) (Vtr4D (_, η2, _, η4))
  = Vtr4D ( ((γ2 * p2 / (γ1 * p1)) * (1 - η2) / η2 + 1) ** (-1)
          , η2
          , ((γ2 * p2 / (γ3 * p3)) * (1 - η2) / η2 + 1) ** (-1)
          , η4
          )

calc_p0 :: TmpC -> PresV
calc_p0 t = vtr3D ( atm * 10 ** (5.824316 - 1930.07 / (t + 378.6))
                  , kgw_cm2 * 10 ** (a - b / (t + 230.0))
                  , atm * 10 ** (5.08599 - 1668.21 / (t + 228.0))
                  )
  where atm = 101325
        kgw_cm2 = 0.967841 * atm
        a = if t > 122 then 8.642 else 6.579
        b = if t > 122 then 2640.0 else 1914.3

mixer :: Mixer
mixer = foldr1 $ liftA2 (+)

feedToX :: Mol_s -> Mol_s -> Mol_s -> Mol_s -> X
feedToX nh3 co2 h2o urea = Vtr4D (nh3 - 2 * co2, co2, h2o, urea)

reactor :: TmpC -> Reactor
reactor ts (Vtr4D (x1, x2, x3, x4)) = Vtr4D (x1, (1 - ζ) * x2, x2 * ζ + x3, x2 * ζ + x4)
  where a = (x1 + 2 * x2) / x2
        b = x3 / x2
        ζ = 0.2616 * a - 0.0194 * a**2 + 0.0382 * a * b - 0.1160 * b
              - (0.02732 * a + 0.1030 * b - 1.640) * ts / 100
              - 0.1394 * (ts / 100)**3 - 1.869

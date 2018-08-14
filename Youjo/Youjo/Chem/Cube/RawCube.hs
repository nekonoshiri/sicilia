module Youjo.Chem.Cube.RawCube
( Charge
, Coord
, IsBohr
, IsMO
, RawCube(..)
, readRawCube
) where

import Data.Char (isSpace)
import Text.Read (readMaybe)

import Youjo.Chem.Atom
import Youjo.Chem.XYZ
import Youjo.Power
import qualified Youjo.String as S

type Charge = Double
type Coord  = Power3 Double
type IsBohr = Bool
type IsMO   = Bool

data RawCube = RawCube { comment :: (String, String)
                       , xyzc :: [(XYZ, Charge)]
                       , origin :: Coord
                       , axisVectA :: Coord
                       , axisVectB :: Coord
                       , axisVectC :: Coord
                       , voldata :: [(Power3 Int, Double)]
                       , isBohr :: IsBohr
                       , infoMO :: Maybe (Int, Int)
                       }
  deriving (Eq, Show)

readRawCube :: String -> Maybe RawCube
readRawCube cont = do
  com1:com2:orgs:vec_a:vec_b:vec_c:rest <- Just (lines cont)
  (isMO, numAtom, org) <- readOrigin orgs
  (isbohr, na, axisVA) <- readVector vec_a
  (_, nb, axisVB) <- readVector vec_b
  (_, nc, axisVC) <- readVector vec_c
  (xyzstr, volhead:voltail) <- Just (splitAt numAtom rest)
  let infomo = if isMO then readNMO volhead else Nothing
      volstr = if isMO then voltail else volhead:voltail
  xyz_c <- mapM readXYZandChg xyzstr
  vol <- readVol na nb nc volstr
  let com = (fairing com1, fairing com2)
  return $ RawCube com xyz_c org axisVA axisVB axisVC vol isbohr infomo

readNXYZ :: String -> Maybe (Int, Coord)
readNXYZ str = do
  ns:xs:ys:zs:_ <- Just (words str)
  n <- readMaybe ns
  x <- readMaybe xs
  y <- readMaybe ys
  z <- readMaybe zs
  return (n, power3 x y z)

readOrigin :: String -> Maybe (IsMO, Int, Coord)
readOrigin str = do
  (n, p) <- readNXYZ str
  return (n < 0, abs n, p)

readVector :: String -> Maybe (IsBohr, Int, Coord)
readVector str = do
  (n, p) <- readNXYZ str
  return (n > 0, abs n, p)

readXYZandChg :: String -> Maybe (XYZ, Charge)
readXYZandChg str = do
  ias:chgs:xs:ys:zs:_ <- Just (words str)
  ia <- readMaybe ias
  atom <- toAtom ia
  chgs <- readMaybe chgs
  x <- readMaybe xs
  y <- readMaybe ys
  z <- readMaybe zs
  return ((atom, power3 x y z), chgs)

readNMO :: String -> Maybe (Int, Int)
readNMO str = do
  nmos:mons:_ <- Just (words str)
  nmo <- readMaybe nmos
  mon <- readMaybe mons
  return (nmo, mon)

readVol :: Int -> Int -> Int -> [String] -> Maybe [(Power3 Int, Double)]
readVol na nb nc str = zip <$> pt <*> dat
  where pt = Just [power3 x y z | x <- [1..na], y <- [1..nb], z <- [1..nc]]
        dat = mapM readMaybe (concatMap words str)

fairing :: String -> String
fairing = dropWhile isSpace . S.splitNewLine

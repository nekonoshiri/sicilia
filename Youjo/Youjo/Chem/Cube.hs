module Youjo.Chem.Cube
( Charge
, Coord
, IsMO
, Cube(..)
, fromRawCube
, readCube
) where

import qualified Youjo.Applicative.Operator as AO
import Youjo.Chem.Cube.RawCube (Charge, Coord, IsMO, IsBohr)
import qualified Youjo.Chem.Cube.RawCube as RC
import Youjo.Chem.XYZ
import Youjo.Power

-- xyz: Angstrom
-- coord: Angstrom
-- value: isBohr (Bohr / Angstrom)
data Cube = Cube { comment :: (String, String)
                 , xyzc :: [(XYZ, Charge)]
                 , cubedata :: [(Coord, Double)]
                 , infoMO :: Maybe (Int, Int)
                 , isBohr :: IsBohr
                 }
  deriving (Eq, Show)

bohr :: Double
bohr = 0.52917721067

fromRawCube :: RC.RawCube -> Cube
fromRawCube rawCube = Cube { comment = RC.comment rawCube
                           , xyzc = map mbohr (RC.xyzc rawCube)
                           , cubedata = cdata
                           , infoMO = RC.infoMO rawCube
                           , isBohr = RC.isBohr rawCube
                           }
  where cdata = map (\(p, d) -> (encoord rawCube p, d)) (RC.voldata rawCube)
        mbohr = if RC.isBohr rawCube
                  then \((atom, p), chg) -> ((atom, fmap (* bohr) p), chg)
                  else id

encoord :: RC.RawCube -> Power3 Int -> Coord
encoord rawCube p = (if RC.isBohr rawCube then fmap (* bohr) else id) cod
  where org = RC.origin rawCube
        aVA = RC.axisVectA rawCube
        aVB = RC.axisVectB rawCube
        aVC = RC.axisVectC rawCube
        (Power3 (i1, i2, i3)) = fmap fromIntegral p
        cod = org AO.+ fmap (*(i1 - 1)) aVA
                  AO.+ fmap (*(i2 - 1)) aVB
                  AO.+ fmap (*(i3 - 1)) aVC

readCube :: String -> Maybe Cube
readCube = fmap fromRawCube . RC.readRawCube

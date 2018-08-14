module Youjo.Chem.XYZ.Prim
( XYZ
) where

import Youjo.Chem.Atom (Atom)
import Youjo.Power (Power3)

type XYZ = (Atom, Power3 Double)

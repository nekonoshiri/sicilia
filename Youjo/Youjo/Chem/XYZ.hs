module Youjo.Chem.XYZ
( XYZ
-- , readXYZ
-- , readXYZfromFile
-- , writeXYZ
-- , writeXYZwithMaybe
) where

import Text.Parsec (parse)

import Youjo.Chem.Atom (Atom)
import Youjo.Chem.XYZ.Parser (parseXYZ)
import Youjo.Chem.XYZ.Prim (XYZ)
import Youjo.Power (Power3)

readXYZ :: String -> (String, [XYZ])
readXYZ cont = case (parse parseXYZ "" cont) of
                 Left  _   -> ("", [])
                 Right res -> res

readXYZline :: FilePath -> IO (String, [XYZ])
readXYZline = undefined

writeXYZ :: String -> [XYZ] -> String
writeXYZ = undefined

writeXYZwithMaybe :: String -> [XYZ] -> Maybe String
writeXYZwithMaybe = undefined

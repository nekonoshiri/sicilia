module Youjo.Chem.XYZ.Crude
( XYZ
, readXYZ
, readXYZfromFile
, writeXYZ
, writeXYZwithMaybe
) where

import Control.Monad (forM_)
import Control.Monad.Trans.Writer
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Youjo.Chem.Atom
import Youjo.Chem.XYZ.Prim (XYZ)
import Youjo.Power (Power3(..))
import qualified Youjo.String as S

readXYZ :: String -> (String, [XYZ])
readXYZ cont = maybe ("", []) id $ do
  _:title:xyz <- Just (lines cont)
  return (S.splitNewLine title, mapMaybe readXYZline xyz)

readXYZline :: String -> Maybe XYZ
readXYZline str = do
  atom_s:x_s:y_s:z_s:_ <- Just (words str)
  atom <- readMaybe atom_s
  x <- readMaybe x_s
  y <- readMaybe y_s
  z <- readMaybe z_s
  return (atom, Power3 (x, y, z))

readXYZfromFile :: FilePath -> IO (String, [XYZ])
readXYZfromFile path = do
  cont <- TIO.readFile path
  return $ readXYZ (T.unpack cont)

writeXYZ :: String -> [XYZ] -> String
writeXYZ title xyz = execWriter $ do
  tells [show (length xyz)]
  tells [title]
  forM_ xyz $ \(atom, Power3 (x, y, z)) -> do
    tells [show atom, " ", show x, " ", show y, " ", show z]
  where tells xs = mapM_ tell xs >> tell "\n"

writeXYZwithMaybe :: String -> [XYZ] -> Maybe String
writeXYZwithMaybe _ [] = Nothing
writeXYZwithMaybe title xyz = Just $ writeXYZ title xyz

module Youjo.String
( isNewLine
, splitNewLine
) where

import qualified Youjo.Char as C

import Prelude hiding (lines, words, unlines, unwords)

isNewLine :: String -> Bool
isNewLine (s:[]) = C.isNewLine s
isNewLine "\r\n" = True
isNewLine "\n\r" = True
isNewLine _ = False

splitNewLine :: String -> String
splitNewLine = filter (not . C.isNewLine)


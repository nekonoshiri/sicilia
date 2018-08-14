module Youjo.Char
( isNewLine
) where

isNewLine :: Char -> Bool
isNewLine '\n' = True
isNewLine '\v' = True
isNewLine '\f' = True
isNewLine '\r' = True
isNewLine '\RS' = True
isNewLine '\x85' = True
isNewLine '\x2028' = True
isNewLine '\x2029' = True
isNewLine _ = False

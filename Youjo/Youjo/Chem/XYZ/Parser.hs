{-# LANGUAGE FlexibleContexts #-}

module Youjo.Chem.XYZ.Parser
( parseXYZ
) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Youjo.Chem.XYZ.Prim (XYZ)

-- I'm not sure what type of space is allowed in XYZ file
allowSpacesList :: [Char]
allowSpacesList = [' ', '\t']

allowSpace :: Stream s m Char => ParsecT s u m Char
allowSpace = satisfy (\c -> c `elem` allowSpacesList)

allowSpaces :: Stream s m Char => ParsecT s u m ()
allowSpaces = skipMany allowSpace

eolf :: Stream s m Char => ParsecT s u m Char
eolf = endOfLine <|> (eof >> return '\n')

xyzDef :: LanguageDef st
xyzDef = emptyDef { nestedComments = False }

xyzLexer :: TokenParser st
xyzLexer = makeTokenParser xyzDef

parseXYZ :: Parsec String u (String, [XYZ])
parseXYZ = do
  allowSpaces
  number_of_atoms <- natural xyzLexer
  comment <- (manyTill anyChar eolf)
  xyz <- sepBy parseXYZline endOfLine
  return (comment, xyz)

parseXYZline :: Parsec String u XYZ
parseXYZline = undefined

{-# LANGUAGE FlexibleContexts #-}
module Text.TinyProto.Parser
  ( proto
  ) where

import Control.Applicative ((<|>))
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC

import qualified Text.TinyProto.Language as L

spaces1 :: P.Stream s m Char => P.ParsecT s u m ()
spaces1 = PC.space >> PC.spaces

letter :: P.Stream s m Char => P.ParsecT s u m Char
letter = PC.oneOf ['A'..'Z'] <|> PC.oneOf ['a'..'z']

decimalDigit :: P.Stream s m Char => P.ParsecT s u m Char
decimalDigit = PC.digit -- PC.oneof ['0'..'9']

octalDigit :: P.Stream s m Char => P.ParsecT s u m Char
octalDigit = PC.octDigit

hexDigit :: P.Stream s m Char => P.ParsecT s u m Char
hexDigit = PC.hexDigit

ident :: P.Stream s m Char => P.ParsecT s u m String
ident = do
  c <- letter
  cs <- P.many (letter <|> decimalDigit <|> PC.char '_')
  return $ c:cs

messageName :: P.Stream s m Char => P.ParsecT s u m L.MessageName
messageName = fmap L.MessageName ident

fieldName :: P.Stream s m Char => P.ParsecT s u m L.FieldName
fieldName = fmap L.FieldName ident

-- (1..9):_   = decimal
-- 0:(x or X) = hex
-- 0:_        = octal
-- otherwise  = error
intLit :: P.Stream s m Char => P.ParsecT s u m Integer
intLit = decimalLit <|> P.try hexLit <|> octalLit

decimalLit :: P.Stream s m Char => P.ParsecT s u m Integer
decimalLit = do
  c <- PC.oneOf ['1'..'9']
  cs <- P.many decimalDigit
  return $ read (c:cs)

octalLit :: P.Stream s m Char => P.ParsecT s u m Integer
octalLit = do
  PC.char '0'
  cs <- P.many octalDigit
  return $ read ("0o0" ++ cs)

hexLit :: P.Stream s m Char => P.ParsecT s u m Integer
hexLit = do
  PC.char '0'
  PC.char 'x' <|> PC.char 'X'
  cs <- P.many1 hexDigit
  return $ read ("0x" ++ cs)

-- need backtrack (`try`) when adding like `int64`
typ :: P.Stream s m Char => P.ParsecT s u m L.Type
typ = do
  s <- PC.string "double" <|> PC.string "int32" <|> PC.string "string"
  case s of
    "double" -> return L.TypeDouble
    "int32"  -> return L.TypeInt32
    "string" -> return L.TypeString
    _ -> P.unexpected s

fieldNumber :: P.Stream s m Char => P.ParsecT s u m Integer
fieldNumber = intLit

field :: P.Stream s m Char => P.ParsecT s u m L.Field
field = do
  t <- typ
  spaces1
  name <- fieldName
  PC.spaces
  PC.char '='
  PC.spaces
  num <- fieldNumber
  PC.spaces
  PC.char ';'
  return $ L.Field t name num

messageBody :: P.Stream s m Char => P.ParsecT s u m L.MessageBody
messageBody = P.between (PC.char '{') (PC.char '}') $ do
  PC.spaces
  msgs <- P.many $ do
    f <- field
    PC.spaces
    return $ L.MessageBodyMainField f
  return $ L.MessageBody msgs

message :: P.Stream s m Char => P.ParsecT s u m L.Message
message = do
  PC.string "message"
  spaces1
  msgName <- messageName
  PC.spaces
  msgBody <- messageBody
  return $ L.Message msgName msgBody

topLevelDef :: P.Stream s m Char => P.ParsecT s u m L.TopLevelDef
topLevelDef = fmap L.TopLevelDefMessage message

proto :: P.Stream s m Char => P.ParsecT s u m L.Proto
proto = do
  PC.spaces
  protos <- P.many $ do
    tdf <- topLevelDef
    PC.spaces
    return $ L.ProtoMainTopLevelDef tdf
  return $ L.Proto protos

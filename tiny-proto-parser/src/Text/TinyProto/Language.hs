module Text.TinyProto.Language where

import Data.List (intersperse)

data Proto = Proto [ProtoMain]
  deriving (Show, Eq)

data ProtoMain = ProtoMainTopLevelDef TopLevelDef
  deriving (Show, Eq)

data TopLevelDef = TopLevelDefMessage Message
  deriving (Show, Eq)

data Message = Message MessageName MessageBody
  deriving (Show, Eq)

data MessageName = MessageName Ident
  deriving (Show, Eq)

data MessageBody = MessageBody [MessageBodyMain]
  deriving (Show, Eq)

data MessageBodyMain = MessageBodyMainField Field
  deriving (Show, Eq)

data Field = Field Type FieldName FieldNumber
  deriving (Show, Eq)

data Type = TypeDouble | TypeInt32 | TypeString
  deriving (Show, Eq)

data FieldName = FieldName Ident
  deriving (Show, Eq)

type FieldNumber = Integer

type Ident = String


indent :: Int -> String
indent i = replicate i ' '

type NewLineCode = String
type Indent = Int

class Show s => ProtoShow s where
  protoShow :: s -> String
  protoShow = prottyShow 0 ""

  prottyShow :: Indent -> NewLineCode -> s -> String
  prottyShow _ _ str = protoShow str

instance ProtoShow Proto where
  prottyShow i n (Proto pms) =
    concat $ intersperse n $ fmap (prottyShow i n) pms

instance ProtoShow ProtoMain where
  prottyShow i n (ProtoMainTopLevelDef tdf) = prottyShow i n tdf

instance ProtoShow TopLevelDef where
  prottyShow i n (TopLevelDefMessage msg) = prottyShow i n msg

instance ProtoShow Message where
  prottyShow i n (Message mName mBody) =
    "message " ++ protoShow mName ++ " {" ++ n ++
      prottyShow i n mBody ++ n ++ "}"

instance ProtoShow MessageName where
  protoShow (MessageName mName) = mName

instance ProtoShow MessageBody where
  prottyShow i n (MessageBody msgBodyMains) =
    concat $ intersperse n $ fmap (prottyShow i n) msgBodyMains

instance ProtoShow MessageBodyMain where
  prottyShow i n (MessageBodyMainField f) = prottyShow i n f

instance ProtoShow Field where
  prottyShow i _ (Field t fName fNum) = indent i ++
    protoShow t ++ " " ++ protoShow fName ++ "=" ++ show fNum ++ ";"

instance ProtoShow Type where
  protoShow TypeDouble = "double"
  protoShow TypeInt32  = "int32"
  protoShow TypeString = "string"

instance ProtoShow FieldName where
  protoShow (FieldName fName) = fName

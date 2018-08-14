module QuizData.MetaData
  ( MetaData
  , readMetaData
  ) where

import Util.Either (readEitherWith, validate)

data MetaData = MetaData { subGenre   :: Int
                         , examGenre  :: Int
                         , series     :: Int
                         , difficulty :: (Int, Int)
                         , pictureID  :: Maybe Int
                         , comment    :: String
                         }
                         deriving (Show, Eq)

readMetaData :: String -> String -> String -> String
             -> String -> String -> String -> String
             -> Either String MetaData
readMetaData gen subGen examGen sers difmin difmax pictID comment = do
  genre     <- readEitherWith "不正なジャンル" gen
  subGenre  <- readEitherWith "不正なサブジャンル" subGen
  examGenre <- readEitherWith "不正な検定ジャンル" examGen
  series    <- readEitherWith "不正なシリーズ" sers
  difMin    <- readEitherWith "不正な難易度" difmin
  difMax    <- readEitherWith "不正な難易度" difmax
  pictureID <- if pictID == ""
                  then return Nothing
                  else fmap Just $ readEitherWith "不正な画像ID" pictID
  validateMetaData genre MetaData { subGenre   = subGenre
                                  , examGenre  = examGenre
                                  , series     = series
                                  , difficulty = (difMin, difMax)
                                  , pictureID  = pictureID
                                  , comment    = comment
                                  }

validateMetaData :: Int -> MetaData -> Either String MetaData
validateMetaData genre metadata = do
  validate "不正な難易度 !(0 <= difMin <= difMax <= 5)"
    (0 <= difMin && difMin <= difMax && difMax <= 5)
  return metadata
    where (difMin, difMax) = difficulty metadata

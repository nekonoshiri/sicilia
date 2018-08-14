{-# LANGUAGE OverloadedStrings #-}

module Youjo.BMS.BMS
( Player
, Rank
, Difficulty
, BMS
) where

-- import Data.Array.IO (IOArray)
-- import qualified Data.Array.MArray as MA
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
-- import Data.Ix
-- import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import Text.Read (readMaybe)

data Player = SinglePlay | CouplePlay | DoublePlay | BattlePlay
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Rank = RankVeryHard | RankHard | RankNormal | RankEasy | RankVeryEasy
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Difficulty = Beginner | Normal | Hyper | Another | Insane
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data BMS = BMS { player     :: Player
               , genre      :: String
               , title      :: String
               , artist     :: String
               , bpm        :: Int
               , midifile   :: FilePath
               , playlevel  :: Int
               , rank       :: Rank
               , volwav     :: Int
               , total      :: Int
               , subtitle   :: String
               , difficulty :: Difficulty
               , subartist  :: String
               , banner     :: FilePath
               , wav        :: IntMap FilePath
               , bmp        :: IntMap FilePath
               , maindata   :: IntMap [Int]
               }
               deriving (Eq, Show, Read)

data BMSKey = PLAYER | GENRE | TITLE | ARTIST | BPM | MIDIFILE
            | PLAYLEVEL | RANK | VOLWWAV | TOTAL | SUBTITLE
            | DIFFICULTY | SUBARTIST | BANNER
            | WAV Int | BMP Int | MAINDATA Int
            deriving (Eq, Ord, Show, Read)

{--

readBMS :: FilePath -> IO BMS
readBMS path = do
  cont <- TIO.readFile path
  let headerList = mapMaybe readBMSline (T.lines cont)
  
  -- headerArr <- MA.newArray (minBound, maxBound) ""
  --               :: IO (IOArray HeaderKey String)
  -- mapM_ (\(key, txt) -> MA.writeArray headerArr key txt) headerList
  return undefined

  return BMS { player = 
               genre  = 
             
             
             
             
             }

readBMSline :: T.Text -> Maybe (HeaderKey, String)
readBMSline txt = do
  (skey:cont:[]) <- Just (T.words txt)
  key <- T.stripPrefix "#" skey
  headerkey <- readMaybe (T.unpack $ T.toUpper key)
  return (headerkey, T.unpack cont)


--}

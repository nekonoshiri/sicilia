module PNG where


import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy as BL
import Control.Monad.State
import Data.Bits (shift)
import Data.Int (Int8, Int32)
import Numeric (showHex)


class Empty a where
  tempty :: a


data ImgHeader = ImgHeader { imgWidth  :: Int32
                           , imgHeight :: Int32
                           , imgDepth  :: Int8
                           , imgCType  :: Int8
                           , imgPStyle :: Int8
                           , imgFStyle :: Int8
                           , imgIStyle :: Int8
                           } deriving (Eq)


instance Show ImgHeader where
  show (ImgHeader w h d ct ps fs is)
    = show w ++ "px * " ++ show h ++ "px"
        ++ "\nbit depth    " ++ show d
        ++ "\ncolor type   " ++ show ct ++ " (" ++ (showctype ct) ++ ")"
        ++ "\ncomp style   " ++ show ps
        ++ "\nfilter style " ++ show fs
        ++ "\ninterlace    " ++ show is ++ " (" ++ (showinterlace is) ++ ")\n"
   where showctype 0 = "GrayScale"
         showctype 2 = "RGB"
         showctype 3 = "Palette"
         showctype 4 = "GrayScale+alpha"
         showctype 6 = "RGB+alpha"
         showctype _ = ""
         showinterlace 0 = "non-interlace"
         showinterlace 1 = "Adam7"
         showinterlace _ = ""


data Palette = Palette { red   :: Int8
                       , green :: Int8
                       , blue  :: Int8
                       } deriving (Eq)


data ChunkData = IHDR ImgHeader
               | PLTE [Palette]
               | IDAT BL.ByteString
               | IEND
               | Other BL.ByteString
               deriving (Eq)


instance Show ChunkData where
  show (IHDR imgh)  = show imgh
  show (PLTE [plt]) = ""
  show (IDAT str)   = showBinaryHex str
  show IEND         = ""
  show (Other str)  = ""
  show _            = ""


data Chunk = Chunk { chunkLen  :: Int32
                   , chunkName :: String
                   , chunkData :: ChunkData
                   , chunkCRC  :: BL.ByteString
                   }
           | EnptyChunk
           deriving (Eq)


instance Show Chunk where
  show Chunk { chunkLen  = chunklen
             , chunkName = chunkname
             , chunkData = chunkdata
             , chunkCRC  = chunkcrc
             }
         = chunkname ++ ": " ++ show chunklen ++ "Bytes, CRC = " 
             ++ showBinaryHex chunkcrc ++ "\n" ++ show chunkdata
  show _ = ""


data PNG = PNG { pngIHDR :: Chunk
               , pngPLTE :: Chunk
               , pngIDAT :: Chunk
               , pngIEND :: Chunk
               , pngOtherChunk :: [Chunk]
               }


instance Empty PNG where
  tempty = PNG EnptyChunk EnptyChunk EnptyChunk EnptyChunk []


instance Show PNG where
  show PNG { pngIHDR = ihdr, pngIDAT = idat } 
    = "PNG FILE--------------------\n" ++ show ihdr ++ "\n"
      ++ show idat ++ "\n"
      ++ "----------------------------"


showBinaryHex :: BL.ByteString -> String
showBinaryHex str = ('[':) . tail . BL.foldr (\s acc -> (padding s) ++ showHex s acc) "]" $ str  
  where padding s
          | s < 16    = " 0"
          | otherwise = " "


type BLMaybe a = StateT BL.ByteString Maybe a


matchHeader :: BL.ByteString -> BLMaybe ()
matchHeader prefix = do
  str <- get
  if prefix `BL.isPrefixOf` str
    then modify $ BL.drop (BL.length prefix)
    else lift Nothing


getBytes :: Integer -> BLMaybe BL.ByteString
getBytes n = do
  let count = fromIntegral n
  (prefix, rests) <- gets (BL.splitAt count)  
  if BL.length prefix < count
    then lift Nothing
    else put rests >> return prefix


getInt8 :: BLMaybe Int8
getInt8 = getBytes 1 >>= return . fromIntegral . BL.head


getInt32 :: BLMaybe Int32
getInt32 = getBytes 4 >>= return . snd . BL.foldr (\x (ct, acc) -> (shift ct 8, acc + fromIntegral x * ct)) (1, 0)


get4Char :: BLMaybe String
get4Char = fmap BL8.unpack $ getBytes 4


getChunkData :: Int32 -> String -> BLMaybe ChunkData
getChunkData 13  "IHDR" = do
  w  <- getInt32
  h  <- getInt32
  d  <- getInt8
  ct <- getInt8
  ps <- getInt8
  fs <- getInt8
  is <- getInt8
  return . IHDR $ ImgHeader w h d ct ps fs is
getChunkData len "PLTE" = getPLTE len []
  where getPLTE l palettes
         | l > 0 = do
             r <- getInt8
             g <- getInt8
             b <- getInt8
             getPLTE (l - 3) $ (Palette r g b):palettes
         | otherwise = return . PLTE . reverse $ palettes
getChunkData len "IDAT" = getBytes (fromIntegral len) >>= return . IDAT
getChunkData 0   "IEND" = return IEND
getChunkData len _      = getBytes (fromIntegral len) >>= return . Other


getChunk :: BLMaybe Chunk
getChunk = do
  len  <- getInt32
  name <- get4Char
  cdat <- getChunkData len name
  crc  <- getBytes 4
  return Chunk { chunkLen = len, chunkName = name, chunkData = cdat, chunkCRC = crc }


parsePNG :: BLMaybe PNG
parsePNG = matchHeader pngsign >> parseLoop tempty
  where pngsign  = BL.pack [0x89, 0x50, 0x4e, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]


parseLoop :: PNG -> BLMaybe PNG
parseLoop png = do
  chn@(Chunk { chunkData = cdata }) <- getChunk
  case cdata of
    IHDR _  -> parseLoop png { pngIHDR = chn }
    PLTE _  -> parseLoop png { pngPLTE = chn }
    IDAT _  -> parseLoop png { pngIDAT = chn }
    IEND    -> return    png { pngIEND = chn }
    Other _ -> parseLoop png { pngOtherChunk = chn:(pngOtherChunk png) }


showfile :: String -> IO ()
showfile title = do
  contents <- BL.readFile title
  let Just (pngdata, rests) = runStateT parsePNG contents
  print pngdata


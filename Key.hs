{-# LANGUAGE GADTs, DeriveGeneric #-}

module Key where

import           Data.Bits
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Internal as B (ByteString)
import           Data.Word
import           GHC.Generics (Generic)

data Key where
  Key :: { arwNKey :: Int } -> Key

newKey :: Key
newKey = undefined

loadFromBuffer :: Key -> KeyFileFormat -> Int -> IO Bool
loadFromBuffer = undefined

keyValid :: Key -> Bool
keyValid = undefined
--   if(!keys.wEKeyBase || !keys.wNKeyBase)

countCrcMD4 :: String -> IO String
countCrcMD4 = undefined

crpB :: String -> Key -> IO String
crpB = undefined

data KeyFileFormat where
  KeyFileFormat :: { wReserved1 :: Word16
                   , wSignFlag  :: Word16
                   , dwCRC0     :: Word32
                   , dwCRC1     :: Word32
                   , dwCRC2     :: Word32
                   , dwCRC3     :: Word32
                   , dwLenBuf   :: Word32
                   , ptrBuffer  :: Word8
                   } -> KeyFileFormat deriving (Show, Generic)

instance Binary KeyFileFormat where
  get = do
    reserved <- getWord16host
    sign <- getWord16host
    crc0 <- getWord32host
    crc1 <- getWord32host
    crc2 <- getWord32host
    crc3 <- getWord32host
    lenBuf <- getWord32host
    buf <- getWord8
    return $ KeyFileFormat reserved sign crc0 crc1 crc2 crc3 lenBuf buf


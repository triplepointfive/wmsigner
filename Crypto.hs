module Crypto where

import           Data.Word
import           Data.Bits
import           Text.Printf (printf)
import           Data.ByteString.Internal (c2w)

us2sz :: String -> String
us2sz [] = []
us2sz (x:[]) = printf "%04x" ( fromOctets [c2w x])
us2sz (x:y:str) = (printf "%04x" ( fromOctets [c2w y, c2w x]) ) ++ us2sz str

fromOctets :: [Word8] -> Word16
fromOctets = foldl accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o

getCLenB :: Int -> Int -> Int
getCLenB = undefined


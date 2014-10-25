module Montgomery where

import           Data.Bits (shiftL, (.&.), Bits)
import           Data.Word (Word8, Word32)

intSize, bitMask, longMask :: Int
intSize = 32
bitMask = 0x80000000
longMask = 0xFFFFFFFF

exponentation :: [Word32] -> [Word8] -> [Word8] -> [Word32]
exponentation x e m = undefined

inverse :: ( Num a, Bits a ) => a -> a
inverse value = -1 * ( iterate (\t -> t * ( 2 - value * t) ) temp !! 4 )
  where
    temp = ( ( ( value + 2 ) .&. 4 ) `shiftL` 1 ) + value

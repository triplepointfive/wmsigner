{-# LANGUAGE GADTs #-}

module Signer where

import           Data.Word (Word8)

import           Algebra (significance)

intSize, shortSize, byteSize, hashSize :: Int
intSize   = 32
shortSize = 16
byteSize  = 8
hashSize  = 128

header, trail :: [Word8]
header = [0x38, 0]
trail = [1, 0]

data Signer where
  Signer :: { expon  :: ![Word8]
            , modulus   :: ![Word8]
            , keyLength :: !Int
            } -> Signer

newSigner :: [Word8] -> [Word8] -> Signer
newSigner expon' modul = Signer { expon  = take exponentLength expon'
                                , modulus   = take modulusLength modul
                                , keyLength = modulusLength * byteSize
                                }
  where
    exponentLength = significance expon'
    modulusLength = significance modul
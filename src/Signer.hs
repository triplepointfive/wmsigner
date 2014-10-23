{-# LANGUAGE GADTs #-}

module Signer where

import           Data.Bits (shiftR)
import           Data.Word (Word8)
import           Text.Printf (printf)

import qualified Data.ByteString as B (ByteString, elem)
import qualified Data.ByteString.Internal as B (c2w)

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

sign :: Signer -> B.ByteString -> Bool -> IO String
sign signer message randomEnabled =
  if B.c2w '\r' `B.elem` message
    then error "Message cannot contain of the following character: '\r'."
    else do
      signature <- signBytes signer message randomEnabled
      return $ concat
        [ if length signature > pos `div` ( intSize `div` shortSize )
          then
            let
              shift = if 0 == pos `div` ( intSize `div` shortSize ) then 0 else shortSize :: Int
              letter = signature !! pos `div` ( intSize `div` shortSize ) :: Int
            in printf "%04x" $ letter `shiftR` shift
          else
            printf "%04x" ( 0 :: Int )
          | pos <- [0, 1 .. keyLength signer `div` shortSize]
        ]


signBytes :: Signer -> B.ByteString -> Bool -> IO [Int]
signBytes = undefined
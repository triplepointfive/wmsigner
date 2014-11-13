{-# LANGUAGE GADTs #-}

module Signer where

import           Data.Bits (shiftR, shiftL, (.|.))
import           Data.Char (ord)
import           Data.Int (Int8, Int16, Int32)
import           Data.List (foldl')
import           Data.Word (Word8, Word32)
import           Text.Printf (printf)

import           Crypto.Hash.MD4 (hash)
import qualified Data.ByteString as B (ByteString, elem, unpack, pack)
import qualified Data.ByteString.Internal as B (c2w)
import           Data.List.Split (chunksOf)

import           Algebra (significance, logicalShiftRight)
import           Montgomery (exponentation)

intSize, shortSize, byteSize, hashSize :: Int
intSize   = 32
shortSize = 16
byteSize  = 8
hashSize  = 128

header, trail :: [Word8]
header = [0x38, 0]
trail = [1, 0]

data Signer where
  Signer :: { expon     :: ![Int32]
            , modulus   :: ![Int32]
            , keyLength :: !Int
            } -> Signer deriving (Show)

newSigner :: [Word8] -> [Word8] -> Signer
newSigner expon' modul = Signer { expon     = cast expon'
                                , modulus   = modulus'
                                , keyLength = significance modul * byteSize
                                }
  where modulus' = cast modul

signUnsafe :: Signer -> String -> String
signUnsafe signer =
  buildSignature signer . signBytes signer ( zeroBuffer signer ) . validate . B.pack . map ( fromIntegral . ord )

validate message = if B.c2w '\r' `B.elem` message
    then error "Message cannot contain of the following character: '\r'."
    else message

zeroBuffer signer = replicate rndBufferLength 0
  where
    rndBufferLength = keyLength signer `div` byteSize
                      - length header
                      - hashSize `div` byteSize
                      - length trail

sign :: Signer -> B.ByteString -> String
sign signer message =
  if B.c2w '\r' `B.elem` message
    then error "Message cannot contain of the following character: '\r'."
    else buildSignature signer signature
  where signature = signBytes signer (zeroBuffer signer) message

buildSignature :: Signer -> [Int32] -> String
buildSignature signer signature = concat
    [ if length signature > pos `div` ( intSize `div` shortSize )
      then
        let
          shift = if 0 == pos `mod` ( intSize `div` shortSize ) then 0 else shortSize :: Int
          letter = signature !! fromIntegral (pos `div` ( intSize `div` shortSize ))
        in printf "%04x" (fromIntegral (letter `logicalShiftRight` shift) :: Int16)
      else
        printf "%04x" ( 0 :: Int )
      | pos <- [0, 1 .. keyLength signer `div` shortSize - 1]
    ]

signBytes :: Signer -> [Word8] -> B.ByteString -> [Int32]
signBytes signer rndBuffer message = exponentation (cast blob) (expon signer) (modulus signer)
  where blob = header ++ B.unpack ( hash message ) ++ rndBuffer ++ trail

cast :: Integral a => [a] -> [Int32]
cast xs = map ( fromOctets . reverse ) $ chunksOf 4 xs

fromOctets :: Integral a => [a] -> Int32
fromOctets = foldl' accum 0
  where accum a o = (a `shiftL` 8) .|. fromIntegral o
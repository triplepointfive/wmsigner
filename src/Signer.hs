{-# LANGUAGE GADTs #-}

module Signer where

import           Control.Monad            (replicateM)
import           Data.Bits                (shiftL, (.|.))
import           Data.Char                (ord)
import           Data.Int                 (Int16, Int32)
import           Data.List                (foldl')
import           Data.Word                (Word8)
import           System.Random            (randomIO)
import           Text.Printf              (printf)

import           Crypto.Hash.MD4          (hash)
import qualified Data.ByteString          as B (ByteString, elem, pack, unpack)
import qualified Data.ByteString.Internal as B (c2w)
import           Data.List.Split          (chunksOf)

import           Algebra                  (logicalShiftRight, significance)
import           Montgomery               (exponentation)

import           Data.Vector  (Vector, fromList, (!))
import qualified Data.Vector  as V (replicate, length, concat)

intSize, shortSize, byteSize, hashSize :: Int
intSize   = 32
shortSize = 16
byteSize  = 8
hashSize  = 128

header, trail :: [Word8]
header = [0x38, 0]
trail = [1, 0]

data Signer where
  Signer :: { expon     :: !(Vector Int32)
            , modulus   :: !(Vector Int32)
            , keyLength :: !Int
            } -> Signer deriving (Show)

newSigner :: [Word8] -> [Word8] -> Signer
newSigner expon' modul = Signer { expon     = fromList (cast expon')
                                , modulus   = modulus'
                                , keyLength = significance (fromList modul) * byteSize
                                }
  where modulus' = fromList (cast modul)

signUnsafe :: Signer -> String -> String
signUnsafe signer = signWithBuffer signer (zeroBuffer signer)

sign :: Signer -> String -> IO String
sign signer message = do
    rndBuf <- randomBuffer signer
    return $ signWithBuffer signer rndBuf message

signWithBuffer :: Signer -> [Word8] -> String -> String
signWithBuffer signer buffer =
  buildSignature signer . signBytes signer buffer . validate . B.pack . map ( fromIntegral . ord )

validate :: B.ByteString -> B.ByteString
validate message = if B.c2w '\r' `B.elem` message
  then error "Message cannot contain of the following character: '\r'."
  else message

zeroBuffer :: Signer -> [Word8]
zeroBuffer signer = replicate (bufferLength signer) 0

randomBuffer :: Signer -> IO [Word8]
randomBuffer signer = replicateM (bufferLength signer) randomIO

bufferLength :: Signer -> Int
bufferLength signer = keyLength signer `div` byteSize
                    - length header
                    - hashSize `div` byteSize
                    - length trail

buildSignature :: Signer -> Vector Int32 -> String
buildSignature signer signature = concat
    [ if V.length signature > pos `div` ( intSize `div` shortSize )
      then
        let
          shift = if 0 == pos `mod` ( intSize `div` shortSize ) then 0 else shortSize :: Int
          letter = signature ! fromIntegral (pos `div` ( intSize `div` shortSize ))
        in printf "%04x" (fromIntegral (letter `logicalShiftRight` shift) :: Int16)
      else
        printf "%04x" ( 0 :: Int )
      | pos <- [0, 1 .. keyLength signer `div` shortSize - 1]
    ]

signBytes :: Signer -> [Word8] -> B.ByteString -> Vector Int32
signBytes signer rndBuffer message = exponentation (fromList $ cast blob) (expon signer) (modulus signer)
  where blob = header ++ B.unpack ( hash message ) ++ rndBuffer ++ trail

cast :: Integral a => [a] -> [Int32]
cast xs = map ( fromOctets . reverse ) $ chunksOf 4 xs

fromOctets :: Integral a => [a] -> Int32
fromOctets = foldl' accum 0
  where accum a o = (a `shiftL` 8) .|. fromIntegral o

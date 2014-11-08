{-# LANGUAGE GADTs #-}

module Signer where

import           Data.Bits (shiftR, shiftL, (.|.))
import           Data.Int (Int8, Int32)
import           Data.List (foldl')
import           Data.Word (Word8, Word32)
import           Text.Printf (printf)

import           Crypto.Hash.MD4 (hash)
import qualified Data.ByteString as B (ByteString, elem, unpack, pack)
import qualified Data.ByteString.Internal as B (c2w)
import           Data.List.Split (chunksOf)

import           Algebra (significance)
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
            } -> Signer

instance Show Signer

newSigner :: [Int8] -> [Int8] -> Signer
newSigner expon' modul = Signer { expon     = cast expon'
                                , modulus   = modulus'
                                , keyLength = (significance modul) * byteSize
                                }
  where modulus' = cast modul

-- sign :: Signer -> B.ByteString -> Bool -> IO String
-- sign signer message randomEnabled =
--   if B.c2w '\r' `B.elem` message
--     then error "Message cannot contain of the following character: '\r'."
--     else do
--       signature <- signBytes signer message randomEnabled
--       return $ concat
--         [ if length signature > pos `div` ( intSize `div` shortSize )
--           then
--             let
--               shift = if 0 == pos `div` ( intSize `div` shortSize ) then 0 else shortSize :: Int
--               letter = signature !! pos `div` ( intSize `div` shortSize ) :: Int
--             in printf "%04x" $ letter `shiftR` shift
--           else
--             printf "%04x" ( 0 :: Int )
--           | pos <- [0, 1 .. keyLength signer `div` shortSize]
--         ]

signBytes :: Signer -> B.ByteString -> Bool -> [Int32]
signBytes signer message randomEnabled = error $ show [(cast blob),(expon signer),(modulus signer)]
    -- exponentation (cast blob) (expon signer) (modulus signer)
  where
    blob = header
           ++ B.unpack ( hash message )
           ++ replicate rndBufferLength 0
           ++ trail
    rndBufferLength = ( keyLength signer ) `div` byteSize
                      - ( length header )
                      - hashSize `div` byteSize
                      - ( length trail )

cast :: Integral a => [a] -> [Int32]
cast xs = map ( fromOctets . reverse ) $ chunksOf 4 xs

fromOctets :: Integral a => [a] -> Int32
fromOctets = foldl' accum 0
  where
    accum a o = (a `shiftL` 8) .|. fromIntegral o
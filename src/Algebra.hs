module Algebra where

import Data.Bits (testBit, bitSize, Bits)
import Data.Int (Int32)

significance :: [a] -> Int
significance = undefined

remainder :: [Int32] -> [Int32] -> [Int32]
remainder lhs rhs = undefined

getBitsCount :: Bits a => [a] -> Int
getBitsCount xs = ( vLenght -1 ) * 32 + ( 32 - ( numberOfLeadingZeros $ last xs ) )
  where vLenght = length xs -- significand?

numberOfLeadingZeros :: Bits a => a -> Int
numberOfLeadingZeros x = length $ takeWhile (not . testBit x) [ size -1 , size - 2 .. 0]
  where size = bitSize x

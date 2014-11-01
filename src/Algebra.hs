module Algebra where

import Data.Bits (testBit, bitSize, Bits, (.&.))
import Data.Int (Int32)

longMask :: Int32
longMask = 0xFFFFFFFF

compareLists :: [Int32] -> [Int32] -> Ordering
compareLists lhs rhs
    | lhsLenght > rhsLenght = GT
    | lhsLenght < rhsLenght = LT
    | otherwise             = comp (reverse lhs) (reverse rhs)
  where
    lhsLenght = significance lhs
    rhsLenght = significance rhs

    comp [] _ = EQ
    comp _ [] = EQ
    comp (l:ls) (r:rs)
      | lb > rb   = GT
      | lb < rb   = LT
      | otherwise = comp ls rs
      where
        lb = l .&. longMask
        rb = r .&. longMask

significance :: [a] -> Int
significance = length

shift :: [a] -> Int -> [a]
shift = undefined

shiftRight :: [a] -> [a]
shiftRight = undefined

sub :: [a] -> [a] -> [a]
sub = undefined

remainder :: [Int32] -> [Int32] -> [Int32]
remainder lhs rhs = divide lhs rhs
  where
    rhsBitsCount = getBitsCount rhs -- check attemption to divide by zero

    divide :: [Int32] -> [Int32] -> [Int32]
    divide l r
      | LT /= compareLists l r = l
      | lhsBitsCount == 0      = l
      | otherwise              =
        let temp' = if compareLists l temp == LT then shiftRight temp else temp
        in divide ( subs l temp' ) r
      where
        lhsBitsCount = getBitsCount l
        temp         = shift r (lhsBitsCount - rhsBitsCount)
    subs :: [Int32] -> [Int32] -> [Int32]
    subs l t =
      if compareLists l t /= LT
        then subs (sub l t) t
        else l

getBitsCount :: Bits a => [a] -> Int
getBitsCount xs = ( vLenght -1 ) * 32 + ( getBitsNumber ( last xs ) )
  where vLenght = significance xs

getBitsNumber :: Bits a => a -> Int
getBitsNumber x = 32 - numberOfLeadingZeros x

numberOfLeadingZeros :: Bits a => a -> Int
numberOfLeadingZeros x = length $ takeWhile (not . testBit x) [size - 1, size - 2 .. 0]
  where size = bitSize x

module Algebra where

import           Data.Bits (testBit, bitSize, Bits, (.&.), shiftL)
import           Data.Int (Int32, Int64)

import           Control.Lens ((&), ix, (.~))

longMask :: Int64
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
        lb = (fromIntegral l) .&. longMask
        rb = (fromIntegral r) .&. longMask

significance :: (Eq a, Bits a, Num a) => [a] -> Int
significance xs = length $ dropWhile (== 0) $ reverse xs

shift :: [a] -> Int -> [a]
shift = undefined

shiftRight :: [a] -> [a]
shiftRight = undefined

sub :: [Int32] -> [Int32] -> [Int32]
sub lhs rhs
    | lhsLength < rhsLength = error "Difference should not be negative."
    | otherwise             = modulo $ rest subscribed
  where
    lhsLength = significance lhs
    rhsLength = significance rhs

    modulo :: ([Int32], Int32) -> [Int32]
    modulo (_, 1) = error "Difference should not be negative."
    modulo (l, _) = l

    subscribed :: ([Int32], Int32)
    subscribed = foldl substr (lhs, 0) [0..rhsLength - 1]
      where
        substr :: ([Int32], Int32) -> Int -> ([Int32], Int32)
        substr (l, borrow) pos = ( l & ix pos .~ (fromIntegral temp), nBorrow )
          where
            temp = ((fromIntegral ( l !! pos ) ) .&. longMask )
              - ((fromIntegral ( rhs !! pos ) ) .&. longMask )
              - fromIntegral borrow
            nBorrow = if temp .&. ( 1 `shiftL` 32 ) /= 0 then 1 else 0

    rest :: ([Int32], Int32) -> ([Int32], Int32)
    rest (ls, b) = foldl substr (ls, b) [rhsLength..lhsLength - 1]
      where
        substr :: ([Int32], Int32) -> Int -> ([Int32], Int32)
        substr (l, borrow) pos = ( l & ix pos .~ (fromIntegral temp), nBorrow )
          where
            temp = ((fromIntegral ( l !! pos ) ) .&. longMask ) - fromIntegral borrow
            nBorrow = if temp .&. ( 1 `shiftL` 32 ) /= 0 then 1 else 0

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

getBitsCount :: (Bits a, Num a) => [a] -> Int
getBitsCount xs = ( vLenght -1 ) * 32 + ( getBitsNumber ( last xs ) )
  where vLenght = significance xs

getBitsNumber :: Bits a => a -> Int
getBitsNumber x = 32 - numberOfLeadingZeros x

numberOfLeadingZeros :: Bits a => a -> Int
numberOfLeadingZeros x = length $ takeWhile (not . testBit x) [size - 1, size - 2 .. 0]
  where size = bitSize x

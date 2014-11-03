module Algebra where

import           Data.Bits (testBit, bitSize, Bits, (.&.), shiftL, shiftR, (.|.))
import           Data.Word (Word64)
import           Data.Int (Int32, Int64)

import           Control.Lens ((&), ix, (.~))

longMask :: Int64
longMask = 0xFFFFFFFF

logicalShiftR :: Integral a => a -> Int -> a
logicalShiftR x i = fromIntegral ((fromIntegral x :: Word64) `shiftR` i)

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

shift :: [Int32] -> Int -> [Int32]
shift lhs rhs = if outWordsCount <= 0 then [0] else undefined
  where
    shiftBits, shiftWords, inBitsCount, inWordsCount, outBitsCount, outWordsCount :: Int
    shiftBits     = (abs rhs) `mod` 32
    shiftWords    = (abs rhs) `div` 32
    inBitsCount   = getBitsCount lhs
    inWordsCount  = inBitsCount `div` 32 + (if inBitsCount `mod` 32 > 0 then 1 else 0)
    outBitsCount  = inBitsCount + rhs
    outWordsCount = outBitsCount `div` 32 + (if outBitsCount `mod` 32 > 0 then 1 else 0)

shiftRight :: [Int32] -> [Int32]
shiftRight value = fst $ foldl right (value, 0) [len-1, len-2..0]
  where
    len = significance value
    right :: ([Int32], Int64) -> Int -> ([Int32], Int64)
    right (v, carry) pos = ( v & ix pos .~ (fromIntegral val), nextCarry )
      where
        temp, nextCarry, val :: Int64
        temp      = (fromIntegral $ v !! pos)             .&. longMask
        nextCarry = (temp .&. 1) `shiftL` (32 - 1)        .&. longMask
        val       = ((temp `logicalShiftR` 1) .|. carry ) .&. longMask

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
      | LT == compareLists l r = l
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

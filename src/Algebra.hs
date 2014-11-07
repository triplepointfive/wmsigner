module Algebra where

import           Data.Bits (testBit, bitSize, Bits, (.&.), shiftL, shiftR, (.|.))
import           Data.Word (Word64, Word32)
import           Data.Int (Int32, Int64)

import           Control.Lens ((&), ix, (.~))

longMask :: Int64
longMask = 0xFFFFFFFF

intSize :: Int
intSize = 32

logicalShiftR :: Integral a => a -> Int -> a
logicalShiftR x i = fromIntegral ((fromIntegral x :: Word64) `shiftR` i)

logicalShiftRight :: Int32 -> Int -> Int32
logicalShiftRight x i = fromIntegral ((fromIntegral x :: Word32) `shiftR` i)

compareLists :: [Int32] -> [Int32] -> Ordering
compareLists lhs rhs
    | lhsLenght > rhsLenght = GT
    | lhsLenght < rhsLenght = LT
    | otherwise             = comp (reverse $ take lhsLenght lhs) (reverse $ take lhsLenght rhs)
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
shift lhs rhs
    | outWordsCount <= 0        = [0]
    | shiftBits == 0 && rhs > 0 = (take shiftWords r0) ++ (take (outWordsCount - shiftWords) lhs)
    | rhs > 0                   =
      let (res, carry) = foldl shRight (r0, 0) [0 .. inWordsCount - 1]
      in if inWordsCount - 1 + shiftWords < outWordsCount
            then res & ix ( inWordsCount + shiftWords ) .~ (res !! (inWordsCount + shiftWords) .|. carry)
            else res
    | shiftBits == 0            = error "3"
    | otherwise                 =
      let carry = if outWordsCount + shiftWords < inWordsCount
                    then (lhs !! (outWordsCount + shiftWords)) `shiftL` ( intSize - shiftBits)
                    else 0
      in fst $ foldl shLeft (r0, carry) [inWordsCount - 1, inWordsCount - 2 .. 0]
  where
    shiftBits, shiftWords, inBitsCount, inWordsCount, outBitsCount, outWordsCount :: Int
    shiftBits     = (abs rhs) `mod` intSize
    shiftWords    = (abs rhs) `div` intSize
    inBitsCount   = getBitsCount lhs
    inWordsCount  = inBitsCount `div` intSize + (if inBitsCount `mod` intSize > 0 then 1 else 0)
    outBitsCount  = inBitsCount + rhs
    outWordsCount = outBitsCount `div` intSize + (if outBitsCount `mod` intSize > 0 then 1 else 0)

    r0 = replicate (max inWordsCount outWordsCount) 0

    shRight, shLeft :: ([Int32], Int32) -> Int -> ([Int32], Int32)
    shRight (res, carry) pos = ( res & ix ( pos + shiftWords ) .~ val, nextCarry )
      where
        temp      = lhs !! pos
        val       = ( temp `shiftL` shiftBits ) .|. carry
        nextCarry = temp `logicalShiftRight` ( intSize - shiftBits )
    shLeft (res, carry) pos = ( res & ix ( pos + shiftWords ) .~ val, nextCarry )
      where
        temp      = lhs !! (pos + shiftWords)
        val       = (temp `logicalShiftRight` shiftBits) .|. carry
        nextCarry = temp `shiftL` ( intSize - shiftBits )

shiftRight :: [Int32] -> [Int32]
shiftRight value = fst $ foldl right (value, 0) [len-1, len-2..0]
  where
    len = significance value
    right :: ([Int32], Int64) -> Int -> ([Int32], Int64)
    right (v, carry) pos = ( v & ix pos .~ (fromIntegral val), nextCarry )
      where
        temp, nextCarry, val :: Int64
        temp      = (fromIntegral $ v !! pos)             .&. longMask
        nextCarry = (temp .&. 1) `shiftL` ( intSize - 1)  .&. longMask
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
            nBorrow = if temp .&. ( 1 `shiftL` intSize ) /= 0 then 1 else 0

    rest :: ([Int32], Int32) -> ([Int32], Int32)
    rest (ls, b) = foldl substr (ls, b) [rhsLength..lhsLength - 1]
      where
        substr :: ([Int32], Int32) -> Int -> ([Int32], Int32)
        substr (l, borrow) pos = ( l & ix pos .~ (fromIntegral temp), nBorrow )
          where
            temp = ((fromIntegral ( l !! pos ) ) .&. longMask ) - fromIntegral borrow
            nBorrow = if temp .&. ( 1 `shiftL` intSize ) /= 0 then 1 else 0

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
getBitsCount xs = ( vLenght - 1 ) * intSize + ( getBitsNumber ( xs !! (vLenght - 1) ) )
  where vLenght = significance xs

getBitsNumber :: Bits a => a -> Int
getBitsNumber x = intSize - numberOfLeadingZeros x

numberOfLeadingZeros :: Bits a => a -> Int
numberOfLeadingZeros x = length $ takeWhile (not . testBit x) [size - 1, size - 2 .. 0]
  where size = bitSize x

resize :: [Int32] -> Int -> [Int32]
resize v l
  | l < 0       = error "Invalid value for length"
  | vLength < l = v ++ (replicate (l - vLength) 0)
  | otherwise   = take l v
  where vLength = length v

normalize :: [Int32] -> [Int32]
normalize x = resize x ( significance x )

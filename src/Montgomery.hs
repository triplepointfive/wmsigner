module Montgomery where

import           Control.Lens ((&), ix, (.~))
import           Data.Bits (shiftL, (.&.), Bits, shiftR)
import           Data.Int (Int32)
import           Data.Word (Word8, Word32, Word64)

import           Algebra (significance)

intSize :: Int
intSize = 32
bitMask, longMask :: Int
bitMask = 0x80000000
longMask = 0xFFFFFFFF

-- Algorithm Montgomery exponentiation
-- INPUT:
--      m = (m[l-1] ... m[0]){b},
--      R = b^l,
--      mQ = m^-1 mod b,
--      e = (e[t] ... e[0]){2}
--           with e[t] = 1,
--           and an integer x, 1 <= x < m.
-- OUTPUT: x^e mod m.
exponentation :: [Word32] -> [Word32] -> [Word32] -> [Word32]
exponentation x e m = undefined
  where
    -- mQ = -m^1 mod b
    mQ = inverse $ head m
    eLength = significance e
    mLenght = significance m
    -- 1. temp = Mont(x, R^2 mod m), A = R mod m.
--     temp = multiplication x r m mQ
--       where r = replicate ( length m ) 0 ++ [1]

-- Algorithm Montgomery multiplication
-- INPUT: integers
--      m = (m[n-1] ... m[1] m[0]){b},
--      x = (x[n-1] ... x[1] x[0]){b},
--      y = (y[n-1] ... y[1] y[0]){b}
--           with 0 <= x, y < m,
--           R = b^n with gcd(m, b) = 1,
--           and mQ = -m^1 mod b.
-- OUTPUT: x * y * R^-1 mod m.
multiplication :: [Int32] -> [Int32] -> [Int32] -> Int32 -> [Int32]
multiplication x y m mQ = foldl iter a0 [0..n-1]
  where
    n = length m -- significance?
    -- 1. A = 0. (Notation: A = (a[n] a[n-1] ... a[1] a[0]){b})
    a0 = replicate (n + 1) 0
    -- 2. For i from 0 to (n - 1) do the following:
    iter :: [Int32] -> Int -> [Int32]
    iter a i =
      let xy, um, temp, carry :: Int
          xy    = ((fromIntegral $ x !! i) .&. longMask) * ((fromIntegral $ head y) .&. longMask)
          um    = u * ((fromIntegral $ head m) .&. longMask)
          temp  = ((fromIntegral $ head a) .&. longMask) + (xy .&. (fromIntegral longMask)) + (um .&. (fromIntegral longMask))
          carry = (xy `logicalShiftR` intSize) + (um `logicalShiftR` intSize) + (temp `logicalShiftR` intSize)
          (_, _, _, carry', a') = foldl proc (xy, um, temp, carry, a) [1..n-1]
          carry'' = carry' + ((fromIntegral $ a' !! n ) .&. longMask)
      in ( a' & ix ( n - 1 ) .~ (fromIntegral carry'') ) & ix n .~ ( fromIntegral $ carry'' `logicalShiftR` intSize )
      where
        -- 2.1 u_i = (a[0] + x[i] * y[0]) * mQ mod b.
        u :: Int
        u = (( (fromIntegral $ head a)
          + ((((fromIntegral $ x !! i) .&. longMask) * ((fromIntegral $ head y) .&. longMask)) .&. longMask))
          * fromIntegral mQ ) .&. longMask
        -- 2.2 A = (A + x[i] * y + u_i * m) / b.
        proc :: (Int, Int, Int, Int, [Int32]) -> Int -> (Int, Int, Int, Int, [Int32])
        proc (xy', um', temp', carry', a') pos = (xy, um, temp, carry, a' & ix ( pos - 1 ) .~ (fromIntegral temp))
          where
            xy, um, temp, carry :: Int
            xy = ((fromIntegral $ x !! i) .&. longMask) * ((fromIntegral $ y !! pos) .&. longMask)
            um = u * ((fromIntegral $ m !! pos) .&. longMask)
            temp = ((fromIntegral $ a' !! pos) .&. longMask)
              + (xy .&. ( fromIntegral longMask))
              + (um .&. ( fromIntegral longMask))
              + (carry' .&. ( fromIntegral longMask))
            carry = (carry' `logicalShiftR` 32)
              + (xy `logicalShiftR` intSize)
              + (um `logicalShiftR` intSize)
              + (temp `logicalShiftR` intSize)

logicalShiftR :: Int -> Int -> Int
logicalShiftR x i = fromIntegral ((fromIntegral x :: Word64) `shiftR` i) :: Int

inverse :: ( Num a, Bits a ) => a -> a
inverse value = -1 * ( iterate (\t -> t * ( 2 - value * t) ) temp !! 4 )
  where
    temp = ( ( ( value + 2 ) .&. 4 ) `shiftL` 1 ) + value

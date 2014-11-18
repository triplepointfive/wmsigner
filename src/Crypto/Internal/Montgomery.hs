module Crypto.Internal.Montgomery where

import           Control.Lens (ix, (&), (.~))
import           Data.Bits    (Bits, shiftL, shiftR, (.&.))
import           Data.Int     (Int32, Int64)

import           Crypto.Internal.Algebra      (logicalShiftR, normalize, remainder, resize,
                               significance)

import           Data.Vector  (Vector, cons, snoc, (!))
import qualified Data.Vector  as V (head, length, replicate)

intSize :: Int
intSize = 32
longMask :: Int
longMask = 0xFFFFFFFF

bitMask :: Int64
bitMask = 0x80000000

-- Algorithm Montgomery exponentiation
-- INPUT:
--      m = (m[l-1] ... m[0]){b},
--      R = b^l,
--      mQ = m^-1 mod b,
--      e = (e[t] ... e[0]){2}
--           with e[t] = 1,
--           and an integer x, 1 <= x < m.
-- OUTPUT: x^e mod m.
exponentation :: Vector Int32 -> Vector Int32 -> Vector Int32 -> Vector Int32
exponentation x e m = normalize a3
  where
    -- mQ = -m^1 mod b
    mQ = inverse $ V.head m
    eLength = significance e
    mLength = significance m
    -- 1. temp = Mont(x, R^2 mod m), A = R mod m.
    temp = multiplication x' r2 m mQ
      where
        r = V.replicate ( 2 * V.length m ) 0 `snoc` 1
        r2 = remainder r m
        x' = if mLength > V.length x then resize x mLength else x
    a0 = remainder a' m
      where
        a' = V.replicate (V.length m) 0 `snoc` 1

    pos0 = eLength - 1

    mask0 :: Int64
    mask0 = head $ dropWhile (\m -> fromIntegral (e ! pos0) .&. m == 0) $ iterate (`logicalShiftR` 1) bitMask

    -- 2. For i from t down to 0 do the following:
    a2  = mont a0 pos0 mask0
    mont :: Vector Int32 -> Int -> Int64 -> Vector Int32
    mont a pos mask
      | pos < 0    = a
      | mask' == 0 = mont a'' (pos - 1) bitMask
      | otherwise  = mont a'' pos mask'
      where
        -- 2.1 A = Mont(A, A).
        a'  = multiplication a a m mQ
        -- 2.2 If e[i] = 1 then A = Mont(A, temp).
        a'' = if 0 /= fromIntegral (e ! pos) .&. mask then multiplication a' temp m mQ else a'
        mask'  = mask `shiftR` 1

    -- 3. A Mont(A, 1).
    one = 1 `cons` V.replicate (V.length m - 1) 0
    a3  = multiplication a2 one m mQ

-- Algorithm Montgomery multiplication
-- INPUT: integers
--      m = (m[n-1] ... m[1] m[0]){b},
--      x = (x[n-1] ... x[1] x[0]){b},
--      y = (y[n-1] ... y[1] y[0]){b}
--           with 0 <= x, y < m,
--           R = b^n with gcd(m, b) = 1,
--           and mQ = -m^1 mod b.
-- OUTPUT: x * y * R^-1 mod m.
multiplication :: Vector Int32 -> Vector Int32 -> Vector Int32 -> Int32 -> Vector Int32
multiplication x y m mQ = foldl iter a0 [0..n-1]
  where
    n = significance m
    -- 1. A = 0. (Notation: A = (a[n] a[n-1] ... a[1] a[0]){b})
    a0 = V.replicate (n + 1) 0
    -- 2. For i from 0 to (n - 1) do the following:
    iter :: Vector Int32 -> Int -> Vector Int32
    iter a i =
      let xy, um, temp, carry :: Int
          xy    = (fromIntegral ( x ! i) .&. longMask) * (fromIntegral ( V.head y) .&. longMask)
          um    = u * (fromIntegral (V.head m) .&. longMask)
          temp  = (fromIntegral (V.head a) .&. longMask) + (xy .&. fromIntegral longMask) + (um .&. fromIntegral longMask)
          carry = (xy `logicalShiftR` intSize) + (um `logicalShiftR` intSize) + (temp `logicalShiftR` intSize)
          (_, _, _, carry', a') = foldl proc (xy, um, temp, carry, a) [1..n-1]
          carry'' = carry' + (fromIntegral ( a' ! n ) .&. longMask)
      in ( a' & ix ( n - 1 ) .~ fromIntegral carry'' ) & ix n .~ fromIntegral ( carry'' `logicalShiftR` intSize )
      where
        -- 2.1 u_i = (a[0] + x[i] * y[0]) * mQ mod b.
        u :: Int
        u = (( fromIntegral (V.head a)
          + (((fromIntegral (x ! i) .&. longMask) * (fromIntegral (V.head y) .&. longMask)) .&. longMask))
          * fromIntegral mQ ) .&. longMask
        -- 2.2 A = (A + x[i] * y + u_i * m) / b.
        proc :: (Int, Int, Int, Int, Vector Int32) -> Int -> (Int, Int, Int, Int, Vector Int32)
        proc (xy', um', temp', carry', a') pos = (xy, um, temp, carry, a' & ix ( pos - 1 ) .~ fromIntegral temp)
          where
            xy, um, temp, carry :: Int
            xy = (fromIntegral (x ! i) .&. longMask) * (fromIntegral (y ! pos) .&. longMask)
            um = u * (fromIntegral (m ! pos) .&. longMask)
            temp = (fromIntegral (a' ! pos) .&. longMask)
              + (xy .&. fromIntegral longMask)
              + (um .&. fromIntegral longMask)
              + (carry' .&. fromIntegral longMask)
            carry = (carry' `logicalShiftR` 32)
              + (xy `logicalShiftR` intSize)
              + (um `logicalShiftR` intSize)
              + (temp `logicalShiftR` intSize)

inverse :: ( Num a, Bits a ) => a -> a
inverse value = -1 * ( iterate (\t -> t * ( 2 - value * t) ) temp !! 4 )
  where
    temp = ( ( ( value + 2 ) .&. 4 ) `shiftL` 1 ) + value

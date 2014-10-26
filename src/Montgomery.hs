module Montgomery where

import           Data.Bits (shiftL, (.&.), Bits, shiftR)
import           Data.Word (Word8, Word32)

import           Algebra (significance)

intSize :: Int
intSize = 32
bitMask, longMask :: Word32
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
    temp = multiplication x r m mQ
      where r = replicate ( length m ) 0 ++ [1]

-- Algorithm Montgomery multiplication
-- INPUT: integers
--      m = (m[n-1] ... m[1] m[0]){b},
--      x = (x[n-1] ... x[1] x[0]){b},
--      y = (y[n-1] ... y[1] y[0]){b}
--           with 0 <= x, y < m,
--           R = b^n with gcd(m, b) = 1,
--           and mQ = -m^1 mod b.
-- OUTPUT: x * y * R^-1 mod m.
multiplication :: [Word32] -> [Word32] -> [Word32] -> Word32 -> [Word32]
multiplication x y m mQ = foldl iter a' [0..n-1]
  where
    n = length m -- significance?
    -- 1. A = 0. (Notation: A = (a[n] a[n-1] ... a[1] a[0]){b})
    a' = replicate (n + 1) 0
    -- 2. For i from 0 to (n - 1) do the following:
    iter :: [Word32] -> Int -> [Word32]
    iter a i =
        let xy, um, temp, carry :: Word32
            xy    = ((x !! i) .&. longMask) * (head y .&. longMask)
            um    = u * (head m .&. longMask)
            temp  = (head a .&. longMask) + (xy .&. longMask) + (um .&. longMask)
            carry = (xy `shiftR` intSize) + (um `shiftR` intSize) + (temp `shiftR` intSize) -- logical shift?
            (_, _, _, carry') = foldl proc (xy, um, temp, carry) [1..n-1]
        in  undefined
      where
        -- 2.1 u_i = (a[0] + x[i] * y[0]) * mQ mod b.
        u :: Word32
        u = (( (head a) + ((((x !! i) .&. longMask) * ((head y) .&. longMask)) .&. longMask)) * mQ ) .&. longMask
        -- 2.2 A = (A + x[i] * y + u_i * m) / b.
        proc :: (Word32, Word32, Word32, Word32) -> Int -> (Word32, Word32, Word32, Word32)
        proc (xy, um, temp, carry) pos = undefined


inverse :: ( Num a, Bits a ) => a -> a
inverse value = -1 * ( iterate (\t -> t * ( 2 - value * t) ) temp !! 4 )
  where
    temp = ( ( ( value + 2 ) .&. 4 ) `shiftL` 1 ) + value

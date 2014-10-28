module Main where

import Data.Word (Word32)

import Test.Hspec

import Montgomery

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "Montgomery multiplication" $ do
      (multiplication
        [-1476460488, -1118323957, 1120423587, -1596341302, 43673, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
        [887153153, 1663090240, 2023161700, -1792225711, -2037349738, 431338827, 936480955, 1341971236, 1200687489, 795067959, -325209191, 1292772351, 905855361, -230695685, -1798442900, 313894787, 1080, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        [-1627447467, 1291058882, -1100545328, 1792006073, -463051918, 1656053193, 1111245943, -258120325, 2076079646, -1933207219, 1644799652, -1802394516, -571686162, 1182004563, -1327376660, -254358572, 1390]
        3397655043)
        `shouldBe`
        ([629254954, -1321769800, 1900654924, 2056436422, 1791614990, 475542676, 547583796, 1358516486, 1650142553, -1578392030, -1395505261, 1389520284, -494774702, 536840267, 790850704, -1567780526, 64, 0])
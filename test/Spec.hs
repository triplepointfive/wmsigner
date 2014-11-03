module Main where

import Test.Hspec

import Data.Int (Int32)

import Algebra
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
    it "Montgomery exponentaion" $ do
      (exponentation
        [-1476460488, -1118323957, 1120423587, -1596341302, 43673, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
        [137802365, 260873917, 45018536, 458828140, 936357922, 329776834, 197077425, 1505777544, -375079653, -869392973, -527458209, -442374499, -1678873588, -1864144942, 1488834064, -1154962690, 191]
        [-1627447467, 1291058882, -1100545328, 1792006073, -463051918, 1656053193, 1111245943, -258120325, 2076079646, -1933207219, 1644799652, -1802394516, -571686162, 1182004563, -1327376660, -254358572, 1390] )
        `shouldBe`
        [1420192846, -514184231, -858634323, -658448382, 52207915, -437346630, -666018004, -2043170752, 1241009014, 879655623, -1601019036, -1233902541, 17763596, -1169198420, 1209444264, 223198074, 1327]
    it "Algebra reminder" $ do
      (remainder
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
        [-1627447467, 1291058882, -1100545328, 1792006073, -463051918, 1656053193, 1111245943, -258120325, 2076079646, -1933207219, 1644799652, -1802394516, -571686162, 1182004563, -1327376660, -254358572, 1390])
        `shouldBe`
        [1602626370, 296859807, -1933305503, 1018504923, 2106164031, -469194707, 1605789230, 1881367321, 127989226, -1751912682, 2087886977, 1741983051, -1090589209, 1428161158, 647631356, -121976833, 889, 0]
    it "Algebra getBitsCount" $ do
      ( getBitsCount
        ([-1627447467, 1291058882, -1100545328, 1792006073, -463051918, 1656053193, 1111245943, -258120325, 2076079646, -1933207219, 1644799652, -1802394516, -571686162, 1182004563, -1327376660, -254358572, 1390] :: [Int32]))
        `shouldBe`
        523
    it "Algebra comparison" $ do
      ( compareLists
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
        [-1627447467, 1291058882, -1100545328, 1792006073, -463051918, 1656053193, 1111245943, -258120325, 2076079646, -1933207219, 1644799652, -1802394516, -571686162, 1182004563, -1327376660, -254358572, 1390])
        `shouldBe`
        GT
    it "Algebra sub" $ do
      ( sub
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
        [-358612992, -665591837, 1510579839, -147338016, 1850563066, -1187214132, -1897113941, -1352120440, -1008856148, 699365326, -728655693, -1920188108, 1574081076, -1434731112, -578236802, 2056657978, -1377953063, 0] )
        `shouldBe`
        [358612992, 665591836, -1510579840, 147338015, -1850563067, 1187214131, 1897113940, 1352120439, 1008856147, -699365327, 728655692, 1920188107, -1574081077, 1434731111, 578236801, -2056657979, 1377953062, 0]
    it "Algebra sub" $ do
      ( shiftRight
        [-717225984, -1331183673, -1273807617, -294676032, -593841163, 1920539032, 500739415, 1590726417, -2017712295, 1398730653, -1457311386, 454591081, -1146805143, 1425505072, -1156473603, -181651339, 1539061170, 1])
        `shouldBe`
        [-358612992, -665591837, 1510579839, -147338016, 1850563066, -1187214132, -1897113941, -1352120440, -1008856148, 699365326, -728655693, -1920188108, 1574081076, -1434731112, -578236802, 2056657978, -1377953063, 0]
    it "Algebra shift" $ do
      ( shift
        [-1627447467, 1291058882, -1100545328, 1792006073, -463051918, 1656053193, 1111245943, -258120325, 2076079646, -1933207219, 1644799652, -1802394516, -571686162, 1182004563, -1327376660, -254358572, 1390]
        22)
        `shouldBe`
        [-717225984, -1331183673, -1273807617, -294676032, -593841163, 1920539032, 500739415, 1590726417, -2017712295, 1398730653, -1457311386, 454591081, -1146805143, 1425505072, -1156473603, -181651339, 1539061170, 1]



{-# LANGUAGE GADTs #-}

module Key where

import qualified Data.ByteString as B
import           Data.Word

countCrcMD4 :: [Word32] -> String -> Int -> [Word32]
countCrcMD4 = undefined

data Key where Key :: Key


newKey :: Key
newKey = undefined

loadFromBuffer :: Key -> Int -> Bool
loadFromBuffer

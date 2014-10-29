module Utils where

import           Control.Monad.State (StateT, liftIO)

data Cond a = a :? a
 
infixl 0 ?
infixl 1 :?
 
(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y

io :: IO b -> StateT a IO b
io = liftIO
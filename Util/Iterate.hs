module Util.Iterate where

class Iterator a where
    nextI :: (a -> a) -> a -> a
    nextI f x = f x
    bottomI :: a

instance Iterator Int  where
    bottomI = 0        

iter :: Int -> Int
iter = nextI (\x -> x - 1)

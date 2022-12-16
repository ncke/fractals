module Main where

import Test.QuickCheck (quickCheck)
import Region

--prop_reverseReverse :: [Char] -> Bool
--prop_reverseReverse s = (reverse . reverse) s == s

--prop_encryptDecrypt :: [Char] -> Bool
--prop_encryptDecrypt s = (encrypt . decrypt) s == s

prop_regionLocation :: (Int, Int) -> (Int, Int) -> Bool
prop_regionLocation locn size = (location reg) == locn
  where
    reg = Region { location = locn, size = size }

main = do
  quickCheck prop_regionLocation
  --quickCheck prop_encryptDecrypt
  --quickCheck prop_reverseReverse
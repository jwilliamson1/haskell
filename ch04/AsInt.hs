import Data.Char (digitToInt)

asInt :: String -> Int
asInt xs = foldl convert 0 xs
              where convert acc x = acc * 10 + digitToInt x
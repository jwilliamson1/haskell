module Main where

import Data.Char (toUpper)
import Control.Monad

main = doStuff
doStuff :: IO ()
doStuff = putStrLn "Write your string: " >> fmap shout getLine >>= putStrLn


shout = map toUpper

writeAll :: [String] -> IO ()
writeAll = mapM_ putStrLn

gen :: String -> [String]
gen = replicate 3
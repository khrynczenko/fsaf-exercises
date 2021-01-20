module Main where

import Practice

main :: IO ()
main = do
    word1 <- getLine
    word2 <- getLine
    print (checkAnagram word1 word2)

module Practice where

import Data.Char
import Data.List

isWord :: String -> Maybe String
isWord word =
    case (null word) of
        True -> Nothing
        False -> case (all isAlpha word) of
            True -> Just word
            False -> Nothing

isAnagram :: String -> String -> Bool
isAnagram word1 word2 = sort word1 == sort word2

checkAnagram :: String -> String -> String
checkAnagram word1 word2 =
    case (isWord word1) of
        Nothing -> "The first word is invalid."
        Just word1 -> case (isWord word2) of
            Nothing -> "The second word is invalid."
            Just word2 -> case (isAnagram word1 word2) of
                True -> "Words are anagrams."
                False -> "Words are not anagrams."


-- Exercise 6
isPalindrome :: String -> Bool
isPalindrome word = word == reverse word


-- Exercise 7
substituteChar :: Char -> Char
substituteChar c =
    case c of
        'e' -> '3'
        'o' -> '0'
        'a' -> '4'
        c -> c

leetspeak :: String -> String
leetspeak word = map substituteChar word
    

function x y = if (x > y) then x + 10 else y

function2 x y =
    case x > y of
        True -> x + 10
        False -> y

-- Exercise 1
absVal x =
    case x < 0 of
        True -> negate x
        False -> x

-- Exercise 2
validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password =
    case (null username, null password) of
        (True, True) -> "Empty username and password"
        (False, True) -> "Empty password"
        (True, False) -> "Empty username"
        (False, False) -> "Ok"

-- Exercise 3
--safeHead :: [a] -> a
--safeHead [] = []
--safeHead (x:xs) = x
-- This will not compile because the type signature show that this function
-- returns an element of a list and we try to return empy list in the first
-- pattern.

-- Exercise 4
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just xs


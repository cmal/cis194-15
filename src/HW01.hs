{-# OPTIONS_GHC -Wall #-}
module HW01 where
-- import Distribution.Simple.Utils (xargs)
-- import Data.ByteString.Builder (doubleLE)

-- Exercise 1 -----------------------------------------
-- |
-- >>> lastDigit 123
-- 3
-- >>> lastDigit 0
-- 0
lastDigit :: Integer -> Integer
lastDigit = flip mod 10

-- | Drop the last digit from a number
-- >>> dropLastDigit 123
-- 12
-- >>> dropLastDigit 5
-- 0
dropLastDigit :: Integer -> Integer
dropLastDigit x = div (x - lastDigit x) 10

-- Exercise 2 -----------------------------------------
-- |
-- >>> toRevDigits 1234
-- [4,3,2,1]
-- >>> toRevDigits 0
-- []
-- >>> toRevDigits (-17)
-- []
toRevDigits :: Integer -> [Integer]
toRevDigits x
  | x <= 0 = []
  | otherwise = lastDigit x:toRevDigits (dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
-- >>> doubleEveryOther [4, 9, 5, 5]
-- [4,18,5,10]
-- >>> doubleEveryOther [0, 0] 
-- [0,0]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:xs)
 | even (length xs) = 2 * x:doubleEveryOther xs
 | otherwise = x:doubleEveryOther xs


-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
-- >>>  sumDigits [10, 5, 18, 4]
-- 19
sumDigits :: [Integer] -> Integer
sumDigits =  sum . (=<<) toRevDigits


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
-- >>> luhn 5594589764218858 
-- True
-- >>> luhn 1234567898765432
-- False
luhn :: Integer -> Bool
luhn x = lastDigit x == lastDigit (sumDigits (doubleEveryOther (toRevDigits (dropLastDigit x))) * 9)

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
-- >>> hanoi 1 "a" "b" "c" 
-- [("a","c")]
-- >>> hanoi 2 "a" "b" "c" 
-- [("a","b"),("a","c"),("b","c")]
-- >>> hanoi 3 "a" "b" "c" 
-- [("a","c"),("a","b"),("c","b"),("a","c"),("b","a"),("b","c"),("a","c")]
-- >>> hanoi 4 "a" "b" "c" 
-- [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b"),("a","c"),("b","c"),("b","a"),("c","a"),("b","c"),("a","b"),("a","c"),("b","c")]
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n < 1 = []
  | n == 1 = [(a, c)]
  | otherwise = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) b a c

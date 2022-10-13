{-# OPTIONS_GHC -Wall #-}
module HW02 where
import Control.Applicative (liftA2)
import Control.Monad (liftM2)

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------
-- >>> exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red]
-- 0
-- >>> exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange]
-- 2
-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches _secret@(x:xs) _guess@(y:ys) = if x == y then 1 + next else next
  where next = exactMatches xs ys

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
-- >>> countColors [Red, Blue, Yellow, Purple]
-- [1,0,1,1,0,1]
-- >>> countColors [Green, Blue, Green, Orange] 
-- [0,2,1,0,1,0]
countColors :: Code -> [Int]
countColors [] = [0, 0, 0, 0, 0, 0]
countColors cs = fmap  (\x -> length $ filter (x==) cs) colors

-- Count number of matches between the actual code and the guess
-- >>> matches [Red, Red, Blue, Blue] [Red, Red, Green, Green]
-- 2
-- >>> matches [Red, Orange, Blue, Blue] [Red, Red, Green, Green]
-- 1
-- >>> matches [Red, Orange, Blue, Blue] [Green, Red, Red, Green]
-- 1
-- >>> matches [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue]
-- 3
matches :: Code -> Code -> Int
matches _ [] = 0
matches [] _ = 0
matches secret guess = sum $ zipWith min (countColors secret) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
-- >>> getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue]
-- Move [Red,Orange,Orange,Blue] 1 3
-- >>> getMove [Green, Red, Red, Red] [Red, Red, Red, Red]
-- Move [Red,Red,Red,Red] 3 3
getMove :: Code -> Code -> Move
getMove secret guess = Move guess (exactMatches secret guess) (matches secret guess)

-- Exercise 4 -----------------------------------------
-- >>> isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Yellow, Purple]
-- True
-- >>> isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Red, Purple] 
-- False
isConsistent :: Move -> Code -> Bool
isConsistent move secret = getMove secret (getGuess move) == move
    where getGuess (Move guess _ _) = guess

-- Exercise 5 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes = filter . isConsistent

-- Exercise 6 -----------------------------------------
-- >>> allCodes 0
-- []
-- >>> allCodes 1
-- [[Red],[Green],[Blue],[Yellow],[Orange],[Purple]]
-- >>> allCodes 2
-- [[Red,Red],[Green,Red],[Blue,Red],[Yellow,Red],[Orange,Red],[Purple,Red],[Red,Green],[Green,Green],[Blue,Green],[Yellow,Green],[Orange,Green],[Purple,Green],[Red,Blue],[Green,Blue],[Blue,Blue],[Yellow,Blue],[Orange,Blue],[Purple,Blue],[Red,Yellow],[Green,Yellow],[Blue,Yellow],[Yellow,Yellow],[Orange,Yellow],[Purple,Yellow],[Red,Orange],[Green,Orange],[Blue,Orange],[Yellow,Orange],[Orange,Orange],[Purple,Orange],[Red,Purple],[Green,Purple],[Blue,Purple],[Yellow,Purple],[Orange,Purple],[Purple,Purple]]
-- >>> length $ allCodes 2
-- 36
-- >>> length $ allCodes 3
-- 216
-- >>> head $ tail $ allCodes 4
-- [Green,Red,Red,Red]

-- >>> length $ filterCodes (Move [Red,Red,Red,Red] 3 3) $ allCodes 4
-- 20

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = fmap (:[]) colors
allCodes n = allCodes (n - 1) >>= go
    where go = \xs -> fmap (:xs) colors

-- Exercise 7 -----------------------------------------

-- >>> getMove [Green, Red, Red, Red] [Red, Red, Red, Red]
-- Move [Red,Red,Red,Red] 3 3
-- >>> solve [Purple, Yellow, Blue, Green]
-- [Move [Orange,Yellow,Blue,Green] 3 3,Move [Yellow,Yellow,Blue,Green] 3 3,Move [Blue,Blue,Blue,Green] 2 2,Move [Green,Green,Green,Green] 1 1,Move [Red,Red,Red,Red] 0 0]
solve :: Code -> [Move]
solve secret = go [] $ allCodes 4
    where
        go moves [_] = moves
        go moves cs = go (move:moves) $ filterCodes move cs 
            where move =  getMove secret $ makeGuess cs
                  makeGuess = head


-- Bonus ----------------------------------------------
-- (WIP) https://en.wikipedia.org/wiki/Mastermind_(board_game)#Worst_case:_Five-guess_algorithm
-- >>> length $ fiveGuess [Purple, Yellow, Blue, Green]
fiveGuess :: Code -> [Move]
fiveGuess secret = go [] (allCodes 4)
    where
        go moves [_] = moves
        go moves cs = go (move:moves) $ filterCodes move cs 
            where move =  getMove secret $ makeGuess (length moves) cs
                  makeGuess 0 _ = [Red, Red, Green, Green]
                  makeGuess _ xs = undefined

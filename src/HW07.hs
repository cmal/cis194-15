{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------
-- >>> liftM (+1) (Just 5) == Just 6
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
  a <- ma
  return $ f a

--   that takes in two indices and swaps the elements at those indices in
-- some Vector. This function should use the safe indexing operation
-- (!?) and not the unsafe one (!). If either of the indices are out of
-- bounds (ie (!?) returns Nothing), then the result should be Nothing.
-- You will probably find the (//) function useful.
-- >>> swapV 0 2 (V.fromList [1, 2, 3]) == Just (V.fromList [3, 2, 1])
-- >>> swapV 0 2 (V.fromList [1, 2]) == Nothing

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV j k v =
  -- liftM2 (//) (Just v) $ mapM (\n -> sequence (n, v !? n)) [j,k] 
  do
  vj <- v !? j
  vk <- v !? k
  return $ v // [(j, vk), (k, vj)]

-- Exercise 2 -----------------------------------------
-- >>> mapM Just [1..10] == Just [1..10]
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f xs = sequence $ f <$> xs

-- >>> getElts [1,3] (V.fromList [0..9]) == Just [1, 3]
getElts :: [Int] -> Vector a -> Maybe [a]
getElts ns v = mapM (v!?) ns

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

-- getRandom :: Random a => Rnd a
-- getRandomR :: Random a => (a, a) -> Rnd a
getRandIntLtN :: Random Int => Rnd Int
getRandIntLtN n = liftM (\r -> floor (n * r)) getRandom

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = liftM (!?) (Just v) $ getRandIntLtN $ length v

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = sequence $ generate n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n range = sequence $ generate n (getRandomR range)

-- Exercise 5 -----------------------------------------
compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v
getFs n = map (\i -> liftM (flip (swapV i) v) (getRandIntLtN (i + 1)) $ reverse [1..(n - 1)]

-- >>> 1 + 1
-- 
shuffle :: Vector a -> Rnd (Vector a)
shuffle v = compose (getFs (length v)) v

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v n = run v n empty pivot empty
  where
    pivot = v ! n
    run v0 0 vl pivot vr = run v0 -1 vl pivot v4
    run [] m vl pivot vr = (vl, pivot, vr)
    run v0 m vl pivot vr = if a < pivot then run v1 k (snoc vl a) pivot vr else run v1 k vl pivot (snot vr a)
      where
        a = head v0
        v1 = tail v0
        k = m - 1

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort empty = empty
qsort v = qsort (do 
  y <- tail v
  guard $ y < x
  return y) <> (cons (head v) qsort (do
    y <- tail v
    guard $ y >= x
    return y))

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR empty = empty
qsortR v = undefined

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100

{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1:1:zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons (f x) $ sIterate f $ f x

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x s) s1 = Cons x (sInterleave s1 s)

-- >>> sTake 3 (sRepeat 0) == [0, 0, 0]
--
sTake :: Int -> Stream a -> [a]
sTake = go []
  where
    go xs 0 _ = xs
    go xs n (Cons x s) = go (x:xs) (n - 1) s

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = Cons 0 (fmap (+1) nats)

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) $ fmap (+1) ruler

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand r0 = Cons r0 $ rand $ (1103515245*r0 + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 362 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 349 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax [x] = Just (x, x)
minMax (x:xs) = do
  (y, z) <- minMax xs
  return $ (min x y, max x z)

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined

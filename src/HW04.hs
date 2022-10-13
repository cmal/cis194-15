{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module HW04 where
import Data.List (intercalate)
import Data.Foldable (Foldable(foldl'))


newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1]

-- Exercise 2 ----------------------------------------
-- >>> P [1, 2, 3] == P [1, 2, 3]
-- True

-- >>> P [1, 2] /= P [1, 2, 3]
-- True

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P xs) (P ys) = xs == ys

-- Exercise 3 -----------------------------------------

-- >>> monomial (1, 0) == "1"
-- True

monomial :: (Eq a1, Eq a2, Num a1, Num a2, Show a1, Show a2) => (a1, a2) -> [Char]
monomial (0, _) = ""
monomial (a, 0) = show a
monomial (a, b) = coefficient a <> power b

coefficient :: (Eq a, Num a, Show a) => a -> [Char]
coefficient 1 = ""
coefficient (-1) = "-"
coefficient n = show n

power :: (Eq a, Num a, Show a) => a -> [Char]
power 0 = ""
power 1 = "x"
power n = "x^" <> show n
-- >>> show (P [1, 0, 0, 2]) -- == "2x^3 + 1"
-- "2x^3 + 1"
-- >>> show (P [0, -1, 2]) -- == "2x^2 + -x"
-- "2x^2 + -x"

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show :: (Num a, Eq a, Show a) => Poly a -> String
    show (P xs) = go xs
        where
            go [] = "0"
            go ys = intercalate " + " $ reverse $ filter (/="") $ monomial <$> zip ys [0..]

-- Exercise 4 -----------------------------------------

zipWithExtended :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithExtended f da db la lb = let len = max (length la) (length lb)
                                    la' = la <> repeat da
                                    lb' = lb <> repeat db
                                in take len $ zipWith f la' lb'


plusHelper :: Num a => [a] -> [a] -> [a]
plusHelper = zipWithExtended f 0 0 where f = (+)
-- >>> plus (P [1, 0, 0, 2]) (P [0, 1, 0, 2, 6])
-- 6x^4 + 4x^3 + x + 1
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P y) (P z) = P $ plusHelper y z

newtype Product a = Product a
newtype Sum a = Sum a
instance  Num a => Semigroup (Sum (Poly a)) where
    (Sum x_) <> (Sum y) = Sum $ plus x_ y

instance Num a => Monoid (Sum (Poly a)) where
    mempty = Sum $ P []
    mappend :: Sum (Poly a) -> Sum (Poly a) -> Sum (Poly a)
    mappend = (<>)

-- Exercise 5 -----------------------------------------
-- >>> times (P [1, 1]) (P [1, 1])
-- x^2 + 2x + 1
-- >>> times (P [2]) (P [3])
-- 6
-- >>> times (P [2, 1]) (P [3])
-- 3x + 6
-- >>> P [5, 0, 1] + P [1, 1, 2] == P [6, 1, 3]
-- True

-- >>> P [1, 0, 1] + P [1, 1] == P [2, 1, 1]
-- True

times :: Num a => Poly a -> Poly a -> Poly a
times (P ys) (P zs) = P (timesHelper ys zs)
    where
        timesHelper [] _ = []
        timesHelper _ [] = []
        timesHelper (y:ys1) zs1 = plusHelper (map (*y) zs1) (0:timesHelper ys1 zs1)

-- Exercise 6 -----------------------------------------

-- >>> P [1, 1, 1] * P [2, 2] == P [2, 4, 4, 2]
-- True

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = times (P [-1])
    fromInteger n = P [fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------
-- >>> applyP (P [1, 2, 1]) 1 == 4
-- True
-- >>> applyP (P [1, 2, 1]) 2 == 9
-- True
applyP :: Num a => Poly a -> a -> a
applyP (P ys) = applyHelper ys
    where
        applyHelper [] _ = 0
        applyHelper (z:zs) k = z + k * applyHelper zs k

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 = deriv
    nderiv n = deriv . nderiv (n - 1)

-- Exercise 9 -----------------------------------------
-- >>> zipWith (*) [1,2,3] [0..]
-- [0,2,6]
-- >>> tail $ zipWith (*) [5,3,1] [0..]
-- [3,2]

-- >>> deriv (P [5, 3, 1]) == P [3, 2]
-- True
instance (Integral a) => Differentiable (Poly a) where
    deriv (P ys)=  P $ tail $ zipWith (*) ys [0..]


{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List(intercalate)


newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
  P xs == P ys = xs == ys
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P [])  = "0"
  show (P [0]) = "0"
  show (P xs)  = intercalate " + " $ terms
    where terms =  notNull $ map term (enum xs)
          notNull = filter (not . null)

term :: (Num a, Eq a, Show a) => (a, Int) -> String
term (0,  _) = ""
term (c,    0) = show c
term (1,    1) = "x"
term ((-1), 1) = "-x"
term (c,    1) = show c ++ "x"
term (1,    e) = "x^" ++ show e
term ((-1), e) = "-x^" ++ show e
term (c,    e) = show c ++ "x^" ++ show e

enum :: [b] -> [(b, Int)]
enum xs = zip xs [0..]

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined


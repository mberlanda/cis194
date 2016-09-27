{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------
toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n = lastDigit(n) : toRevDigits(dropLastDigit(n))

toDigits :: Integer -> [Integer]
toDigits n = reverse (toRevDigits n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (n:[]) = [n]
doubleEveryOther (n:m:ns) = n:(2*m):(doubleEveryOther ns)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n: []) = sum (toRevDigits n)
sumDigits (n: ns) = sum (toRevDigits n) + sumDigits(ns)

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = sumDigits(doubleEveryOther(toRevDigits n)) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a, c)]

hanoi n a b c = moveToB ++ moveTop ++ moveToC
  where
-- move n − 1 discs from a to b using c as temporary storage
   moveToB = hanoi (n-1) a c b
-- move the top disc from a to c
   moveTop = hanoi 1 a b c
-- move n − 1 discs from b to c using a as temporary storage.
   moveToC = hanoi (n-1) b a c
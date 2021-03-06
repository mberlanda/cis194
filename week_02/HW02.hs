{-# OPTIONS_GHC -Wall #-}
module HW02 where

import Data.List

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

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches actual guess = foldl countIfMatch 0 transposed
  where transposed = transpose [actual, guess]
        countIfMatch x [y, z] = if y == z then (1 + x) else x
        countIfMatch x [] = x
        countIfMatch _ [_] = undefined
        countIfMatch _ (_ : (_ : (_ : _))) = undefined

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors [] = []
countColors ys = map (\x -> countOccurrencies x ys) colors
  where countOccurrencies x list = length (filter (==x) list)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actual guess = sum (map minimum transposedCount)
  where transposedCount = transpose [countColors(actual), countColors(guess)]  

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess = Move guess exactMatch nonExactMatch
  where exactMatch = exactMatches actual guess
        nonExactMatch = matches actual guess - exactMatch

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess e n) actual = (Move guess e n) == getMove actual guess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m cs = filter (\c -> isConsistent m c) cs

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = addElement . allCodes $ n-1
  where addElement codes = concatMap(\c -> (map (\cc -> cc:c) colors)) codes

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve c = f(allCodes.length $ c)
  where f [] = []
        f (x:xs) = let m = getMove c x
                    in m : f (filterCodes m xs)


-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
-- CIS 194, Spring 2015
--
-- Test cases for HW 03

module HW03Tests where

import HW03

data Test    = forall a. Show a => Test String (a -> Bool) [a]
data Failure = forall a. Show a => Fail String [a]

instance Show Failure where
    show (Fail s as) = "Failed Test \"" ++ s
                       ++ "\" on inputs " ++ show as

runTest :: Test -> Maybe Failure
runTest (Test s f as) = case filter (not . f) as of
                          [] -> Nothing
                          fs -> Just $ Fail s fs


-- Exercise 1 -----------------------------------------
stx :: State
stx = extend empty "x" 50

-- Exercise 2 -----------------------------------------
evalE empty (Op (Val 1) Eql (Val 2)) == 0
evalE empty (Op (Val 1) Plus (Val 2)) == 3
evalE empty (Op (Val 1) Minus (Val 2)) == -1

-- Exercise 3 -----------------------------------------
desugar (Incr "A") == DAssign "A" (Op (Var "A") Plus (Val 1))

-- Exercise 4 -----------------------------------------
let s = evalSimple empty (DAssign "A" (Val 10))
in s "A" == 10

let s = run (extend empty "In" 4) factorial in s "Out" == 24
let s = run (extend empty "In" 5) factorial in s "Out" == 120
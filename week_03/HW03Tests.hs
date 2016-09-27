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

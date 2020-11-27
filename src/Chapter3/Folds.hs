{-# LANGUAGE NamedFieldPuns #-}

module Chapter3.Folds where

import Chapter3.ParametricPolymorphism
import Data.List

-- The product function computes the product of a list of integers
-- Write it using pattern matching, without resorting to any higher-order function
product' :: [Int] -> Int

-- Write it as a fold
product'' :: [Int] -> Int

-- The minimumClient function computes the Client with the shortest name
-- Write it using pattern matching, without resorting to any higher-order function
minimumClient' :: [Client i] -> Client i

-- Write it as a fold
minimumClient'' :: [Client i] -> Client i

-- The all function computes the conjunction (&&) of a list of Boolean values
-- Write it using pattern matching, without resorting to any higher-order function
all' :: [Bool] -> Bool

-- Write it as a fold
all'' :: [Bool] -> Bool

-- Try to write a minimumBy function such that the order is taken by first applying a function g on the result
minimumBy :: Ord b => (a -> b) -> [a] -> a

{-# LANGUAGE NamedFieldPuns #-}

module Chapter3.Folds where

import Chapter3.ParametricPolymorphism
import Data.List

-- The product function computes the product of a list of integers
-- Write it using pattern matching, without resorting to any higher-order function
product' :: [Int] -> Int
product' [] = 1
product' (n : ns) = n * product' ns

-- Write it as a fold
product'' :: [Int] -> Int
product'' = foldr (*) 1

-- The minimumClient function computes the Client with the shortest name
-- Write it using pattern matching, without resorting to any higher-order function
minimumClient' :: [Client i] -> Client i
minimumClient' [] = error "No Clients"
minimumClient' [c] = c
minimumClient' (c1 : c2 : cs) = minimumClient'' (cx : cs)
  where
    cx
      | getNameLength c1 <= getNameLength c2 = c1
      | otherwise = c2

-- Write it as a fold
minimumClient'' :: [Client i] -> Client i
minimumClient'' = foldl1' shortestName
  where
    shortestName c1 c2
      | getNameLength c1 <= getNameLength c2 = c1
      | otherwise = c2

getNameLength :: Client i -> Int
getNameLength Individual {person} = length . firstName $ person
getNameLength other = length $ clientName other

-- The all function computes the conjunction (&&) of a list of Boolean values
-- Write it using pattern matching, without resorting to any higher-order function
all' :: [Bool] -> Bool
all' [] = True
all' (b : bs) = b && all' bs

-- Write it as a fold
all'' :: [Bool] -> Bool
all'' = foldr (&&) True

-- Try to write a minimumBy function such that the order is taken by first applying a function g on the result
minimumBy :: Ord b => (a -> b) -> [a] -> a
minimumBy g = foldr1 (\x y -> if g x <= g y then x else y)

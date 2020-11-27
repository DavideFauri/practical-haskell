{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Chapter3.FunctionsAsParameters where

import Chapter3.ParametricPolymorphism

apply3f2 :: (Integer -> Integer) -> Integer -> Integer
apply3f2 f x = 3 * f (x + 2)

equalTuples :: [(Integer, Integer)] -> [Bool]
equalTuples = map (\(x, y) -> x == y)

sayHello :: [String] -> [String]
sayHello =
  map
    ( \case
        "Alejandro" -> "Hello, writer"
        name -> "Welcome, " ++ name
    )

multiplyByN :: Integer -> (Integer -> Integer)
multiplyByN n = (n *)

double :: [Integer] -> [Integer]
double = map (* 2)

duplicateOdds :: [Integer] -> [Integer]
duplicateOdds = map (* 2) . filter odd

-- Using the function filter as the basis for your solution, write the following functions:

-- filterOnes, which returns only the elements equal to the constant 1.
filterOnes :: (Eq a, Num a) => [a] -> [a]

-- filterANumber, which returns only the elements equal to some number that is given via a parameter.
filterANumber :: (Eq a, Num a) => a -> [a] -> [a]

-- filterNot, which performs the reverse duty of filter. It returns only those elements of the list that do not fulfill the condition.
filterNot :: (a -> Bool) -> [a] -> [a]

-- filterGovOrgs, which takes a list of Clients (as defined before) and returns only those that are government organizations.
-- Write filterGovOrgs using an auxiliary function isGovOrg
filterGovOrgs :: [Client i] -> [Client i]

-- Write filterGovOrgs using a \case expression
filterGovOrgs' :: [Client i] -> [Client i]

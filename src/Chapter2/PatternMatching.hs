module Chapter2.PatternMatching where

import Chapter2.DataTypes

companyName :: Client -> Maybe String
companyName client = case client of
  Company name _ _ _ -> Just name
  _ -> Nothing

clientName :: Client -> String
clientName (GovOrg name) = name
clientName (Company name _ _ _) = name
clientName (Individual (Person fNm lNm _) _) = fNm ++ " " ++ lNm

f :: Client -> String
f client = case client of
  Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
  _ -> "There is no boss"

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n -1) + fibonacci (n -2)

-- Write a function that returns the number of clients of each gender.
-- You may need to define an auxiliary data type to hold the results of this function.
clientsPerGender :: [Client] -> [(Gender, Int)]

-- Write a function that, given a list of time machines, decreases their price by some percentage.
discountTimeMachines :: Float -> [TimeMachine] -> [TimeMachine]

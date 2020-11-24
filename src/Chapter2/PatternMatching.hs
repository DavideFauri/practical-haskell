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
clientsPerGender clients = [(Male, countMale), (Female, countFemale), (Unknown, countUnknown)]
  where
    countMale = howMany Male individuals
    countFemale = howMany Female individuals
    countUnknown = howMany Unknown individuals + length notIndividuals
    individuals = filter isIndividual clients
    notIndividuals = filter (not . isIndividual) clients
    isIndividual (Individual _ _) = True
    isIndividual _ = False
    howMany gender people = length $ filter (hasThisGender gender) people
      where
        hasThisGender Male (Individual (Person _ _ Male) _) = True
        hasThisGender Female (Individual (Person _ _ Female) _) = True
        hasThisGender Unknown (Individual (Person _ _ Unknown) _) = True
        hasThisGender _ _ = False

-- Write a function that, given a list of time machines, decreases their price by some percentage.
discountTimeMachines :: Float -> [TimeMachine] -> [TimeMachine]
discountTimeMachines _ [] = []
discountTimeMachines percentage ((TimeMachine ma mo na wh pr) : tms) = TimeMachine ma mo na wh (pr * percentage) : discountTimeMachines percentage tms

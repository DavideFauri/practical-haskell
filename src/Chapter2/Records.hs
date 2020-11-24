{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Chapter2.Records where

import Data.Char (toUpper)

data ClientR
  = GovOrgR {clientRName :: String}
  | CompanyR
      { clientRName :: String,
        companyId :: Integer,
        person :: PersonR,
        duty :: String
      }
  | IndividualR {person :: PersonR}
  deriving (Show)

data PersonR = PersonR
  { firstName :: String,
    lastName :: String
  }
  deriving (Show)

greet :: ClientR -> String
greet IndividualR {person = PersonR {..}} = "Hi, " ++ firstName
greet CompanyR {..} = "Hi, " ++ clientRName
greet GovOrgR {} = "Welcome"

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@PersonR {firstName = initial : rest} =
  let newName = toUpper initial : rest
   in p {firstName = newName}
nameInCapitals p@PersonR {firstName = ""} = p

-- rewrite the TimeMachine data type defined earlier using records.

-- Write a function that, given a list of time machines, decreases their price by some percentage.
discountTimeMachines :: Float -> [TimeMachine] -> [TimeMachine]

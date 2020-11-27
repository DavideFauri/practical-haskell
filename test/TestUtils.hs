{-# LANGUAGE FlexibleInstances #-}

module TestUtils where

import qualified Chapter2.DataTypes as D (Client (..), Gender (..), Person (..), TimeMachine (..))
import qualified Chapter2.Records as R (TimeMachine (..))
import qualified Chapter3.ParametricPolymorphism as P (Client (..), Person (..))
import Control.Exception (SomeException, handleJust)
-- import Test.Tasty ( TestTree )

import Data.List (intercalate, isPrefixOf)
import Test.QuickCheck.Function
import Test.Tasty.ExpectedFailure (wrapTest)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, oneof)
import Test.Tasty.Runners
import Unsafe.Coerce (unsafeCoerce)

-- Handling testCases which we want to raise an Exception

expectException :: String -> TestTree -> TestTree
expectException s = expectExceptions [s]

expectExceptions :: [String] -> TestTree -> TestTree
expectExceptions prefixes = wrapTest (handleJust selector handler)
  where
    selector :: SomeException -> Maybe SomeException
    selector e
      | not . null . prefixesMatching $ e = Just e
      | otherwise = Nothing
    handler e =
      pure
        Result
          { resultOutcome = Success,
            resultDescription = "Exception matches: " <> (intercalate " OR " . map quoted) (prefixesMatching e),
            resultShortDescription = "FAIL (expected)",
            resultTime = 0
          }
    prefixesMatching e = filter (`isPrefixOf` show e) prefixes
    quoted s = "\"" <> s <> "\""

-- Generating a fake arbitrary function from a specified one (like pure False :: Gen Bool)

-- Horrible hack of course, but it works as long as we only use `applyFun (Fun _ f) = f`
-- The unsafeCoerce is just to avoid raising "undefined" exceptions when trying to show the function itself
wrapSpecifiedFunction :: (Function a) => (a -> b) -> Fun a b
wrapSpecifiedFunction f = Fun (function f, undefined, unsafeCoerce True) f

-- Arbitrary Instances

newtype AnyGender = AnyGender {gender :: D.Gender} deriving (Show)

instance Arbitrary AnyGender where
  arbitrary = oneof [pure $ AnyGender D.Male, pure $ AnyGender D.Female, pure $ AnyGender D.Unknown]

newtype AnyClient = AnyClient {client :: D.Client} deriving (Show)

instance Arbitrary AnyClient where
  arbitrary =
    oneof
      [ AnyClient <$> (D.GovOrg <$> arbitrary),
        AnyClient <$> (D.Company <$> arbitrary <*> arbitrary <*> (person <$> arbitrary) <*> arbitrary),
        AnyClient <$> (D.Individual <$> (person <$> arbitrary) <*> arbitrary)
      ]

newtype AnyParamClient i = AnyParamClient {paramClient :: P.Client i} deriving (Show)

instance Arbitrary i => Arbitrary (AnyParamClient i) where
  arbitrary =
    oneof
      [ AnyParamClient <$> (P.GovOrg <$> arbitrary <*> arbitrary),
        AnyParamClient <$> (P.Company <$> arbitrary <*> arbitrary <*> (paramPerson <$> arbitrary) <*> arbitrary),
        AnyParamClient <$> (P.Individual <$> arbitrary <*> (paramPerson <$> arbitrary))
      ]

newtype AnyPerson = AnyPerson {person :: D.Person} deriving (Show)

instance Arbitrary AnyPerson where
  arbitrary = AnyPerson <$> (D.Person <$> arbitrary <*> arbitrary <*> (gender <$> arbitrary))

newtype AnyParamPerson = AnyParamPerson {paramPerson :: P.Person} deriving (Show)

instance Arbitrary AnyParamPerson where
  arbitrary = AnyParamPerson <$> (P.Person <$> arbitrary <*> arbitrary)

data AnyTimeDirection = Forward' | Backward' | BiDirectional' deriving (Show)

instance Arbitrary AnyTimeDirection where
  arbitrary = oneof [pure Forward', pure Backward', pure BiDirectional']

--FIX How to get rid of unsafeCoerce?
-- TL;DR: I have to use an arbitrary type because I don't know the implementation of TimeDirection
-- Does TimeDirection have two constructors? Three? More? I cannot derive an Arbitrary instance for an unknown sum type!
-- And even if I can, how do I check the coverage on that data type? I don't know the constructor names!
-- So best thing I can do is to coerce arbitrray values of my type to TimeDirection, hoping to match its cardinality.
-- But when coercing back for tabulating (see commented code above), the internal representation stay the same and that's bad.
-- ex. coercing an AnyTimeDirection (cardinality 3) to a TimeDirection that's only Forward | Backward (cardinality 2) results in:
-- ~33% Forward, ~67% Backward <-- all the "BiDirectional'" get coerced into the last sum type!
-- The internal representation stays the same, though, so when coercing back to AnyTimeDirection:
-- ~33% Forward', ~33% Backward', ~33% BiDirectional' <-- I get NO insight on the cardinality of TimeDirection
newtype AnyTimeMachine = AnyTimeMachine {getTM :: D.TimeMachine} deriving (Show)

instance Arbitrary AnyTimeMachine where
  arbitrary =
    AnyTimeMachine
      <$> ( D.TimeMachine
              <$> (arbitrary :: Gen String)
              <*> (arbitrary :: Gen Integer)
              <*> (arbitrary :: Gen String)
              <*> unsafeCoerce (arbitrary :: Gen AnyTimeDirection)
              <*> (arbitrary :: Gen Float)
          )

newtype AnyRecordTimeMachine = AnyRecordTimeMachine {getRTM :: R.TimeMachine} deriving (Show)

instance Arbitrary AnyRecordTimeMachine where
  arbitrary =
    AnyRecordTimeMachine
      <$> ( R.TimeMachine
              <$> (arbitrary :: Gen String)
              <*> (arbitrary :: Gen Integer)
              <*> (arbitrary :: Gen String)
              <*> unsafeCoerce (arbitrary :: Gen AnyTimeDirection)
              <*> (arbitrary :: Gen Float)
          )

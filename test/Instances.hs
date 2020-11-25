module Instances () where

import qualified Chapter2.DataTypes as D (Client (..), Gender (..), Person (..), TimeMachine (..))
import qualified Chapter2.Records as R (TimeMachine (..))
import qualified Chapter3.ParametricPolymorphism as P (Client (..), Person (..))
import Test.Tasty
import Test.Tasty.QuickCheck
import Unsafe.Coerce

instance Arbitrary D.Gender where
  arbitrary = oneof [pure D.Male, pure D.Female, pure D.Unknown]

instance Arbitrary D.Client where
  arbitrary =
    oneof
      [ D.GovOrg <$> arbitrary,
        D.Company <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        D.Individual <$> arbitrary <*> arbitrary
      ]

instance Arbitrary i => Arbitrary (P.Client i) where
  arbitrary =
    oneof
      [ P.GovOrg <$> arbitrary <*> arbitrary,
        P.Company <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        P.Individual <$> arbitrary <*> arbitrary
      ]

instance Arbitrary D.Person where
  arbitrary = D.Person <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary P.Person where
  arbitrary = P.Person <$> arbitrary <*> arbitrary

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
instance Arbitrary D.TimeMachine where
  arbitrary =
    D.TimeMachine
      <$> (arbitrary :: Gen String)
      <*> (arbitrary :: Gen Integer)
      <*> (arbitrary :: Gen String)
      <*> unsafeCoerce (arbitrary :: Gen AnyTimeDirection)
      <*> (arbitrary :: Gen Float)

instance Arbitrary R.TimeMachine where
  arbitrary =
    R.TimeMachine
      <$> (arbitrary :: Gen String)
      <*> (arbitrary :: Gen Integer)
      <*> (arbitrary :: Gen String)
      <*> unsafeCoerce (arbitrary :: Gen AnyTimeDirection)
      <*> (arbitrary :: Gen Float)

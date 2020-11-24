{-# LANGUAGE ScopedTypeVariables #-}

module TestChapter2 where

import Chapter2.DataTypes
import Chapter2.ListsOfLists
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Unsafe.Coerce

chapter2Tests :: TestTree
chapter2Tests =
  testGroup
    "Chapter 2"
    [ testListsOfLists,
      testDataTypes,
      testPatternMatching,
      testMatchesGuards,
      testRecords
    ]

-- EXERCISE 2-1

testListsOfLists :: TestTree
testListsOfLists =
  testGroup
    "Exercise 2-1"
    [ testOneTwoThree,
      testABC,
      testGreetings,
      testABCDE,
      testEmptyElement,
      testEmptyOrFirstEmpty,
      testOnlyOneElement,
      testConcatenateTwo
    ]

testOneTwoThree :: TestTree
testOneTwoThree =
  testGroup
    "[1,2,3] literal"
    [ testCase "Write the list literal" $ listOneTwoThree @?= ([1, 2, 3] :: [Int]),
      testCase "Using only (:) and []" $ assertBool "" True --TODO
    ]

testABC :: TestTree
testABC =
  testGroup
    "['a','b','c'] literal"
    [ testCase "Write the list literal" $ listABC @?= "abc",
      testCase "Using only (:) and []" $ assertBool "" True --TODO
    ]

testGreetings :: TestTree
testGreetings =
  testGroup
    "[\"hello\",\"hola\"] literal"
    [ testCase "Write the list literal" $ listGreetings @?= ["hello", "hola"],
      testCase "Using only (:) and []" $ assertBool "" True --TODO
    ]

testABCDE :: TestTree
testABCDE =
  testGroup
    "[['a','b','c'],['d','e']] literal"
    [ testCase "Write the list literal" $ listABCDE @?= [['a', 'b', 'c'], ['d', 'e']],
      testCase "Using only (:) and []" $ assertBool "" True --TODO
    ]

testEmptyElement :: TestTree
testEmptyElement =
  testGroup
    "[[]] literal"
    [ testCase "Write the list literal" $ (listEmptyElement :: [[Int]]) @?= [[]],
      testCase "Using only (:) and []" $ assertBool "" True --TODO
    ]

-- Write an expression that checks whether a list of lists is empty, [], or its first element is empty, like [[],['a','b']].
testEmptyOrFirstEmpty :: TestTree
testEmptyOrFirstEmpty =
  testGroup
    "Check if a list is empty or its first element is empty"
    [ testCase "...when list is empty" $ listEmptyOrFirstEmpty [] @?= True,
      testProperty "... when first element is empty" $
        forAll
          (arbitrary :: Gen [[Int]]) --TODO: use other types than Int
          (listEmptyOrFirstEmpty . ([] :)),
      testProperty "... when the above is not true" $
        forAll
          (prependNonEmptyTo arbitrary :: Gen [[Int]]) --TODO: use other types than Int
          (not . listEmptyOrFirstEmpty)
    ]

prependNonEmptyTo :: forall a. Arbitrary a => Gen [[a]] -> Gen [[a]]
prependNonEmptyTo xs = do
  x <- arbitrary :: Gen (NonEmptyList a)
  fmap (getNonEmpty x :) xs

-- Write an expression that checks whether a list has only one element.
testOnlyOneElement :: TestTree
testOnlyOneElement =
  testGroup
    "Check if a list has only one element"
    [ testCase "... when list is empty" $ listOnlyOneElement [] @?= False,
      testProperty "... when it has one element" $
        forAll
          (arbitrary :: Gen Int) --TODO: use other types than Int
          (listOnlyOneElement . pure),
      testProperty "... when it has two or more elements" $
        forAll
          (arbitrary `suchThat` (\l -> length l >= 2) :: Gen [Int]) --TODO: use other types than Int
          (not . listOnlyOneElement)
    ]

-- Write an expression that concatenates two lists given inside another list.
testConcatenateTwo :: TestTree
testConcatenateTwo =
  testGroup
    "Concatenate two lists given inside another list"
    [ expectFailBecause "The test should raise an error" $
        testCase "... when there are no lists" $
          assertBool "" $ seq (concatenateTwoLists []) True,
      testProperty "... when there is only one list" $
        forAll
          (arbitrary `suchThat` (\l -> length l == 1) :: Gen [[Int]]) --TODO: use other types than Int
          (\l -> concatenateTwoLists l == head l),
      testProperty "... when there are at least two lists" $
        forAll
          (arbitraryTwoOrMoreLists :: Gen [[Int]]) --TODO: use other types than Int
          (\l@(a : b : _) -> concatenateTwoLists l == a ++ b)
    ]

arbitraryTwoOrMoreLists :: forall a. Arbitrary a => Gen [[a]]
arbitraryTwoOrMoreLists = do
  as <- arbitrary :: Gen [a]
  bs <- arbitrary :: Gen [a]
  rest <- arbitrary :: Gen [[a]]
  pure $ as : bs : rest

-- EXERCISE 2-4

testDataTypes :: TestTree
testDataTypes =
  testGroup
    "Exercise 2-4"
    [ testGenderShowable,
      testClientGeneration,
      testTimeMachine
    ]

testGenderShowable :: TestTree
testGenderShowable =
  testProperty "Gender datatype is Showable" $
    forAll
      (arbitrary :: Gen Gender)
      propGender
  where
    genders = ["Male", "Female", "Unknown"]
    minProbEachGender = 30.0 -- percent
    propGender g =
      checkCoverage $
        coverTable "Gender" (zip genders $ repeat minProbEachGender) $
          tabulate "Gender" [show g] $
            show g `elem` genders

instance Arbitrary Gender where
  arbitrary = oneof [pure Male, pure Female, pure Unknown]

testClientGeneration :: TestTree
testClientGeneration =
  testProperty "Client datatype can be generated" $
    forAll
      (arbitrary :: Gen Client)
      propClient
  where
    clients = ["GovOrg", "Company", "Individual"]
    minProbEachClient = 30.0 -- percent
    getConstructor = head . words . show
    propClient c =
      checkCoverage $
        coverTable "Client" (zip clients $ repeat minProbEachClient) $
          tabulate "Client" [getConstructor c] $
            getConstructor c `elem` clients

instance Arbitrary Client where
  arbitrary =
    oneof
      [ GovOrg <$> arbitrary,
        Company <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        Individual <$> arbitrary <*> arbitrary
      ]

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary <*> arbitrary

testTimeMachine :: TestTree
testTimeMachine =
  testProperty "TimeMachine datatype can be generated" $
    forAll
      (arbitrary :: Gen TimeMachine)
      propTimeMachine
  where
    getConstructor = head . words . show
    propTimeMachine tm@(TimeMachine _ _ _ dir _) =
      -- checkCoverage $
      -- coverTable "Direction" [("Forward'", 30.0), ("Backward'", 30.0), ("BiDirectional'", 30.0)] $
      -- tabulate "Direction" [show (unsafeCoerce dir :: AnyTimeDirection)] $
      getConstructor tm == "TimeMachine"

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
instance Arbitrary TimeMachine where
  arbitrary = TimeMachine <$> arbitrary <*> arbitrary <*> arbitrary <*> unsafeCoerce (arbitrary :: Gen AnyTimeDirection) <*> arbitrary

-- EXERCISE 2-5

testPatternMatching :: TestTree
testPatternMatching = testGroup "Exercise 2-5" []

-- EXERCISE 2-6

testMatchesGuards :: TestTree
testMatchesGuards = testGroup "Exercise 2-6" []

-- EXERCISE 2-7

testRecords :: TestTree
testRecords = testGroup "Exercise 2-7" []

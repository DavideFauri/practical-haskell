{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestChapter2 where

import Chapter2.DataTypes hiding (TimeMachine (..))
import qualified Chapter2.DataTypes as D (TimeMachine (..))
import Chapter2.ListsOfLists
import Chapter2.MatchesAndGuards
import Chapter2.PatternMatching hiding (discountTimeMachines)
import qualified Chapter2.PatternMatching as P (discountTimeMachines)
import qualified Chapter2.Records as R (TimeMachine (..), discountTimeMachines)
import Data.Function (on)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import TestUtils

-- import Unsafe.Coerce (unsafeCoerce) -- See issue with AnyTimeDirection in TestUtils.hs

chapter2Tests :: TestTree
chapter2Tests =
  testGroup
    "Chapter 2"
    [ testListsOfLists,
      testDataTypes,
      testPatternMatching,
      testMatchesAndGuards,
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
    [ expectException "" $
        testCase "... when there are no lists" $
          assertBool "" $ seq (concatenateTwoLists []) True,
      testProperty "... when there is only one list" $
        forAll
          (arbitrary `suchThat` (\l -> length l == 1) :: Gen [[Int]]) --TODO: use other types than Int
          (\l -> concatenateTwoLists l == head l),
      testProperty "... when there are at least two lists" $
        forAll
          (arbitrary `suchThat` (\l -> length l >= 2) :: Gen [[Int]]) --TODO: use other types than Int
    ]

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
      (arbitrary :: Gen AnyGender)
      propGender
  where
    genders = ["Male", "Female", "Unknown"]
    minProbEachGender = 30.0 -- percent
    propGender g =
      checkCoverage $
        coverTable "Gender" (zip genders $ repeat minProbEachGender) $
          tabulate "Gender" [show (gender g)] $
            show (gender g) `elem` genders

testClientGeneration :: TestTree
testClientGeneration =
  testProperty "Client datatype can be generated and is Showable" $
    forAll
      (arbitrary :: Gen AnyClient)
      propClient
  where
    clients = ["GovOrg", "Company", "Individual"]
    minProbEachClient = 30.0 -- percent
    getConstructor = head . words . show
    propClient c =
      checkCoverage $
        coverTable "Client" (zip clients $ repeat minProbEachClient) $
          tabulate "Client" [getConstructor (client c)] $
            getConstructor (client c) `elem` clients

testTimeMachine :: TestTree
testTimeMachine =
  testProperty "TimeMachine datatype can be generated and is Showable" $
    forAll
      (arbitrary :: Gen AnyTimeMachine)
      propTimeMachine
  where
    getConstructor = head . words . show

    -- ------ See issue with AnyTimeDirection in TestUtils.hs -------
    -- propTimeMachine machine@(AnyTimeMachine (D.TimeMachine _ _ _ dir _)) =
    -- checkCoverage $
    -- coverTable "Direction" [("Forward'", 30.0), ("Backward'", 30.0), ("BiDirectional'", 30.0)] $
    -- tabulate "Direction" [show (unsafeCoerce dir :: AnyTimeDirection)] $

    propTimeMachine machine =
      getConstructor (getTM machine) == "TimeMachine"

-- EXERCISE 2-5

testPatternMatching :: TestTree
testPatternMatching =
  testGroup
    "Exercise 2-5"
    [ testCountGenders,
      testDiscount
    ]

testCountGenders :: TestTree
testCountGenders =
  testGroup
    "Client gender counting"
    [ testCountShape,
      testCountCorrect
    ]

testCountShape :: TestTree
testCountShape =
  testProperty "...returns a list with the correct shape" $
    forAll
      arbitraryGenderCount
      propCountShape
  where
    propCountShape (clients, _) = isCorrectShape $ clientsPerGender (map client clients)
    isCorrectShape [(Male, _), (Female, _), (Unknown, _)] = True
    isCorrectShape _ = False

testCountCorrect :: TestTree
testCountCorrect =
  testProperty "...returns the correct count for genders" $
    forAll
      arbitraryGenderCount
      propCountCorrect
  where
    propCountCorrect (clients, counts) = and $ zipWith compareCounts (clientsPerGender (map client clients)) counts
    compareCounts = (==) `on` snd

arbitraryGenderCount :: Gen ([AnyClient], [(Gender, Int)])
arbitraryGenderCount = do
  males <- listOf (individualOfGender Male)
  females <- listOf (individualOfGender Female)
  unknowns <- listOf (oneof [individualOfGender Unknown, arbitrary `suchThat` (not . isIndividual . client)])
  clients <- shuffle . concat $ [males, females, unknowns]
  let counts = [(Male, length males), (Female, length females), (Unknown, length unknowns)]
  pure (clients, counts)

individualOfGender :: Gender -> Gen AnyClient
individualOfGender g =
  AnyClient
    <$> ( Individual
            <$> ( Person
                    <$> arbitrary
                    <*> arbitrary
                    <*> pure g
                )
            <*> arbitrary
        )

isIndividual :: Client -> Bool
isIndividual (Individual _ _) = True
isIndividual _ = False

testDiscount :: TestTree
testDiscount =
  testProperty "Time Machines are discounted correctly" $
    forAll
      arbitraryTimeMachinesAndDiscount
      propPriceIsDiscounted
  where
    getPrice (D.TimeMachine _ _ _ _ price) = price
    propPriceIsDiscounted (d, tms) = map ((d *) . getPrice . getTM) tms == (map getPrice . P.discountTimeMachines d) (map getTM tms)

arbitraryTimeMachinesAndDiscount :: Gen (Float, [AnyTimeMachine])
arbitraryTimeMachinesAndDiscount = (,) <$> arbitrary <*> arbitrary

-- EXERCISE 2-6

testMatchesAndGuards :: TestTree
testMatchesAndGuards =
  testGroup
    "Exercise 2-6"
    [ testAckermann,
      testUnzip
    ]

testAckermann :: TestTree
testAckermann =
  testGroup
    "(beginning of) Ackermann function is correct"
    [testSingleAckermann n m exp | n <- ns, m <- ms | exp <- expected_results]
  where
    ns = [0 .. 3]
    ms = [0 .. 2]
    expected_results = [1, 2, 3, 2, 3, 4, 3, 5, 7, 5, 13, 29]

testSingleAckermann :: Integer -> Integer -> Integer -> TestTree
testSingleAckermann n m exp =
  testCase ("Testing ackermann " <> show n <> " " <> show m) $
    ackermann n m @?= exp

testUnzip :: TestTree
testUnzip =
  testProperty "Unzipping correctly" $
    forAll
      (arbitraryZips :: Gen ([Int], [Int], [(Int, Int)]))
      propUnzip
  where
    propUnzip (as, bs, zs) = Chapter2.MatchesAndGuards.unzip zs == (as, bs)

arbitraryZips :: forall a b. (Arbitrary a, Arbitrary b) => Gen ([a], [b], [(a, b)])
arbitraryZips = do
  as <- arbitrary :: Gen [a]
  bs <- arbitrary :: Gen [b]
  let zs = zip as bs
  let n_zs = length zs
  pure (take n_zs as, take n_zs bs, zs)

-- EXERCISE 2-7

testRecords :: TestTree
testRecords =
  testGroup
    "Exercise 2-7"
    [ testRecordTimeMachines,
      testRecordDiscount
    ]

testRecordTimeMachines :: TestTree
testRecordTimeMachines =
  testProperty "Time Machine record type can be generated and is Showable" $
    forAll
      (arbitrary :: Gen AnyRecordTimeMachine)
      propRecordTimeMachine
  where
    getConstructor = head . words . show
    propRecordTimeMachine tm =
      getConstructor (getRTM tm) == "TimeMachine"

testRecordDiscount :: TestTree
testRecordDiscount =
  testProperty "Record Time Machines are discounted correctly" $
    forAll
      arbitraryRecordTimeMachinesAndDiscount
      propPriceIsDiscounted
  where
    getPrice R.TimeMachine {R.price} = price
    propPriceIsDiscounted (d, tms) = map ((d *) . getPrice . getRTM) tms == (map getPrice . R.discountTimeMachines d) (map getRTM tms)

arbitraryRecordTimeMachinesAndDiscount :: Gen (Float, [AnyRecordTimeMachine])
arbitraryRecordTimeMachinesAndDiscount = (,) <$> arbitrary <*> arbitrary

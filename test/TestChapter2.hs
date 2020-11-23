{-# LANGUAGE ScopedTypeVariables #-}

module TestChapter2 where

import Chapter2.ListsOfLists
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

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

testDataTypes :: TestTree
testDataTypes = testGroup "Exercise 2-4" []

testPatternMatching :: TestTree
testPatternMatching = testGroup "Exercise 2-5" []

testMatchesGuards :: TestTree
testMatchesGuards = testGroup "Exercise 2-6" []

testRecords :: TestTree
testRecords = testGroup "Exercise 2-7" []

-- EXERCISE 2-1

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
      expectFail $
        testProperty "... when the above is not true" $
          forAll
            (prependNonEmptyTo arbitrary :: Gen [[Int]]) --TODO: use other types than Int
            listEmptyOrFirstEmpty
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
      expectFail $
        testProperty "... when it has two or more elements" $
          forAll
            (arbitrary `suchThat` (\l -> length l >= 2) :: Gen [Int]) --TODO: use other types than Int
            listOnlyOneElement
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

-- EXERCISE 2-5

-- EXERCISE 2-6

-- EXERCISE 2-7

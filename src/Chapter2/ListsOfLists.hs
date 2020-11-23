module Chapter2.ListsOfLists where

-- Rewrite [1,2,3] using only (:) and the empty list constructor, []
listOneTwoThree :: [Int]

-- Rewrite ['a','b','c'] using only (:) and the emtpy list constructor, []
listABC :: [Char]

-- Rewrite ["hello","hola"] using only (:) and the empty list constructor, []
listGreetings :: [String]

-- Rewrite [['a','b','c'],['d','e']]  using only (:) and the empty list constructor, []
listABCDE :: [[Char]]

-- Rewrite [[]]  using only (:) and the empty list constructor, []
listEmptyElement :: [[a]]

-- Write an expression that checks whether a list of lists is empty, [], or its first element is empty, like [[],['a','b']].
listEmptyOrFirstEmpty :: [[a]] -> Bool

-- Write an expression that checks whether a list has only one element.
-- It should return True for ['a'] and False for [] or ['a','b'].
listOnlyOneElement :: [a] -> Bool

-- Write an expression that concatenates two lists given inside another list.
-- For example, it should return "abcde" for ["abc","de"]
concatenateTwoLists :: [[a]] -> [a]

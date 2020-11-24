module Chapter2.ListsOfLists where

-- Rewrite [1,2,3] using only (:) and the empty list constructor, []
listOneTwoThree :: [Int]
listOneTwoThree = 1 : 2 : 3 : []

-- Rewrite ['a','b','c'] using only (:) and the emtpy list constructor, []
listABC :: [Char]
listABC = 'a' : 'b' : 'c' : []

-- Rewrite ["hello","hola"] using only (:) and the empty list constructor, []
listGreetings :: [String]
listGreetings = "hello" : "hola" : []

-- Rewrite [['a','b','c'],['d','e']]  using only (:) and the empty list constructor, []
listABCDE :: [[Char]]
listABCDE = ('a' : 'b' : 'c' : []) : ('d' : 'e' : []) : []

-- Rewrite [[]]  using only (:) and the empty list constructor, []
listEmptyElement :: [[a]]
listEmptyElement = [] : []

-- Write an expression that checks whether a list of lists is empty, [], or its first element is empty, like [[],['a','b']].
listEmptyOrFirstEmpty :: [[a]] -> Bool
listEmptyOrFirstEmpty xs = null xs || (null . head $xs)

-- Write an expression that checks whether a list has only one element.
-- It should return True for ['a'] and False for [] or ['a','b'].
listOnlyOneElement :: [a] -> Bool
listOnlyOneElement [] = False
listOnlyOneElement [_] = True
listOnlyOneElement _ = False

-- Write an expression that concatenates two lists given inside another list.
-- For example, it should return "abcde" for ["abc","de"]
concatenateTwoLists :: [[a]] -> [a]
concatenateTwoLists [] = error "Input is invalid"
concatenateTwoLists [x] = x
concatenateTwoLists (x : y : _) = x ++ y

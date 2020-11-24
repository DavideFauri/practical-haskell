module Chapter2.MatchesAndGuards where

ifibonacci :: Integer -> Maybe Integer
ifibonacci n | n < 0 = Nothing
ifibonacci 0 = Just 0
ifibonacci 1 = Just 1
ifibonacci n
  | otherwise =
    let Just f1 = ifibonacci (n -1)
        Just f2 = ifibonacci (n -2)
     in Just (f1 + f2)

binom :: Int -> Int -> Int
binom _ 0 = 1
binom n k
  | n == k = 1
  | otherwise = binom (n -1) (k -1) - binom (n -1) k

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = mod x y == 0

specialMultiples :: Integer -> String
specialMultiples n
  | multipleOf n 2 = show n ++ " is multiple of 2"
  | multipleOf n 3 = show n ++ " is multiple of 3"
  | multipleOf n 5 = show n ++ " is multiple of 5"
  | otherwise = show n ++ " is a beautiful number"

-- Define the famous ackermann function. Try using guards
ackermann :: Integer -> Integer -> Integer

-- Define the unzip function, which takes a list of tuples and returns two lists, one with all the first components and other one with the seconds.
unzip :: [(a, b)] -> ([a], [b])

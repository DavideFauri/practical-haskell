module Chapter3.ParametricPolimorphism where

maybeString :: Maybe a -> String
maybeString (Just _) = "Just"
maybeString Nothing = "Nothing"

data Client i
  = GovOrg {clientId :: i, clientName :: String}
  | Company
      { clientId :: i,
        clientName :: String,
        person :: Person,
        duty :: String
      }
  | Individual {clientId :: i, person :: Person}
  deriving (Show, Eq, Ord)

-- Eq and Ord will be introduced in Chapter 4

data Person = Person {firstName :: String, lastName :: String}
  deriving (Show, Eq, Ord)

nttf :: Client Char
nttf = GovOrg 'n' "NTTF"

data Triple a b c = Triple a b c

data SamePair a = SamePair a a

-- Try to understand what the following functions do and which type will be inferred by the interpreter.
-- Give the most polymorphic answer from all possible ones.

swapTriple (x, y, z) = (y, z, x)

duplicate x = (x, x)

nothing _ = Nothing

index [] = []
index [x] = [(0, x)]
index (x : xs) =
  let indexed@((n, _) : _) = index xs
   in (n + 1, x) : indexed

maybeA [] = 'a'

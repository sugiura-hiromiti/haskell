data Thing = Shoe | Ship | SealingWax | Cabbage | King
  deriving (Show)

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _ = True

data FailableDouble = Failure | Ok Double
  deriving (Show)

ex01 = Ok 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = Ok (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (Ok d) = d

data Person = Person String Int Thing
  deriving (Show)

brent :: Person
brent = Person "Brent" 32 SealingWax

stan = Person "Stan" 25 Cabbage

-- getAge :: Person -> Int
getAge (Person _ a _) = a

{-
 data AlgebraicDataType = Constructor1 Type11 Type 12
                        | Constructor2 Type21
                        | Constructor3 Type31 Type32 Type33 Type34
                        | Constructor4
 -}

baz :: Person -> String
baz p@(Person n _ _) = "the name is" ++ show p ++ " with " ++ n

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ "🫠"
checkFav (Person n _ _) = n ++ "🫠🫠🫠"

ex03 = case "Hello" of
  [] -> 3
  ('H' : s) -> length s
  _ -> 7

data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x l) = x * intListProd l

ex04 :: IntList
ex04 = Cons 7 (Cons 8 (Cons 9 Empty))

data BTree
  = Leaf Char
  | Node BTree Int BTree
  deriving (Show)

btree = Node (Leaf 'a') 1 (Node (Node (Leaf 'b') 2 (Leaf 'c')) 3 (Leaf 'd'))

showBTree (Leaf c) = [c]
showBTree (Node l n r) = showBTree l ++ "--" ++ show n ++ "--" ++ showBTree r

main = do
  print $ baz brent
  print $ checkFav brent
  print $ checkFav stan
  print $ intListProd ex04
  print $ showBTree btree

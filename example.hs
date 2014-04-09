data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving(Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)


data Person = Person { 
                firstName :: String,
                lastName :: String,
                age :: Int,
                height :: Float,
                phoneNumber :: String,
                flavor :: String
                } deriving (Show)
-- By using this record syntax, haskell automatically creates these functions:
-- firstName, lastName, age, height, phoneNumber, and flavor


data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- These define some functions for the Vector type

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Being part of Show and Read gives us:
    -- show Wednesday ---> "Wednesday"
    -- read "Saturday" ---> Saturday
-- Eq and Ord give:
    -- Saturday == Sunday ---> False
    -- Saturday > Friday ---> True
-- Bounded gives:
    -- minBound :: Day ---> Monday
    -- maxBound :: Day ---> Sunday
-- Enum gives:
    -- succ Monday ---> Tuesday
    -- pred Saturday ---> Friday
    -- [Thursday .. Sunday] ---> [Thursday, Friday, Saturday, Sunday]
    -- [minBound .. maxBound] :: [Day] ---> [Monday,  Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
    
-- Simple Binary Tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

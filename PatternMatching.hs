import Data.List

myNot True  = False
myNot False = True

sumList (x:xs) = x + sumList xs
sumList []     = 0
third (a, b, c) = c
complicated (True, a, x:xs, 5) = (a, xs)

myLen (x:xs) = 1 + myLen xs
myLen [] = 0

myRev :: [a] -> [a]
myRev (x:xs) = myRev xs ++ [x]
myRev [] = []

palindrome :: [a] -> [a]
palindrome [] = []
palindrome (x:xs) = [x] ++ palindrome xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome ls = ls == reverse ls

data Tree a = Node (Maybe (a, Tree a, Tree a)) deriving (Show)

insertNode val (Node Nothing) = Node (Just (val, Node Nothing, Node Nothing))
insertNode val tree@(Node (Just (nodeVal, left, right)))
 | val < nodeVal = Node (Just (nodeVal, insertNode val left, right))
 | val > nodeVal = Node (Just (nodeVal, left, insertNode val right))
 | otherwise = tree
 
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x (Node Nothing) = False
treeElem x (Node (Just (a, left, right)))
 | x == a = True
 | x < a = treeElem x left
 | x > a = treeElem x right
 
nums = [8,6,4,1,7,3,5]
numsTree = foldr insertNode (Node Nothing) nums
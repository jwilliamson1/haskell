-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String] 
  deriving (Show)

data MagazineInfo = Magazine Int String [String] 
  deriving (Show)

myBook = Book 3 "turds" ["lewis"]
myMag = Magazine 3 "turds" ["lewis"]
type CustomerID = Int
type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody
  deriving (Show)

myReview = BetterReview myBook 1 "Arg this book sucks a big fat sag penor!"

type CardHolder = String
type CardNumber = String
type Address = [String]

bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

nicerID      (Book id _     _      ) = id
nicerTitle   (Book _  title _      ) = title
nicerAuthors (Book _  _     authors) = authors

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList :: (List a) -> [a]
toList (Cons x xs) = x : toList xs
toList Nil = []

aConsForm = fromList [Just True, Nothing, Just False]

data MyTree a = Node (Maybe (a, MyTree a, MyTree a))
  deriving (Show)

aNilList = toList(Nil)
aMonoList = toList(Cons 1 Nil)   
aMultiList = toList(Cons 2 (Cons 1 Nil ))

empty = Node Nothing
left_node = Node (Just("left_node", empty, empty))
right_node = Node (Just("right_node", empty, empty))
parent = Node (Just("parent", left_node, right_node))

mySecond :: [a] -> a

mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)

safeSecond :: [a] -> Maybe a

safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))

tidySecond :: [a] -> Maybe a

tidySecond (_:x:_) = Just x
tidySecond _       = Nothing
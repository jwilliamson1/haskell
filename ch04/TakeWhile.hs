myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs)
  | res == True = x: myTakeWhile p xs
  | otherwise = []
  where res = p x

myTakeWhile_foldr :: (a -> Bool) -> [a] -> [a]
myTakeWhile_foldr p xs = foldr step [] xs
                        where step a b = if p a then a : b else []
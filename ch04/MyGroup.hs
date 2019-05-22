myGroup :: (Eq a) => [a] -> [[a]]
myGroup = foldl appendToListOrNewGroup []

appendToListOrNewGroup :: (Eq a) => [[a]] -> a -> [[a]]
appendToListOrNewGroup [] e = [e] : []
appendToListOrNewGroup (x:xs) e | e == head x = (e:x) : xs
                                | otherwise = x : appendToListOrNewGroup xs e
--e is element from outer list
-- groupings are accumulated list of lists from outer list
-- grouping is a single list that can be null or contain a list of a single value
-- a will be the head of the list
-- acc will be the update list with the value either appended as a new list or added to an existing list
appendToListOrNewGroupFoldl :: (Eq a) => [[a]] -> a -> [[a]]
appendToListOrNewGroupFoldl groupings e = foldr step [] groupings
                                            where step grouping [] = [[e]]
                                                  step grouping z = if e == head grouping then (e:grouping):z else if z == [] then (grouping:[e]:z) else (grouping:z)

myGroupl :: (Eq a) => [a] -> [[a]]
myGroupl l = foldl appendToListOrNewGroupFoldl [] l

group' :: Eq a => [a] -> [[a]]
group' = foldr f []
  where f x []        = [[x]]
        f x (ys@(y:_):yss)
          | x == y     = (x:ys):yss
          | otherwise = [x]:ys:yss
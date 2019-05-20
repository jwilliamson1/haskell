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
appendToListOrNewGroupFoldl groupings e = foldr step [[e]] groupings
                                            where step [] z = z      
                                                  step grouping z = if e == head grouping then (e:grouping) : init z else (grouping:z)
                                                  

myGroupl :: (Eq a) => [a] -> [[a]]
myGroupl l = foldl appendToListOrNewGroupFoldl [] l
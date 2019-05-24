any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr ((||) . p) False
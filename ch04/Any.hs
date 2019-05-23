any' :: (a -> Bool) -> [a] -> Bool
any' p xs = foldr step False xs
            where step a b = p a || b
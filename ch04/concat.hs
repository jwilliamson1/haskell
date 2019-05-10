myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs
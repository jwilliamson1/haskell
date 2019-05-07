splitWith :: (a -> Bool) -> [a] -> [[a]]

splitWith f [] = []
splitWith f x = 
  let (first, rest) = break f (dropWhile f x)
    in case first of 
       [] -> []
       xs -> xs: case rest of
                [] -> []
                ys -> splitWith f ys 
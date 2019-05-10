import Data.Char (digitToInt, isDigit)
import Data.List(foldl')

asInt :: String -> Int
asInt [] = 0
asInt (x:xs) = if x == '-' then negate (numericPart xs) else numericPart (x:xs)
    where numericPart [] =  0
          numericPart (x:xs) = foldl' convert 0 (x:xs)
                                where convert acc x = acc * 10 + digitToInt x
               
asInt_Either :: String -> Either String Int
asInt_Either [] = Left "empty string"
asInt_Either ('-':[]) = Left "negative (-) with no number"
asInt_Either ('-':xs) = Left "negative number"
asInt_Either (x:xs) = numericPart (x:xs)
    where numericPart (x:xs) = foldl' convert (Right 0) (x:xs)
                                where convert (Right acc) x = if isDigit(x) then Right (acc * 10 + digitToInt x) else Left ("'" ++ [x] ++ "' is not a digit")
                                      convert (Left e) _ = (Left e)
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = fixLines

splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
  in pre : case suf of
    ('\r':'\n':rest) -> splitLines rest
    ('\r':rest)      -> splitLines rest
    ('\n':rest)     -> splitLines rest
    _                -> []

isLineTerminator c = c == '\r' || c == '\n'

fixLines :: String -> String
fixLines = unlines . transposePairs . splitLines

firstOfLine :: String -> String
firstOfLine = fst . break isSpace . dropWhile isSpace
    where isSpace = (==' ')

firstsOfLines :: [String] -> [String]
firstsOfLines =  map firstOfLine . filter (\str -> str /= [])

transposePairs :: [[Char]] -> [[Char]]
tranposePairs [] = []
transposePairs [x] = []
transposePairs [x, y] = transpose x y
transposePairs (x:y:ps) = (transpose x y) ++ (transposePairs ps)

transpose :: [Char] -> [Char] -> [[Char]]
transpose [] [] = []
transpose a [] = [a]
transpose [] b = [b]
transpose (a:as) (b:bs) = (a:b:[]) : transpose as bs
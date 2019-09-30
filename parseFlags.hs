import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

themselvesTimes :: [Int] -> [Int]
themselvesTimes xs = xs >>= \n -> replicate n n

type Input = String
type Output = Maybe String
type Directory = Maybe String
data Flags = Input String | Output (Maybe String) | Directory (Maybe String) deriving (Show)

hyphen :: Parser Char
hyphen = oneOf "-"

flags :: Parser Char
flags = oneOf "iod"

readExpr :: String -> String
readExpr input = case parse parseFlags "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (readExpr expr)

spaces :: Parser ()
spaces = skipMany space

parseFlags :: Parser Flags
parseFlags = do
  first <- hyphen
  flag <- flags  
  let flag = first:flag
  return $ case flag of
    "i" -> Input "input"
    "d" -> Directory $ Just("directory")
    "o" -> Output $ Just("output")
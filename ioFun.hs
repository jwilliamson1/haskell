import qualified Data.Text as T
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  	
    return(T.toUpper(name))
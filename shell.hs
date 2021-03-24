import Calc
import Function
import System.IO
prompt :: IO ()
prompt = putStr "Topos> " >> hFlush stdout

shell :: [Bind] -> IO ()
shell binds = do
    prompt
    str <- getLine
    case _eval binds $ toExp str of
        Left s -> do
            putStrLn $ "Error: " ++ s
            shell binds
        Right (expr, binds2) -> do
            putStrLn $ concat $ _fromExp expr
            shell binds2

main = do
    shell []

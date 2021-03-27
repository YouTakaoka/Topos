import Calc
import Function
import System.IO
prompt :: IO ()
prompt = putStr "Topos> " >> hFlush stdout

shell :: [Bind] -> IO ()
shell binds = do
    prompt
    str <- getLine
    case _eval binds (toExp str) of
        Left s -> do putStrLn s
        Right ((res: []), binds2) -> do
            print res
            shell binds2
        Right ([], binds2) -> do
            shell binds2
        Right (expr, _) -> do
            putStrLn $ "Parse error: " ++ (show expr)
            shell binds

main = do
    shell []

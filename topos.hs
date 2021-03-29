import Calc
import Function
import System.Environment (getArgs)

interpret :: [String] -> [Bind] -> IO ()
interpret (str: rest) binds =
    case _eval binds (toExp str) of
        Left s -> do putStrLn s
        Right ((Print res: []), binds2) -> do
            putStrLn res
            interpret rest binds2
        Right ((Err e: []), _) -> do
            putStrLn $ "Error: " ++ e
        Right ((_: []), binds2) -> do
            interpret rest binds2
        Right ([], binds2) -> do
            interpret rest binds2
        Right (expr, _) -> do
            putStrLn $ "Parse error: " ++ (show expr)
            return ()
interpret [] _ = return ()

main = do
    (file_name : _) <- getArgs
    cs <- readFile file_name
    let ls = lines cs
    interpret ls []

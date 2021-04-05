import Calc
import Function
import Shell
import System.Environment (getArgs)

interpret :: [String] -> [Bind] -> IO ()
interpret (str: rest) binds =
    case _eval Normal binds (toExp str) of
        (Err s, _) -> do
            putStrLn s
        (Print res, binds2) -> do
            putStrLn res
            interpret rest binds2
        (_, binds2) -> do
            interpret rest binds2
interpret [] _ = return ()

main = do
    args <- getArgs
    case args of
        [] -> do
            shell []
        (file_name : _) -> do
            cs <- readFile file_name
            let ls = lines cs
            interpret ls []

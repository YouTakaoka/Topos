import Eval
import Types
import Utils
import Shell
import System.Environment (getArgs)
import System.IO

interpret :: Int -> [String] -> [Bind] -> IO ()
interpret cnt (str: rest) binds =
    case _eval M_Normal binds (toExp str) of
        Error e -> do
            hPutStrLn stderr $ "Line " ++ show cnt ++ ": " ++ show e
        Result (Print res, binds2) -> do
            putStrLn res
            interpret (cnt + 1) rest binds2
        Result (_, binds2) -> do
            interpret (cnt + 1) rest binds2
interpret _ [] _ = return ()

main = do
    args <- getArgs
    case args of
        [] -> do
            shell []
        (file_name : _) -> do
            cs <- readFile file_name
            let ls = lines cs
            interpret 1 ls []

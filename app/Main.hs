import Eval
import Types
import Utils
import Shell
import System.Environment (getArgs)
import System.IO

interpret :: Int -> [String] -> [Bind] -> IO ()
interpret _ [] _ = return ()
interpret cnt strs binds =
    let (cnt2, expr, rest) = getExpression cnt strs
    in case _eval M_Normal binds expr of
        Error e -> do
            hPutStrLn stderr $ "Line " ++ show cnt2 ++ ": " ++ show e
        Result (Print res, binds2) -> do
            putStrLn res
            interpret (cnt2 + 1) rest binds2
        Result (_, binds2) -> do
            interpret (cnt2 + 1) rest binds2

main = do
    args <- getArgs
    case args of
        [] -> do
            shell []
        (file_name : _) -> do
            cs <- readFile file_name
            let ls = lines cs
            interpret 1 ls []

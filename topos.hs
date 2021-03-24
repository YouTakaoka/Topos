import Calc
import Function
import System.Environment (getArgs)

interpret :: [String] -> [Bind] -> IO ()
interpret (str: rest) binds =
    case _eval binds (toExp str) of
        Left s -> do print s
        Right ((Print res: _), binds2) -> do
            print res
            interpret rest binds2
        Right (_, binds2) -> do
            interpret rest binds2
        _ -> return ()
interpret [] _ = return ()

main = do
    (file_name : _) <- getArgs
    cs <- readFile file_name
    let ls = lines cs
    interpret ls []

module Shell where
import Types
import Eval
import Utils
import System.IO
prompt :: IO ()
prompt = putStr "Topos> " >> hFlush stdout

shell :: [Bind] -> IO ()
shell binds = do
    prompt
    str <- getLine
    case _eval M_Normal binds (toExp str) of
        (Err e, binds) -> do
            putStrLn e
            shell binds
        (Null, binds2) -> do
            shell binds2
        (res, binds2) -> do
            print res
            shell binds2
        
module Shell where
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
        (Err e, binds) -> do
            putStrLn e
            shell binds
        (Null, binds2) -> do
            shell binds2
        (res, binds2) -> do
            print res
            shell binds2
        
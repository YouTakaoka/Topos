module Shell where
import Types
import Eval
import Utils
import System.IO
prompt :: IO ()
prompt = putStr "Topos> " >> hFlush stdout

getExpression :: IO Exp
getExpression = do
    prompt
    str <- getLine
    let expr = toExp str
    if null expr
        then return expr
        else
            case last expr of
                Tobe "\\" ->
                    (init expr ++) <$> getExpression
                _ -> return expr    

shell :: [Bind] -> IO ()
shell binds = do
    expr <- getExpression
    case _eval M_Normal binds expr of
        Error e -> do
            putStrLn $ "Error: " ++ show e
            shell binds
        Result (Null, binds2) -> do
            shell binds2
        Result (res, binds2) -> do
            print res
            shell binds2
        
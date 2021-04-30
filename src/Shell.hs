module Shell where
import Types
import Eval
import Utils
import System.IO
import System.Console.Haskeline

getExpression :: InputT IO Exp
getExpression = do
    maybeLine <- getInputLine "Topos> "
    case maybeLine of
        Nothing     -> return [EOF] -- EOF / control-d
        Just "exit" -> return [EOF]
        Just line -> do
            let expr = toExp line
            if null expr
                then return expr
                else
                    case last expr of
                        Tobe "\\" ->
                            (init expr ++) <$> getExpression
                        _ -> return expr    

mySettings :: Settings IO
mySettings = defaultSettings { historyFile = Just ".topos.hist" }

shell :: [Bind] -> IO ()
shell binds = do
    expr <- runInputT mySettings getExpression
    case _eval M_Normal binds expr of
        Error e -> do
            putStrLn $ "Error: " ++ show e
            shell binds
        Result (EOF, _) -> return ()
        Result (Null, binds2) -> do
            shell binds2
        Result (res, binds2) -> do
            print res
            shell binds2
        
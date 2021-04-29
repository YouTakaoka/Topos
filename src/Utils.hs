module Utils where
import Parser
import Types
import Ops
import Debug.Trace
import Data.List

_traceShow :: Show a => a -> a
_traceShow x = trace (show x) x

_traceIf :: Show a => Bool -> String -> a -> a
_traceIf b s x = if b then trace s x else x

smbls = ["(", ")", "[", "]", "{", "}", "->", "<", ">", ",", ":", "==", "!=", "&&", "||", "!", "+", "-", "*", "//", "/", "%", "$", "=", "#", "|"]

_isInitialSym :: String -> String -> Maybe String
_isInitialSym str sym
    | take l str == sym = Just (drop l str)
    | otherwise = Nothing
    where l = length sym

_initialSym :: String -> [String] -> Maybe (String, String)
_initialSym str [] = Nothing
_initialSym str (sym : syms) =
    case _isInitialSym str sym of
        Just rest -> Just (sym, rest)
        Nothing -> _initialSym str syms

_divStrBySyms :: [String] -> String -> String -> Exp
_divStrBySyms syms wrk inp =  --wrkの初期値は空
    case _initialSym inp syms of
        Nothing ->
            case inp of
                [] -> []
                [c] -> [Tobe (wrk ++ [c])]
                (c : rest) -> _divStrBySyms syms (wrk ++ [c]) rest
        Just (sym, rest) ->
            ([Tobe wrk | not (wrk == "")])
                ++ [Tobe sym] ++ _divStrBySyms syms [] rest

_toExp0 :: String -> Exp
_toExp0 str = concatMap (_divStrBySyms smbls []) (words str)

_toExp :: Char -> String -> Bool -> Exp -> Exp
_toExp q str inq expr =  -- inq: 引用符の中にいるかどうかのフラグ．初期状態でfalse
    case divListBy q str of
        Nothing -> expr ++ _toExp0 str
        Just (_, str1, str2)
            | inq -> _toExp q str2 (not inq) $ expr ++ [Str str1]
            | otherwise -> _toExp q str2 (not inq) $ expr ++ _toExp0 str1

_removeComment :: Exp -> Exp
_removeComment expr =
    case divListBy (Tobe "#") expr of
        Nothing -> expr
        Just (_, expr1, expr2) -> expr1

toExp :: String -> Exp
toExp str = _removeComment $ _toExp '"' str False []

_fromExp :: Exp -> [String]
_fromExp = map show

_subst :: Exp -> Wrd -> Wrd -> Exp
_subst ws target sbst =
    case divListBy target ws of
        Nothing -> ws
        Just (_, ws1, ws2) -> _subst (ws1 ++ [sbst] ++ ws2) target sbst

_mulSubst :: Exp -> [Bind] -> Exp
_mulSubst ws (Bind { identifier = target, value = sbst } : sbsts) =
    _mulSubst (_subst ws (Tobe target) sbst) sbsts
_mulSubst ws [] = ws

_toType :: Wrd -> Either Error Type
_toType (Tobe "String") = Right T_String
_toType (Tobe "Int") = Right T_Int
_toType (Tobe "Double") = Right T_Double
_toType (Tobe "Bool") = Right T_Bool
_toType (Tobe s) = Left $ UnknownKeywordError s
_toType (Type t) = Right t

toType :: Exp -> Either Error Type
toType [w] = _toType w
toType [Tobe "List", w] =
    case _toType w of
        Left e -> Left e
        Right t -> Right $ T_List t

_isListType :: Type -> Bool
_isListType (T_List _) = True
_isListType _ = False

checkBinSig :: (Type, Type) -> BinSig -> Maybe (Either Type Type)
checkBinSig (t1, t2) (t1', t2', t3') =
    case _typeCheck [] t1 t1' of
        Nothing -> Nothing
        Just binds ->
            case _typeCheck binds t2 t2' of
                Nothing -> Just $ Left $ _typeSub binds t2'
                Just binds2 -> Just $ Right $ _typeSub binds2 t3'

matchBinSig :: (Type, Type) -> [BinSig] -> Either [Type] Type
matchBinSig _ [] = Left []
matchBinSig (t1, t2) ((t1', t2', t3'): binsigs) =
    case checkBinSig (t1, t2) (t1', t2', t3') of
        Nothing -> matchBinSig (t1, t2) binsigs
        Just (Right t) -> Right t
        Just (Left t) ->
            case matchBinSig (t1, t2) binsigs of
                Right t' -> Right t'
                Left ts -> Left (t:ts)

selectBinSigFirstUnique :: [BinSig] -> [Type]
selectBinSigFirstUnique binsigs =
    delete T_Null $ nub $ map (\ (x, _, _) -> x) binsigs

validateBinSig :: (Type, Type) -> [BinSig] -> Either Error Type
validateBinSig (t1, t2) binsigs =
    case matchBinSig (t1, t2) binsigs of
        Left [] ->
            let ts = selectBinSigFirstUnique binsigs
            in Left TypeError { expected_types=ts, got_type=t1,
                    message_TE="Type mismatch at the first argument. Expected: " ++ show ts ++ ", but got: " ++ show t1 }
        Left ts ->
            Left TypeError { expected_types=ts, got_type=t2,
                message_TE="Type mismatch at the second argument. Expected: " ++ show ts ++ ", but got: " ++ show t2 }
        Right t -> Right t

matchUnSig :: Type -> [UnSig] -> Either [Type] Type
matchUnSig _ [] = Left []
matchUnSig t1 ((t1', t2'): unsigs) =
    case _typeCheck [] t1 t1' of
        Nothing -> 
            case matchUnSig t1 unsigs of
                Right t -> Right t
                Left ts -> Left (t1':ts)
        Just binds -> Right $ _typeSub binds t2'

validateUnSig :: Type -> [UnSig] -> Either Error Type
validateUnSig t1 unsigs =
    case matchUnSig t1 unsigs of
        Right t -> Right t
        Left ts -> Left TypeError { expected_types=ts, got_type=t1,
                        message_TE="Type mismatch at the argument. Expected: " ++ show ts ++ ", but got: " ++ show t1 }

matchFuncSig :: [Bind] -> [(Type, Type)] -> Either (Type, Type, Int) [Bind] -- Intは残りのリストの長さ
matchFuncSig binds [] = Right binds
matchFuncSig binds ((t1, t2): tps) =
    case _typeCheck binds t1 t2 of
        Nothing -> Left (t1, _typeSub binds t2, length tps)
        Just binds2 -> matchFuncSig binds2 tps

baseNum :: Int -> String
baseNum 1 = "first"
baseNum 2 = "second"
baseNum 3 = "third"
baseNum 4 = "fourth"
baseNum 5 = "fifth"
baseNum n = show n ++ "th"

validateFuncSig :: [Type] -> [Type] -> Type -> Either Error Type
validateFuncSig ts1 ts2 t =
    case matchFuncSig [] (zip ts1 ts2) of
        Right binds -> Right $ _typeSub binds t
        Left (t1, t2, n_rest) ->
            let n = length ts1 - n_rest
            in Left $ TypeError { expected_types=[t2], got_type=t1,
                message_TE="Type mismatch at the " ++ baseNum n ++ " argument. Expected: " ++ show t2 ++ ", but got: " ++ show t1 }

addToTypeErrorMessage :: Error -> String -> Error
addToTypeErrorMessage te str =
    TypeError { expected_types=expected_types te, got_type=got_type te,
                            message_TE=str ++ message_TE te }

_macroGen :: Function -> Exp -> Exp
_macroGen Function { args = as, ret_t = _, ret = expr } arguments =
    let binds = map (\ ((t, id), val) -> Bind { identifier = id, value = val, vtype = t }) $ zip as arguments
    in _mulSubst expr binds

_deleteAll :: Wrd -> Exp -> Exp
_deleteAll w expr =
    case divListBy w expr of
        Nothing -> expr
        Just (_, expr1, expr2) -> expr1 ++ _deleteAll w expr2

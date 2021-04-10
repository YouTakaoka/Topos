module Utils where
import Parser
import Types
import Ops
import Debug.Trace

_traceShow :: Show a => a -> a
_traceShow x = trace (show x) x

_traceIf :: Show a => Bool -> String -> a -> a
_traceIf b s x = if b then trace s x else x

smbls = ["(", ")", "[", "]", "{", "}", "->", "<", ">", ",", ":", "==", "!=", "&&", "||", "!", "+", "-", "*", "/", "=", "#", "|"]

_isInitialSym :: String -> String -> Maybe String
_isInitialSym str sym
    | (take l str) == sym = Just (drop l str)
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
                (c : []) -> [Tobe (wrk ++ [c])]
                (c : rest) -> _divStrBySyms syms (wrk ++ [c]) rest
        Just (sym, rest) ->
            (if wrk == "" then [] else [Tobe wrk])
                ++ [Tobe sym] ++ _divStrBySyms syms [] rest

_toExp0 :: String -> Exp
_toExp0 str = concat $ map (_divStrBySyms smbls []) $ words str

_toExp :: Char -> String -> Bool -> Exp -> Exp
_toExp q str inq expr =  -- inq: 引用符の中にいるかどうかのフラグ．初期状態でfalse
    case divListBy q str of
        Nothing -> expr ++ (_toExp0 str)
        Just (_, str1, str2)
            | inq -> _toExp q str2 (not inq) $ expr ++ [Str str1]
            | otherwise -> _toExp q str2 (not inq) $ expr ++ (_toExp0 str1)

toExp :: String -> Exp
toExp str = _toExp '"' str False []

_fromExp :: Exp -> [String]
_fromExp expr = map show expr

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

_typeCheck :: EvalMode -> [Bind] -> Maybe Error -- NothingならOK，Justはエラー
_typeCheck mode [] = Nothing
_typeCheck mode (Bind { identifier = id, value = v, vtype = t } : binds)
    | actype <= t = _typeCheck mode binds
    | otherwise = Just $ TypeError t actype $
                    "Type mismatch of variable `" ++ id ++ "`. Expected type is `" ++ (show t) ++ "` but input type is `" ++ (show actype) ++ "`."
    where
        actype = case mode of
                    M_Normal -> _getType v
                    M_TypeCheck -> (\ (TypeCheck tp) -> tp) v

_macroGen :: Function -> (Exp -> Either Error Exp)
_macroGen (Function { args = as, ret_t = _, ret = expr }) =
    \ arguments ->
        case length as == length arguments of
            False -> Left $ InternalError "Number of arguments of function does not match."
            True ->
                let binds = map (\ ((t, id), val) -> Bind { identifier = id, value = val, vtype = t }) $ zip as arguments
                in case _typeCheck M_Normal binds of
                    Nothing -> Right $ _mulSubst expr binds
                    Just e -> Left e

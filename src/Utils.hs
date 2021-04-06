module Utils where
import Parser
import Debug.Trace

data Type = T_Int | T_Double | T_Bool | T_String | T_Func | T_BinaryOp | T_UnaryOp | T_FunctionOp | T_PreList | T_List Type | T_EmptyList | T_Tuple [Type] | T_Print | T_Function { args_t :: [Type], return_t :: Type } | T_Unknown | T_Type | T_Error | T_TypeCheck deriving (Eq, Show)

instance Ord Type where
    (<=) T_EmptyList (T_List _) = True
    (<=) T_BinaryOp T_Func = True
    (<=) T_UnaryOp T_Func = True
    (<=) T_FunctionOp T_Func = True
    (<=) (T_Function {}) T_Func = True
    (<=) t1 t2 = t1 == t2

type BinaryOp = Wrd -> Wrd -> Wrd
type UnaryOp = Wrd -> Wrd
type FunctionOp = (Int, Exp -> Wrd)
data Op = BinOp BinaryOp | UnOp UnaryOp | FuncOp FunctionOp
instance Show Op where
    show (BinOp _) = "[BinOp]"
    show (UnOp _) = "[UnOp]"
    show (FuncOp _) = "[FuncOp]"
type StrOp = (String, Op)

data Function = Function { args :: [(Type, String)], ret_t :: Type, ret :: Exp } deriving (Show, Eq)
data Func = Fun Function | Operator StrOp deriving Show
data Bind = Bind { identifier :: String, value :: Wrd, vtype :: Type } deriving (Eq, Show)
data Wrd = Str String | Func Func | Bnd Bind | Print String | Tobe String | Double Double | Int Int | Bool Bool | Null | List Exp | ToEval Exp | Err String | Pair (Wrd, Wrd) | PreList [Exp] | Type Type | Contents Exp | Tuple Exp | TypeCheck Type
instance Eq Wrd where
    (==) (Str a) (Str b) = a == b
    (==) (Func (Operator (a, _))) (Func (Operator (b, _))) = a == b
    (==) (Bnd bind1) (Bnd bind2) = bind1 == bind2
    (==) (Tobe a) (Tobe b) = a == b
    (==) (Double a) (Double b) = a == b
    (==) (Int a) (Int b) = a == b
    (==) (Bool a) (Bool b) = a == b
    (==) (Tuple a) (Tuple b) = a == b
    (==) (List l1) (List l2) = l1 == l2
    (==) _ _ = False
instance Show Wrd where
    show (Str s) = s
    show (Func (Operator (s, _))) = "[Operator:" ++ s ++ "]"
    show (Func (Fun f)) = show f 
    show (Bnd bind) = show bind
    show (Print p) = p
    show (Tobe s) = s
    show (Double n) = show n
    show (Int n) = show n
    show (Bool b) = show b
    show Null = ""
    show (Err s) = s
    show (List l) = show l
    show (Pair t) = show t
    show (Tuple a) = show a
    show (ToEval _) = "[ToEval]"

type Exp = [Wrd]
data EvalMode = M_Normal | M_TypeCheck

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

_toType :: Wrd -> Type
_toType (Tobe "String") = T_String
_toType (Tobe "Int") = T_Int
_toType (Tobe "Double") = T_Double
_toType (Tobe "Bool") = T_Bool
_toType (Tobe "UnaryOp") = T_UnaryOp
_toType (Tobe "BinaryOp") = T_BinaryOp
_toType (Tobe "FunctionOp") = T_FunctionOp
_toType (Type t) = t

toType :: Exp -> Type
toType (w: []) = _toType w
toType (Tobe "List": (w: [])) = T_List $ _toType w

_getType :: Wrd -> Type
_getType (Str _) = T_String
_getType (Int _) = T_Int
_getType (Double _) = T_Double
_getType (Bool _) = T_Bool
_getType (Tobe _) = T_Unknown
_getType (Err _) = T_Error
_getType (List (w: _)) = T_List $ _getType w
_getType (List []) = T_EmptyList
_getType (Tuple tp) = T_Tuple $ map _getType tp
_getType (Func (Operator (_, UnOp _))) = T_UnaryOp
_getType (Func (Operator (_, BinOp _))) = T_BinaryOp
_getType (Func (Operator (_, FuncOp _))) = T_FunctionOp
_getType (Func (Fun (Function { args = as, ret_t = rt, ret = _ }))) =
    let ast = map (\ (t, a) -> t) as
    in T_Function { args_t = ast, return_t = rt }
_getType (Type _) = T_Type

_isListType :: Type -> Bool
_isListType (T_List _) = True 
_isListType _ = False

_typeCheck :: [Bind] -> Maybe String -- NothingならOK，Justはエラー
_typeCheck [] = Nothing
_typeCheck (Bind { identifier = id, value = v, vtype = t } : binds)
    | actype <= t = _typeCheck binds
    | otherwise = Just $ "Type mismatch of variable `" ++ id ++ "`. Expected type is `" ++ (show t) ++ "` but input type is `" ++ (show actype) ++ "`."
    where
        actype = _getType v

_typeCheck_TC :: [Bind] -> Maybe String
_typeCheck_TC [] = Nothing
_typeCheck_TC (Bind { identifier = id, value = (TypeCheck actype), vtype = t} : binds)
    | actype <= t = _typeCheck_TC binds
    | otherwise = Just $ "Type mismatch of variable `" ++ id ++ "`. Expected type is `" ++ (show t) ++ "` but input type is `" ++ (show actype) ++ "`."

_isConsistentType :: Exp -> Maybe Type
_isConsistentType (w: []) = Just $ _getType w
_isConsistentType (w: rest) =
    case _isConsistentType rest of
    Nothing -> Nothing
    Just t -> if _getType w == t then Just t else Nothing

isConsistentType :: Exp -> Bool
isConsistentType expr = _isConsistentType expr /= Nothing

toList :: Exp -> Wrd
toList expr =
    if isConsistentType expr then List expr else Err $ "List: Inconsistent type: " ++ (show expr)

isFunctionType :: Wrd -> Bool
isFunctionType (Type (T_Function {})) = True
isFunctionType (Type T_FunctionOp) = True
isFunctionType _ = False

_macroGen :: Function -> (Exp -> Either String Exp)
_macroGen (Function { args = as, ret_t = _, ret = expr }) =
    \ arguments ->
        case length as == length arguments of
            False -> Left "Number of arguments of function does not match."
            True ->
                let binds = map (\ ((t, id), val) -> Bind { identifier = id, value = val, vtype = t }) $ zip as arguments
                in case _typeCheck binds of
                    Nothing -> Right $ _mulSubst expr binds
                    Just s -> Left s

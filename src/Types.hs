module Types where

import Parser
import Data.List

data Type = T_Int 
        | T_Double 
        | T_Bool 
        | T_String 
        | T_Func T_Function 
        | T_UnaryOp 
        | T_BinaryOp 
        | T_FunctionOp 
        | T_PreList 
        | T_List Type 
        | T_EmptyList 
        | T_Tuple [Type] 
        | T_Print 
        | T_Unknown 
        | T_Type 
        | T_Error 
        | T_TypeCheck 
        | T_ToEval 
        | T_Num 
        | T_Additive 
        | T_Ord 
        | T_Eq
        | T_Null 
        | T_Any
        | T_Printable
        | T_TypeVar Type String
        deriving (Eq, Show)

typeEq :: Type -> Type -> Bool
typeEq T_Unknown _ = False
typeEq _ T_Unknown = False
typeEq t1 t2 = t1 == t2

_typeCheck :: [Bind] -> Type -> Type -> Maybe [Bind]
_typeCheck binds T_EmptyList (T_List _) = Just binds
_typeCheck binds _ T_Any = Just binds
_typeCheck binds T_Int T_Printable = Just binds
_typeCheck binds T_Bool T_Printable = Just binds
_typeCheck binds T_Double T_Printable = Just binds
_typeCheck binds T_String T_Printable = Just binds
_typeCheck binds (T_List a) T_Printable = _typeCheck binds a T_Printable
_typeCheck binds (T_Tuple []) T_Printable = Just binds
_typeCheck binds (T_Tuple (w: ws)) T_Printable =
    case _typeCheck binds w T_Printable of
        Just binds2 -> _typeCheck binds2 (T_Tuple ws) T_Printable
        Nothing -> Nothing
_typeCheck binds T_Int T_Num = Just binds
_typeCheck binds T_Double T_Num = Just binds
_typeCheck binds T_Int T_Additive = Just binds
_typeCheck binds T_Double T_Additive = Just binds
_typeCheck binds T_String T_Additive = Just binds
_typeCheck binds (T_List _) T_Additive = Just binds
_typeCheck binds T_Int T_Ord = Just binds
_typeCheck binds T_Double T_Ord = Just binds
_typeCheck binds T_Int T_Eq = Just binds
_typeCheck binds T_Double T_Eq = Just binds
_typeCheck binds T_Bool T_Eq = Just binds
_typeCheck binds T_String T_Eq = Just binds
_typeCheck binds T_Num T_Eq = Just binds
_typeCheck binds (T_List t) T_Eq = _typeCheck binds t T_Eq
_typeCheck binds (T_Tuple ls) T_Eq = foldl (\ x y ->
    case x of
        Nothing -> Nothing
        Just bs1 ->
            case y of
                Nothing -> Nothing
                Just bs2 -> Just $ bs1 ++ bs2)
    (Just []) $ map (\ x -> _typeCheck binds x T_Eq) ls
_typeCheck binds t1 (T_TypeVar t2 str) =
    case divList (\ b -> identifier b == str) binds of
        Nothing ->
            case _typeCheck binds t1 t2 of
                Nothing -> Nothing
                Just _ -> Just (Bind { identifier=str, vtype=T_Type, value=Type t1 } : binds)
        Just (b, _, _) ->
            _typeCheck binds t1 $ (\ (Type x) -> x) $ value b
_typeCheck binds (T_List t1) (T_List t2) = _typeCheck binds t1 t2
_typeCheck binds (T_Tuple []) (T_Tuple []) = Just binds
_typeCheck binds (T_Tuple []) _ = Nothing
_typeCheck binds _ (T_Tuple []) = Nothing
_typeCheck binds (T_Tuple (t1: ls1)) (T_Tuple (t2: ls2)) =
    case _typeCheck binds t1 t2 of
        Nothing -> Nothing
        Just binds2 -> _typeCheck binds2 (T_Tuple ls1) (T_Tuple ls2)
_typeCheck binds (T_Func T_Function { args_t=as1, return_t=rt1 }) (T_Func T_Function { args_t=as2, return_t=rt2 }) =
    case _typeCheck binds (T_Tuple as1) (T_Tuple as2) of
        Nothing -> Nothing
        Just binds2 -> _typeCheck binds2 rt1 rt2
_typeCheck binds t1 t2 = if typeEq t1 t2 then Just binds else Nothing

_typeSub :: [Bind] -> Type -> Type
_typeSub binds (T_TypeVar t str) =
    case divList (\ b -> identifier b == str) binds of
        Nothing -> T_TypeVar t str
        Just (b, _, _) -> (\ (Type x) -> x) $ value b
_typeSub binds (T_List t) = T_List $ _typeSub binds t
_typeSub binds (T_Tuple ls) = T_Tuple $ map (_typeSub binds) ls
_typeSub binds (T_Func T_Function { funcName_t=fname, args_t=as, return_t=rt, priority_ft=prt }) =
    T_Func T_Function { funcName_t=fname, args_t=map (_typeSub binds) as, return_t=_typeSub binds rt, priority_ft=prt }
_typeSub _ t = t

type BinaryOp = Wrd -> Wrd -> Either Error Wrd
type UnaryOp = Wrd -> Either Error Wrd
type FunctionOp = (Exp -> Either Error Wrd)

type BinSig = (Type, Type, Type)
type UnSig = (Type, Type)
type FuncSig = ([Type], Type)
data OperatorSig = BinSig [BinSig] | UnSig [UnSig] | FuncSig FuncSig deriving Show

data Op = BinOp (BinaryOp, [BinSig])
        | UnOp (UnaryOp, [UnSig])
        | FuncOp (FunctionOp, FuncSig)
instance Show Op where
    show (BinOp _) = "[BinOp]"
    show (UnOp _) = "[UnOp]"
    show (FuncOp _) = "[FuncOp]"

data Function = Function { funcName :: String, args :: [(Type, String)], ret_t :: Type, ret :: Exp, priority_f :: Int }
            | Operator { opName :: String, operator :: Op, priority :: Int } deriving Show
data T_Function = T_Function { funcName_t :: String, args_t :: [Type], return_t :: Type, priority_ft :: Int } 
            | T_Operator { opName_t :: String, operator_sig :: OperatorSig, priority_t :: Int } deriving Show
instance Eq T_Function where
    (==) T_Function { args_t = at1, return_t = rt1 } T_Function { args_t = at2, return_t = rt2} = at1 == at2 && rt1 == rt2
    (==) T_Operator { opName_t=s1 } T_Operator { opName_t = s2 } = s1 == s2
    (==) _ _ = False

data Bind = Bind { identifier :: String, value :: Wrd, vtype :: Type } deriving (Eq, Show)
data Wrd = Str String 
        | Func Function 
        | Bnd Bind 
        | Print String 
        | Tobe String 
        | Double Double 
        | Int Int 
        | Bool Bool 
        | Null 
        | List Exp 
        | ToEval Exp 
        | Err String 
        | Pair (Wrd, Wrd) 
        | PreList [Exp] 
        | Type Type 
        | Contents Exp 
        | Tuple Exp 
        | TypeCheck Type
instance Eq Wrd where
    (==) (Str a) (Str b) = a == b
    (==) (Func Operator { opName=a }) (Func Operator { opName=b }) = a == b
    (==) (Bnd bind1) (Bnd bind2) = bind1 == bind2
    (==) (Tobe a) (Tobe b) = a == b
    (==) (Double a) (Double b) = a == b
    (==) (Int a) (Int b) = a == b
    (==) (Bool a) (Bool b) = a == b
    (==) (Tuple a) (Tuple b) = a == b
    (==) (List l1) (List l2) = l1 == l2
    (==) (TypeCheck t1) (TypeCheck t2) = t1 == t2
    (==) (ToEval te1) (ToEval te2) = te1 == te2
    (==) (Print a) (Print b) = a == b
    (==) (Type a) (Type b) = a == b
    (==) _ _ = False
instance Show Wrd where
    show (Str s) = s
    show (Func (Operator { opName = s })) = "[Operator:" ++ s ++ "]"
    show (Func f) = show $ getFunctionSignature f
    show (Bnd bind) = show bind
    show (Print p) = p
    show (Tobe s) = s
    show (Double n) = show n
    show (Int n) = show n
    show (Bool b) = show b
    show Null = ""
    show (List l) = show l
    show (Pair t) = show t
    show (Tuple tpl) = "(" ++ intercalate "," (map show tpl) ++ ")"
    show (ToEval _) = "[ToEval]"
    show (TypeCheck t) = "[TypeCheck:" ++ show t ++ "]"
    show (PreList ls) = "(Prelist: " ++ show ls ++ ")"
    show (Type t) = show t

type Exp = [Wrd]
data EvalMode = M_Normal | M_TypeCheck

data Error = 
        UnknownKeywordError String
        | ParseError String 
        | TypeError { expected_types :: [Type], got_type :: Type, message_TE :: String }
        | SyntaxError String 
        | ValueError String 
        | InternalError String

instance Eq Error where
    (==) (UnknownKeywordError s1) (UnknownKeywordError s2) = s1 == s2
    (==) (TypeError t1 t2 _) (TypeError t1' t2' _) = t1 == t1' && t2 == t2'

instance Show Error where
    show (UnknownKeywordError s) = "Error: Unknown keyword: " ++ s
    show (ParseError s) = s
    show (TypeError _ _ s) = s
    show (SyntaxError s) = s
    show (ValueError s) = s
    show (InternalError s) = s

data Result = Result (Wrd, [Bind]) | Error Error
instance Show Result where
    show (Result (w, _)) = show w
    show (Error e) = show e
instance Eq Result where
    (==) (Result (w1, _)) (Result (w2, _)) = w1 == w2
    (==) (Error e1) (Error e2) = e1 == e2
    (==) _ _ = False

data Parenthesis = ParFound (Exp, Exp, Exp) | ParNotFound | ParError String

getFunctionSignature :: Function -> T_Function
getFunctionSignature Function { funcName=name, args = as, ret_t = rt, ret = _, priority_f=prt } =
    let ast = map fst as
    in T_Function { funcName_t=name, args_t = ast, return_t = rt, priority_ft=prt }

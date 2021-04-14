module Types where

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
        | T_Null 
        | T_Any
        deriving (Eq, Show)

typeEq :: Type -> Type -> Bool
typeEq T_Unknown _ = False
typeEq _ T_Unknown = False
typeEq t1 t2 = t1 == t2

instance Ord Type where
    (<=) T_EmptyList (T_List _) = True
    (<=) t1 t2 = typeEq t1 t2

type BinaryOp = Wrd -> Wrd -> Either Error Wrd
type UnaryOp = Wrd -> Either Error Wrd
type FunctionOp = (Int, Exp -> Either Error Wrd)
data Op = BinOp BinaryOp | UnOp UnaryOp | FuncOp FunctionOp
instance Show Op where
    show (BinOp _) = "[BinOp]"
    show (UnOp _) = "[UnOp]"
    show (FuncOp _) = "[FuncOp]"

data Function = Function { args :: [(Type, String)], ret_t :: Type, ret :: Exp }
            | Operator { opName :: String, operator :: Op, priority :: Int } deriving Show
data T_Function = T_Function { args_t :: [Type], return_t :: Type } 
            | T_Operator { opName_t :: String, operator_t :: Op, priority_t :: Int } deriving Show
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
    show (TypeCheck t) = show t
    show (PreList ls) = "(Prelist: " ++ show ls ++ ")"

type Exp = [Wrd]
data EvalMode = M_Normal | M_TypeCheck

data Error = UnknownKeywordError String | ParseError String | TypeError Type Type String | SyntaxError String | ValueError String | InternalError String

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

data Parenthesis = ParFound (Exp, Exp, Exp) | ParNotFound | ParError String -- TODO: 型構築子名にParをつける

getFunctionSignature :: Function -> T_Function
getFunctionSignature Function { args = as, ret_t = rt, ret = _ } =
    let ast = map (\ (t, a) -> t) as
    in T_Function { args_t = ast, return_t = rt }

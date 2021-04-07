module Types where

data Type = T_Int | T_Double | T_Bool | T_String | T_Func T_Func | T_UnaryOp | T_BinaryOp | T_FunctionOp | T_PreList | T_List Type | T_EmptyList | T_Tuple [Type] | T_Print | T_Unknown | T_Type | T_Error | T_TypeCheck deriving (Eq, Show)

instance Ord Type where
    (<=) T_EmptyList (T_List _) = True
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
data T_Func = T_Function { args_t :: [Type], return_t :: Type } | T_Operator (String, Op) deriving Show
instance Eq T_Func where
    (==) (T_Function { args_t = at1, return_t = rt1 }) (T_Function { args_t = at2, return_t = rt2}) = at1 == at2 && rt1 == rt2
    (==) (T_Operator (s1, _)) (T_Operator (s2, _)) = s1 == s2
    (==) _ _ = False
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
    show (TypeCheck t) = show t

type Exp = [Wrd]
data EvalMode = M_Normal | M_TypeCheck

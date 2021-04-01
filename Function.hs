module Function where
import Parser
import Debug.Trace

data Type = T_Int | T_Double | T_Bool | T_String
type BinaryOp = Wrd -> Wrd -> Wrd
type UnaryOp = Wrd -> Wrd
type FunctionOp = (Int, Exp -> Wrd)
data Op = BinOp BinaryOp | UnOp UnaryOp | FuncOp FunctionOp
type StrOp = (String, Op)
data Fun = Function (Exp, Exp)| Operator StrOp
data Bind = Bind { identifier :: String, value :: Wrd} deriving (Eq, Show)
data Wrd = Str String | Func Fun | Bnd Bind | Print String | Tobe String | Double Double | Int Int | Bool Bool | Null | List Exp | ToEval Exp | Err String | Pair (Wrd, Wrd) | PreList [Exp] | Type Type
instance Eq Wrd where
    (==) (Str a) (Str b) = a == b
    (==) (Func (Operator (a, _))) (Func (Operator (b, _))) = a == b
    (==) (Func (Function a)) (Func (Function b)) = a == b
    (==) (Bnd bind1) (Bnd bind2) = bind1 == bind2
    (==) (Tobe a) (Tobe b) = a == b
    (==) (Double a) (Double b) = a == b
    (==) (Int a) (Int b) = a == b
    (==) (Bool a) (Bool b) = a == b
    (==) _ _ = False
instance Show Wrd where
    show (Str s) = s
    show (Func (Operator (s, _))) = "[Operator:" ++ s ++ "]"
    show (Func (Function f)) = show f
    show (Func (Operator (s, FuncOp _))) = "[FuncOp:" ++ s ++ "]"
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
    show (ToEval _) = "[ToEval]"

type Exp = [Wrd]

_toExp0 :: String -> Exp
_toExp0 str = map (\ w -> Tobe w) $ words str

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

_macroGen :: Fun -> (Exp -> Exp)
_macroGen (Function (ws, expr)) =
    \ args -> 
        _mulSubst expr $ map (\ (Tobe a, val) -> Bind { identifier = a, value = val }) (zip ws args)

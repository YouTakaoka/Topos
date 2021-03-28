module Function where
import Parser
import Debug.Trace

type BinaryOp = Wrd -> Wrd -> Wrd
type UnaryOp = Wrd -> Wrd
data Op = BinOp BinaryOp | UnOp UnaryOp
type StrOp = (String, Op)
data Fun = Function (Exp, Exp) | Operator Op -- 仮引数文字列のリストと式
type Bind = (Wrd, Exp)
data Wrd = Str String | Func Fun | Bnd Bind | Print String | Tobe String | Num Double | Bool Bool | Null | List Exp | Err String | Pair (Wrd, Wrd)
instance Eq Wrd where
    (==) (Str a) (Str b) = a == b
    (==) (Func (Function a)) (Func (Function b)) = a == b
    (==) (Bnd a) (Bnd b) = a == b
    (==) (Tobe a) (Tobe b) = a == b
    (==) (Num a) (Num b) = a == b
    (==) (Bool a) (Bool b) = a == b
    (==) _ _ = False
instance Show Wrd where
    show (Str s) = s
    show (Func (Function f)) = show f
    show (Bnd (w, ex)) = show (show w, map show ex)
    show (Print p) = p
    show (Tobe s) = s
    show (Num n) = show n
    show (Bool b) = show b
    show Null = ""
    show (Err s) = s
    show (List l) = show l
    show (Pair t) = show t

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

_subst :: Exp -> Wrd -> Exp -> Exp
_subst ws target sbst =
    case divListBy target ws of
        Nothing -> ws
        Just (_, ws1, ws2) -> _subst (ws1 ++ sbst ++ ws2) target sbst

_mulSubst :: Exp -> [Bind] -> Exp
_mulSubst ws ((target, sbst) : sbsts) =
    _mulSubst (_subst ws target sbst) sbsts
_mulSubst ws [] = ws

_macroGen :: Fun -> (Exp -> Exp)
_macroGen (Function (ws, expr)) =
    \ args -> 
        let argsl = map (\ a -> [a]) args
        in _mulSubst expr (zip ws argsl)

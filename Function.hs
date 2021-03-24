module Function where
import Parser
import Debug.Trace

type Fun = (Exp, Exp) -- 仮引数文字列のリストと式
type Bind = (Wrd, Exp)
data Wrd = Str String | Func Fun | Bnd Bind | Print String
instance Eq Wrd where
    (==) (Str a) (Str b) = a == b
    (==) (Func a) (Func b) = a == b
    (==) (Bnd a) (Bnd b) = a == b
    (==) _ _ = False
instance Show Wrd where
    show (Str s) = s
    show (Func f) = show f
    show (Bnd (w, ex)) = show (show w, map show ex)
    show (Print p) = p

type Exp = [Wrd]

_toExp :: [String] -> Exp
_toExp ss = map (\ s -> Str s) ss

_fromExp :: Exp -> [String]
_fromExp expr = map show expr

_subst :: [Wrd] -> Wrd -> Wrd -> [Wrd]
_subst ws target sbst =
    case divListBy target ws of
        Nothing -> ws
        Just (_, ws1, ws2) -> _subst (ws1 ++ [sbst] ++ ws2) target sbst

_mulSubst :: [Wrd] -> [Bind] -> [Wrd]
_mulSubst ws ((target, (sbst : _)) : sbsts) =
    _mulSubst (_subst ws target sbst) sbsts
_mulSubst ws [] = ws

_macroGen :: Fun -> (Exp -> Exp)
_macroGen (ws, expr) =
    \ args -> 
        let argsl = map (\ a -> [a]) args
        in _mulSubst expr (zip ws argsl)

_removeSpace :: Exp -> Exp
_removeSpace ex = concat $ divListInto (Str " ") ex

_removeEmpty :: Exp -> Exp
_removeEmpty ex = concat $ divListInto (Str "") ex

trim :: Exp -> Exp
trim ex = _removeEmpty $ _removeSpace ex

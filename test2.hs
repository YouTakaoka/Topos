type Fun = (Exp, Exp) -- 仮引数文字列のリストと式
type Bind = (Wrd, Exp)
data Wrd = Str String | Func Fun | Bnd Bind
instance Eq Wrd where
    (==) (Str a) (Str b) = a == b
    (==) (Func a) (Func b) = a == b
    (==) (Bnd a) (Bnd b) = a == b
    (==) _ _ = False
instance Show Wrd where
    show (Str s) = show s
    show (Func f) = show f
    show (Bnd (w, ex)) = show (show w, map show ex)

type Exp = [Wrd]

_toExp :: [String] -> Exp
_toExp ss = map (\ s -> Str s) ss

_fromExp :: Exp -> [String]
_fromExp expr = map (\ (Str s) -> s) expr

beg = Str "("
end = Str ")"

str = "( ( ) )"

f :: Exp -> String -> String
f (s:s1) s2 =
    case s of
        Str "(" -> f s1 (s2 ++ "(")
        _ -> f s1 (s2 ++ ")")
f [] s2 = s2

main = do
    print $ f (_toExp $ words str) " "

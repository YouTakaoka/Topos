import Debug.Trace

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

data Parenthesis = Found (Exp, Exp, Exp) | NotFound | Error String

type Parser a = [a] -> Maybe (a, [a], [a])

_divList :: (a -> Bool) -> [a] -> Parser a
_divList p (x : xs1) xs2 -- xs2 には初期値 [] を入れる
    | p x       = Just (x, xs2, xs1)
    | otherwise = _divList p xs1 (xs2 ++ [x])
_divList _ [] _ = Nothing

divList :: (a -> Bool) -> Parser a
divList p xs = _divList p xs []

_toExp :: [String] -> Exp
_toExp ss = map (\ s -> Str s) ss

_fromExp :: Exp -> [String]
_fromExp expr = map (\ (Str s) -> s) expr

beg = Str "("
end = Str ")"

_findParenthesis :: Exp -> Integer -> Parenthesis
_findParenthesis ws cnt = -- cnt は初期値 -1
    case divList (\ w -> w == beg || w == end) ws of
    Nothing -> NotFound
    Just (d, ws1, ws2)
        | (d, cnt) == (beg, -1) ->
            case _findParenthesis ws2 1 of
            Found (_, ex2, ex3) -> Found (ws1, ex2, ex3)
            Error s -> Error s
            NotFound -> Error ("括弧閉じてないです:" ++ show ws2)
        | (d, cnt) == (end, 1) ->
            trace (show end) (Found ([], ws1, ws2))
        | otherwise ->
            let Found (_, ex2, ex3) = _findParenthesis ws2 $ (if d == beg then (+) else (-)) cnt 1
            in Found ([], ws1 ++ [d] ++ ws2, ex3)

main = do
    let Found (exp1, exp2, exp3) = _findParenthesis (_toExp $ words "4 / ( ( 3 + 5 ) * 2 )") (-1)
    print $ show exp1
    print $ show exp2
    print $ show exp3

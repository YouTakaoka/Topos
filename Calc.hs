module Calc where
import Parser
import Ops
import Function
import Debug.Trace

data Parenthesis = Found (Exp, Exp, Exp) | NotFound | Error String

beg :: Wrd
beg = Str "("
end :: Wrd
end = Str ")"

_traceShow :: Show a => a -> a
_traceShow x = trace (show x) x

_traceIf :: Show a => Bool -> String -> a -> a
_traceIf b s x = if b then trace s x else x

_findParenthesis :: Exp -> Integer -> Parenthesis
_findParenthesis ws cnt = -- cnt は初期値 -1
    case divList (\ w -> w == beg || w == end) ws of
    Nothing -> NotFound
    Just (d, ws1, ws2)
        | (d, cnt) == (beg, -1) ->
            case _findParenthesis ws2 1 of
            Found (_, ex2, ex3) -> Found (ws1, ex2, ex3)
            Error s -> Error s
            NotFound -> Error ("End of parenthesis not found:" ++ show ws2)
        | (d, cnt) == (end, 1) ->
            Found ([], ws1, ws2)
        | otherwise ->
            case _findParenthesis ws2 (if d == beg then cnt + 1 else cnt - 1) of
            Found (_, ex2, ex3) -> Found ([], ws1 ++ [d] ++ ex2, ex3)
            NotFound -> Error "End of parenthesis not found."

findParenthesis :: Exp -> Parenthesis
findParenthesis ws = _findParenthesis ws (-1)

_iterOps :: [StrOp] -> Exp -> Maybe (StrOp, (Exp, Exp))
_iterOps [] _ = Nothing
_iterOps ((op, f) : ops) ws =
    case divListBy (Str op) ws of
        Nothing -> _iterOps ops ws
        Just (_, ws1, ws2) -> Just ((op, f), (ws1, ws2))

_numIn :: Wrd -> Exp -> Integer
_numIn w ex = sum $ map (\ v -> if v == w then 1 else 0) ex 

_isReplaceable :: [Bind] -> Exp -> Bool
_isReplaceable binds ex = (<) 0 $ sum $ map (\ (w, _) -> _numIn w ex) binds

_eval :: [Bind] -> Exp -> Either String (Exp, [Bind]) -- 初期状態で第二引数は空リスト
_eval binds (Str "Function" : rest) =
    case divListBy (Str "->") rest of
        Just (_, ex1, ex2) -> Right ([(Func (ex1, ex2))], binds)
        Nothing -> Left ("`Function` statement must be accompanied with `->` operator: " ++ (show rest))
_eval binds (Str "let" : rest) =
    let Just (_, (w:_), ex) = divListBy (Str "=") rest
        (Right (ex2, binds2)) = _eval binds ex
    in _eval ([(w, ex2)] ++ binds2) [(Str "")]
_eval binds ws =
    case findParenthesis ws of -- 括弧探し
    NotFound -> -- 括弧見つからなかった
        case ws of -- 最初が関数かどうかを見る
        (Func fun : expr) -> _eval binds $ (_macroGen fun) expr
        _ ->
            case _iterOps _opls ws of -- オペレータ探し
            Nothing -> -- オペレーターが見つからなかった
                if _isReplaceable binds ws
                then _eval binds $ _mulSubst ws binds 
                else Right (ws, binds)
            Just ((op, f), (ws1, ws2)) -> -- オペレータが見つかった
                case (_eval binds ws1, _eval binds ws2) of
                (Right (res1, _), Right (res2, _)) -> Right ((f res1 res2), binds)
                (Left s, _) -> Left s
                (_, Left s) -> Left s
    Found (ws1, ws2, ws3) -> -- 括弧見つかった
        case _eval binds ws2 of
        Right (res, binds2) -> _eval binds2 $ ws1 ++ res ++ ws3
        Left s -> Left s
    Error s -> Left s

eval :: String -> String
eval str =
    case _eval [] $ _toExp $ words str of
    Right (res, _) -> head $ _fromExp res
    Left s -> s

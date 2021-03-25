module Calc where
import Parser
import Ops
import Function
import Debug.Trace

data Parenthesis = Found (Exp, Exp, Exp) | NotFound | Error String

_traceShow :: Show a => a -> a
_traceShow x = trace (show x) x

_traceIf :: Show a => Bool -> String -> a -> a
_traceIf b s x = if b then trace s x else x

_findParenthesis :: Exp -> Integer -> Wrd -> Wrd -> Parenthesis
_findParenthesis ws cnt b e = -- cnt は初期値 -1
    case divList (\ w -> w == b || w == e) ws of
    Nothing -> NotFound
    Just (d, ws1, ws2)
        | (d, cnt) == (b, -1) ->
            case _findParenthesis ws2 1 b e of
            Found (_, ex2, ex3) -> Found (ws1, ex2, ex3)
            Error s -> Error s
            NotFound -> Error ("End of parenthesis not found:" ++ show ws2)
        | (d, cnt) == (e, 1) ->
            Found ([], ws1, ws2)
        | otherwise ->
            case _findParenthesis ws2 (if d == b then cnt + 1 else cnt - 1) b e of
            Found (_, ex2, ex3) -> Found ([], ws1 ++ [d] ++ ex2, ex3)
            NotFound -> Error "End of parenthesis not found."

findParenthesis :: Exp -> String -> String -> Parenthesis
findParenthesis ws b e = _findParenthesis ws (-1) (Tobe b) (Tobe e)

_iterOps :: [StrOp] -> Exp -> Maybe (StrOp, (Exp, Exp))
_iterOps [] _ = Nothing
_iterOps ((op, f) : ops) ws =
    case divListBy (Tobe op) ws of
        Nothing -> _iterOps ops ws
        Just (_, ws1, ws2) -> Just ((op, f), (ws1, ws2))

_numIn :: Wrd -> Exp -> Integer
_numIn w ex = sum $ map (\ v -> if v == w then 1 else 0) ex 

_isReplaceable :: [Bind] -> Exp -> Bool
_isReplaceable binds ex = (<) 0 $ sum $ map (\ (w, _) -> _numIn w ex) binds

_eval :: [Bind] -> Exp -> Either String (Exp, [Bind]) -- 初期状態で第二引数は空リスト
_eval binds (Tobe "Function" : rest) =
    case divListBy (Tobe "->") rest of
        Just (_, ex1, ex2) -> Right ([(Func (ex1, ex2))], binds)
        Nothing -> Left ("`Function` statement must be accompanied with `->` operator: " ++ (show rest))
_eval binds (Tobe "let" : rest) =
    let Just (_, (w:_), ex) = divListBy (Tobe "=") rest
        (Right (ex2, binds2)) = _eval binds ex
    in _eval ([(w, ex2)] ++ binds2) [Null]
_eval binds (Tobe "if" : rest) =
    let Just (_, cond, rest2) = divListBy (Tobe "then") rest
        Just (_, thn, els) = divListBy (Tobe "else") rest2
    in case _eval binds cond of
        Left s -> Left s
        Right ((Bool truth : _), binds2) -> if truth then _eval binds2 thn else _eval binds2 els
        _ -> Left "Entered a non-boolean value into `if` statement."
_eval binds ws =
    case findParenthesis ws "(" ")" of -- 括弧探し
    Found (ws1, ws2, ws3) -> -- 括弧見つかった
        case _eval binds ws2 of
        Right (res, binds2) -> _eval binds2 $ ws1 ++ res ++ ws3
        Left s -> Left s
    Error s -> Left s
    NotFound -> -- 括弧見つからなかった
        case ws of -- 最初が関数かどうかを見る
        (Func fun : expr) -> _eval binds $ (_macroGen fun) expr
        _ ->
            case _iterOps _opls ws of -- オペレータ探し
            Nothing -> -- オペレーターが見つからなかった
                if _isReplaceable binds ws
                then _eval binds $ _mulSubst ws binds 
                else case ws of -- 最終的にここに行き着く！！
                    [] -> Right ([], binds)
                    (w: []) ->
                        case w of
                        (Tobe "True") -> Right ([(Bool True)], binds)
                        (Tobe "False") -> Right ([(Bool False)], binds)
                        (Tobe n) -> Right ([(Num (read n :: Double))], binds)
                        _ -> Right ([w], binds)
                    _ -> Left ("Parse failed: " ++ show ws)
            Just ((op, f), (ws1, ws2)) -> -- オペレータが見つかった
                case (_eval binds ws1, _eval binds ws2) of
                (Right (res1, _), Right (res2, _)) -> Right ((f res1 res2), binds)
                (Left s, _) -> Left s
                (_, Left s) -> Left s

eval :: String -> String
eval str =
    case _eval [] $ toExp str of
    Right (res, _) -> head $ _fromExp res
    Left s -> s

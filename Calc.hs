module Calc where
import Parser
import Ops
import Function
import Text.Read
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

_iterOps :: [StrOp] -> Exp -> Maybe StrOp
_iterOps strops expr =
    case dropWhile (\ sop -> divListBy (Func $ Operator sop) expr == Nothing) strops of
        [] -> Nothing
        (sop: _) -> Just sop

_numIn :: Wrd -> Exp -> Integer
_numIn w ex = sum $ map (\ v -> if v == w then 1 else 0) ex 

_isReplaceable :: [Bind] -> Exp -> Bool
_isReplaceable binds ex = (<) 0 $ sum $ map (\ bind -> _numIn (Tobe $ identifier bind) ex) binds

_isFunction :: Wrd -> Bool
_isFunction (Func (Fun _)) = True
_isFunction (Func (Operator (_, FuncOp _))) = True
_isFunction _ = False

_isOp :: Wrd -> Bool
_isOp (Func (Operator _)) = True
_isOp _ = False

myReadDouble :: Either String Wrd -> Either String Wrd
myReadDouble (Right u) = Right u
myReadDouble (Left s) =
    case readMaybe s :: Maybe Double of
        Nothing -> Left s
        Just u -> Right (Double u)

myReadBool :: Either String Wrd -> Either String Wrd
myReadBool (Right w) = Right w
myReadBool (Left s) =
    case readMaybe s :: Maybe Bool of
        Nothing -> Left s
        Just w -> Right (Bool w)

myReadInt :: Either String Wrd -> Either String Wrd
myReadInt (Right u) = Right u
myReadInt (Left s) =
    case readMaybe s :: Maybe Int of
        Nothing -> Left s
        Just u -> Right (Int u)

_evalWrd :: Wrd -> Wrd
_evalWrd (Tobe s) =
    case myReadBool $ myReadDouble $ myReadInt (Left s) of
        Left _ -> Tobe s
        Right w -> w
_evalWrd w = w
    
_subOp :: StrOp -> Exp -> Exp
_subOp (str, op) expr =
    case divListBy (Tobe str) expr of
        Nothing -> expr
        Just (_, ws1, ws2) -> _subOp (str, op) $ ws1 ++ [Func (Operator (str, op))] ++ ws2

_mulSubOp :: [StrOp] -> Exp -> Exp
_mulSubOp (strop: []) expr = _subOp strop expr
_mulSubOp (strop: strops) expr = _mulSubOp strops $ _subOp strop expr

_toList :: [Bind] -> Exp -> Wrd -- 引数はカンマ区切りの式
_toList binds expr =
    let ls = divListInto (Tobe ",") expr
    in List $ map (\ ex -> fst $ _eval binds ex) ls

_toPair :: [Bind] -> Exp -> Wrd -- 引数はカンマ区切りの式
_toPair binds expr =
    case divListBy (Tobe ",") expr of
    Nothing ->
        Err ("',' not found: " ++ (show expr))
    Just (_, expr1, expr2) ->
        let (w1, _) = _eval binds expr1
            (w2, _) = _eval binds expr2
        in Pair (w1, w2)

_applyOp :: Op -> Exp -> Wrd -> Exp -> Exp
_applyOp op ws1 y rest2 =
    case op of
    BinOp binop -> 
        let x = last ws1
            rest1 = init ws1
            in rest1 ++ [binop (_evalWrd x) (_evalWrd y)] ++ rest2
    UnOp unop ->
        ws1 ++ [unop (_evalWrd y)] ++ rest2

_bind :: [Bind] -> Exp -> (Wrd, [Bind])
_bind binds rest =
 case divListBy (Tobe "=") rest of
        Nothing ->
            (Err "Syntax error: missing `=`", binds)
        Just (_, (Tobe w: []), expr) ->
            let (rhs, _) = _eval binds expr
            in (rhs, (Bind { identifier = w, value = rhs, vtype = _getType rhs } : binds))
        _ -> (Err "Syntax error: You should specify only one symbol to bind value.", binds)

_eval :: [Bind] -> Exp -> (Wrd, [Bind]) -- 初期状態で第一引数は空リスト
_eval binds (Tobe "Function" : rest) =
    case divListBy (Tobe ":") rest of
    Nothing -> (Err "Function: Syntax error, missing `:`", binds)
    Just (_, types, rest2) ->
        case divListBy (Tobe "->") types of
        Nothing -> (Err "Function: Syntax error, missing `->`", binds)
        Just (_, as_t, (Tobe ret_t : [])) ->
            case divListBy (Tobe "->") rest2 of
            Nothing -> (Err "Function: Syntax error, missing `->`", binds)
            Just (_, as, expr)
                | length as_t /= length as -> (Err "Function: Mismatch numbers of types and arguments.", binds)
                | otherwise ->
                    let ass = map (\ (Tobe a) -> a) as
                        ts = map (\ (Tobe t) -> toType t) as_t
                        rt = toType ret_t
                    in (Func $ Fun (Function { args = zip ts ass, ret_t = rt, ret = expr }), binds)
_eval binds (Tobe "let" : rest) = _bind binds rest
_eval binds (Tobe "letn" : rest) =
    let (_, binds2) = _bind binds rest
    in (Null, binds2)
_eval binds (Tobe "if" : rest) =
    let Just (_, cond, rest2) = divListBy (Tobe "then") rest
        Just (_, thn, els) = divListBy (Tobe "else") rest2
    in case _eval binds cond of
        (Bool truth, binds2) ->
            if truth then (fst $ _eval binds2 thn, binds) else (fst $ _eval binds2 els, binds)
        _ -> (Err "Entered a non-boolean value into `if` statement.", binds)
_eval binds expr =
    let ws = map _evalWrd $ _mulSubOp _opls_dec $ _mulSubst expr binds
    in
        case divListBy (Tobe "#") ws of --コメント探し
        Just (_, expr1, expr2) ->
            _eval binds expr1
        Nothing ->
            case findParenthesis ws "(" ")" of -- 括弧探し
            Found (ws1, ws2, ws3) -> -- 括弧見つかった
                let (rslt, _) = _eval binds ws2
                in _eval binds $ ws1 ++ [rslt] ++ ws3
            Error s -> (Err s, binds)
            NotFound -> -- 括弧見つからなかった
                case findParenthesis ws "[" "]" of -- リスト探し
                Found (ws1, ws2, ws3) ->
                    _eval binds $ ws1 ++ [_toList binds ws2] ++ ws3
                Error s -> (Err s, binds)
                NotFound ->
                    case findParenthesis ws "((" "))" of -- タプル探し
                    Found (ws1, ws2, ws3) ->
                        _eval binds $ ws1 ++ [_toPair binds ws2] ++ ws3
                    Error s -> (Err s, binds)
                    NotFound ->
                        case divList _isFunction ws of -- 関数探し
                        Just (Func (Fun f), expr1, expr2) -> -- 関数
                            let l = length $ args f
                                as = take l expr2
                                rest = drop l expr2
                                (rslt, _) = _eval binds $ (_macroGen f) as
                            in _eval binds $ expr1 ++ [rslt] ++ rest
                        Just (Func (Operator (_, FuncOp (l, op))), ws1, ws2) -> -- 関数オペレータ
                            let args = map _evalWrd $ take l ws2
                                rest = drop l ws2
                            in _eval binds $ ws1 ++ [op args] ++ rest
                        Nothing ->
                            case _iterOps _opls_dec ws of -- オペレータ探し
                            Just strop -> -- オペレータが見つかった
                                let Just (Func (Operator (_, op)), ws1, (y: rest2)) = divListBy (Func (Operator strop)) ws
                                in _eval binds $ _applyOp op ws1 y rest2
                            Nothing -> -- オペレータ見つからなかった
                                case ws of
                                    [] -> (Null, binds)
                                    ((PreList pls) : rest) ->
                                        let ls = map (\ expr -> fst $ _eval binds expr) pls
                                        in _eval binds (List ls : rest)
                                    (Tobe s: []) -> (Err $ "Unknown keyword: " ++ s, binds)
                                    (w : []) -> (w, binds)
                                    _ -> (Err $ "Parse failed: " ++ show ws, binds)

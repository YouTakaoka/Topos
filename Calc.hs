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
_isFunction (Func (Function _)) = True
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
        Left s -> Err ("Failed to parse: " ++ s)
        Right w -> w
_evalWrd w = w

_evalToWrd :: [Bind] -> Exp -> Wrd
_evalToWrd binds expr =
    case _eval binds expr of
        Left s -> Err s
        Right ((w:[]), _) -> w
        Right (ws, _) -> Err ("Result became to more than one words: " ++ show ws)

_subOp :: StrOp -> Exp -> Exp
_subOp (str, op) expr =
    case divListBy (Tobe str) expr of
        Nothing -> expr
        Just (_, ws1, ws2) -> _subOp (str, op) $ ws1 ++ [Func (Operator (str, op))] ++ ws2

_mulSubOp :: [StrOp] -> Exp -> Exp
_mulSubOp (strop: []) expr = _subOp strop expr
_mulSubOp (strop: strops) expr = _mulSubOp strops $ _subOp strop expr

_evalOldBinds :: [Bind] -> [Bind] -> Exp -> Either String (Exp, [Bind])
_evalOldBinds binds binds2 expr =
    case _eval binds2 expr of
        Left s -> Left s
        Right (res, _) -> Right (res, binds)

_toList :: [Bind] -> Exp -> Wrd -- 引数はカンマ区切りの式
_toList binds expr =
    case divListBy (Tobe ",") expr of
    Nothing ->
        case _eval binds expr of
        Left s -> Err s
        Right ((w: []), _) -> List [w]
        Right (expr2, _) -> Err ("Parse error: " ++ (show expr2))
    Just (_, a, rest) ->
        case _eval binds a of
        Left s -> Err s
        Right ((w: []), _) ->
            let (List ws) = _toList binds rest
            in List (w : ws)
        Right (expr2, _) -> Err ("Parse error: " ++ (show expr2))

_toPair :: [Bind] -> Exp -> Wrd -- 引数はカンマ区切りの式
_toPair binds expr =
    case divListBy (Tobe ",") expr of
    Nothing ->
        Err ("',' not found: " ++ (show expr))
    Just (_, expr1, expr2) ->
        case (_eval binds expr1, _eval binds expr2) of
        (Left s, _) -> Err s
        (_, Left s) -> Err s
        (Right ((w1: []), _), Right ((w2: []), _)) ->
            Pair (w1, w2)
        _ -> Err "Parse error"

_applyOp :: Op -> Exp -> Wrd -> Exp -> Exp
_applyOp op ws1 y rest2 =
    case op of
    BinOp binop -> 
        let x = last ws1
            rest1 = init ws1
            in rest1 ++ [binop (_evalWrd x) (_evalWrd y)] ++ rest2
    UnOp unop ->
        ws1 ++ [unop (_evalWrd y)] ++ rest2

_typeCheck :: String -> Wrd -> Bool
_typeCheck "String" (Str _) = True
_typeCheck "Int" (Int _) = True
_typeCheck "Double" (Double _) = True
_typeCheck "Bool" (Bool _) = True
_typeCheck "Func" (Func _) = True
_typeCheck _ _ = False

_bind :: [Bind] -> Exp -> Either String (Exp, [Bind])
_bind binds rest =
 case divListBy (Tobe "=") rest of
        Nothing ->
            Left "Syntax error: missing `=`"
        Just (_, (Tobe w: []), expr) ->
            case _eval binds expr of
                Left s -> Left s
                Right ((rhs: []), _) ->
                    Right ([rhs], (Bind { identifier = w, value = rhs } : binds))
                _ -> Left "`=`: Evaluation error."
        _ -> Left "Syntax error: You should specify only one symbol to bind value."

_eval :: [Bind] -> Exp -> Either String (Exp, [Bind]) -- 初期状態で第一引数は空リスト
_eval binds (Tobe "Function" : rest) =
    case divListBy (Tobe "->") rest of
        Just (_, args, expr) -> Right ([(Func (Function (args, expr)))], binds)
        Nothing -> Left ("`Function` statement must be accompanied with `->` operator: " ++ (show rest))
_eval binds (Tobe "let" : rest) =
    case _bind binds rest of
        Left s -> Left s
        Right (expr, binds2) -> Right (expr, binds2)
_eval binds (Tobe "letn" : rest) =
    case _bind binds rest of
        Left s -> Left s
        Right (_, binds2) -> Right ([], binds2)
_eval binds (Tobe "if" : rest) =
    let Just (_, cond, rest2) = divListBy (Tobe "then") rest
        Just (_, thn, els) = divListBy (Tobe "else") rest2
    in case _eval binds cond of
        Left s -> Left s
        Right ((Bool truth : _), binds2) ->
            if truth then _evalOldBinds binds binds2 thn else _evalOldBinds binds binds2 els
        _ -> Left "Entered a non-boolean value into `if` statement."
_eval binds expr =
    let ws = _mulSubOp _opls_dec $ _mulSubst expr binds
    in
        case divListBy (Tobe "#") ws of --コメント探し
        Just (_, expr1, expr2) ->
            _eval binds expr1
        Nothing ->
            case findParenthesis ws "(" ")" of -- 括弧探し
            Found (ws1, ws2, ws3) -> -- 括弧見つかった
                case _eval binds ws2 of
                Right (res, binds2) -> _eval binds2 $ ws1 ++ res ++ ws3
                Left s -> Left s
            Error s -> Left s
            NotFound -> -- 括弧見つからなかった
                case findParenthesis ws "[" "]" of -- リスト探し
                Found (ws1, ws2, ws3) ->
                    _eval binds $ ws1 ++ [_toList binds ws2] ++ ws3
                Error s -> Left s
                NotFound ->
                    case findParenthesis ws "((" "))" of -- タプル探し
                    Found (ws1, ws2, ws3) ->
                        _eval binds $ ws1 ++ [_toPair binds ws2] ++ ws3
                    Error s -> Left s
                    NotFound ->
                        case divList _isFunction ws of -- 関数探し
                        Just (Func (Function f), expr1, expr2) -> -- 関数
                            let l = length $ fst f
                                args = take l expr2
                                rest = drop l expr2
                            in  case _eval binds $ (_macroGen (Function f)) args of
                                    Right (rslt, _) -> _eval binds $ expr1 ++ [Tobe "("] ++ rslt ++ [Tobe ")"] ++ rest
                                    Left s -> Left s
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
                                    [] -> Right ([], binds)
                                    (ToEval expr: rest) -> -- 「後でevaる」を処理
                                        case _eval binds expr of
                                        Left s -> Left s
                                        Right (rslt, binds2) ->
                                            _eval binds2 $ rslt ++ rest
                                    ((PreList pls) : rest) ->
                                        let ls = map (_evalToWrd binds) pls
                                        in _eval binds (List ls : rest)
                                    (w: []) -> Right ([_evalWrd w], binds)
                                    _ -> Left ("Parse failed: " ++ show ws)

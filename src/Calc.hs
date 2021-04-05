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
            NotFound -> Error "Mismatch of parenthesis."

findParenthesis :: Exp -> String -> String -> Parenthesis
findParenthesis ws b e = _findParenthesis ws (-1) (Tobe b) (Tobe e)

_iterOps :: [StrOp] -> Exp -> Maybe StrOp
_iterOps strops expr =
    case dropWhile (\ sop -> divListBy (Func $ Operator sop) expr == Nothing) strops of
        [] -> Nothing
        (sop: _) -> Just sop

_iterOps_T :: [StrOp] -> TExp -> Maybe StrOp
_iterOps_T strops texpr =
    case dropWhile (\ sop -> divListBy (TFunc $ Operator sop) texpr == Nothing) strops of
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

_isFunction_T :: TWrd -> Bool
_isFunction_T (TFunc (Fun _)) = True
_isFunction_T (TFunc (Operator (_, FuncOp _))) = True
_isFunction_T _ = False

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

_applyOp :: Op -> Exp -> Wrd -> Exp -> Exp
_applyOp op ws1 y rest2 =
    case op of
    BinOp binop -> 
        let x = last ws1
            rest1 = init ws1
            in rest1 ++ [binop (_evalWrd x) (_evalWrd y)] ++ rest2
    UnOp unop ->
        ws1 ++ [unop (_evalWrd y)] ++ rest2

_applyOp_T :: Op_T -> TExp -> TWrd -> TExp -> TExp
_applyOp_T op ws1 y rest2 =
    case op of
    BinOp_T binop -> 
        let x = last ws1
            rest1 = init ws1
            in rest1 ++ [binop x y] ++ rest2
    UnOp_T unop ->
        ws1 ++ [unop y] ++ rest2

_bind :: EvalMode -> [Bind] -> Exp -> (Wrd, [Bind])
_bind mode binds rest =
 case divListBy (Tobe "=") rest of
        Nothing ->
            (Err "Syntax error: missing `=`", binds)
        Just (_, (Tobe w: []), expr) ->
            let (rhs, _) = _eval mode binds expr
            in (rhs, (Bind { identifier = w, value = rhs, vtype = _getType rhs } : binds))
        _ -> (Err "Syntax error: You should specify only one symbol to bind value.", binds)

data TypeOrTypeContents = TP Type | TContents [Type]

_evalFunctionSignature :: Exp -> Either String TypeOrTypeContents
_evalFunctionSignature expr = -- exprは<>の中身
    case divListBy (Tobe "Function") expr of
        Nothing ->
            case findParenthesis expr "(" ")" of
                Error s -> Left s
                Found (expr2, expr3, expr4) ->
                    case _evalFunctionSignature expr3 of
                        Left s -> Left s
                        Right (TContents ts) ->
                            _evalFunctionSignature $ expr2 ++ [Type (T_Tuple ts)] ++ expr4
                NotFound ->
                    case divListBy (Tobe "->") expr of
                        Nothing ->
                            Right $ TContents $ map (\ ex -> toType ex) $ divListInto (Tobe ",") expr
                        Just (_, expr1, t_r) ->
                            let as_t = map (\ ex -> toType ex) $ divListInto (Tobe ",") expr1
                            in Right $ TP $ T_Function { args_t = as_t, return_t = toType t_r }
        Just (_, expr1, expr2) ->
            case findParenthesis expr2 "<" ">" of
                Error s -> Left s
                NotFound -> Left "Syntax error: `<` not found after `Function`"
                Found ([], expr3, expr4) ->
                    case _evalFunctionSignature expr3 of
                        Left s -> Left s
                        Right (TP t) -> _evalFunctionSignature $ expr1 ++ [Type t] ++ expr4
                        _ -> Left "Syntax error: missing `->`"
                _ -> Left "Syntax error: `<` must follow just after `Function`"

_eval :: EvalMode -> [Bind] -> Exp -> (Wrd, [Bind])
_eval mode binds (Tobe "Function" : rest) =
    case divListBy (Tobe ":") rest of
    Nothing -> (Err "Function: Syntax error, missing `:`", binds)
    Just (_, rest1, rest2) ->
        case findParenthesis rest1 "<" ">" of
        Error s -> (Err s, binds)
        NotFound -> (Err "Function: Syntax error, `<` must follow just after `Function`", binds)
        Found ([], expr1, []) ->
            case _evalFunctionSignature expr1 of
            Left s -> (Err s, binds)
            Right (TP (T_Function { args_t = ts , return_t = rt })) ->
                case divListBy (Tobe "->") rest2 of
                Nothing -> (Err "Function: Syntax error, missing `->`", binds)
                Just (_, as, expr2)
                    | length ts /= length as -> (Err "Function: Mismatch numbers of types and arguments.", binds)
                    | otherwise ->
                        let ass = map (\ (Tobe a) -> a) as
                            f = Function { args = zip ts ass, ret_t = rt, ret = expr2 }
                        in case mode of
                            TypeCheck -> (Func $ Fun f, binds)
                            Normal ->
                                case functionTypeCheck binds f of
                                Err s -> (Err s, binds)
                                _ -> (Func $ Fun f, binds)
        _ -> (Err "Function: Syntax error.", binds)
_eval mode binds (Tobe "let" : rest) = _bind mode binds rest
_eval mode binds (Tobe "letn" : rest) =
    let (_, binds2) = _bind mode binds rest
    in (Null, binds2)
_eval mode binds (Tobe "if" : rest) =
    let Just (_, cond, rest2) = divListBy (Tobe "then") rest
        Just (_, thn, els) = divListBy (Tobe "else") rest2
    in case _eval mode binds cond of
        (Bool truth, binds2) ->
            if truth then (fst $ _eval mode binds2 thn, binds) else (fst $ _eval mode binds2 els, binds)
        _ -> (Err "Entered a non-boolean value into `if` statement.", binds)
_eval mode binds expr =
    case divListBy (Tobe "#") expr of --コメント探し
    Just (_, expr1, expr2) ->
        _eval mode binds expr1
    Nothing ->
        case findParenthesis expr "(" ")" of
        Error s -> (Err s, binds)
        Found (expr1, expr2, expr3) ->
            let res = case _eval mode binds expr2 of
                        (Contents ls, _) -> Tuple ls
                        (w, _) -> w
            in _eval mode binds $ expr1 ++ [res] ++ expr3
        NotFound ->
            case findParenthesis expr "[" "]" of
            Error s -> (Err s, binds)
            Found (expr1, expr2, expr3) ->
                let res = case _eval mode binds expr2 of
                            (Contents ls, _) -> toList ls
                            (w, _) -> w
                in _eval mode binds $ expr1 ++ [res] ++ expr3
            NotFound ->
                let ls = case mode of
                        Normal -> map (fst . _evalFunctions binds) $ divListInto (Tobe ",") expr
                        TypeCheck -> map (functionTypeEval binds) $ divListInto (Tobe ",") expr
                in case ls of
                    (w : []) -> (w, binds)
                    _ -> (Contents ls, binds)

_evalFunctions :: [Bind] -> Exp -> (Wrd, [Bind]) -- 初期状態で第一引数は空リスト
_evalFunctions binds expr =
    let ws = map _evalWrd $ _mulSubOp _opls_dec $ _mulSubst expr binds
    in
        case divList _isFunction ws of -- 関数探し
        Just (Func (Fun f), expr1, expr2) -> -- 関数
            let l = length $ args f
                as = take l expr2
                rest = drop l expr2
            in case (_macroGen f) as of
                Left s -> (Err s, binds)
                Right rslt -> _eval Normal binds $ expr1 ++ [Tobe "("] ++ rslt ++ [Tobe ")"] ++ rest
        Just (Func (Operator (_, FuncOp (l, op))), ws1, ws2) -> -- 関数オペレータ
            let args = map _evalWrd $ take l ws2
                rest = drop l ws2
            in _eval Normal binds $ ws1 ++ [op args] ++ rest
        Nothing ->
            case _iterOps _opls_dec ws of -- オペレータ探し
            Just strop -> -- オペレータが見つかった
                let Just (Func (Operator (_, op)), ws1, (y: rest2)) = divListBy (Func (Operator strop)) ws
                in _eval Normal binds $ _applyOp op ws1 y rest2
            Nothing -> -- オペレータ見つからなかった
                case ws of
                    [] -> (Null, binds)
                    (PreList pls : []) ->
                        let ls = map (\ expr -> fst $ _eval Normal binds expr) pls
                        in (List ls, binds)
                    (Tobe s: []) -> (Err $ "Unknown keyword: " ++ s, binds)
                    (w : []) -> (w, binds)
                    _ -> (Err $ "Parse failed: " ++ show ws, binds)

functionTypeCheck :: [Bind] -> Function -> Wrd
functionTypeCheck binds f = fst $ _functionTypeEval binds $ _typeExprGen binds f

_typeExprGen :: [Bind] -> Function -> TExp
_typeExprGen binds (Function { args = as, ret_t = rt, ret = expr }) =
    let expr2 = map convToType $ map _evalWrd $ _mulSubOp _opls_dec $ _mulSubst expr binds
        binds2 = map (\ (t, id) -> Bind {identifier = id, value = Type t, vtype = T_Type }) as
    in _mulSubst_T expr2 binds2

functionTypeEval :: [Bind] -> Exp -> Wrd
functionTypeEval binds expr = 
    let texpr = map convToType $ map _evalWrd $ _mulSubOp _opls_dec $ _mulSubst expr binds
    in fst $ _functionTypeEval binds texpr

_functionTypeEval :: [Bind] -> TExp -> (Wrd, [Bind])
_functionTypeEval binds texpr =
        case divList _isFunction_T texpr of -- 関数探し
        Just (TFunc (Fun (Function { args = as, ret_t = rt })), texpr1, texpr2) -- 関数
            | tpchk -> _functionTypeEval binds $ texpr1 ++ [TWrd rt] ++ rest
            | otherwise -> (Err "Mismatch input type of function.", binds)
            where
                len = length as
                args = take len texpr2
                rest = drop len texpr2
                at = map (\ (t, _) -> t) as
                tpchk = (sum $ map (\ (t1, TWrd t2) -> if t1 == t2 then 0 else 1) $ zip at args) == 0
        Just (TFunc (Operator (opName, FuncOp (len, _))), texpr1, texpr2) -> -- 関数オペレータ
            let args = take len texpr2
                rest = drop len texpr2
                FuncOp_T (_, op) = _typeFunction opName
            in case op args of
                    TErr s -> (Err s, binds)
                    TWrd t -> _functionTypeEval binds $ texpr1 ++ [TWrd t] ++ rest
        Nothing ->
            case _iterOps_T _opls_dec texpr of -- オペレータ探し
            Just strop -> -- オペレータが見つかった
                let Just (TFunc (Operator (opName, _)), texpr1, (y: rest2)) = divListBy (TFunc (Operator strop)) texpr
                    op = _typeFunction opName
                in _functionTypeEval binds $ _applyOp_T op texpr1 y rest2
            Nothing -> -- オペレータ見つからなかった
                case texpr of
                    [] -> (Null, binds)
                    (TPreList tpls : []) ->
                        let ls = map (\ tex -> fst $ _functionTypeEval binds tex) tpls
                        in
                            case head ls of
                                Err s -> (Err s, binds)
                                Type t -> (Type (T_List t), binds)
                                _ -> (Err "TypeCheck: Parse error.", binds)
                    (TWrd w : []) -> (Type w, binds)
                    _ -> (Err $ "Parse failed: " ++ show texpr, binds)


module Eval where
import Parser
import Types
import Ops
import Utils
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

_iterOps :: EvalMode -> [StrOp] -> Exp -> Maybe (StrOp, Exp, Exp)
_iterOps mode strops expr =
    let f = case mode of
                M_Normal -> Func . Operator
                M_TypeCheck -> TypeCheck . T_Func . T_Operator
    in case dropWhile (\ sop -> divListBy (f sop) expr == Nothing) strops of
        [] -> Nothing
        (sop: _) -> 
            let Just (_, ws1, ws2) = divListBy (f sop) expr
            in case mode of
                M_Normal -> Just (sop, ws1, ws2)
                M_TypeCheck ->
                    let opName = fst sop
                        op_t = _typeFunction opName
                    in Just ((opName, op_t), ws1, ws2)

_numIn :: Wrd -> Exp -> Integer
_numIn w ex = sum $ map (\ v -> if v == w then 1 else 0) ex 

_isReplaceable :: [Bind] -> Exp -> Bool
_isReplaceable binds ex = (<) 0 $ sum $ map (\ bind -> _numIn (Tobe $ identifier bind) ex) binds

_isFunction :: EvalMode -> Wrd -> Bool
_isFunction M_Normal (Func _) = True
_isFunction M_TypeCheck (TypeCheck (T_Func {})) = True
_isFunction _ _ = False

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

_evalWrd :: EvalMode -> Wrd -> Wrd
_evalWrd mode (Tobe s) =
    case myReadBool $ myReadDouble $ myReadInt (Left s) of
        Left _ -> Tobe s
        Right w ->
            case mode of
                M_Normal -> w
                M_TypeCheck -> TypeCheck $ _getType w
_evalWrd mode w = w
    
_subOp :: EvalMode -> StrOp -> Exp -> Exp
_subOp mode (opName, op) expr =
    case divListBy (Tobe opName) expr of
        Nothing -> expr
        Just (_, ws1, ws2) ->
            case mode of
                M_Normal -> _subOp mode (opName, op) $ ws1 ++ [Func (Operator (opName, op))] ++ ws2
                M_TypeCheck ->
                    let op_t = _typeFunction opName 
                    in _subOp mode (opName, op) $ ws1 ++ [TypeCheck $ T_Func (T_Operator (opName, op_t))] ++ ws2

_mulSubOp :: EvalMode -> [StrOp] -> Exp -> Exp
_mulSubOp mode (strop: []) expr = _subOp mode strop expr
_mulSubOp mode (strop: strops) expr = _mulSubOp mode strops $ _subOp mode strop expr

_applyOp :: StrOp -> Exp -> Exp -> Exp
_applyOp (opName, op) ws1 (y : rest2) =
    case op of
    BinOp binop -> 
        let x = last ws1
            rest1 = init ws1
        in rest1 ++ [binop x y] ++ rest2
    UnOp unop ->
        ws1 ++ [unop y] ++ rest2
    FuncOp (nargs, fnop) ->
        let args = take nargs (y : rest2)
            rest = drop nargs (y : rest2)
        in ws1 ++ [fnop args] ++ rest

_applyFunction :: EvalMode -> Wrd -> Exp -> Exp -> Either String Exp
_applyFunction mode f_w expr1 expr2 =
    case mode of
        M_Normal ->
            let (Func (Fun f)) = f_w
                l = length $ args f
                as = take l expr2
                rest = drop l expr2
            in case (_macroGen f) as of
                Left s -> Left s
                Right rslt -> Right $ expr1 ++ [Tobe "("] ++ rslt ++ [Tobe ")"] ++ rest
        M_TypeCheck ->
            let TypeCheck (T_Func (T_Function { args_t = as_t, return_t = rt })) = f_w
                l = length as_t
                as = take l expr2
                rest = drop l expr2
                binds_tc = map (\ (t, a) -> Bind { identifier = "", value = a, vtype = t }) $ zip as_t as
            in case _typeCheck mode binds_tc of
                    Just s -> Left s
                    Nothing -> Right $ expr1 ++ [Tobe "("] ++ [TypeCheck rt] ++ [Tobe ")"] ++ rest

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
                            in Right $ TP $ T_Func $ T_Function { args_t = as_t, return_t = toType t_r }
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
            Right (TP (T_Func f_t)) ->
                let ts = args_t f_t
                    rt = return_t f_t
                in case divListBy (Tobe "->") rest2 of
                    Nothing -> (Err "Function: Syntax error, missing `->`", binds)
                    Just (_, as, expr2)
                        | length ts /= length as -> (Err "Function: Mismatch numbers of types and arguments.", binds)
                        | otherwise ->
                            let ass = map (\ (Tobe a) -> a) as
                                f = Function { args = zip ts ass, ret_t = rt, ret = expr2 }
                            in case mode of
                                M_TypeCheck -> (TypeCheck (T_Func f_t), binds)
                                M_Normal ->
                                    case functionTypeCheck binds f of
                                    Err s -> (Err s, binds)
                                    _ -> (Func $ Fun f, binds)
        _ -> (Err "Function: Syntax error.", binds)
_eval mode binds (Tobe "let" : rest) = _bind mode binds rest
_eval mode binds (Tobe "letn" : rest) =
    let (_, binds2) = _bind mode binds rest
    in (Null, binds2)
_eval mode binds (Tobe "if" : rest) =
    case divListBy (Tobe "then") rest of
    Nothing -> (Err "Syntax error: Missing `then` keyword in `if` statement.", binds)
    Just (_, cond, rest2) ->
        case divListBy (Tobe "else") rest2 of
        Nothing -> (Err "Syntax error: Missing `else` keyword in `if` statement.", binds)
        Just (_, thn, els) ->
            case mode of
            M_Normal ->
                case _eval mode binds cond of
                (Bool truth, binds2) ->
                    if truth then (fst $ _eval mode binds2 thn, binds) else (fst $ _eval mode binds2 els, binds)
                (w, _) -> (Err $ "Entered a non-boolean value into `if` statement: " ++ (show w), binds)
            M_TypeCheck ->
                case _eval mode binds thn of
                    (Err s, _) -> (Err s, binds)
                    (TypeCheck t1, binds1) ->
                        case _eval mode binds1 els of
                            (Err s, _) -> (Err s, binds)
                            (TypeCheck t2, binds2)
                                | typeEq t1 t2 -> if t1 == T_Any then (TypeCheck t2, binds2) else (TypeCheck t1, binds2)
                                | otherwise -> (Err $ "Mismatch of return type in `if` statement: Return type of `then` part is `" ++ (show t1) ++ "`, but that of `else` part is `" ++ (show t2) ++ "`" , binds)
                    (w, _) -> (Err $ "Unexpected return value: " ++ (show w) , binds)
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
                let ls = map (fst . _evalFunctions mode binds) $ divListInto (Tobe ",") expr
                in case ls of
                    (w : []) -> (w, binds)
                    _ -> (Contents ls, binds)

_evalFunctions :: EvalMode -> [Bind] -> Exp -> (Wrd, [Bind]) -- 初期状態で第一引数は空リスト
_evalFunctions mode binds expr =
    let ws = map (_evalWrd mode) $ _mulSubOp mode _opls_dec $ _mulSubst expr binds
    in
        case divList (_isFunction mode) ws of -- 関数探し
        Just (Func (Fun f), expr1, expr2) -> -- 関数
            case _applyFunction mode (Func (Fun f)) expr1 expr2 of
                Right rslt -> _eval mode binds rslt
                Left s -> (Err s, binds)
        Just (TypeCheck (T_Func (T_Function { args_t = as_t, return_t = rt })), expr1, expr2) ->
            case _applyFunction mode (TypeCheck (T_Func (T_Function { args_t = as_t, return_t = rt }))) expr1 expr2 of
                Right rslt -> _eval mode binds rslt
                Left s -> (Err s, binds)
        Just (Func (Operator (opName, FuncOp fnop)), ws1, ws2) -> -- 関数オペレータ
            _eval mode binds $ _applyOp (opName, FuncOp fnop) ws1 ws2
        Just (TypeCheck(T_Func (T_Operator (opName, FuncOp fnop))), ws1, ws2) ->
            _eval mode binds $ _applyOp (opName, FuncOp fnop) ws1 ws2
        _ ->
            case _iterOps mode _opls_dec ws of -- オペレータ探し
            Just (sop, ws1, ws2) -> -- オペレータが見つかった
                _eval mode binds $ _applyOp sop ws1 ws2
            Nothing -> -- オペレータ見つからなかった
                case ws of
                    [] -> (Null, binds)
                    (PreList pls : []) ->
                        let ls = map (\ expr -> fst $ _eval mode binds expr) pls
                        in (List ls, binds)
                    (Tobe s: []) -> (Err $ "Unknown keyword: " ++ s, binds)
                    (w : []) -> (w, binds)
                    _ -> (Err $ "Parse failed: " ++ show ws, binds)

functionTypeCheck :: [Bind] -> Function -> Wrd
functionTypeCheck binds f = fst $ _eval M_TypeCheck binds $ _typeExprGen f

_typeExprGen :: Function -> Exp
_typeExprGen (Function { args = as, ret_t = rt, ret = expr }) =
    let 
        binds = map (\ (t, id) -> Bind {identifier = id, value = TypeCheck t, vtype = T_TypeCheck }) as
    in _mulSubst expr binds

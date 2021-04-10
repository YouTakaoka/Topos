module Eval where
import Parser
import Types
import Ops
import Utils
import Text.Read
import Debug.Trace

_findParenthesis :: Exp -> Integer -> Wrd -> Wrd -> Parenthesis
_findParenthesis ws cnt b e = -- cnt は初期値 -1
    case divList (\ w -> w == b || w == e) ws of
    Nothing -> ParNotFound
    Just (d, ws1, ws2)
        | (d, cnt) == (b, -1) ->
            case _findParenthesis ws2 1 b e of
            ParFound (_, ex2, ex3) -> ParFound (ws1, ex2, ex3)
            ParError s -> ParError s
            ParNotFound -> ParError ("End of parenthesis not found:" ++ show ws2)
        | (d, cnt) == (e, 1) ->
            ParFound ([], ws1, ws2)
        | otherwise ->
            case _findParenthesis ws2 (if d == b then cnt + 1 else cnt - 1) b e of
            ParFound (_, ex2, ex3) -> ParFound ([], ws1 ++ [d] ++ ex2, ex3)
            ParNotFound -> ParError "Mismatch of parenthesis."

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
_isFunction M_Normal (Func Fun {}) = True
_isFunction M_Normal (Func (Operator (_, FuncOp _))) = True
_isFunction M_TypeCheck (TypeCheck (T_Func T_Function {})) = True
_isFunction M_TypeCheck (TypeCheck (T_Func (T_Operator (_, FuncOp _)))) = True
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
_evalWrd M_TypeCheck (Str s) = TypeCheck T_String
_evalWrd _ w = w
    
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

_examineInput :: Wrd -> Maybe Error
_examineInput (Tobe s) = Just $ UnknownKeywordError s
_examineInput _ = Nothing

_examineInputList :: Exp -> Maybe Error
_examineInputList expr =
    let ls = map _examineInput expr
    in case divList (\ x -> case x of { Just _ -> True ; Nothing -> False }) ls of
        Just (Just e, _, _) -> Just e
        Nothing -> Nothing

_applyOp :: StrOp -> Exp -> Exp -> Either Error Exp -- TODO: 引数の型がTobe型でないかチェック
_applyOp (opName, op) ws1 (y : rest2) =
    case op of
    BinOp binop -> 
        let x = last ws1
            rest1 = init ws1
        in case binop x y of
            Left e -> Left e
            Right w -> Right $ rest1 ++ [w] ++ rest2
    UnOp unop ->
        case unop y of
            Left e -> Left e
            Right w -> Right $ ws1 ++ [w] ++ rest2
    FuncOp (nargs, fnop)
        | length (y : rest2) < nargs -> Left $ ParseError $ "Supplied too few arguments to function operator `" ++ opName ++ "`"
        | otherwise ->
            let args = take nargs (y : rest2)
                rest = drop nargs (y : rest2)
            in case fnop args of
                Left e -> Left e
                Right w -> Right $ ws1 ++ [w] ++ rest

_applyFunction :: EvalMode -> Wrd -> Exp -> Exp -> Either Error Exp
_applyFunction mode f_w expr1 expr2 =
    case mode of
        M_Normal ->
            let (Func (Fun f)) = f_w
                l = length $ args f
                as = take l expr2
                rest = drop l expr2
            in case _macroGen f as of
                Left e -> Left e
                Right rslt -> Right $ expr1 ++ [Tobe "("] ++ rslt ++ [Tobe ")"] ++ rest
        M_TypeCheck ->
            let TypeCheck (T_Func (T_Function { args_t = as_t, return_t = rt })) = f_w
                l = length as_t
                as = take l expr2
                rest = drop l expr2
                binds_tc = map (\ (t, a) -> Bind { identifier = "", value = a, vtype = t }) $ zip as_t as
            in case _typeCheck mode binds_tc of
                    Just e -> Left e
                    Nothing -> Right $ expr1 ++ [Tobe "("] ++ [TypeCheck rt] ++ [Tobe ")"] ++ rest

_evalNewBinds :: EvalMode -> [Bind] -> [Bind] -> Exp -> Result
_evalNewBinds mode binds_new binds_old expr =
    case _eval mode binds_new expr of
        Error e -> Error e
        Result (w, _) -> Result (w, binds_old)

_bind :: EvalMode -> [Bind] -> Exp -> Result
_bind mode binds rest =
    case divListBy (Tobe "=") rest of
        Nothing ->
            Error $ SyntaxError "Missing `=` in `let` statement."
        Just (_, [Tobe w], expr) ->
            case _eval mode binds expr of
                Error e -> Error e
                Result (rhs, _) ->
                    Result (rhs, Bind { identifier = w, value = rhs, vtype = _getType rhs } : binds)
        _ -> Error $ SyntaxError "Specify only one symbol to bind value."

data TypeOrTypeContents = TP Type | TContents [Type]

_evalFunctionSignature :: Exp -> Either Error TypeOrTypeContents
_evalFunctionSignature expr = -- exprは<>の中身
    case divListBy (Tobe "Function") expr of
        Nothing ->
            case findParenthesis expr "(" ")" of
                ParError s -> Left $ ParseError s
                ParFound (expr2, expr3, expr4) ->
                    case _evalFunctionSignature expr3 of
                        Left s -> Left s
                        Right (TContents ts) ->
                            _evalFunctionSignature $ expr2 ++ [Type (T_Tuple ts)] ++ expr4
                ParNotFound ->
                    case divListBy (Tobe "->") expr of
                        Nothing ->
                            Right $ TContents $ map toType $ divListInto (Tobe ",") expr
                        Just (_, expr1, t_r) ->
                            let as_t = map toType $ divListInto (Tobe ",") expr1
                            in Right $ TP $ T_Func $ T_Function { args_t = as_t, return_t = toType t_r }
        Just (_, expr1, expr2) ->
            case findParenthesis expr2 "<" ">" of
                ParError s -> Left $ ParseError s
                ParNotFound -> Left $ SyntaxError "`<` not found after `Function`"
                ParFound ([], expr3, expr4) ->
                    case _evalFunctionSignature expr3 of
                        Left s -> Left s
                        Right (TP t) -> _evalFunctionSignature $ expr1 ++ [Type t] ++ expr4
                        _ -> Left $ SyntaxError "Missing `->` in `Function` statement"
                _ -> Left $ SyntaxError "`<` must follow just after `Function`"

_evalEach :: EvalMode -> [Bind] -> [Exp] -> Either Error Exp
_evalEach mode binds ls =
    let res_l = map (_eval mode binds) ls
    in case divList (\ x -> case x of { Error _ -> True ; _ -> False }) res_l of
        Nothing -> Right $ map (\ (Result (w, _)) -> w) res_l
        Just (Error e, _, _) -> Left e

_evalFunctionsEach :: EvalMode -> [Bind] -> [Exp] -> Either Error Exp
_evalFunctionsEach mode binds ls =
    let res_l = map (_evalFunctions mode binds) ls
    in case divList (\ x -> case x of { Error _ -> True ; _ -> False }) res_l of
        Nothing -> Right $ map (\ (Result (w, _)) -> w) res_l
        Just (Error e, _, _) -> Left e

_eval :: EvalMode -> [Bind] -> Exp -> Result
_eval mode binds (Tobe "Function" : rest) =
    case divListBy (Tobe ":") rest of
    Nothing -> Error $ SyntaxError "Missing `:` in `Function` statement."
    Just (_, rest1, rest2) ->
        case findParenthesis rest1 "<" ">" of
        ParError s -> Error $ ParseError s
        ParNotFound -> Error $ SyntaxError "`<` must follow just after `Function`"
        ParFound ([], expr1, []) ->
            case _evalFunctionSignature expr1 of
            Left e -> Error e
            Right (TP (T_Func f_t)) ->
                let ts = args_t f_t
                    rt = return_t f_t
                in case divListBy (Tobe "->") rest2 of
                    Nothing -> Error $ SyntaxError "Missing `->` in `Function` statement."
                    Just (_, as, expr2)
                        | length ts /= length as -> Error $ SyntaxError "Mismatch numbers of types and arguments in `Function` statement."
                        | otherwise ->
                            let ass = map (\ (Tobe a) -> a) as
                                f = Function { args = zip ts ass, ret_t = rt, ret = expr2 }
                            in case mode of
                                M_TypeCheck -> Result (TypeCheck (T_Func f_t), binds)
                                M_Normal ->
                                    case functionTypeCheck binds f of
                                    Left e -> Error e
                                    _ -> Result (Func $ Fun f, binds)
        _ -> Error $ SyntaxError "Parhaps needless string got into `Function` statement."
_eval mode binds (Tobe "define" : rest) =
    case mode of
        M_TypeCheck -> Error $ SyntaxError "Cannot use `define` statement in function literal."
        M_Normal ->
            case divListBy (Tobe "as") rest of
                Nothing -> Error $ SyntaxError "Keyword `as` not found in `define` statement."
                Just (_, [Tobe id], Tobe "Function" : rest2) ->
                    let rest2' = (Tobe "Function" : rest2)
                        bind = Bind {identifier=id, value=ToEval rest2', vtype=T_ToEval }
                    in _eval mode (bind : binds) rest2'
                _ -> Error $ SyntaxError "`Function` statement must follow just after `as` keyword."
_eval mode binds (Tobe "let" : rest) = _bind mode binds rest
_eval mode binds (Tobe "letn" : rest) =
    case _bind mode binds rest of
        Error e -> Error e
        Result (_, binds2) -> Result (Null, binds2)
_eval mode binds (Tobe "if" : rest) =
    case divListBy (Tobe "then") rest of
    Nothing -> Error $ SyntaxError "Missing `then` keyword in `if` statement."
    Just (_, cond, rest2) ->
        case divListBy (Tobe "else") rest2 of
        Nothing -> Error $ SyntaxError "Missing `else` keyword in `if` statement."
        Just (_, thn, els) ->
            case mode of
            M_Normal ->
                case _eval mode binds cond of
                Result (Bool truth, binds2) ->
                    _evalNewBinds mode binds2 binds $ if truth then  thn else els
                Result (w, _) -> Error $ SyntaxError $ "Entered a non-boolean value into `if` statement: " ++ show w
                Error e -> Error e
            M_TypeCheck ->
                case _eval mode binds thn of
                    Error e -> Error e
                    Result (TypeCheck t1, binds) ->
                        case _eval mode binds els of
                            Error e -> Error e
                            Result (TypeCheck t2, binds)
                                | typeEq t1 t2 -> if t1 == T_Any then Result (TypeCheck t2, binds) else Result (TypeCheck t1, binds)
                                | otherwise -> Error $ SyntaxError $ "Mismatch of return type in `if` statement: Return type of `then` part is `" ++ (show t1) ++ "`, but that of `else` part is `" ++ (show t2) ++ "`"
                    Result (w, _) -> Error $ InternalError $ "Unexpected return type: " ++ show (_getType w)
_eval mode binds expr =
    case divListBy (Tobe "#") expr of --コメント探し
    Just (_, expr1, expr2) ->
        _eval mode binds expr1
    Nothing ->
        case findParenthesis expr "(" ")" of
        ParError s -> Error $ ParseError s
        ParFound (expr1, expr2, expr3) ->
            case _eval mode binds expr2 of
                Result (Contents ls, _) -> _eval mode binds $ expr1 ++ [Tuple ls] ++ expr3
                Result (w, _) -> _eval mode binds $ expr1 ++ [w] ++ expr3
                Error e -> Error e
        ParNotFound ->
            case findParenthesis expr "[" "]" of
            ParError s -> Error $ ParseError s
            ParFound (expr1, expr2, expr3) ->
                case _eval mode binds expr2 of
                Result (Contents ls, _) ->
                    case toList ls of
                        Left e -> Error e
                        Right wls -> _eval mode binds $ expr1 ++ [wls] ++ expr3
                Result (w, _) -> _eval mode binds $ expr1 ++ [w] ++ expr3
                Error e -> Error e
            ParNotFound ->
                case _evalFunctionsEach mode binds $ divListInto (Tobe ",") expr of
                    Left e -> Error e
                    Right [w] -> Result (w, binds)
                    Right ls -> Result (Contents ls, binds)

_evalFunctions :: EvalMode -> [Bind] -> Exp -> Result -- 初期状態で第一引数は空リスト
_evalFunctions mode binds expr =
    let ws = map (_evalWrd mode) $ _mulSubOp mode _opls_dec $ _mulSubst expr binds
    in case _examineInputList ws of
        Just e -> Error e
        Nothing ->
            case divList (\ x -> case x of { ToEval _ -> True ; _ -> False }) ws of
            Just (ToEval to_eval, expr1, expr2) ->
                case _eval mode binds to_eval of
                Error e -> Error e
                Result (w, binds2) -> _eval mode binds2 $ expr1 ++ [w] ++ expr2
            Nothing ->
                case divList (\ x -> case x of { PreList _ -> True ; _ -> False }) ws of
                Just (PreList pls, expr1, expr2) ->
                    case _evalEach mode binds pls of
                        Left e -> Error e
                        Right ls ->
                            case toList ls of
                                Left e -> Error e
                                Right wls -> _eval mode binds $ expr1 ++ [wls] ++ expr2
                Nothing ->
                    case divList (_isFunction mode) ws of -- 関数探し
                    Just (Func (Fun f), expr1, expr2) -> -- 関数
                        case _applyFunction mode (Func (Fun f)) expr1 expr2 of
                            Right rslt -> _eval mode binds rslt
                            Left e -> Error e
                    Just (TypeCheck (T_Func (T_Function { args_t = as_t, return_t = rt })), expr1, expr2) ->
                        case _applyFunction mode (TypeCheck (T_Func (T_Function { args_t = as_t, return_t = rt }))) expr1 expr2 of
                            Right rslt -> _eval mode binds rslt
                            Left e -> Error e
                    Just (Func (Operator (opName, FuncOp fnop)), ws1, ws2) -> -- 関数オペレータ
                        case _applyOp (opName, FuncOp fnop) ws1 ws2 of
                            Left e -> Error e
                            Right ex -> _eval mode binds ex
                    Just (TypeCheck(T_Func (T_Operator (opName, FuncOp fnop))), ws1, ws2) ->
                        case _applyOp (opName, FuncOp fnop) ws1 ws2 of
                            Left e -> Error e
                            Right ex -> _eval mode binds ex
                    Nothing ->
                        case _iterOps mode _opls_dec ws of -- オペレータ探し
                        Just (sop, ws1, ws2) -> -- オペレータが見つかった
                            case _applyOp sop ws1 ws2 of
                                Left e -> Error e
                                Right ex -> _eval mode binds ex
                        Nothing -> -- オペレータ見つからなかった
                            case ws of
                                [] -> Result (Null, binds)
                                (Tobe s: []) -> Error $ UnknownKeywordError s
                                [w] -> Result (w, binds)
                                _ -> Error $ InternalError $ "Evaluation failed: " ++ show ws

functionTypeCheck :: [Bind] -> Function -> Either Error Wrd
functionTypeCheck binds f = 
    case _eval M_TypeCheck binds $ _typeExprGen f of
        Error e -> Left e
        Result (w, _) -> Right w

_typeExprGen :: Function -> Exp
_typeExprGen (Function { args = as, ret_t = rt, ret = expr }) =
    let 
        binds = map (\ (t, id) -> Bind {identifier = id, value = TypeCheck t, vtype = T_TypeCheck }) as
    in _mulSubst expr binds

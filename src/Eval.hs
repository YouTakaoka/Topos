module Eval where
import Parser
import Types
import Ops
import Utils
import Text.Read
import Debug.Trace
import Data.Maybe

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

_opLvMatch :: EvalMode -> Int -> Wrd -> Bool
_opLvMatch M_Normal lv (Func Operator { priority=prt }) = prt == lv
_opLvMatch M_TypeCheck lv (TypeCheck (T_Func T_Operator { priority_t=prt })) = prt == lv
_opLvMatch _ _ _ = False

_iterOps :: EvalMode -> Int -> Exp -> Maybe (Wrd, Exp, Exp)
_iterOps _ (-1) _ = Nothing
_iterOps mode lv expr = -- lv: 初期値9
    case divList (_opLvMatch mode lv) expr of
        Nothing -> _iterOps mode (lv - 1) expr
        Just (fw, expr1, expr2) -> Just (fw, expr1, expr2)

_isFunction :: EvalMode -> Wrd -> Bool
_isFunction M_Normal (Func Function {}) = True
_isFunction M_Normal (Func Operator { operator=FuncOp _}) = True
_isFunction M_TypeCheck (TypeCheck (T_Func T_Function {})) = True
_isFunction M_TypeCheck (TypeCheck (T_Func T_Operator { operator_sig=FuncSig _})) = True
_isFunction _ _ = False

_isOp :: Wrd -> Bool
_isOp (Func Operator {}) = True
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
_evalWrd M_TypeCheck (Func f) = TypeCheck $ _getType $ Func f
_evalWrd _ w = w
    
_subOp :: EvalMode -> Function -> Exp -> Exp
_subOp mode op expr =
    case divListBy (Tobe $ opName op) expr of
        Nothing -> expr
        Just (_, ws1, ws2) ->
            case mode of
                M_Normal -> _subOp mode op $ ws1 ++ [Func op] ++ ws2
                M_TypeCheck -> _subOp mode op $ ws1 ++ [TypeCheck $ _getType $ Func op] ++ ws2

_mulSubOp :: EvalMode -> [Function] -> Exp -> Exp
_mulSubOp mode [strop] expr = _subOp mode strop expr
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

_applyOp :: String -> Op -> Exp -> Exp -> Either Error Exp -- TODO: 引数の型がTobe型でないかチェック
_applyOp name op ws1 (y : rest2) =
    case op of
    BinOp (binop, sigs)
        | null ws1 ->
            case validateBinSig (T_Null, _getType y) sigs of
            Left te -> Left $ addToTypeErrorMessage te $ "`" ++ name ++ "`: "
            Right _ ->
                case binop Null y of
                    Left e -> Left e
                    Right w -> Right $ w: rest2
        | otherwise ->
            let x = last ws1
                rest1 = init ws1
            in case validateBinSig (_getType x, _getType y) sigs of
                Left te -> Left $ addToTypeErrorMessage te $ "`" ++ name ++ "`: "
                Right _ ->
                    case binop x y of
                    Left e -> Left e
                    Right w -> Right $ rest1 ++ [w] ++ rest2
    UnOp (unop, sigs) ->
        case validateUnSig (_getType y) sigs of
        Left te -> Left $ addToTypeErrorMessage te $ "`" ++ name ++ "`: "
        Right _ ->
            case unop y of
                Left e -> Left e
                Right w -> Right $ ws1 ++ [w] ++ rest2
    FuncOp (fnop, sig)
        | length (y : rest2) < nargs ->
            Left $ ParseError $ "Supplied too few arguments to function `" ++ name ++ "`"
        | otherwise ->
            let args = take nargs (y : rest2)
                rest = drop nargs (y : rest2)
            in case validateFuncSig (map _getType args) (fst sig) (snd sig) of
                Left te -> Left $ addToTypeErrorMessage te $ "`" ++ name ++ "`: "
                Right _ ->
                    case fnop args of
                    Left e -> Left e
                    Right w -> Right $ ws1 ++ [w] ++ rest
        where
            nargs = length $ fst sig
_applyOp name op _ [] =
    case op of
        BinOp _ -> Left $ ParseError $ "Missing second argument for binary operator `" ++ name ++ "`"
        UnOp _ -> Left $ ParseError $ "Missing argument for Unary operator `" ++ name ++ "`"
        FuncOp _ -> Left $ ParseError $ "Missing arguments for function `" ++ name ++ "`"

validateOp :: String -> OperatorSig -> Exp -> Exp -> Either Error Exp
validateOp name op_sig ws1 (TypeCheck y_t: rest2) =
    case op_sig of
        BinSig binsigs ->
            if null ws1 then
                case validateBinSig (T_Null, y_t) binsigs of
                    Right t -> Right $ TypeCheck t : rest2
                    Left te -> Left $ addToTypeErrorMessage te $ "`" ++ name ++ "`: "
            else
                let TypeCheck x_t = last ws1
                    rest1 = init ws1
                in case validateBinSig (x_t, y_t) binsigs of
                    Right t -> Right $ rest1 ++ [TypeCheck t] ++ rest2
                    Left te -> Left $ addToTypeErrorMessage te $ "`" ++ name ++ "`: "
        UnSig unsigs ->
            case validateUnSig y_t unsigs of
                Right t -> Right $ ws1 ++ [TypeCheck t] ++ rest2
                Left te -> Left $ addToTypeErrorMessage te $ "`" ++ name ++ "`: "
        FuncSig (ts, t_r) ->
            let l = length ts
                ws2 = TypeCheck y_t: rest2
            in if length ws2 < l
                then
                    Left $ ParseError $ "Supplied too few arguments to function `" ++ name ++ "`"
                else
                    let ts2 = map (\ (TypeCheck x) -> x) ws2
                    in case validateFuncSig ts2 ts t_r of
                        Right t -> Right $ ws1 ++ [TypeCheck t] ++ drop l ws2
                        Left te -> Left $ addToTypeErrorMessage te $ "`" ++ name ++ "`: "

_applyFunction :: Function -> Exp -> Exp -> Either Error Exp
_applyFunction f expr1 expr2 =
    let l = length $ args f
    in if length expr2 < l
        then Left $ ParseError $
                "Function `" ++ funcName f ++ "`: Too few arguments supplied. Expected " ++ show l ++ ", but given only " ++ show (length expr2) ++ "."
        else let
                as = take l expr2
                rest = drop l expr2
                T_Func T_Function { funcName_t=name, args_t = as_t, return_t = rt } = _getType $ Func f
            in case validateFuncSig (map _getType as) as_t rt of
                Right _ -> Right $ expr1 ++ [Tobe "("] ++ _macroGen f as ++ [Tobe ")"] ++ rest
                Left te -> Left $ addToTypeErrorMessage te $ "Function `" ++ name ++ "`: "

_validateFunction :: T_Function -> Exp -> Exp -> Either Error Exp
_validateFunction T_Function { funcName_t=name, args_t = as_t, return_t = rt } expr1 expr2 =
    let l = length as_t
    in if length expr2 < l
        then Left $ ParseError $
                "Function `" ++ name ++ "`: Too few arguments supplied. Expected " ++ show l ++ ", but given only " ++ show (length expr2) ++ "."
        else let
                as = take l expr2
                rest = drop l expr2
            in case validateFuncSig (map unveilTypeCheck as) as_t rt of
                    Left te -> Left $ addToTypeErrorMessage te $ "Function `" ++ name ++ "`: "
                    Right t -> Right $ expr1 ++ [Tobe "("] ++ [TypeCheck t] ++ [Tobe ")"] ++ rest

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

_evalFunctionSignature :: String -> Exp -> Either Error TypeOrTypeContents
_evalFunctionSignature name expr = -- exprは<>の中身
    case divListBy (Tobe "Function") expr of
        Nothing ->
            case findParenthesis expr "(" ")" of
                ParError s -> Left $ ParseError s
                ParFound (expr2, expr3, expr4) ->
                    case _evalFunctionSignature name expr3 of
                        Left s -> Left s
                        Right (TContents ts) ->
                            _evalFunctionSignature name $ expr2 ++ [Type (T_Tuple ts)] ++ expr4
                ParNotFound ->
                    case divListBy (Tobe "->") expr of
                        Nothing ->
                            let ls = map toType $ divListInto (Tobe ",") expr
                            in case divList (\ x -> case x of { Left _ -> True ; Right _ -> False }) ls of
                                Nothing -> Right $ TContents $ map (\ (Right t) -> t) ls
                                Just (Left e, _, _) -> Left e
                        Just (_, expr1, t_r) ->
                            let ls = map toType $ divListInto (Tobe ",") expr1
                            in case divList (\ x -> case x of { Left _ -> True ; Right _ -> False }) ls of
                                Nothing ->
                                    let as_t = map (\ (Right t) -> t) ls
                                    in case toType t_r of
                                        Right rt -> Right $ TP $ T_Func $ T_Function { funcName_t=name, args_t = as_t, return_t = rt, priority_ft=9 }
                                        Left e -> Left e
                                Just (Left e, _, _) -> Left e
        Just (_, expr1, expr2) ->
            case findParenthesis expr2 "<" ">" of
                ParError s -> Left $ ParseError s
                ParNotFound -> Left $ SyntaxError "`<` not found after `Function`"
                ParFound ([], expr3, expr4) ->
                    case _evalFunctionSignature name expr3 of
                        Left s -> Left s
                        Right (TP t) -> _evalFunctionSignature name $ expr1 ++ [Type t] ++ expr4
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

compileFunction :: EvalMode -> [Bind] -> Exp -> String -> Either Error Function
compileFunction mode binds rest name =
    case divListBy (Tobe ":") rest of
    Nothing -> Left $ SyntaxError "Missing `:` in `Function` statement."
    Just (_, rest1, rest2) ->
        case findParenthesis rest1 "<" ">" of
        ParError s -> Left $ ParseError s
        ParNotFound -> Left $ SyntaxError "`<` must follow just after `Function`"
        ParFound ([], expr1, []) ->
            case _evalFunctionSignature name expr1 of
            Left e -> Left e
            Right (TP (T_Func f_t)) ->
                let ts = args_t f_t
                    rt = return_t f_t
                in case divListBy (Tobe "->") rest2 of
                    Nothing -> Left $ SyntaxError "Missing `->` in `Function` statement."
                    Just (_, as, expr2)
                        | length ts /= length as -> Left $ SyntaxError "Mismatch numbers of types and arguments in `Function` statement."
                        | otherwise ->
                            let ass = map (\ (Tobe a) -> a) as
                                f = Function { funcName=name, args = zip ts ass, ret_t = rt, ret = expr2, priority_f=9 }
                            in Right f
        _ -> Left $ SyntaxError "Parhaps needless words got into `Function` statement."

_eval :: EvalMode -> [Bind] -> Exp -> Result
_eval mode binds (Tobe "Function" : rest) =
    case compileFunction mode binds rest "(anonymous)" of
        Left e -> Error e
        Right f -> functionTypeCheck mode binds f
_eval mode binds (Tobe "define" : rest) =
    case mode of
        M_TypeCheck -> Error $ SyntaxError "Cannot use `define` statement in function literal."
        M_Normal ->
            case divListBy (Tobe "as") rest of
                Nothing -> Error $ SyntaxError "Keyword `as` not found in `define` statement."
                Just (_, [Tobe id], Tobe "Function" : rest2) ->
                    case compileFunction mode binds rest2 id of
                        Left e -> Error e
                        Right f ->
                            let b = Bind { identifier=id, vtype=_getType $ Func f, value=Func f }
                            in functionTypeCheck mode (b:binds) f
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
                    _evalNewBinds mode binds2 binds $ if truth then thn else els
                Result (w, _) -> Error $ SyntaxError $ "Entered a non-boolean value into `if` statement: " ++ show w
                Error e -> Error e
            M_TypeCheck ->
                case _eval mode binds thn of
                    Error e -> Error e
                    Result (TypeCheck t1, binds) ->
                        case _eval mode binds els of
                            Error e -> Error e
                            Result (TypeCheck t2, binds)
                                | typeEq t1 t2 -> Result (TypeCheck t1, binds)
                                | otherwise -> Error $ TypeError { expected_types=[t1], got_type=t2,
                                    message_TE="Mismatch of return type in `if` statement: Return type of `then` part is `"
                                        ++ show t1 ++ "`, but that of `else` part is `" ++ show t2 ++ "`" }
                    Result (w, _) -> Error $ InternalError $ "Unexpected return type: " ++ show (_getType w)
_eval mode binds expr =
    case findParenthesis expr "(" ")" of
    ParError s -> Error $ ParseError s
    ParFound (expr1, expr2, expr3) ->
        case _eval mode binds expr2 of
            Result (Contents ls, _) -> _eval mode binds $ expr1 ++ [Tuple ls] ++ expr3
            Result (w, binds2) -> _eval mode binds2 $ expr1 ++ [w] ++ expr3
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
            Result (Null, _) -> _eval mode binds $ expr1 ++ [List []] ++ expr3
            Result (w, _) -> _eval mode binds $ expr1 ++ [List [w]] ++ expr3
            Error e -> Error e
        ParNotFound ->
            case _evalFunctionsEach mode binds $ divListInto (Tobe ",") expr of
                Left e -> Error e
                Right [] -> Result (Null, binds)
                Right [w] -> Result (w, binds)
                Right ls -> Result (Contents ls, binds)

_evalFunctions :: EvalMode -> [Bind] -> Exp -> Result -- 初期状態で第一引数は空リスト
_evalFunctions mode binds expr =
    let ws = _deleteAll Null $ map (_evalWrd mode) $ _mulSubOp mode _opls $ _mulSubst expr binds
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
                    Just (Func Function { funcName=name, args=as, ret_t=rt, ret=r, priority_f=prt }, expr1, expr2) -> -- 関数
                        case _applyFunction (Function { funcName=name, args=as, ret_t=rt, ret=r, priority_f=prt }) expr1 expr2 of
                            Right rslt -> _eval mode binds rslt
                            Left e -> Error e
                    Just (TypeCheck (T_Func tfunc), expr1, expr2) ->
                        case mode of
                            M_Normal -> Error $ InternalError "`TypeCheck` found in Normal mode."
                            M_TypeCheck ->
                                case _validateFunction tfunc expr1 expr2 of
                                    Right rslt -> _eval mode binds rslt
                                    Left e -> Error e
                    Just (Func Operator { opName=name, operator=op }, ws1, ws2) -> -- 関数オペレータ
                        case _applyOp name op ws1 ws2 of
                            Left e -> Error e
                            Right ex -> _eval mode binds ex
                    Nothing ->
                        case _iterOps mode 9 ws of -- オペレータ探し
                        Just (Func Operator { opName=name, operator=op }, ws1, ws2) -> -- オペレータが見つかった
                            case _applyOp name op ws1 ws2 of
                                Left e -> Error e
                                Right ex -> _eval mode binds ex
                        Just (TypeCheck (T_Func T_Operator { opName_t=name, operator_sig=sig }), ws1, ws2) ->
                            case validateOp name sig ws1 ws2 of
                                Left e -> Error e
                                Right ex -> _eval mode binds ex
                        Nothing -> -- オペレータ見つからなかった
                            case ws of
                                [] -> Result (Null, binds)
                                [Tobe s] -> Error $ UnknownKeywordError s
                                [w] -> Result (w, binds)
                                _ -> Error $ InternalError $ "Evaluation failed: " ++ show ws

unveilTypeCheck :: Wrd -> Type
unveilTypeCheck w =
    case w of
        TypeCheck t -> t
        List ls -> T_List (unveilTypeCheck $ head ls)
        Tuple ls -> T_Tuple (map unveilTypeCheck ls)
        _ -> _getType w

functionTypeCheck :: EvalMode -> [Bind] -> Function -> Result
functionTypeCheck mode binds f = 
    case mode of
        M_TypeCheck -> Result (TypeCheck (_getType $ Func f), binds)
        M_Normal ->
            case _eval M_TypeCheck binds $ _typeExprGen f of
            Error e -> Error e
            Result (w, _) ->
                let t = unveilTypeCheck w
                in if t == rt
                    then Result (Func f, binds)
                    else Error $ TypeError { expected_types=[rt], got_type=t,
                            message_TE="Function `" ++ funcName f ++ "`: Return type of function mismatch. Specified type is `" ++ show rt
                            ++ "` but Topos predicts `" ++ show t ++ "`" }
    where
        rt = ret_t f

_typeExprGen :: Function -> Exp
_typeExprGen Function { args = as, ret_t = rt, ret = expr } =
    let 
        binds = map (\ (t, id) -> Bind {identifier = id, value = TypeCheck t, vtype = T_TypeCheck }) as
    in _mulSubst expr binds

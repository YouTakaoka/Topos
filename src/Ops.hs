module Ops where
import Parser
import Types
import Data.List
import Debug.Trace

_opls :: [StrOp]  -- 優先順位の低い順に並べる
_opls = [
            ("print", _print),
            ("||", _or),
            ("&&", _and),
            ("!", _not),
            ("==", _eq),
            ("!=", _neq),
            (">", _gt),
            (">=", _geq),
            ("<", _lt),
            ("<=", _leq),
            ("+", _add),
            ("-", _sub),
            ("*", _mul),
            ("/", _div),
            ("succ", _succ),
            ("head", _head),
            ("tail", _tail),
            ("pop", _pop),
            ("isEmpty", _isEmpty),
            ("take", _take),
            ("seq", _seq),
            ("map", _map),
            ("fst", _fst),
            ("snd", _snd)
        ]

_opls_dec = reverse _opls

_typeFunction :: String -> Op
_typeFunction op
    | op == "*" = BinOp _mul_t
    | op == "+" = BinOp _add_t
    | op == "-" = BinOp _sub_t
    | op == "/" = BinOp _div_t
    | op == "||" = BinOp _or_t
    | op == "&&" = BinOp _and_t
    | op == "!" = UnOp _not_t
    | op == "==" = BinOp _eq_t
    | op == "!=" = BinOp _neq_t
    | op == ">" = BinOp _gt_t
    | op == ">=" = BinOp _geq_t
    | op == "<" = BinOp _lt_t
    | op == "<=" = BinOp _leq_t
    | op == "print" = UnOp _print_t
    | op == "succ" = UnOp _succ_t
    | op == "head" = UnOp _head_t
    | op == "tail" = UnOp _tail_t
    | op == "pop" = UnOp _pop_t
    | op == "isEmpty" = UnOp _isEmpty_t
    | op == "take" = FuncOp (2, _take_t)
    | op == "seq" = FuncOp (2, _seq_t)
    | op == "map" = FuncOp (2, _map_t)
    | op == "fst" = UnOp _fst_t
    | op == "snd" = UnOp _snd_t

_print0 :: Wrd -> Either Error Wrd
_print0 w = Right $ Print (show w)

_print :: Op
_print = UnOp _print0

_print_t :: Wrd -> Either Error Wrd
_print_t (TypeCheck _) = Right $ TypeCheck T_Print

_mul0 :: Wrd -> Wrd -> Either Error Wrd
_mul0 (Double x) (Double y) = Right $ Double (x * y)
_mul0 (Int x) (Int y) = Right $ Int (x * y)
_mul0 (Int x) (Double y) = Right $ Double ((fromIntegral x) * y)
_mul0 (Double x) (Int y) = Right $ Double (x * (fromIntegral y))
_mul0 x y = Left $ ValueError $ "`*`: Illegal input value: x=" ++ (show x) ++ ", y=" ++ (show y)

_div0 :: Wrd -> Wrd -> Either Error Wrd
_div0 _ (Int 0) = Left $ ValueError "Zero division error."
_div0 _ (Double 0.0) = Left $ ValueError "Zero division error."
_div0 (Double x) (Double y) = Right $ Double (x / y)
_div0 (Int x) (Int y) = Right $ Double ((fromIntegral x) / (fromIntegral y))
_div0 (Int x) (Double y) = Right $ Double ((fromIntegral x) / y)
_div0 (Double x) (Int y) = Right $ Double (x / (fromIntegral y))
_div0 x y = Left $ ValueError $ "`/`: Illegal input value: x=" ++ (show x) ++ ", y=" ++ (show y)

_add0 :: Wrd -> Wrd -> Either Error Wrd
_add0 (Double x) (Double y) = Right $ Double (x + y)
_add0 (Int x) (Int y) = Right $ Int (x + y)
_add0 (Int x) (Double y) = Right $ Double ((fromIntegral x) + y)
_add0 (Double x) (Int y) = Right $ Double (x + (fromIntegral y))
_add0 (Str s1) (Str s2) = Right $ Str (s1 ++ s2)
_add0 (List l1) (List l2) = toList (l1 ++ l2)
_add0 x y = Left $ ValueError $ "`+`: Illegal input value: x=" ++ (show x) ++ ", y=" ++ (show y)

_sub0 :: Wrd -> Wrd -> Either Error Wrd
_sub0 (Double x) (Double y) = Right $ Double (x - y)
_sub0 (Int x) (Int y) = Right $ Int (x - y)
_sub0 (Int x) (Double y) = Right $ Double ((fromIntegral x) - y)
_sub0 (Double x) (Int y) = Right $ Double (x - (fromIntegral y))
_sub0 x y = Left $ ValueError $ "`-`: Illegal input value: x=" ++ (show x) ++ ", y=" ++ (show y)

_mul :: Op
_mul = BinOp _mul0

_mul_t :: Wrd -> Wrd -> Either Error Wrd
_mul_t (TypeCheck T_Double) (TypeCheck T_Double) = Right $ TypeCheck T_Double
_mul_t (TypeCheck T_Int) (TypeCheck T_Int) = Right $ TypeCheck T_Int
_mul_t (TypeCheck T_Int) (TypeCheck T_Double) = Right $ TypeCheck T_Double
_mul_t (TypeCheck T_Double) (TypeCheck T_Int) = Right $ TypeCheck T_Double
_mul_t (TypeCheck T_Double) (TypeCheck t) = Left $ TypeError T_Num t $
        "`*`: Illegal input type in the second argument: Expected `Int` or `Double`, but got `" ++ show t ++ "`"
_mul_t (TypeCheck T_Int) (TypeCheck t) = Left $ TypeError T_Num t $
        "`*`: Illegal input type in the second argument: Expected `Int` or `Double`, but got `" ++ show t ++ "`"
_mul_t (TypeCheck t) _ = Left $ TypeError T_Num t $
        "`*`: Illegal input type in the first argument: Expected `Int` or `Double`, but got `" ++ show t ++ "`"

_div :: Op
_div = BinOp _div0

_div_t :: Wrd -> Wrd -> Either Error Wrd
_div_t (TypeCheck T_Double) (TypeCheck T_Double) = Right $ TypeCheck T_Double
_div_t (TypeCheck T_Int) (TypeCheck T_Int) = Right $ TypeCheck T_Double
_div_t (TypeCheck T_Int) (TypeCheck T_Double) = Right $ TypeCheck T_Double
_div_t (TypeCheck T_Double) (TypeCheck T_Int) = Right $ TypeCheck T_Double
_div_t (TypeCheck T_Double) (TypeCheck t) = Left $ TypeError T_Num t $
        "`/`: Illegal input type in the second argument: Expected `Int` or `Double`, but got `" ++ show t ++ "`"
_div_t (TypeCheck T_Int) (TypeCheck t) = Left $ TypeError T_Num t $
        "`/`: Illegal input type in the second argument: Expected `Int` or `Double`, but got `" ++ show t ++ "`"
_div_t (TypeCheck t) _ = Left $ TypeError T_Num t $
        "`/`: Illegal input type in the first argument: Expected `Int` or `Double`, but got `" ++ show t ++ "`"

_add :: Op
_add = BinOp _add0

_add_t :: Wrd -> Wrd -> Either Error Wrd
_add_t (TypeCheck T_Double) (TypeCheck T_Double) = Right $ TypeCheck T_Double
_add_t (TypeCheck T_Int) (TypeCheck T_Int) = Right $ TypeCheck T_Int
_add_t (TypeCheck T_Int) (TypeCheck T_Double) = Right $ TypeCheck T_Double
_add_t (TypeCheck T_Double) (TypeCheck T_Int) = Right $ TypeCheck T_Double
_add_t (TypeCheck t) (TypeCheck T_Double) = Left $ TypeError T_Num t $
    "`+`: Illegal input type: Type of second argumrnt is `Double` but that of first argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_add_t (TypeCheck t) (TypeCheck T_Int) = Left $ TypeError T_Num t $
    "`+`: Illegal input type: Type of second argumrnt is `Int` but that of first argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_add_t (TypeCheck T_Double) (TypeCheck t) = Left $ TypeError T_Num t $
    "`+`: Illegal input type: Type of first argumrnt is `Double` but that of second argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_add_t (TypeCheck T_Int) (TypeCheck t) = Left $ TypeError T_Num t $
    "`+`: Illegal input type: Type of first argumrnt is `Int` but that of second argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_add_t (TypeCheck T_String) (TypeCheck T_String) = Right $ TypeCheck T_String
_add_t (TypeCheck t) (TypeCheck T_String) = Left $ TypeError T_String t $
    "`+`: Illegal input type: Type of second argument is `String` but that of first argument is `" ++ show t ++ "`"
_add_t (TypeCheck T_String) (TypeCheck t) = Left $ TypeError T_String t $
    "`+`: Illegal input type: Type of first argument is `String` but that of second argument is `" ++ show t ++ "`"
_add_t (TypeCheck (T_List t1)) (TypeCheck (T_List t2)) =
    if t1 == t2 then Right $ TypeCheck (T_List t1) else Left $
        TypeError t1 t2 $ "`+`: Type mismatch of lists: `" ++ (show t1) ++ "` and `" ++ (show t2) ++ "`"
_add_t (TypeCheck t) (TypeCheck (T_List t2)) = Left $ TypeError (T_List t2) t $
    "`+`: Illegal input type: Type of second argument is `List " ++ show t2 ++ "` but that of first argument is `" ++ show t ++ "`"
_add_t (TypeCheck (T_List t1)) (TypeCheck t) = Left $ TypeError (T_List t1) t $
    "`+`: Illegal input type: Type of first argument is `List " ++ show t1 ++ "` but that of second argument is `" ++ show t ++ "`"
_add_t (TypeCheck t) _ = Left $ TypeError T_Additive t $
    "`+`: Illegal input type in the first argument: Expected `Additive` class type but got `" ++ show t ++ "`"

_sub :: Op
_sub = BinOp _sub0

_sub_t :: Wrd -> Wrd -> Either Error Wrd
_sub_t (TypeCheck T_Double) (TypeCheck T_Double) = Right $ TypeCheck T_Double
_sub_t (TypeCheck T_Int) (TypeCheck T_Int) = Right $ TypeCheck T_Int
_sub_t (TypeCheck T_Int) (TypeCheck T_Double) = Right $ TypeCheck T_Double
_sub_t (TypeCheck T_Double) (TypeCheck T_Int) = Right $ TypeCheck T_Double
_sub_t (TypeCheck T_Double) (TypeCheck t) = Left $ TypeError T_Num t $
        "`-`: Illegal input type in the second argument: Expected `Int` or `Double`, but got `" ++ show t ++ "`"
_sub_t (TypeCheck T_Int) (TypeCheck t) = Left $ TypeError T_Num t $
        "`-`: Illegal input type in the second argument: Expected `Int` or `Double`, but got `" ++ show t ++ "`"
_sub_t (TypeCheck t) _ = Left $ TypeError T_Num t $
        "`-`: Illegal input type in the first argument: Expected `Int` or `Double`, but got `" ++ show t ++ "`"

_succ_t :: Wrd -> Either Error Wrd
_succ_t (TypeCheck T_Int) = Right $ TypeCheck T_Int
_succ_t (TypeCheck T_Double) = Right $ TypeCheck T_Double
_succ_t (TypeCheck t) = Left $ TypeError T_Num t $
    "succ: Illegal input type: Expected `Int` or `Double`, but got `" ++ show t ++ "`"

_succ0 :: Wrd -> Either Error Wrd
_succ0 (Int x) = Right $ Int (x + 1)
_succ0 (Double x) = Right $ Double (x + 1)
_succ0 x = Left $ ValueError $ "succ: Illegal input value: " ++ (show x)

_succ :: Op
_succ = UnOp _succ0

_eq_t :: Wrd -> Wrd -> Either Error Wrd
_eq_t (TypeCheck t1) (TypeCheck t2) =
    if t1 == t2 then Right $ TypeCheck T_Bool else Left $ TypeError t1 t2 $
        "`=`: Type mismatch of both sides: LHS=" ++ (show t1) ++ ", RHS=" ++ (show t2)

_eq :: Op
_eq = BinOp (\ a b -> Right $ Bool (a == b))

_neq_t :: Wrd -> Wrd -> Either Error Wrd
_neq_t (TypeCheck t1) (TypeCheck t2) =
    if t1 == t2 then Right $ TypeCheck T_Bool else Left $ TypeError t1 t2 $
        "`!=`: Type mismatch of both sides: LHS=" ++ (show t1) ++ ", RHS=" ++ (show t2)

_neq :: Op
_neq = BinOp (\ a b -> Right $ Bool (not (a == b)))

_gt_t :: Wrd -> Wrd -> Either Error Wrd
_gt_t (TypeCheck T_Double) (TypeCheck T_Double) = Right $ TypeCheck T_Bool
_gt_t (TypeCheck T_Int) (TypeCheck T_Int) = Right $ TypeCheck T_Bool
_gt_t (TypeCheck T_Double) (TypeCheck T_Int) = Right $ TypeCheck T_Bool
_gt_t (TypeCheck T_Int) (TypeCheck T_Double) = Right $ TypeCheck T_Bool
_gt_t (TypeCheck t) (TypeCheck T_Double) = Left $ TypeError T_Num t $
    "`>`: Illegal input type: Type of second argumrnt is `Double` but that of first argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_gt_t (TypeCheck t) (TypeCheck T_Int) = Left $ TypeError T_Num t $
    "`>`: Illegal input type: Type of second argumrnt is `Int` but that of first argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_gt_t (TypeCheck T_Double) (TypeCheck t) = Left $ TypeError T_Num t $
    "`>`: Illegal input type: Type of first argumrnt is `Double` but that of second argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_gt_t (TypeCheck T_Int) (TypeCheck t) = Left $ TypeError T_Num t $
    "`>`: Illegal input type: Type of first argumrnt is `Int` but that of second argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_gt_t (TypeCheck t) _ = Left $ TypeError T_Ord t $
    "`>`: Illegal input type: Expected `Ord` class type but got `" ++ show t ++ "`"

_gt0 :: Wrd -> Wrd -> Either Error Wrd
_gt0 (Double x) (Double y) = Right $ Bool (x > y)
_gt0 (Int x) (Int y) = Right $ Bool (x > y)
_gt0 (Int x) (Double y) = Right $ Bool ((fromIntegral x) > y)
_gt0 (Double x) (Int y) = Right $ Bool (x > (fromIntegral y))

_gt :: Op
_gt = BinOp _gt0

_geq_t :: Wrd -> Wrd -> Either Error Wrd
_geq_t (TypeCheck T_Double) (TypeCheck T_Double) = Right $ TypeCheck T_Bool
_geq_t (TypeCheck T_Int) (TypeCheck T_Int) = Right $ TypeCheck T_Bool
_geq_t (TypeCheck T_Double) (TypeCheck T_Int) = Right $ TypeCheck T_Bool
_geq_t (TypeCheck T_Int) (TypeCheck T_Double) = Right $ TypeCheck T_Bool
_geq_t (TypeCheck t) (TypeCheck T_Double) = Left $ TypeError T_Num t $
    "`>=`: Illegal input type: Type of second argumrnt is `Double` but that of first argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_geq_t (TypeCheck t) (TypeCheck T_Int) = Left $ TypeError T_Num t $
    "`>=`: Illegal input type: Type of second argumrnt is `Int` but that of first argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_geq_t (TypeCheck T_Double) (TypeCheck t) = Left $ TypeError T_Num t $
    "`>=`: Illegal input type: Type of first argumrnt is `Double` but that of second argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_geq_t (TypeCheck T_Int) (TypeCheck t) = Left $ TypeError T_Num t $
    "`>=`: Illegal input type: Type of first argumrnt is `Int` but that of second argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_geq_t (TypeCheck t) _ = Left $ TypeError T_Ord t $
    "`>=`: Illegal input type: Expected `Ord` class type but got `" ++ show t ++ "`"

_geq0 :: Wrd -> Wrd -> Either Error Wrd
_geq0 (Double x) (Double y) = Right $ Bool (x >= y)
_geq0 (Int x) (Int y) = Right $ Bool (x >= y)
_geq0 (Int x) (Double y) = Right $ Bool ((fromIntegral x) >= y)
_geq0 (Double x) (Int y) = Right $ Bool (x >= (fromIntegral y))

_geq :: Op
_geq = BinOp _geq0

_lt_t :: Wrd -> Wrd -> Either Error Wrd
_lt_t (TypeCheck T_Double) (TypeCheck T_Double) = Right $ TypeCheck T_Bool
_lt_t (TypeCheck T_Int) (TypeCheck T_Int) = Right $ TypeCheck T_Bool
_lt_t (TypeCheck T_Double) (TypeCheck T_Int) = Right $ TypeCheck T_Bool
_lt_t (TypeCheck T_Int) (TypeCheck T_Double) = Right $ TypeCheck T_Bool
_lt_t (TypeCheck t) (TypeCheck T_Double) = Left $ TypeError T_Num t $
    "`<`: Illegal input type: Type of second argumrnt is `Double` but that of first argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_lt_t (TypeCheck t) (TypeCheck T_Int) = Left $ TypeError T_Num t $
    "`<`: Illegal input type: Type of second argumrnt is `Int` but that of first argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_lt_t (TypeCheck T_Double) (TypeCheck t) = Left $ TypeError T_Num t $
    "`<`: Illegal input type: Type of first argumrnt is `Double` but that of second argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_lt_t (TypeCheck T_Int) (TypeCheck t) = Left $ TypeError T_Num t $
    "`<`: Illegal input type: Type of first argumrnt is `Int` but that of second argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_lt_t (TypeCheck t) _ = Left $ TypeError T_Ord t $
    "`<`: Illegal input type: Expected `Ord` class type but got `" ++ show t ++ "`"

_lt0 :: Wrd -> Wrd -> Either Error Wrd
_lt0 (Double x) (Double y) = Right $ Bool (x < y)
_lt0 (Int x) (Int y) = Right $ Bool (x < y)
_lt0 (Int x) (Double y) = Right $ Bool ((fromIntegral x) < y)
_lt0 (Double x) (Int y) = Right $ Bool (x < (fromIntegral y))

_lt :: Op
_lt = BinOp _lt0

_leq_t :: Wrd -> Wrd -> Either Error Wrd
_leq_t (TypeCheck T_Double) (TypeCheck T_Double) = Right $ TypeCheck T_Bool
_leq_t (TypeCheck T_Int) (TypeCheck T_Int) = Right $ TypeCheck T_Bool
_leq_t (TypeCheck T_Double) (TypeCheck T_Int) = Right $ TypeCheck T_Bool
_leq_t (TypeCheck T_Int) (TypeCheck T_Double) = Right $ TypeCheck T_Bool
_leq_t (TypeCheck t) (TypeCheck T_Double) = Left $ TypeError T_Num t $
    "`<=`: Illegal input type: Type of second argumrnt is `Double` but that of first argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_leq_t (TypeCheck t) (TypeCheck T_Int) = Left $ TypeError T_Num t $
    "`<=`: Illegal input type: Type of second argumrnt is `Int` but that of first argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_leq_t (TypeCheck T_Double) (TypeCheck t) = Left $ TypeError T_Num t $
    "`<=`: Illegal input type: Type of first argumrnt is `Double` but that of second argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_leq_t (TypeCheck T_Int) (TypeCheck t) = Left $ TypeError T_Num t $
    "`<=`: Illegal input type: Type of first argumrnt is `Int` but that of second argument is `" ++ show t ++
    "` (expected `Double` or `Int`)"
_leq_t (TypeCheck t) _ = Left $ TypeError T_Ord t $
    "`<=`: Illegal input type: Expected `Ord` class type but got `" ++ show t ++ "`"

_leq0 :: Wrd -> Wrd -> Either Error Wrd
_leq0 (Double x) (Double y) = Right $ Bool (x <= y)
_leq0 (Int x) (Int y) = Right $ Bool (x <= y)
_leq0 (Int x) (Double y) = Right $ Bool ((fromIntegral x) <= y)
_leq0 (Double x) (Int y) = Right $ Bool (x <= (fromIntegral y))

_leq :: Op
_leq = BinOp _leq0

_and_t :: Wrd -> Wrd -> Either Error Wrd
_and_t (TypeCheck T_Bool) (TypeCheck T_Bool) = Right $ TypeCheck T_Bool
_and_t (TypeCheck t) (TypeCheck T_Bool) = Left $ TypeError T_Bool t $
    "`&&`: Illegal input type in the first argument: Expected `Bool` but got `" ++ show t ++ "`"
_and_t _ (TypeCheck t) = Left $ TypeError T_Bool t $
    "`&&`: Illegal input type in the second argument: Expected `Bool` but got `" ++ show t ++ "`"

_and :: Op
_and = BinOp (\ (Bool a) (Bool b) -> Right $ Bool (a && b))

_or_t :: Wrd -> Wrd -> Either Error Wrd
_or_t (TypeCheck T_Bool) (TypeCheck T_Bool) = Right $ TypeCheck T_Bool
_or_t (TypeCheck t) (TypeCheck T_Bool) = Left $ TypeError T_Bool t $
    "`||`: Illegal input type in the first argument: Expected `Bool` but got `" ++ show t ++ "`"
_or_t _ (TypeCheck t) = Left $ TypeError T_Bool t $
    "`||`: Illegal input type in the second argument: Expected `Bool` but got `" ++ show t ++ "`"

_or :: Op
_or = BinOp (\ (Bool a) (Bool b) -> Right $ Bool (a || b))

_not_t :: Wrd -> Either Error Wrd
_not_t (TypeCheck T_Bool) = Right $ TypeCheck T_Bool
_not_t (TypeCheck t) = Left $ TypeError T_Bool t $ "`!`: Illegal input type: Expected `Bool` but got `" ++ show t ++ "`"

_not :: Op
_not = UnOp (\ (Bool b) -> Right $ Bool (not b))

_head_t :: Wrd -> Either Error Wrd
_head_t (TypeCheck (T_List t)) = Right $ TypeCheck t
_head_t (TypeCheck t) = Left $ TypeError (T_List T_Any) t $
    "head: Illegal input type: Expected `List a` but got `" ++ show t ++ "`"

_head0 :: Wrd -> Either Error Wrd
_head0 (List []) = Left $ ValueError "head: Empty list."
_head0 (List (x : _)) = Right x

_head :: Op
_head = UnOp _head0

_tail_t :: Wrd -> Either Error Wrd
_tail_t (TypeCheck (T_List t)) = Right $ TypeCheck (T_List t)
_tail_t (TypeCheck t) = Left $ TypeError (T_List T_Any) t $
    "tail: Illegal input type: Expected `List a` but got `" ++ show t ++ "`"

_tail0 :: Wrd -> Either Error Wrd
_tail0 (List (_ : xs)) = Right $ List xs
_tail0 (List []) = Left $ ValueError "tail: Empty list."

_tail :: Op
_tail = UnOp _tail0

_pop_t :: Wrd -> Either Error Wrd
_pop_t (TypeCheck (T_List t)) = Right $ TypeCheck (T_Tuple [t, T_List t])
_pop_t (TypeCheck t) = Left $ TypeError (T_List T_Any) t $
    "pop: Illegal input type: Expected `List a` but got `" ++ show t ++ "`"

_pop0 :: Wrd -> Either Error Wrd
_pop0 (List (x : xs)) = Right $ Tuple [x, List xs]
_pop0 (List []) = Left $ ValueError "pop: Empty list."

_pop :: Op
_pop = UnOp _pop0

_isEmpty_t :: Wrd -> Either Error Wrd
_isEmpty_t (TypeCheck (T_List _)) = Right $ TypeCheck T_Bool
_isEmpty_t (TypeCheck t) = Left $ TypeError (T_List T_Any) t $
    "isEmpty: Illegal input type: Expected `List a` but got `" ++ show t ++ "`"

_isEmpty :: Op
_isEmpty = UnOp (\ (List ls) -> Right $ Bool (ls == []))

_take_t :: Exp -> Either Error Wrd
_take_t [TypeCheck T_Int, TypeCheck (T_List t)] = Right $ TypeCheck (T_List t)
_take_t [TypeCheck T_Int, TypeCheck t] = Left $ TypeError (T_List T_Any) t$
    "take: Illegal input typein the second argument: Expected `List a` but got `" ++ show t ++ "`"
_take_t [TypeCheck t, _] = Left $ TypeError (T_List T_Any) t$
    "take: Illegal input typein the first argument: Expected `Int` but got `" ++ show t ++ "`"

_take0 :: Exp -> Either Error Wrd
_take0 [Int n, List ls] = Right $ List $ take n ls
_take0 ex = Left $ ValueError $ "take: Illegal input value: " ++ (show ex)

_take :: Op
_take = FuncOp (2, _take0)

_seq_t :: Exp -> Either Error Wrd
_seq_t [TypeCheck T_Int, TypeCheck T_Int] = Right $ TypeCheck (T_List T_Int)
_seq_t [TypeCheck t, TypeCheck T_Int] = Left $ TypeError T_Int t $
    "seq: Illegal input type in the first argument: Expected `Int` but got `" ++ (show t) ++ "`"
_seq_t [_, TypeCheck t] = Left $ TypeError T_Int t $
    "seq: Illegal input type in the second argument: Expected `Int` but got `" ++ (show t) ++ "`"

_seq0 :: Exp -> Either Error Wrd
_seq0 (Int n : (Int m : [])) = Right $ List $ map (\ x -> Int x) [n .. m]
_seq0 ex = Left $ ValueError $ "seq: Illegal input value: " ++ (show ex)

_seq :: Op
_seq = FuncOp (2, _seq0)

_map_t :: Exp -> Either Error Wrd
_map_t [TypeCheck (T_Func {}), TypeCheck (T_List t)] = Right $ TypeCheck T_PreList
_map_t [TypeCheck t, TypeCheck (T_List _)] = Left $ TypeError (T_Func T_Function { args_t = [], return_t = T_Any }) t $
    "map: Illegal input type in the first argument: Expected `Function` but got `" ++ show t ++ "`"
_map_t [_, TypeCheck t] = Left $ TypeError (T_List T_Any) t $
    "map: Illegal input type in the second argument: Expected `Function` but got `" ++ show t ++ "`"

_map0 :: Exp -> Either Error Wrd
_map0 (Func f : (List ls : [])) = Right $ PreList $ map (\ w -> [Func f, w]) ls
_map0 ex = Left $ ValueError $ "map: Illegal input value: " ++ (show ex)

_map :: Op
_map = FuncOp (2, _map0)

_fst_t :: Wrd -> Either Error Wrd
_fst_t (TypeCheck (T_Tuple (t : _))) = Right $ TypeCheck t
_fst_t (TypeCheck t) = Left $ TypeError (T_Tuple []) t $
    "fst: Illegal input type: Expected `Tuple` but got `" ++ show t ++ "`"

_fst :: Op
_fst = UnOp (\ (Tuple (w1 : _)) -> Right $ w1)

_snd_t :: Wrd -> Either Error Wrd
_snd_t (TypeCheck (T_Tuple (_ : (t : _)))) = Right $ TypeCheck t
_snd_t (TypeCheck t) = Left $ TypeError (T_Tuple []) t $
    "snd: Illegal input type: Expected `Tuple` but got `" ++ show t ++ "`"

_snd :: Op
_snd = UnOp (\ (Tuple (_ : (w2 : _))) -> Right $ w2)

_getType :: Wrd -> Type
_getType (Str _) = T_String
_getType (Int _) = T_Int
_getType (Double _) = T_Double
_getType (Bool _) = T_Bool
_getType (Tobe _) = T_Unknown
_getType (List (w: _)) = T_List $ _getType w
_getType (List []) = T_EmptyList
_getType (Tuple tp) = T_Tuple $ map _getType tp
_getType (Func (Operator (opName, _))) = T_Func (T_Operator (opName, _typeFunction opName))
_getType (Func (Fun (Function { args = as, ret_t = rt, ret = _ }))) =
    let ast = map (\ (t, a) -> t) as
    in T_Func (T_Function { args_t = ast, return_t = rt })
_getType (Type _) = T_Type

_isConsistentType :: Exp -> Either (Type, Type) Type
_isConsistentType [w] = Right $ _getType w
_isConsistentType (w: rest) =
    case _isConsistentType rest of
    Left ts -> Left ts
    Right t ->
        let t' = _getType w
        in if t == t' then Right t else Left (t, t')

toList :: Exp -> Either Error Wrd
toList expr =
    case _isConsistentType expr of
        Right _ -> Right $ List expr
        Left (t1, t2) -> Left $ TypeError t1 t2 $
            "[]: Inconsistent type: Found type `" ++ show t1 ++ "` and `" ++ show t2 ++ "`"

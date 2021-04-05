module Ops where
import Parser
import Function
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

_typeFunction :: String -> Op_T
_typeFunction op
    | op == "*" = BinOp_T _mul_t
    | op == "+" = BinOp_T _add_t
    | op == "-" = BinOp_T _sub_t
    | op == "/" = BinOp_T _div_t
    | op == "||" = BinOp_T _or_t
    | op == "&&" = BinOp_T _and_t
    | op == "!" = UnOp_T _not_t
    | op == "==" = BinOp_T _eq_t
    | op == "!=" = BinOp_T _neq_t
    | op == ">" = BinOp_T _gt_t
    | op == ">=" = BinOp_T _geq_t
    | op == "<" = BinOp_T _lt_t
    | op == "<=" = BinOp_T _leq_t
    | op == "print" = UnOp_T _print_t
    | op == "succ" = UnOp_T _succ_t
    | op == "head" = UnOp_T _head_t
    | op == "tail" = UnOp_T _tail_t
    | op == "pop" = UnOp_T _pop_t
    | op == "isEmpty" = UnOp_T _isEmpty_t
    | op == "take" = FuncOp_T (2, _take_t)
    | op == "seq" = FuncOp_T (2, _seq_t)
    | op == "map" = FuncOp_T (2, _map_t)
    | op == "fst" = UnOp_T _fst_t
    | op == "snd" = UnOp_T _snd_t

_print0 :: Wrd -> Wrd
_print0 (Err e) = Err e
_print0 w = Print (show w)

_print :: Op
_print = UnOp _print0

_print_t :: TWrd -> TWrd
_print_t (TWrd T_Error) = TWrd T_Error
_print_t (TWrd _) = TWrd T_Print

_mul0 :: Wrd -> Wrd -> Wrd
_mul0 (Double x) (Double y) = Double (x * y)
_mul0 (Int x) (Int y) = Int (x * y)
_mul0 (Int x) (Double y) = Double ((fromIntegral x) * y)
_mul0 (Double x) (Int y) = Double (x * (fromIntegral y))
_mul0 x y = Err $ "`*`: Illegal input value: x=" ++ (show x) ++ ", y=" ++ (show y)

_div0 :: Wrd -> Wrd -> Wrd
_div0 _ (Int 0) = Err "Zero division error."
_div0 _ (Double 0.0) = Err "Zero division error."
_div0 (Double x) (Double y) = Double (x / y)
_div0 (Int x) (Int y) = Double ((fromIntegral x) / (fromIntegral y))
_div0 (Int x) (Double y) = Double ((fromIntegral x) / y)
_div0 (Double x) (Int y) = Double (x / (fromIntegral y))
_div0 x y = Err $ "`/`: Illegal input value: x=" ++ (show x) ++ ", y=" ++ (show y)

_add0 :: Wrd -> Wrd -> Wrd
_add0 (Double x) (Double y) = Double (x + y)
_add0 (Int x) (Int y) = Int (x + y)
_add0 (Int x) (Double y) = Double ((fromIntegral x) + y)
_add0 (Double x) (Int y) = Double (x + (fromIntegral y))
_add0 (Str s1) (Str s2) = Str (s1 ++ s2)
_add0 (List l1) (List l2) = toList (l1 ++ l2)
_add0 x y = Err $ "`+`: Illegal input value: x=" ++ (show x) ++ ", y=" ++ (show y)

_sub0 :: Wrd -> Wrd -> Wrd
_sub0 (Double x) (Double y) = Double (x - y)
_sub0 (Int x) (Int y) = Int (x - y)
_sub0 (Int x) (Double y) = Double ((fromIntegral x) - y)
_sub0 (Double x) (Int y) = Double (x - (fromIntegral y))
_sub0 x y = Err $ "`-`: Illegal input value: x=" ++ (show x) ++ ", y=" ++ (show y)

_mul :: Op
_mul = BinOp _mul0

_mul_t :: TWrd -> TWrd -> TWrd
_mul_t (TWrd T_Double) (TWrd T_Double) = TWrd T_Double
_mul_t (TWrd T_Int) (TWrd T_Int) = TWrd T_Int
_mul_t (TWrd T_Int) (TWrd T_Double) = TWrd T_Double
_mul_t (TWrd T_Double) (TWrd T_Int) = TWrd T_Double
_mul_t t_x t_y = TErr $ "`*`: Illegal input type: t_x=" ++ (show t_x) ++ ", t_y=" ++ (show t_y)

_div :: Op
_div = BinOp _div0

_div_t :: TWrd -> TWrd -> TWrd
_div_t (TWrd T_Double) (TWrd T_Double) = TWrd T_Double
_div_t (TWrd T_Int) (TWrd T_Int) = TWrd T_Double
_div_t (TWrd T_Int) (TWrd T_Double) = TWrd T_Double
_div_t (TWrd T_Double) (TWrd T_Int) = TWrd T_Double
_div_t t_x t_y = TErr $ "`/`: Illegal input type: t_x=" ++ (show t_x) ++ ", t_y=" ++ (show t_y)

_add :: Op
_add = BinOp _add0

_add_t :: TWrd -> TWrd -> TWrd
_add_t (TWrd T_Double) (TWrd T_Double) = TWrd T_Double
_add_t (TWrd T_Int) (TWrd T_Int) = TWrd T_Int
_add_t (TWrd T_Int) (TWrd T_Double) = TWrd T_Double
_add_t (TWrd T_Double) (TWrd T_Int) = TWrd T_Double
_add_t (TWrd T_String) (TWrd T_String) = TWrd T_String
_add_t (TWrd (T_List t1)) (TWrd (T_List t2)) =
    if t1 == t2 then TWrd (T_List t1) else TErr $ "`+`: TWrd mismatch of lists: t1=" ++ (show t1) ++ ", t2=" ++ (show t2)
_add_t t_x t_y = TErr $ "`+`: Illegal input type: t_x=" ++ (show t_x) ++ ", t_y=" ++ (show t_y)

_sub :: Op
_sub = BinOp _sub0

_sub_t :: TWrd -> TWrd -> TWrd
_sub_t (TWrd T_Double) (TWrd T_Double) = TWrd T_Double
_sub_t (TWrd T_Int) (TWrd T_Int) = TWrd T_Int
_sub_t (TWrd T_Int) (TWrd T_Double) = TWrd T_Double
_sub_t (TWrd T_Double) (TWrd T_Int) = TWrd T_Double
_sub_t t_x t_y = TErr $ "`-`: Illegal input type: t_x=" ++ (show t_x) ++ ", t_y=" ++ (show t_y)

_succ_t :: TWrd -> TWrd
_succ_t (TWrd T_Int) = TWrd T_Int
_succ_t (TWrd T_Double) = TWrd T_Double
_succ_t t = TErr $ "succ: Illegal input type: " ++ (show t)

_succ0 :: Wrd -> Wrd
_succ0 (Int x) = Int (x + 1)
_succ0 (Double x) = Double (x + 1)
_succ0 x = Err $ "succ: Illegal input value: " ++ (show x)

_succ :: Op
_succ = UnOp _succ0

_eq_t :: TWrd -> TWrd -> TWrd
_eq_t (TWrd t1) (TWrd t2) =
    if t1 == t2 then TWrd T_Bool else TErr $ "`=`: TWrd mismatch of both sides: LHS=" ++ (show t1) ++ ", RHS=" ++ (show t2)

_eq :: Op
_eq = BinOp (\ a b -> Bool (a == b))

_neq_t :: TWrd -> TWrd -> TWrd
_neq_t (TWrd t1) (TWrd t2) =
    if t1 == t2 then TWrd T_Bool else TErr $ "`!=`: TWrd mismatch of both sides: LHS=" ++ (show t1) ++ ", RHS=" ++ (show t2)

_neq :: Op
_neq = BinOp (\ a b -> Bool (not (a == b)))

_gt_t :: TWrd -> TWrd -> TWrd
_gt_t (TWrd T_Double) (TWrd T_Double) = TWrd T_Bool
_gt_t (TWrd T_Int) (TWrd T_Int) = TWrd T_Bool
_gt_t (TWrd T_Double) (TWrd T_Int) = TWrd T_Bool
_gt_t (TWrd T_Int) (TWrd T_Double) = TWrd T_Bool
_gt_t (TWrd t1) (TWrd t2) = TErr $ "`>`: Illegal input type: LHS=" ++ (show t1) ++ ", RHS=" ++ (show t2)

_gt0 :: Wrd -> Wrd -> Wrd
_gt0 (Double x) (Double y) = Bool (x > y)
_gt0 (Int x) (Int y) = Bool (x > y)
_gt0 (Int x) (Double y) = Bool ((fromIntegral x) > y)
_gt0 (Double x) (Int y) = Bool (x > (fromIntegral y))

_gt :: Op
_gt = BinOp _gt0

_geq_t :: TWrd -> TWrd -> TWrd
_geq_t (TWrd T_Double) (TWrd T_Double) = TWrd T_Bool
_geq_t (TWrd T_Int) (TWrd T_Int) = TWrd T_Bool
_geq_t (TWrd T_Double) (TWrd T_Int) = TWrd T_Bool
_geq_t (TWrd T_Int) (TWrd T_Double) = TWrd T_Bool
_geq_t (TWrd t1) (TWrd t2) = TErr $ "`>=`: Illegal input type: LHS=" ++ (show t1) ++ ", RHS=" ++ (show t2)

_geq0 :: Wrd -> Wrd -> Wrd
_geq0 (Double x) (Double y) = Bool (x >= y)
_geq0 (Int x) (Int y) = Bool (x >= y)
_geq0 (Int x) (Double y) = Bool ((fromIntegral x) >= y)
_geq0 (Double x) (Int y) = Bool (x >= (fromIntegral y))

_geq :: Op
_geq = BinOp _geq0

_lt_t :: TWrd -> TWrd -> TWrd
_lt_t (TWrd T_Double) (TWrd T_Double) = TWrd T_Bool
_lt_t (TWrd T_Int) (TWrd T_Int) = TWrd T_Bool
_lt_t (TWrd T_Double) (TWrd T_Int) = TWrd T_Bool
_lt_t (TWrd T_Int) (TWrd T_Double) = TWrd T_Bool
_lt_t (TWrd t1) (TWrd t2) = TErr $ "`<`: Illegal input type: LHS=" ++ (show t1) ++ ", RHS=" ++ (show t2)

_lt0 :: Wrd -> Wrd -> Wrd
_lt0 (Double x) (Double y) = Bool (x < y)
_lt0 (Int x) (Int y) = Bool (x < y)
_lt0 (Int x) (Double y) = Bool ((fromIntegral x) < y)
_lt0 (Double x) (Int y) = Bool (x < (fromIntegral y))

_lt :: Op
_lt = BinOp _lt0

_leq_t :: TWrd -> TWrd -> TWrd
_leq_t (TWrd T_Double) (TWrd T_Double) = TWrd T_Bool
_leq_t (TWrd T_Int) (TWrd T_Int) = TWrd T_Bool
_leq_t (TWrd T_Double) (TWrd T_Int) = TWrd T_Bool
_leq_t (TWrd T_Int) (TWrd T_Double) = TWrd T_Bool
_leq_t (TWrd t1) (TWrd t2) = TErr $ "`>=`: Illegal input type: LHS=" ++ (show t1) ++ ", RHS=" ++ (show t2)

_leq0 :: Wrd -> Wrd -> Wrd
_leq0 (Double x) (Double y) = Bool (x <= y)
_leq0 (Int x) (Int y) = Bool (x <= y)
_leq0 (Int x) (Double y) = Bool ((fromIntegral x) <= y)
_leq0 (Double x) (Int y) = Bool (x <= (fromIntegral y))

_leq :: Op
_leq = BinOp _leq0

_and_t :: TWrd -> TWrd -> TWrd
_and_t (TWrd T_Bool) (TWrd T_Bool) = TWrd T_Bool
_and_t (TWrd t1) (TWrd t2) = TErr $ "`&&`: Illegal input type: LHS=" ++ (show t1) ++ ", RHS=" ++ (show t2)

_and :: Op
_and = BinOp (\ (Bool a) (Bool b) ->  Bool (a && b))

_or_t :: TWrd -> TWrd -> TWrd
_or_t (TWrd T_Bool) (TWrd T_Bool) = TWrd T_Bool
_or_t (TWrd t1) (TWrd t2) = TErr $ "`||`: Illegal input type: LHS=" ++ (show t1) ++ ", RHS=" ++ (show t2)

_or :: Op
_or = BinOp (\ (Bool a) (Bool b) ->  Bool (a || b))

_not_t :: TWrd -> TWrd
_not_t (TWrd T_Bool) = TWrd T_Bool
_not_t (TWrd t) = TErr $ "`!`: Illegal input type: " ++ (show t)

_not :: Op
_not = UnOp (\ (Bool b) -> Bool (not b))

_head_t :: TWrd -> TWrd
_head_t (TWrd (T_List t)) = TWrd t
_head_t (TWrd t) = TErr $ "head: Illegal input type: " ++ (show t)

_head0 :: Wrd -> Wrd
_head0 (List []) = Err "head: Empty list."
_head0 (List (x : _)) = x

_head :: Op
_head = UnOp _head0

_tail_t :: TWrd -> TWrd
_tail_t (TWrd (T_List t)) = TWrd (T_List t)
_tail_t (TWrd t) = TErr $ "tail: Illegal input type: " ++ (show t)

_tail0 :: Wrd -> Wrd
_tail0 (List (_ : xs)) = List xs
_tail0 (List []) = Err "tail: Empty list."

_tail :: Op
_tail = UnOp _tail0

_pop_t :: TWrd -> TWrd
_pop_t (TWrd (T_List t)) = TWrd (T_Tuple [t, T_List t])
_pop_t (TWrd t) = TErr $ "pop: Illegal input type: " ++ (show t)

_pop0 :: Wrd -> Wrd
_pop0 (List (x : xs)) = Tuple [x, List xs]
_pop0 (List []) = Err "pop: Empty list."

_pop :: Op
_pop = UnOp _pop0

_isEmpty_t :: TWrd -> TWrd
_isEmpty_t (TWrd (T_List _)) = TWrd T_Bool
_isEmpty_t (TWrd t) = TErr $ "isEmpty: Illegal input type: " ++ (show t)

_isEmpty :: Op
_isEmpty = UnOp (\ (List ls) -> Bool (ls == []))

_take_t :: TExp -> TWrd
_take_t (TWrd T_Int : (TWrd (T_List t) : [])) = TWrd (T_List t)
_take_t expr = TErr $ "take: Illegal input type: " ++ (show expr)

_take0 :: Exp -> Wrd
_take0 (Int n : (List ls : [])) = List $ take n ls
_take0 ex = Err $ "take: Illegal input value: " ++ (show ex)

_take :: Op
_take = FuncOp (2, _take0)

_seq_t :: TExp -> TWrd
_seq_t (TWrd T_Int : (TWrd T_Int : [])) = TWrd (T_List T_Int)
_seq_t expr = TErr $ "seq: Illegal input type: " ++ (show expr)

_seq0 :: Exp -> Wrd
_seq0 (Int n : (Int m : [])) = List $ map (\ x -> Int x) [n .. m]
_seq0 ex = Err $ "seq: Illegal input value: " ++ (show ex)

_seq :: Op
_seq = FuncOp (2, _seq0)

_map_t :: TExp -> TWrd
_map_t (TWrd T_Func : (TWrd (T_List t) : [])) = TWrd T_PreList
_map_t expr = TErr $ "seq: Illegal input type: " ++ (show expr)

_map0 :: Exp -> Wrd
_map0 (Func f : (List ls : [])) = PreList $ map (\ w -> [Func f, w]) ls
_map0 ex = Err $ "map: Illegal input value: " ++ (show ex)

_map :: Op
_map = FuncOp (2, _map0)

_fst_t :: TWrd -> TWrd
_fst_t (TWrd (T_Tuple (t : _))) = TWrd t
_fst_t w = TErr $ "fst: Illegal input type: " ++ (show w)

_fst :: Op
_fst = UnOp (\ (Tuple (w1 : _)) -> w1)

_snd_t :: TWrd -> TWrd
_snd_t (TWrd (T_Tuple (_ : (t : _)))) = TWrd t
_snd_t w = TErr $ "snd: Illegal input type: " ++ (show w)

_snd :: Op
_snd = UnOp (\ (Tuple (_ : (w2 : _))) -> w2)


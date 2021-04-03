module Ops where
import Parser
import Function
import Data.List
import Debug.Trace

_print0 :: Wrd -> Wrd
_print0 (Err e) = Err e
_print0 w = Print (show w)

_print :: Op
_print = UnOp _print0

_mul0 :: Wrd -> Wrd -> Wrd
_mul0 (Double x) (Double y) = Double (x * y)
_mul0 (Int x) (Int y) = Int (x * y)
_mul0 (Int x) (Double y) = Double ((fromIntegral x) * y)
_mul0 (Double x) (Int y) = Double (x * (fromIntegral y))
_mul0 (Type T_Double) (Type T_Double) = Type T_Double
_mul0 (Type T_Int) (Type T_Int) = Type T_Int
_mul0 (Type T_Int) (Type T_Double) = Type T_Double
_mul0 (Type T_Double) (Type T_Int) = Type T_Double
_mul0 x y = Err $ "`*`: Illegal input value: x=" ++ (show x) ++ ", y=" ++ (show y)

_div0 :: Wrd -> Wrd -> Wrd
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
_add0 x y = Err $ "`+`: Illegal input value: x=" ++ (show x) ++ ", y=" ++ (show y)

_sub0 :: Wrd -> Wrd -> Wrd
_sub0 (Double x) (Double y) = Double (x - y)
_sub0 (Int x) (Int y) = Int (x - y)
_sub0 (Int x) (Double y) = Double ((fromIntegral x) - y)
_sub0 (Double x) (Int y) = Double (x - (fromIntegral y))
_sub0 x y = Err $ "`-`: Illegal input value: x=" ++ (show x) ++ ", y=" ++ (show y)

_mul :: Op
_mul = BinOp _mul0

_div :: Op
_div = BinOp _div0

_add :: Op
_add = BinOp _add0

_sub :: Op
_sub = BinOp _sub0

_succ0 :: Wrd -> Wrd
_succ0 (Int x) = Int (x + 1)
_succ0 (Double x) = Double (x + 1)
_succ0 x = Err $ "succ: Illegal input value: " ++ (show x)

_succ :: Op
_succ = UnOp _succ0

_eq :: Op
_eq = BinOp (\ a b -> Bool (a == b))

_neq :: Op
_neq = BinOp (\ a b -> Bool (not (a == b)))

_gt0 :: Wrd -> Wrd -> Wrd
_gt0 (Double x) (Double y) = Bool (x > y)
_gt0 (Int x) (Int y) = Bool (x > y)
_gt0 (Int x) (Double y) = Bool ((fromIntegral x) > y)
_gt0 (Double x) (Int y) = Bool (x > (fromIntegral y))

_gt :: Op
_gt = BinOp _gt0

_geq0 :: Wrd -> Wrd -> Wrd
_geq0 (Double x) (Double y) = Bool (x >= y)
_geq0 (Int x) (Int y) = Bool (x >= y)
_geq0 (Int x) (Double y) = Bool ((fromIntegral x) >= y)
_geq0 (Double x) (Int y) = Bool (x >= (fromIntegral y))

_geq :: Op
_geq = BinOp _geq0

_lt0 :: Wrd -> Wrd -> Wrd
_lt0 (Double x) (Double y) = Bool (x < y)
_lt0 (Int x) (Int y) = Bool (x < y)
_lt0 (Int x) (Double y) = Bool ((fromIntegral x) < y)
_lt0 (Double x) (Int y) = Bool (x < (fromIntegral y))

_lt :: Op
_lt = BinOp _lt0

_leq0 :: Wrd -> Wrd -> Wrd
_leq0 (Double x) (Double y) = Bool (x <= y)
_leq0 (Int x) (Int y) = Bool (x <= y)
_leq0 (Int x) (Double y) = Bool ((fromIntegral x) <= y)
_leq0 (Double x) (Int y) = Bool (x <= (fromIntegral y))

_leq :: Op
_leq = BinOp _leq0

_and :: Op
_and = BinOp (\ (Bool a) (Bool b) ->  Bool (a && b))

_or :: Op
_or = BinOp (\ (Bool a) (Bool b) ->  Bool (a || b))

_not :: Op
_not = UnOp (\ (Bool b) -> Bool (not b))

_head0 :: Wrd -> Wrd
_head0 (List []) = Err "head: Empty list."
_head0 (List (x : _)) = x

_head :: Op
_head = UnOp _head0

_tail0 :: Wrd -> Wrd
_tail0 (List (_ : xs)) = List xs
_tail0 (List []) = Err "tail: Empty list."

_tail :: Op
_tail = UnOp _tail0

_pop0 :: Wrd -> Wrd
_pop0 (List (x : xs)) = Pair (x, List xs)
_pop0 (List []) = Err "pop: Empty list."

_pop :: Op
_pop = UnOp _pop0

_isEmpty :: Op
_isEmpty = UnOp (\ (List ls) -> Bool (ls == []))

_take0 :: Exp -> Wrd
_take0 (Int n : (List ls : [])) = List $ take n ls
_take0 ex = Err $ "take: Illegal input value: " ++ (show ex)

_take :: Op
_take = FuncOp (2, _take0)

_seq0 :: Exp -> Wrd
_seq0 (Int n : (Int m : [])) = List $ map (\ x -> Int x) [n .. m]
_seq0 ex = Err $ "seq: Illegal input value: " ++ (show ex)

_seq :: Op
_seq = FuncOp (2, _seq0)

_map0 :: Exp -> Wrd
_map0 (Func f : (List ls : [])) = PreList $ map (\ w -> [Func f, w]) ls
_map0 ex = Err $ "map: Illegal input value: " ++ (show ex)

_map :: Op
_map = FuncOp (2, _map0)

_fst :: Op
_fst = UnOp (\ (Tuple (w1 : _)) -> w1)

_snd :: Op
_snd = UnOp (\ (Tuple (_ : (w2 : _))) -> w2)

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

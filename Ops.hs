module Ops where
import Parser
import Function
import Data.List
import Debug.Trace

_print :: Op
_print = UnOp (\ w -> Print (show w))

_mul0 :: Wrd -> Wrd -> Wrd
_mul0 (Num x) (Num y) = Num (x * y)

_div0 :: Wrd -> Wrd -> Wrd
_div0 (Num x) (Num y) = Num (x / y)

_add0 :: Wrd -> Wrd -> Wrd
_add0 (Num x) (Num y) = Num (x + y)
_add0 x y = Err $ "_add0: Illegal value input: x=" ++ (show x) ++ ", y=" ++ (show y)

_sub0 :: Wrd -> Wrd -> Wrd
_sub0 (Num x) (Num y) = Num (x - y)

_mul :: Op
_mul = BinOp _mul0

_div :: Op
_div = BinOp _div0

_add :: Op
_add = BinOp _add0

_sub :: Op
_sub = BinOp _sub0

_succ0 :: Wrd -> Wrd
_succ0 (Num x) = Num (x + 1)

_succ :: Op
_succ = UnOp _succ0

_eq :: Op
_eq = BinOp (\ a b -> Bool (a == b))

_neq :: Op
_neq = BinOp (\ a b -> Bool (not (a == b)))

_gt :: Op
_gt = BinOp (\ (Num a) (Num b) -> Bool (a > b))

_geq :: Op
_geq = BinOp (\ (Num a) (Num b) -> Bool (a >= b))

_lt :: Op
_lt = BinOp (\ (Num a) (Num b) -> Bool (a < b))

_leq :: Op
_leq = BinOp (\ (Num a) (Num b) -> Bool (a <= b))

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

_take :: Op
_take = UnOp (\ (Num n) -> Func $ FuncOp $ UnOp (\ (List ls) -> List (take (truncate n) ls)))

_map :: Op
_map = UnOp (\ (Func f) -> Func $ FuncOp $ UnOp (\ (List ls) -> PreList $ map (\ w -> [Func f, w]) ls))

_fst :: Op
_fst = UnOp (\ (Pair (w1, w2)) -> w1)

_snd :: Op
_snd = UnOp (\ (Pair (w1, w2)) -> w2)

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
            ("map", _map),
            ("fst", _fst),
            ("snd", _snd)
        ]

_opls_dec = reverse _opls

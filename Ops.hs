module Ops where
import Parser
import Function
import Debug.Trace

_double :: Wrd -> Double
_double (Num x) = x

_print :: Op
_print = UnOp (\ w -> Print (show w))

_mul :: Op
_mul = BinOp (\ x y -> Num ((_double x) * (_double y)))

_div :: Op
_div = BinOp (\ x y -> Num ((_double x) / (_double y)))

_add :: Op
_add = BinOp (\ x y -> Num ((_double x) + (_double y)))

_sub :: Op
_sub = BinOp (\ x y -> Num ((_double x) - (_double y)))

_succ :: Op
_succ = UnOp (\ x -> Num ((_double x) + 1.0))

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

_head :: Op
_head = UnOp (\ (List ls) -> head ls)

_opls :: [StrOp]  -- 優先順位の低い順に並べる
_opls = [
            ("print", _print),
            ("head", _head),
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
            ("succ", _succ)
        ]

_opls_dec = reverse _opls

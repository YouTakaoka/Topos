module Ops where
import Parser
import Function
import Debug.Trace

type Op = Exp -> Exp -> Exp
type StrOp = (String, Op)

_binary :: (Wrd -> Wrd -> Wrd) -> Op
_binary f xs (y : yrest) = 
    let x = last xs
        xrest = init xs
    in xrest ++ [f x y] ++ yrest

_double :: Wrd -> Double
_double (Num x) = x

_mulBin :: Wrd -> Wrd -> Wrd
_mulBin x y = Num ((_double x) * (_double y))

_print :: Op
_print _ expr = [Print (concat $ _fromExp expr)]

_mul :: Op
_mul = _binary _mulBin

_divBin :: Wrd -> Wrd -> Wrd
_divBin x y = Num ((_double x) / (_double y))

_div :: Op
_div = _binary _divBin

_addBin :: Wrd -> Wrd -> Wrd
_addBin x y = Num ((_double x) + (_double y))

_add :: Op
_add = _binary _addBin

_subBin :: Wrd -> Wrd -> Wrd
_subBin x y = Num ((_double x) - (_double y))

_sub :: Op
_sub = _binary _subBin

_succ :: Op
_succ xs (Num y : ys) = xs ++ [Num (y + 1.0)] ++ ys

_eqBin :: Wrd -> Wrd -> Wrd
_eqBin a b = Bool (a == b)

_eq :: Op
_eq = _binary _eqBin

_neqBin :: Wrd -> Wrd -> Wrd
_neqBin a b = Bool (not (a == b))

_neq :: Op
_neq = _binary _eqBin

_gtBin :: Wrd -> Wrd -> Wrd
_gtBin (Num a) (Num b) = Bool (a > b)

_gt :: Op
_gt = _binary _gtBin

_geqBin :: Wrd -> Wrd -> Wrd
_geqBin (Num a) (Num b) = Bool (a >= b)

_geq :: Op
_geq = _binary _geqBin

_ltBin :: Wrd -> Wrd -> Wrd
_ltBin (Num a) (Num b) = Bool (a < b)

_lt :: Op
_lt = _binary _ltBin

_leqBin :: Wrd -> Wrd -> Wrd
_leqBin (Num a) (Num b) = Bool (a <= b)

_leq :: Op
_leq = _binary _leqBin

_andBin :: Wrd -> Wrd -> Wrd
_andBin (Bool a) (Bool b) = Bool (a && b)

_and :: Op
_and = _binary _andBin

_orBin :: Wrd -> Wrd -> Wrd
_orBin (Bool a) (Bool b) = Bool (a || b)

_or :: Op
_or = _binary _orBin

_not :: Op
_not xs (Bool b : ys) = xs ++ [Bool (not b)] ++ ys

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
            ("succ", _succ)
        ]

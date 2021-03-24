module Ops where
import Parser
import Function
import Debug.Trace

type Op = Exp -> Exp -> Exp
type StrOp = (String, Op)

_binary :: (Wrd -> Wrd -> Wrd) -> Op
_binary f (x:_) (y:_) = [f x y]

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
_succ _ (Num x : _) = [Num (x + 1.0)]

_opls :: [StrOp]  -- 優先順位の低い順に並べる
_opls = [
            ("print", _print),
            ("+", _add),
            ("-", _sub),
            ("*", _mul),
            ("/", _div),
            ("succ", _succ)
        ]

module Ops where
import Parser
import Types
import Data.List
import Debug.Trace

_opls :: [Function]  -- 0:優先順位低 -> 9:高
_opls = [
            Operator { opName="print", operator=_print, priority=0 },
            Operator { opName="||", operator=_or, priority=1 },
            Operator { opName="&&", operator=_and, priority=2 },
            Operator { opName="!", operator=_not, priority=3 },
            Operator { opName="==", operator=_eq, priority=4 },
            Operator { opName="!=", operator=_neq, priority=4 },
            Operator { opName=">", operator=_gt, priority=4 },
            Operator { opName=">=", operator=_geq, priority=4 },
            Operator { opName="<", operator=_lt, priority=4 },
            Operator { opName="<=", operator=_leq, priority=4 },
            Operator { opName="+", operator=_add, priority=5 },
            Operator { opName="-", operator=_sub, priority=5 },
            Operator { opName="*", operator=_mul, priority=6 },
            Operator { opName="/", operator=_div, priority=6 },
            Operator { opName="//", operator=_quot, priority=6 },
            Operator { opName="%", operator=_mod, priority=6 },
            Operator { opName="^", operator=_pow, priority=7 },
            Operator { opName="$", operator=_tostr, priority=8 },
            Operator { opName="succ", operator=_succ, priority=9 },
            Operator { opName="head", operator=_head, priority=9 },
            Operator { opName="tail", operator=_tail, priority=9 },
            Operator { opName="pop", operator=_pop, priority=9 },
            Operator { opName="isEmpty", operator=_isEmpty, priority=9 },
            Operator { opName="take", operator=_take, priority=9 },
            Operator { opName="drop", operator=_drop, priority=9 },
            Operator { opName="length", operator=_length, priority=9 },
            Operator { opName="seq", operator=_seq, priority=9 },
            Operator { opName="map", operator=_map, priority=9 },
            Operator { opName="fst", operator=_fst, priority=9 },
            Operator { opName="snd", operator=_snd, priority=9 }
        ]

_print0 :: Wrd -> Either Error Wrd
_print0 w = Right $ Print (show w)

_print :: Op
_print = UnOp (_print0, [(T_Printable, T_Print)])

_mul :: Op
_mul = BinOp (_mul0, _mulSigs)

_mulSigs :: [(Type, Type, Type)]
_mulSigs = [(T_Int, T_Int, T_Int), (T_Int, T_Double, T_Double), (T_Double, T_Int, T_Double), (T_Double, T_Double, T_Double)]

_mul0 :: Wrd -> Wrd -> Either Error Wrd
_mul0 (Double x) (Double y) = Right $ Double (x * y)
_mul0 (Int x) (Int y) = Right $ Int (x * y)
_mul0 (Int x) (Double y) = Right $ Double ((fromIntegral x) * y)
_mul0 (Double x) (Int y) = Right $ Double (x * (fromIntegral y))

_div :: Op
_div = BinOp (_div0, [(T_Int, T_Int, T_Double), (T_Int, T_Double, T_Double), (T_Double, T_Int, T_Double), (T_Double, T_Double, T_Double)])

_div0 :: Wrd -> Wrd -> Either Error Wrd
_div0 _ (Int 0) = Left $ ValueError "Zero division error."
_div0 _ (Double 0.0) = Left $ ValueError "Zero division error."
_div0 (Double x) (Double y) = Right $ Double (x / y)
_div0 (Int x) (Int y) = Right $ Double ((fromIntegral x) / (fromIntegral y))
_div0 (Int x) (Double y) = Right $ Double ((fromIntegral x) / y)
_div0 (Double x) (Int y) = Right $ Double (x / (fromIntegral y))

_add :: Op
_add = BinOp (_add0,
            [(T_Int, T_Int, T_Int), (T_Int, T_Double, T_Double),
            (T_Double, T_Int, T_Double), (T_Double, T_Double, T_Double),
            (T_String, T_String, T_String),
            (T_List (T_TypeVar T_Any "a"), T_List (T_TypeVar T_Any "a"), T_List (T_TypeVar T_Any "a"))
            ])

_add0 :: Wrd -> Wrd -> Either Error Wrd
_add0 (Double x) (Double y) = Right $ Double (x + y)
_add0 (Int x) (Int y) = Right $ Int (x + y)
_add0 (Int x) (Double y) = Right $ Double ((fromIntegral x) + y)
_add0 (Double x) (Int y) = Right $ Double (x + (fromIntegral y))
_add0 (Str s1) (Str s2) = Right $ Str (s1 ++ s2)
_add0 (List l1) (List l2) = toList (l1 ++ l2)

_sub :: Op
_sub = BinOp (_sub0, [
        (T_Null, T_Int, T_Int), (T_Null, T_Double, T_Double),
        (T_Int, T_Int, T_Int), (T_Int, T_Double, T_Double),
        (T_Double, T_Int, T_Double), (T_Double, T_Double, T_Double)
        ])

_sub0 :: Wrd -> Wrd -> Either Error Wrd
_sub0 Null (Int y) = Right $ Int (-y)
_sub0 Null (Double y) = Right $ Double (-y)
_sub0 (Double x) (Double y) = Right $ Double (x - y)
_sub0 (Int x) (Int y) = Right $ Int (x - y)
_sub0 (Int x) (Double y) = Right $ Double ((fromIntegral x) - y)
_sub0 (Double x) (Int y) = Right $ Double (x - (fromIntegral y))

_mod :: Op
_mod = BinOp (_mod0, [(T_Int, T_Int, T_Int)])

_mod0 :: BinaryOp
_mod0 (Int x) (Int y) = Right $ Int $ mod x y

_quot :: Op
_quot = BinOp (_quot0, [(T_Int, T_Int, T_Int)])

_quot0 :: BinaryOp
_quot0 (Int x) (Int y) = Right $ Int $ div x y

_succ :: Op
_succ = FuncOp (_succ0, ([T_Int], T_Int))

_succ0 :: FunctionOp
_succ0 [Int x] = Right $ Int (x + 1)

_pow :: Op
_pow = BinOp (_pow0, [
        (T_Int, T_Int, T_Int), (T_Int, T_Double, T_Double),
        (T_Double, T_Int, T_Double), (T_Double, T_Double, T_Double)
        ])

_pow0 :: BinaryOp
_pow0 (Int x) (Int y) = Right $ Int $ x ^ y
_pow0 (Int x) (Double y) = Right $ Double $ (fromIntegral x) ** y
_pow0 (Double x) (Int y) = Right $ Double $ x ^ y
_pow0 (Double x) (Double y) = Right $ Double $ x ** y

_tostr :: Op
_tostr = UnOp (_tostr0, [(T_Int, T_String), (T_Double, T_String), (T_Bool, T_String)])

_tostr0 :: UnaryOp
_tostr0 (Int x) = Right $ Str $ show x
_tostr0 (Double x) = Right $ Str $ show x
_tostr0 (Bool x) = Right $ Str $ show x

_eq :: Op
_eq = BinOp ((\ a b -> Right $ Bool (a == b)), [
        (T_TypeVar T_Eq "a", T_TypeVar T_Eq "a", T_Bool)
    ])

_neq :: Op
_neq = BinOp ((\ a b -> Right $ Bool (not (a == b))), [
        (T_TypeVar T_Eq "a", T_TypeVar T_Eq "a", T_Bool)
    ])

_gt0 :: Wrd -> Wrd -> Either Error Wrd
_gt0 (Double x) (Double y) = Right $ Bool (x > y)
_gt0 (Int x) (Int y) = Right $ Bool (x > y)
_gt0 (Int x) (Double y) = Right $ Bool ((fromIntegral x) > y)
_gt0 (Double x) (Int y) = Right $ Bool (x > (fromIntegral y))

_gt :: Op
_gt = BinOp (_gt0, [(T_TypeVar T_Ord "a", T_TypeVar T_Ord "a", T_Bool)])

_geq0 :: Wrd -> Wrd -> Either Error Wrd
_geq0 (Double x) (Double y) = Right $ Bool (x >= y)
_geq0 (Int x) (Int y) = Right $ Bool (x >= y)
_geq0 (Int x) (Double y) = Right $ Bool ((fromIntegral x) >= y)
_geq0 (Double x) (Int y) = Right $ Bool (x >= (fromIntegral y))

_geq :: Op
_geq = BinOp (_geq0, [(T_TypeVar T_Ord "a", T_TypeVar T_Ord "a", T_Bool)])

_lt0 :: Wrd -> Wrd -> Either Error Wrd
_lt0 (Double x) (Double y) = Right $ Bool (x < y)
_lt0 (Int x) (Int y) = Right $ Bool (x < y)
_lt0 (Int x) (Double y) = Right $ Bool ((fromIntegral x) < y)
_lt0 (Double x) (Int y) = Right $ Bool (x < (fromIntegral y))

_lt :: Op
_lt = BinOp (_lt0, [(T_TypeVar T_Ord "a", T_TypeVar T_Ord "a", T_Bool)])

_leq0 :: Wrd -> Wrd -> Either Error Wrd
_leq0 (Double x) (Double y) = Right $ Bool (x <= y)
_leq0 (Int x) (Int y) = Right $ Bool (x <= y)
_leq0 (Int x) (Double y) = Right $ Bool ((fromIntegral x) <= y)
_leq0 (Double x) (Int y) = Right $ Bool (x <= (fromIntegral y))

_leq :: Op
_leq = BinOp (_leq0, [(T_TypeVar T_Ord "a", T_TypeVar T_Ord "a", T_Bool)])

_and :: Op
_and = BinOp ((\ (Bool a) (Bool b) -> Right $ Bool (a && b)), [(T_Bool, T_Bool, T_Bool)])

_or :: Op
_or = BinOp ((\ (Bool a) (Bool b) -> Right $ Bool (a || b)), [(T_Bool, T_Bool, T_Bool)])

_not :: Op
_not = UnOp ((\ (Bool b) -> Right $ Bool (not b)), [(T_Bool, T_Bool)])

_head :: Op
_head = UnOp (_head0, [(T_List (T_TypeVar T_Any "a"), T_TypeVar T_Any "a")])

_head0 :: Wrd -> Either Error Wrd
_head0 (List []) = Left $ ValueError "head: Empty list."
_head0 (List (x : _)) = Right x

_tail :: Op
_tail = UnOp (_tail0, [(T_List (T_TypeVar T_Any "a"), T_List (T_TypeVar T_Any "a"))])

_tail0 :: Wrd -> Either Error Wrd
_tail0 (List (_ : xs)) = Right $ List xs
_tail0 (List []) = Left $ ValueError "tail: Empty list."

_pop0 :: Wrd -> Either Error Wrd
_pop0 (List (x : xs)) = Right $ Tuple [x, List xs]
_pop0 (List []) = Left $ ValueError "pop: Empty list."

_pop :: Op
_pop = UnOp (_pop0, [(
            T_List (T_TypeVar T_Any "a"),
            T_Tuple [T_TypeVar T_Any "a", T_List (T_TypeVar T_Any "a")]
        )])

_isEmpty :: Op
_isEmpty = UnOp ((\ (List ls) -> Right $ Bool (ls == [])), [(
        T_List T_Any,
        T_Bool
    )])

_length :: Op
_length = UnOp ((\ (List ls) -> Right $ Int (length ls)), [(
        T_List T_Any,
        T_Int
    )])

_take :: Op
_take = FuncOp (_take0, (
        [T_Int, T_List (T_TypeVar T_Any "a")],
        T_List (T_TypeVar T_Any "a")
    ))

_take0 :: Exp -> Either Error Wrd
_take0 [Int n, List ls] =
    if length ls < n
        then Left $ ValueError "take: Length of given list is shorter than specified integer."
        else Right $ List $ take n ls

_drop :: Op
_drop = FuncOp (_drop0, (
        [T_Int, T_List (T_TypeVar T_Any "a")],
        T_List (T_TypeVar T_Any "a")
    ))

_drop0 :: Exp -> Either Error Wrd
_drop0 [Int n, List ls] =
    if length ls < n
        then Left $ ValueError "drop: Length of given list is shorter than specified integer."
        else Right $ List $ drop n ls

_seq0 :: Exp -> Either Error Wrd
_seq0 (Int n : (Int m : [])) = Right $ List $ map (\ x -> Int x) [n .. m]

_seq :: Op
_seq = FuncOp (_seq0, (
        [T_Int, T_Int],
        T_List T_Int
    ))

_map :: Op
_map = FuncOp (_map0, (
        [T_Func T_Function { funcName_t="", args_t=[T_TypeVar T_Any "a"], return_t=T_TypeVar T_Any "b", priority_ft=9 }, T_List (T_TypeVar T_Any "a")],
        T_List (T_TypeVar T_Any "b")
    ))

_map0 :: Exp -> Either Error Wrd
_map0 [Func f, List ls] = Right $ PreList $ map (\ w -> [Func f, w]) ls

_fst :: Op
_fst = UnOp (\ (Tuple [w1, _]) -> Right w1, [
        (T_Tuple [T_TypeVar T_Any "a", T_Any], T_TypeVar T_Any "a")
    ])

_snd :: Op
_snd = UnOp (\ (Tuple [_, w2]) -> Right w2, [
        (T_Tuple [T_Any, T_TypeVar T_Any "a"], T_TypeVar T_Any "a")
    ])

_getType :: Wrd -> Type
_getType (Str _) = T_String
_getType (Int _) = T_Int
_getType (Double _) = T_Double
_getType (Bool _) = T_Bool
_getType (Tobe _) = T_Unknown
_getType (List (w: _)) = T_List $ _getType w
_getType (List []) = T_EmptyList
_getType (Tuple tp) = T_Tuple $ map _getType tp
_getType (Func Operator { opName=name, operator=BinOp (_, sigs), priority=prt }) =
    T_Func (T_Operator {opName_t=name, operator_sig=BinSig sigs, priority_t=prt})
_getType (Func Operator { opName=name, operator=UnOp (_, sigs), priority=prt }) =
    T_Func (T_Operator {opName_t=name, operator_sig=UnSig sigs, priority_t=prt})
_getType (Func Operator { opName=name, operator=FuncOp (_, sig), priority=prt }) =
    T_Func T_Function { funcName_t=name, args_t=fst sig, return_t=snd sig, priority_ft=prt }
_getType (Func f) = T_Func $ getFunctionSignature f
_getType (Type _) = T_Type
_getType (TypeCheck _) = T_TypeCheck

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
        Left (t1, t2) -> Left $ TypeError { expected_types=[t1], got_type=t2,
            message_TE="List: Inconsistent type of elements: Found type `" ++ show t1 ++ "` and `" ++ show t2 ++ "`" }

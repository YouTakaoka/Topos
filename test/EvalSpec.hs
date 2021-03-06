module EvalSpec (spec) where

import Test.Hspec
import Types
import Ops
import Utils
import Eval

spec :: Spec
spec = do
    let Result (_, fact_binds) = _eval M_Normal  [] $ toExp "define fact as Function < Int -> Int >: x -> if x > 0 then x * (fact (x - 1)) else 1"
        Result (_, sqr_binds) = _eval M_Normal  [] $ toExp "define sqr as Function < Int -> Int >: x -> x * x"
        Result (_, sqr_binds2) = _eval M_Normal  [] $ toExp "let sqr = Function < Int -> Int >: x -> x * x"
    describe "_eval（ノーマルモード）" $ do
        it "ただの計算0" $
            _eval M_Normal [] (toExp "4 * 2") `shouldBe` Result (Int 8, [])
        it "ただの計算1" $
            _eval M_Normal [] (toExp "4 * ( 2 + 3 )") `shouldBe` Result (Int 20, [])
        it "ただの計算2" $
            _eval M_Normal [] (toExp "3 * succ 4") `shouldBe` Result (Int 15, [])
        it "ただの計算3" $
            _eval M_Normal [] (toExp "4 / ((3 + 5) * 2)") `shouldBe` Result (Double 0.25, [])
        it "ただの計算4" $
            _eval M_Normal [] (toExp "10 % 3") `shouldBe` Result (Int 1, [])
        it "ただの計算5" $
            _eval M_Normal [] (toExp "10 // 3") `shouldBe` Result (Int 3, [])
        it "ただの計算6" $
            _eval M_Normal [] (toExp "(-5) * 3") `shouldBe` Result (Int (-15), [])
        it "floor" $
            _eval M_Normal [] (toExp "floor 5.3") `shouldBe` Result (Int 5, [])
        it "toDouble" $
            _eval M_Normal [] (toExp "toDouble 5") `shouldBe` Result (Double 5.0, [])
        it "関数リテラル1" $
            _eval M_Normal [] (toExp "(Function <Int -> Int>: x -> x * x) 3") `shouldBe` Result (Int 9, [])
        it "if条件分岐1" $
            _eval M_Normal [] (toExp "if 4 > 5 then \"hoge\" else \"fuga\"") `shouldBe` Result (Str "fuga", [])
        it "if条件分岐2" $
            _eval M_Normal [Bind {identifier="x", value=Int 5, vtype=T_Int}] (toExp "if x > 0 then \"hoge\" else \"fuga\"") `shouldBe` Result (Str "hoge", [Bind {identifier="x", value=Int 5, vtype=T_Int}])
        it "ifの中でlet" $
            _eval M_Normal [] (toExp "if (let a = 4) > 3 then 2 * a else a - 2") `shouldBe` Result (Int 8, [])
        it "letnでローカル変数" $
            _eval M_Normal [] (toExp "(Function <Int -> Int>: x -> (letn a = 4) a * x) 3") `shouldBe` Result (Int 12, [])
        it "関数の中でif" $
            _eval M_Normal [] (toExp "(Function < Int -> String >: x -> if x > 3 then \"hoge\" else \"fuga\") 4") `shouldBe` Result (Str "hoge", [])
        it "再帰関数" $
            _eval M_Normal fact_binds (toExp "fact 4") `shouldBe` Result (Int 24, fact_binds)
        it "計算の順序1" $
            _eval M_Normal [] (toExp "1 + succ 3") `shouldBe` Result (Int 5, [])
        it "計算の順序2" $
            _eval M_Normal fact_binds (toExp "1 + fact 3") `shouldBe` Result (Int 7, fact_binds)
        it "計算の順序3" $
            _eval M_Normal sqr_binds (toExp "1 + sqr 3") `shouldBe` Result (Int 10, sqr_binds)
        it "計算の順序4" $
            _eval M_Normal sqr_binds2 (toExp "1 + sqr 3") `shouldBe` Result (Int 10, sqr_binds2)
        it "計算の順序5" $
            _eval M_Normal [] (toExp "1 + (Function <Int -> Int>: x -> x * x) 3") `shouldBe` Result (Int 10, [])
        it "計算の順序6" $
            _eval M_Normal [] (toExp "2 ^ 4") `shouldBe` Result (Int 16, [])
        it "計算の順序7" $
            _eval M_Normal [] (toExp "4 ^ 0.5") `shouldBe` Result (Double 2.0, [])
        it "リスト処理（PreList）のバグ" $
            _eval M_Normal [] (toExp "[1,2,3] + map succ [4,5]") `shouldBe` Result (List [Int 1, Int 2, Int 3, Int 5, Int 6], [])
        it "take(1)" $
            _eval M_Normal [] (toExp "take 3 [1,2,3,4,5]") `shouldBe` Result (List [Int 1, Int 2, Int 3], [])
        it "take(2)" $
            _eval M_Normal [] (toExp "take 3 [1,2,3]") `shouldBe` Result (List [Int 1, Int 2, Int 3], [])
        it "drop(1)" $
            _eval M_Normal [] (toExp "drop 3 [1,2,3,4,5]") `shouldBe` Result (List [Int 4, Int 5], [])
        it "drop(2)" $
            _eval M_Normal [] (toExp "drop 3 [1,2,3]") `shouldBe` Result (List [], [])
        it "length" $
            _eval M_Normal [] (toExp "length [1,2,3]") `shouldBe` Result (Int 3, [])
        it "isEmpty 1" $
            _eval M_Normal [] (toExp "isEmpty []") `shouldBe` Result (Bool True, [])
        it "isEmpty 2" $
            _eval M_Normal [] (toExp "isEmpty [1]") `shouldBe` Result (Bool False, [])
        it "コメント" $
            _eval M_Normal sqr_binds (toExp "1 + succ 3 #hogehoge") `shouldBe` Result (Int 5, sqr_binds)
        it "タプル" $
            _eval M_Normal [] (toExp "print (pop [1,2,3])") `shouldBe` Result (Print "(1,[2,3])", [])
        it "文字列" $
            _eval M_Normal [] (toExp "\"I'm \" + $30 + \" years old.\"") `shouldBe` Result (Str "I'm 30 years old.", [])
        it "文字列" $
            _eval M_Normal [] (toExp "\"I'm \" + $(25 + 5) + \" years old.\"") `shouldBe` Result (Str "I'm 30 years old.", [])
        it "関数の中でmap 1" $
            _eval M_Normal sqr_binds (toExp "(Function <Int -> List Int>: n -> map sqr (seq 1 n)) 3") `shouldBe` Result (List [Int 1,Int 4,Int 9], [])
        it "関数の中でmap 2" $
            _eval M_Normal [] (toExp "(Function <Int -> List Int>: n -> map succ (seq 1 n)) 3") `shouldBe` Result (List [Int 2,Int 3,Int 4], [])
    describe "_evalFunctions（タイプチェックモード）" $ do
        it "関数タイプチェック" $
            _evalFunctions M_TypeCheck [] [TypeCheck T_Int, Tobe "*", TypeCheck T_Int] `shouldBe` Result (TypeCheck T_Int, [])
        it "_mulSubOp" $
            _mulSubOp M_TypeCheck _opls [Tobe "*"] `shouldBe` [TypeCheck (T_Func (T_Operator { opName_t="*", operator_sig=BinSig _mulSigs, priority_t=6 }))]
        it "無理やり置き換え" $
            _evalFunctions M_TypeCheck [] [TypeCheck T_Int, TypeCheck (T_Func (T_Operator { opName_t="*", operator_sig=BinSig _mulSigs, priority_t=6 })), TypeCheck T_Int] `shouldBe` Result (TypeCheck T_Int, []) 
        it "Strが変換されない件1" $
            _eval M_TypeCheck [] [Str "hoge"] `shouldBe` Result (TypeCheck T_String, [])
        it "Strが変換されない件2" $
            _evalFunctions M_TypeCheck [] [Str "hoge"] `shouldBe` Result (TypeCheck T_String, [])
        it "Intは変換されるか？" $
            _evalFunctions M_TypeCheck [] [Tobe "2"] `shouldBe` Result (TypeCheck T_Int, [])
    describe "エラーチェック" $ do
        it "UnknownKeywordError 1" $
            _eval M_Normal [] (toExp "1 + hoge") `shouldBe` Error (UnknownKeywordError "hoge")
        it "UnknownKeywordError 2" $
            _eval M_Normal [] (toExp "let g = Function <UnaryOp, Int -> Int > : op x -> op ( op x )")
                `shouldBe` Error (UnknownKeywordError "UnaryOp")
        it "関数の型エラー1" $
            _eval M_Normal [] (toExp "Function <Int -> Double>: x -> x") `shouldBe` Error (TypeError { expected_types=[T_Double], got_type=T_Int, message_TE="" })
        it "関数の型エラー2" $
            _eval M_Normal [] (toExp "Function <Int, Double -> Int>: x y -> x * y") `shouldBe` Error (TypeError { expected_types=[T_Int], got_type=T_Double, message_TE="" })
        it "関数の型エラー3" $
            _eval M_Normal [] (toExp "Function <List Int, List Double -> List Int>: ls1 ls2 -> ls1 + ls2") `shouldBe` Error (TypeError { expected_types=[T_List T_Int], got_type=T_List T_Double, message_TE="" })
        it "関数の型エラー3.5(成功例)" $
            _eval M_Normal [] (toExp "(Function <List Int, List Int -> List Int>: ls1 ls2 -> ls1 + ls2) [1,2] [3,4,5]") `shouldBe` Result (List [Int 1,Int 2,Int 3,Int 4,Int 5], [])
        it "関数の型エラー4" $
            _eval M_Normal [] (toExp "Function <Int, String -> Double>: x y -> x ^ y") `shouldBe` Error (TypeError { expected_types=[T_Int, T_Double], got_type=T_String, message_TE="" } )
        it "mapの型エラー" $
            _eval M_Normal sqr_binds (toExp "Function <List String -> List String>: ls -> map sqr ls") `shouldBe` Error (TypeError { expected_types=[T_List T_Int], got_type=T_List T_String, message_TE="" } )
        it "_typeSub" $
            _typeSub [Bind { identifier="a", vtype=T_Type, value=Type T_Int }] (T_List $ T_TypeVar T_Any "a") `shouldBe` T_List T_Int
        it "_typeCheck" $
            _typeCheck [] (T_Func T_Function { funcName_t="", args_t=[T_Int], return_t=T_String, priority_ft=9 }) (T_Func T_Function { funcName_t="", args_t=[T_TypeVar T_Any "a"], return_t=T_TypeVar T_Any "b", priority_ft=9 }) `shouldBe` Just [Bind { identifier="b", vtype=T_Type, value=Type T_String }, Bind { identifier="a", vtype=T_Type, value=Type T_Int }]
        it "validateFuncSig" $
            validateFuncSig [T_Func T_Function {  funcName_t="", args_t=[T_Int], return_t=T_String, priority_ft=9 }, T_Int] [T_Func T_Function { funcName_t="", args_t=[T_TypeVar T_Any "a"], return_t=T_TypeVar T_Any "b", priority_ft=9 }, T_TypeVar T_Any "a"] (T_TypeVar T_Any "b") `shouldBe` Right T_String
        it "演算子の型エラー1" $
            _eval M_Normal [] (toExp "2 * \"hoge\"") `shouldBe` Error TypeError { expected_types=[T_Int, T_Double], got_type=T_String, message_TE="" }
        it "関数の型エラー(実行時) " $
            _eval M_Normal [] (toExp "(Function <String -> String>: x -> x) 5") `shouldBe`
                Error TypeError { expected_types=[T_String], got_type=T_Int, message_TE="" }
        it "関数の型エラー時に関数名を表示" $
            (message_TE $ (\ (Error x) -> x) $ _eval M_Normal sqr_binds (toExp "sqr \"hoge\"")) `shouldBe` "Function `sqr`: Type mismatch at the first argument. Expected: T_Int, but got: T_String"
        it "letnでローカル変数（スコープ）" $
            _eval M_Normal [] (toExp "(Function <Int -> Int>: x -> (letn a = 4) a * x) a") `shouldBe` Error (UnknownKeywordError "a")
        it "takeで長さ足りなかったパターン" $
            _eval M_Normal [] (toExp "take 4 [1,2,3]") `shouldBe` Error (ValueError "take: Length of given list is shorter than specified integer.")
        it "dropで長さ足りなかったパターン" $
            _eval M_Normal [] (toExp "drop 4 [1,2,3]") `shouldBe` Error (ValueError "drop: Length of given list is shorter than specified integer.")

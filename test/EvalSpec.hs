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
        it "関数リテラル1" $
            _eval M_Normal [] (toExp "(Function <Int -> Int>: x -> x * x) 3") `shouldBe` Result (Int 9, [])
        it "if条件分岐1" $
            _eval M_Normal [] (toExp "if 4 > 5 then \"hoge\" else \"fuga\"") `shouldBe` Result (Str "fuga", [])
        it "if条件分岐2" $
            _eval M_Normal [Bind {identifier="x", value=Int 5, vtype=T_Int}] (toExp "if x > 0 then \"hoge\" else \"fuga\"") `shouldBe` Result (Str "hoge", [Bind {identifier="x", value=Int 5, vtype=T_Int}])
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
        it "isEmpty 1" $
            _eval M_Normal [] (toExp "isEmpty []") `shouldBe` Result (Bool True, [])
        it "isEmpty 2" $
            _eval M_Normal [] (toExp "isEmpty [1]") `shouldBe` Result (Bool False, [])
        it "コメント" $
            _eval M_Normal sqr_binds (toExp "1 + succ 3 #hogehoge") `shouldBe` Result (Int 5, sqr_binds)
        it "タプル" $
            _eval M_Normal  [] (toExp "print (pop [1,2,3])") `shouldBe` Result (Print "(1,[2,3])", [])
        it "文字列" $
            _eval M_Normal  [] (toExp "\"I'm \" + $30 + \" years old.\"") `shouldBe` Result (Str "I'm 30 years old.", [])
        it "文字列" $
            _eval M_Normal  [] (toExp "\"I'm \" + $(25 + 5) + \" years old.\"") `shouldBe` Result (Str "I'm 30 years old.", [])
    describe "_evalFunctions（タイプチェックモード）" $ do
        it "関数タイプチェック" $
            _evalFunctions M_TypeCheck [] [TypeCheck T_Int, Tobe "*", TypeCheck T_Int] `shouldBe` Result (TypeCheck T_Int, [])
        it "_mulSubOp" $
            _mulSubOp M_TypeCheck _opls_dec [Tobe "*"] `shouldBe` [TypeCheck (T_Func (T_Operator { opName_t="*", operator_t=BinOp _mul_t, priority_t=6 }))]
        it "無理やり置き換え" $
            _evalFunctions M_TypeCheck [] [TypeCheck T_Int, TypeCheck (T_Func (T_Operator { opName_t="*", operator_t=BinOp _mul_t, priority_t=6 })), TypeCheck T_Int] `shouldBe` Result (TypeCheck T_Int, []) 
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
            _eval M_Normal [] (toExp "Function <Int -> Double>: x -> x") `shouldBe` Error (TypeError T_Double T_Int "")
        it "関数の型エラー2" $
            _eval M_Normal [] (toExp "Function <Int, Double -> Int>: x y -> x * y") `shouldBe` Error (TypeError T_Int T_Double "")
        it "関数の型エラー3" $
            _eval M_Normal [] (toExp "Function <List Int, List Double -> List Int>: ls1 ls2 -> ls1 + ls2") `shouldBe` Error (TypeError T_Int T_Double "")
        it "関数の型エラー4" $
            _eval M_Normal [] (toExp "Function <Int, String -> Double>: x y -> x ^ y") `shouldBe` Error (TypeError T_Num T_String "")

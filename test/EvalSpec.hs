module EvalSpec (spec) where

import Test.Hspec
import Types
import Ops
import Utils
import Eval

spec :: Spec
spec = do
    let fact_binds = snd $ _eval M_Normal  [] $ toExp "let fact = Function < Int -> Int >: x -> if x > 0 then x * (fact (x - 1)) else 1"
    describe "_eval（ノーマルモード）" $ do
        it "ただの計算1" $
            _eval M_Normal [] (toExp "4 * ( 2 + 3 )") `shouldBe` (Int 20, [])
        it "ただの計算2" $
            _eval M_Normal [] (toExp "3 * succ 4") `shouldBe` (Int 15, [])
        it "ただの計算3" $
            _eval M_Normal [] (toExp "4 / ((3 + 5) * 2)") `shouldBe` (Double 0.25, [])
        it "関数リテラル1" $
            _eval M_Normal [] (toExp "(Function <Int -> Int>: x -> x * x) 3") `shouldBe` (Int 9, [])
        it "if条件分岐1" $
            _eval M_Normal [] (toExp "if 4 > 5 then \"hoge\" else \"fuga\"") `shouldBe` (Str "fuga", [])
        it "if条件分岐2" $
            _eval M_Normal [Bind {identifier="x", value=Int 5, vtype=T_Int}] (toExp "if x > 0 then \"hoge\" else \"fuga\"") `shouldBe` (Str "hoge", [Bind {identifier="x", value=Int 5, vtype=T_Int}])
        it "関数の中でif" $
            _eval M_Normal [] (toExp "(Function < Int -> Str >: x -> if x > 3 then \"hoge\" else \"fuga\") 4") `shouldBe` (Str "hoge", [])
        it "再帰関数" $
            _eval M_Normal fact_binds (toExp "fact 4") `shouldBe` (Int 24, fact_binds)
    describe "_evalFunctions（タイプチェックモード）" $ do
        it "関数タイプチェック" $
            _evalFunctions M_TypeCheck [] [TypeCheck T_Int, Tobe "*", TypeCheck T_Int] `shouldBe` (TypeCheck T_Int, [])
        it "_mulSubOp" $
            _mulSubOp M_TypeCheck _opls_dec [Tobe "*"] `shouldBe` [TypeCheck (T_Func (T_Operator ("*", BinOp _mul_t)))]
        it "無理やり置き換え" $
            _evalFunctions M_TypeCheck [] [TypeCheck T_Int, TypeCheck (T_Func (T_Operator ("*", BinOp _mul_t))), TypeCheck T_Int] `shouldBe` (TypeCheck T_Int, []) 

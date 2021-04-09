module EvalSpec (spec) where

import Test.Hspec
import Types
import Ops
import Utils
import Eval

spec :: Spec
spec = do
    let fact_binds = snd $ _eval M_Normal  [] $ toExp "define fact as Function < Int -> Int >: x -> if x > 0 then x * (fact (x - 1)) else 1"
        sqr_binds = snd $ _eval M_Normal  [] $ toExp "define sqr as Function < Int -> Int >: x -> x * x"
        sqr_binds2 = snd $ _eval M_Normal  [] $ toExp "let sqr = Function < Int -> Int >: x -> x * x"
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
        it "計算の順序1" $
            _eval M_Normal [] (toExp "1 + succ 3") `shouldBe` (Int 5, [])
        it "計算の順序2" $
            _eval M_Normal fact_binds (toExp "1 + fact 3") `shouldBe` (Int 7, fact_binds)
        it "計算の順序3" $
            _eval M_Normal sqr_binds (toExp "1 + sqr 3") `shouldBe` (Int 10, sqr_binds)
        it "計算の順序4" $
            _eval M_Normal sqr_binds2 (toExp "1 + sqr 3") `shouldBe` (Int 10, sqr_binds2)
        it "計算の順序5" $
            _eval M_Normal [] (toExp "1 + (Function <Int -> Int>: x -> x * x) 3") `shouldBe` (Int 10, [])
    describe "_evalFunctions（タイプチェックモード）" $ do
        it "関数タイプチェック" $
            _evalFunctions M_TypeCheck [] [TypeCheck T_Int, Tobe "*", TypeCheck T_Int] `shouldBe` (TypeCheck T_Int, [])
        it "_mulSubOp" $
            _mulSubOp M_TypeCheck _opls_dec [Tobe "*"] `shouldBe` [TypeCheck (T_Func (T_Operator ("*", BinOp _mul_t)))]
        it "無理やり置き換え" $
            _evalFunctions M_TypeCheck [] [TypeCheck T_Int, TypeCheck (T_Func (T_Operator ("*", BinOp _mul_t))), TypeCheck T_Int] `shouldBe` (TypeCheck T_Int, []) 
        it "Strが変換されない件1" $
            _eval M_TypeCheck [] [Str "hoge"] `shouldBe` (TypeCheck T_String, [])
        it "Strが変換されない件2" $
            _evalFunctions M_TypeCheck [] [Str "hoge"] `shouldBe` (TypeCheck T_String, [])
        it "Intは変換されるか？" $
            _evalFunctions M_TypeCheck [] [Tobe "2"] `shouldBe` (TypeCheck T_Int, [])

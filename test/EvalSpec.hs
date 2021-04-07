module EvalSpec (spec) where

import Test.Hspec
import Types
import Ops
import Utils
import Eval

spec :: Spec
spec = do
    describe "_eval（ノーマルモード）" $
        it "ただの計算1" $
            _eval M_Normal [] (toExp "4 * ( 2 + 3 )") `shouldBe` (Int 20, [])
    describe "_eval（ノーマルモード）" $
        it "ただの計算2" $
            _eval M_Normal [] (toExp "3 * succ 4") `shouldBe` (Int 15, [])
    describe "_eval（ノーマルモード）" $
        it "ただの計算3" $
            _eval M_Normal [] (toExp "4 / ((3 + 5) * 2)") `shouldBe` (Double 0.25, [])

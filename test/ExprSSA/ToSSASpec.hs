module ExprSSA.ToSSASpec (spec) where

import Test.Hspec
import ExprSSA.Types
import ExprSSA.ToSSA

-- | Wrapper to convert an 'Expr' to an 'SSAProg' using the basic counter fresh
-- name generator, with final result named @return@.
exprToSSA' :: Expr -> SSAProg
exprToSSA' = runGenState . exprToSSA (Just "return")

-- | Helper to encode the "return" final result pattern.
lastStmt :: SSAExpr -> SSAStmt
lastStmt = SSAStmt "return"

spec :: Spec
spec = do
  describe "Expr -> SSA conversion" $ do
    it "converts a plain value expression" $
      let expr = ENum 42
          prog = lastStmt (SSAExprValue (SSAValueNum 42)) : []
       in exprToSSA' expr `shouldBe` prog
    it "converts a plain var expression" $
      let expr = EVar "x"
          prog = lastStmt (SSAExprValue (SSAValueVar "x")) : []
       in exprToSSA' expr `shouldBe` prog

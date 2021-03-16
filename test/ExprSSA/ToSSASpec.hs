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
    it "converts a plain value expression: 42 == \"return := 42\"" $
      let expr = ENum 42
          prog = [ lastStmt (SSAExprValue (SSAValueNum 42)) ]
       in exprToSSA' expr `shouldBe` prog
    it "converts a plain var expression:    x == \"return :=  x\"" $
      let expr = EVar "x"
          prog = [ lastStmt (SSAExprValue (SSAValueVar "x")) ]
       in exprToSSA' expr `shouldBe` prog
    it "converts a depth-1 binop expression: x+1     == \"return := x+1\"" $
      let expr = EBinOp Add (EVar "x") (ENum 1)
          prog = [ lastStmt (SSAExprBinOp Add (SSAValueVar "x") (SSAValueNum 1)) ]
       in exprToSSA' expr `shouldBe` prog
    it "converts a depth-2 binop expression: (x+y)*2 == \"L0 := x+y; return = L0*2\"" $
      let expr = EBinOp Mul (EBinOp Add (EVar "x") (EVar "y")) (ENum 2)
          prog = [ SSAStmt "L0" (SSAExprBinOp Add (SSAValueVar "x") (SSAValueVar "y"))
                 , lastStmt (SSAExprBinOp Mul (SSAValueVar "L0") (SSAValueNum 2)) ]
       in exprToSSA' expr `shouldBe` prog

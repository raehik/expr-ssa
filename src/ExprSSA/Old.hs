module ExprSSA.Old where

import           ExprSSA.Types
import           ExprSSA.Util
import qualified Data.Text as Text

-- | Original Erlang algorithm, with fresh name generator threaded through.
exprToSSAOrig :: Name -> Expr -> SSAProg
exprToSSAOrig returnName expr = go [] 0 [(returnName, expr)]
  where
    go :: SSAProg -> Int -> [(Name, Expr)] -> SSAProg
    go prog _      []                         = prog
    go prog gen ((exprVar, expr):taggedExprs) =
        let (ssaExpr, gen', taggedExprs') =
                case expr of
                  ENum x -> (SSAExprValue (SSAValueNum x), gen, taggedExprs)
                  EVar v -> (SSAExprValue (SSAValueVar v), gen, taggedExprs)
                  EBinOp op e1 e2 ->
                    let (gen',  taggedExprs',  e1SSAVal) = processInnerExpr gen  taggedExprs  e1
                        (gen'', taggedExprs'', e2SSAVal) = processInnerExpr gen' taggedExprs' e2
                     in (SSAExprBinOp op e1SSAVal e2SSAVal, gen'', taggedExprs'')
         in go (SSAStmt exprVar ssaExpr : prog) gen' taggedExprs'
    varGen :: Int -> Name
    varGen = Text.cons 'L' . tshow
    -- process the LHS/RHS of a binop (with plenty of threaded state)
    -- since if they're plain values, we can use them as-is
    processInnerExpr :: Int -> [(Name, Expr)] -> Expr -> (Int, [(Name, Expr)], SSAValue)
    processInnerExpr gen texs = \case
      ENum x -> (gen, texs, SSAValueNum x)
      EVar v -> (gen, texs, SSAValueVar v)
      e      -> (gen+1, (varGen gen, e):texs, SSAValueVar (varGen gen))

module ExprSSA.Examples where

import           ExprSSA.Types
import           ExprSSA.ToSSA
import           ExprSSA.Pretty
import qualified Data.Text.IO   as Text

-- | Translate an 'Expr' to an 'SSAProg' using a counter for fresh names, and
--   pretty print to stdout.
testTranslateExprAndPrint :: Expr -> IO ()
testTranslateExprAndPrint =
    Text.putStrLn . prettySSAProg . runGenState . exprToSSA (Just "return")

-- | Pretty print an 'Expr' to stdout.
testPrintExpr :: Expr -> IO ()
testPrintExpr =
    Text.putStrLn . prettyExpr Nothing

--------------------------------------------------------------------------------

-- 1*b + (2*b+1*b)*0
-- (stupid example lol?)
ex1 :: Expr
ex1 =
  EBinOp Add
    (EBinOp Mul
      (ENum 1)
      (EVar "b"))
    (EBinOp Mul
      (EBinOp Add
        (EBinOp Mul
          (ENum 2)
          (EVar "b"))
        (EBinOp Mul
          (ENum 2)
          (EVar "b")))
      (ENum 0))

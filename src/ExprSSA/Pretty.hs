module ExprSSA.Pretty where

import           ExprSSA.Types
import           ExprSSA.Util
import qualified Data.Text      as Text
import           Data.Text (Text)

prettySSAProg :: SSAProg -> Text
prettySSAProg = Text.intercalate "\n" . map prettySSAStmt

prettySSAStmt :: SSAStmt -> Text
prettySSAStmt (SSAStmt var expr) = var <> " := " <> prettySSAExpr expr

prettySSAExpr :: SSAExpr -> Text
prettySSAExpr = \case
  SSAExprValue x -> prettySSAValue x
  SSAExprBinOp op e1 e2 ->
    prettySSAValue e1 <> prettyBinOp op <> prettySSAValue e2

prettySSAValue :: SSAValue -> Text
prettySSAValue = \case
  SSAValueNum x -> tshow x
  SSAValueVar v -> v

prettyBinOp :: BinOp -> Text
prettyBinOp = \case
  Add -> "+"
  Mul -> "*"

--------------------------------------------------------------------------------

-- | Expression precedence.
--
-- Only certain operators have precedence, other expressions will be Nothing.
type Prec = Maybe Int

-- | Pretty print an 'Expr' with minimal brackets.
--
-- If printing a fully-contained expression (not in context of another
-- expression), pass 'Nothing' for precedence.
prettyExpr :: Prec -> Expr -> Text
prettyExpr _          (ENum x) = tshow x
prettyExpr _          (EVar v) = v
prettyExpr parentPrec e@(EBinOp op e1 e2) =
    if   precIsHigher parentPrec curPrec
    then "(" <> next <> ")"
    else        next
  where
    prettyNext = prettyExpr curPrec
    next = prettyNext e1 <> prettyBinOp op <> prettyNext e2
    curPrec = prec e

prec :: Expr -> Prec
prec (EBinOp op _ _) = Just (precBinOp op)
prec _               = Nothing

precBinOp :: BinOp -> Int
precBinOp Add = 10
precBinOp Mul = 20

-- Almost Maybe (Ord a), but False if there's a Nothing anywhere.
precIsHigher :: Prec -> Prec -> Bool
precIsHigher Nothing           _              = False
precIsHigher _                 Nothing        = False
precIsHigher (Just precParent) (Just precCur) = precParent > precCur

--------------------------------------------------------------------------------

-- | Pretty print an 'Expr' without trying to minimise brackets.
--
-- Simpler, perhaps useful for testing.
prettyExprNoPrec :: Expr -> Text
prettyExprNoPrec (ENum x) = tshow x
prettyExprNoPrec (EVar v) = v
prettyExprNoPrec (EBinOp op e1 e2) =
    "(" <> prettyExprNoPrec e1 <> prettyBinOp op <> prettyExprNoPrec e2 <> ")"

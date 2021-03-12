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

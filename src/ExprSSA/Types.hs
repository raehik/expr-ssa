{-# LANGUAGE FlexibleInstances #-}

-- | Type definitions, including instances for the fresh name generator.

module ExprSSA.Types where

import           ExprSSA.Util
import qualified Data.Text      as Text
import           Data.Text (Text)
import           Control.Monad.State.Lazy

type Name = Text

data Expr
  = ENum Int
  | EVar Name
  | EBinOp BinOp Expr Expr
    deriving (Eq, Ord, Show)

data BinOp
  = Add
  | Mul
    deriving (Eq, Ord, Show)

type SSAProg = [SSAStmt]
data SSAStmt = SSAStmt Name SSAExpr
    deriving (Eq, Ord, Show)

data SSAExpr
  = SSAExprValue SSAValue
  | SSAExprBinOp BinOp SSAValue SSAValue
    deriving (Eq, Ord, Show)

data SSAValue
  = SSAValueNum Int
  | SSAValueVar Name
    deriving (Eq, Ord, Show)

-- | Monad providing generation of fresh names.
--
-- There are options for name generation:
--   * pure, deterministic generator
--     * simple counter
--     * PRNG
--   * impure, non-deterministic generator (IO-based [P]RNG)
class Monad m => MonadGen m where
    -- | Generate a fresh name.
    fresh :: m Name

instance MonadGen (State Int) where
    fresh = do
        counter <- get
        put (counter+1)
        return (Text.cons 'L' $ tshow counter)

runGenState :: State Int a -> a
runGenState = flip evalState 0

{-# LANGUAGE FlexibleInstances #-}

{- Thoughts.
To make this more like a compiler, I suppose SSA variables should be marked with
where they came from: are they local variables that should be calculated in the
current program, or are they expected to be in the environment? Could extend the
types to make that distinction.
-}

module ExprSSA.ToSSA where

import           ExprSSA.Types
import           ExprSSA.Util
import qualified Data.Map       as Map
import           Data.Map (Map)
import           Data.Maybe (catMaybes)

-- | Translate the given 'Expr' to an 'SSAProg'.
--
-- The algorithm works top-down on a stack of expressions, each tagged with a
-- variable. For each tagged expression, any subexpressions are tagged with a
-- fresh variable and added to the processing stack, and an SSA statement is
-- generated (referring to any subexpressions using their fresh variables).
--
-- The program is built in reverse (@cons@ing on at each step), meaning the
-- earlier an expression is tagged, the later it will be assigned to in the
-- resulting program. This way, the subexpressions of an expression are always
-- defined before use.
--
-- If a 'Just Name' is provided, the expression result is assigned to it.
-- Otherwise, a fresh name is generated.
--
-- Pulling name generation out of the algorithm allows for much more
-- composability: see how translation is defined in clear steps, and how the
-- main algorithm only glues things together (with 0 pattern matching!). And the
-- leaf expression direct translation optimization is a simple reuseable monadic
-- action (it's transparent, I don't even need to mention it in the code or docs
-- for the main algorithm).
--
-- TODO: Interestingly, doing the compression mid-translation is quite a lot
-- more efficient and results in less code. We even skip the requirement of
-- building an environment and carrying it around. Is this the classic compiler
-- case of "either slow and composable, or fast and complicated"?
--
-- TODO: can we prove 'NonEmpty SSAProg'? it's defo true due to 'Expr's never
-- being void and the stack always starting at length 1, but doesn't seem easy
-- due to how I pass data around (I start with an empty program, and have to run
-- the first step of the algorithm to get anything)
exprToSSA :: MonadGen m => Maybe Name -> Expr -> m SSAProg
exprToSSA v e = do
    returnName <- maybe fresh return v
    go [] [(returnName, e)]
  where
    go :: MonadGen m => SSAProg -> [(Name, Expr)] -> m SSAProg
    go prog []                            = return prog
    go prog ((exprVar, expr):taggedExprs) = do
        (ssaExpr, taggedFoundExprs) <- exprToSSAShallow expr
        let ssaStmt = SSAStmt exprVar ssaExpr
            taggedExprs' = prepend taggedExprs taggedFoundExprs
         in go (ssaStmt : prog) taggedExprs'

-- | Convert the topmost node of an 'Expr' to an 'SSAExpr', and return it
--   alongside tagged pending child 'Expr's.
--
-- Any child 'Expr's are returned "tagged" with fresh names, to be processed
-- later.
exprToSSAShallow :: MonadGen m => Expr -> m (SSAExpr, [(Name, Expr)])
exprToSSAShallow = \case
  -- leaf exprs can be converted to a single SSA statement
  ENum x -> return (SSAExprValue (SSAValueNum x), [])
  EVar v -> return (SSAExprValue (SSAValueVar v), [])
  -- binop exprs we inspect: we only need to schedule their non-leaf subexprs
  EBinOp op e1 e2 -> do
      (e1sv, e1e) <- exprToSSAValue e1
      (e2sv, e2e) <- exprToSSAValue e2
      let ssaExpr = SSAExprBinOp op e1sv e2sv
          taggedExprs = catMaybes [e2e, e1e]
      return (ssaExpr, taggedExprs)

-- | Turn an 'Expr' into an 'SSAValue'.
--
-- Certain expressions (leaf expressions) can be translated directly. Others
-- (non-leaf expressions) are generated a fresh name and returned alongside it,
-- intended to be used for further processing.
exprToSSAValue :: MonadGen m => Expr -> m (SSAValue, Maybe (Name, Expr))
exprToSSAValue = \case
  ENum x -> return (SSAValueNum x, Nothing)
  EVar v -> return (SSAValueVar v, Nothing)
  e      -> fresh >>= \v -> return (SSAValueVar v, Just (v, e))

-- | Remove all non-op assignments in the 'SSAProg'.
--
-- TODO: what about chains L0 = 1, L1 = L0, L2 = L1? may not handle ideally
compressSSA :: SSAProg -> SSAProg
compressSSA = go Map.empty
  where
    go :: Map Name SSAValue -> SSAProg -> SSAProg
    go _   []               = []
    go env (SSAStmt v e:ss) = case e of
      SSAExprValue val      -> go (Map.insert v val env) ss
      SSAExprBinOp op v1 v2 ->
        -- replace instances of vars in env with their respective value
        let v1' = tryReplace v1
            v2' = tryReplace v2
         in SSAStmt v (SSAExprBinOp op v1' v2'): go env ss
      where
        tryReplace :: SSAValue -> SSAValue
        tryReplace = \case
          val@(SSAValueVar var) -> Map.findWithDefault val var env
          val -> val

-- | Rename all variables in the 'SSAProg'.
--
-- TODO: I don't like the recursion here, but my other two attempts didn't work.
renameSSA :: MonadGen m => SSAProg -> m SSAProg
renameSSA = go Map.empty
  where
    go :: MonadGen m => Map Name Name -> SSAProg -> m SSAProg
    go _   []               = return []
    go env (SSAStmt v e:ss) = do
        v' <- fresh
        let env' = Map.insert v v' env
            e'   = renameSSAExpr e
        out <- go env' ss
        return (SSAStmt v' e':out)
      where
        renameSSAExpr :: SSAExpr -> SSAExpr
        renameSSAExpr = \case
          SSAExprValue val -> SSAExprValue (rename val)
          SSAExprBinOp op v1 v2 -> SSAExprBinOp op (rename v1) (rename v2)
        rename :: SSAValue -> SSAValue
        rename = \case
          SSAValueVar var -> SSAValueVar (Map.findWithDefault var var env)
          val -> val

{- TODO broken due to foldrM order (output is bad, state threading is good)
-- | Rename all variables in the 'SSAProg'.
renameSSA' :: MonadGen m => SSAProg -> m SSAProg
renameSSA' prog = do
    out <- foldrM go ([], Map.empty) prog
    return $ fst out
  where
    go :: MonadGen m
       => SSAStmt -> (SSAProg, Map Name Name) -> m (SSAProg, Map Name Name)
    go (SSAStmt v e) (prog, env) = do
        v' <- fresh
        let env' = Map.insert v v' env
            e'   = renameSSAExpr e
        return (SSAStmt v' e':prog, env')
      where
        renameSSAExpr :: SSAExpr -> SSAExpr
        renameSSAExpr = \case
          SSAExprValue v -> SSAExprValue (rename v)
          SSAExprBinOp op v1 v2 -> SSAExprBinOp op (rename v1) (rename v2)
        rename :: SSAValue -> SSAValue
        rename = \case
          SSAValueVar v -> SSAValueVar (Map.findWithDefault v v env)
          v -> v
-}

{-
-- | Rename all variables in the 'SSAProg'.
renameSSA :: MonadGen m => SSAProg -> m SSAProg
renameSSA = go Map.empty []
  where
    go :: MonadGen m => Map Name Name -> SSAProg -> SSAProg -> m SSAProg
    go _   prog []               = return prog
    go env prog (SSAStmt v e:ss) = do
        v' <- fresh
        let env' = Map.insert v v' env
            e'   = renameSSAExpr e
        go env' (SSAStmt v' e':prog) ss
      where
        renameSSAExpr :: SSAExpr -> SSAExpr
        renameSSAExpr = \case
          SSAExprValue v -> SSAExprValue (rename v)
          SSAExprBinOp op v1 v2 -> SSAExprBinOp op (rename v1) (rename v2)
        rename :: SSAValue -> SSAValue
        rename = \case
          SSAValueVar v -> SSAValueVar (Map.findWithDefault v v env)
          v -> v
-}

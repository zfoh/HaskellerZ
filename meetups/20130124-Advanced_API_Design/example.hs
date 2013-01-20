{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
module Main where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Data.Maybe          (fromMaybe)
import Text.Printf         (printf)

import Compiler.Hoopl hiding ((<*>))

import qualified Compiler.Hoopl as H
import qualified Data.Set       as S

-- nodes {{{1
type Variable   = String
type Value      = Int

data Expr =
    ExVar Variable
  | ExConst Value
  | ExOp Expr Operation Expr
  | ExCall String [Expr]

data Operation =
  Plus | Minus | Equals

data Node e x where
  Lbl    :: Label                  -> Node C O
  Assign :: Variable -> Expr       -> Node O O
  Stmt   :: Expr                   -> Node O O
  Branch :: Label                  -> Node O C
  Cond   :: Expr -> Label -> Label -> Node O C
  Return ::                           Node O C

instance NonLocal Node where
  entryLabel (Lbl l)        = l
  successors (Branch l)     = [l]
  successors (Cond _ l1 l2) = [l1, l2]
  successors Return         = []

-- printing {{{1
instance Show Operation where
  show Plus   = "+"
  show Minus  = "-"
  show Equals = "=="

instance Show Expr where
  showsPrec _ (ExVar v) = showString v
  showsPrec _ (ExConst v) = shows v
  showsPrec _ (ExOp lhs op rhs) =
      showChar '('
    . shows lhs
    . showChar ' '
    . shows op
    . showChar ' '
    . shows rhs
    . showChar ')'
  showsPrec _ (ExCall f ps) =
      showString f
    . shows ps

instance Show (Node e x) where
  show (Lbl lbl)      = printf "\n%s:\n" (show lbl)
  show (Assign v e)   = printf "%s = %s;" v (show e)
  show (Stmt e)       = printf "%s;" (show e)
  show (Branch lbl)   = printf "goto %s;" (show lbl)
  show (Cond e tl el) = printf "if %s then goto %s else goto %s;" (show e) (show tl) (show el)
  show (Return)       = "return;"

instance Show (Graph Node e x) where
  show = showGraph show

-- analysis {{{1
type LiveFact = S.Set Variable

liveLattice :: DataflowLattice LiveFact
liveLattice = DataflowLattice {
      fact_name = "Liveness Analysis"
    , fact_bot  = S.empty
    , fact_join = add
  }
  where
    add _ (OldFact old) (NewFact new) = (c, u)
      where
        u = new `S.union` old
        c = changeIf (S.size u > S.size old)

liveTransfer :: BwdTransfer Node LiveFact
liveTransfer = mkBTransfer3 trFirst trMiddle trLast
  where
    trFirst :: Node C O -> LiveFact -> LiveFact
    trFirst (Lbl _) = id

    trMiddle :: Node O O -> LiveFact -> LiveFact
    trMiddle (Assign v e) = addUses e . S.delete v
    trMiddle (Stmt e)     = addUses e

    trLast :: Node O C -> FactBase LiveFact -> LiveFact
    trLast (Branch l)     = fact l
    trLast (Cond e tl el) = addUses e . (S.union <$> fact tl <*> fact el)
    trLast Return         = const $ fact_bot liveLattice

    fact :: Label -> FactBase LiveFact -> LiveFact
    fact l = fromMaybe (fact_bot liveLattice) . lookupFact l

    addUses :: Expr -> LiveFact -> LiveFact
    addUses (ExVar v)        = S.insert v
    addUses (ExConst _)      = id
    addUses (ExOp lhs _ rhs) = addUses lhs . addUses rhs
    addUses (ExCall _ ps)    = foldl (.) id (map addUses ps)

liveRewrite :: BwdRewrite M Node LiveFact
liveRewrite = mkBRewrite3 rwFirst rwMiddle rwLast
  where
    rwFirst :: Node C O -> LiveFact -> M (Maybe (Graph Node C O))
    rwFirst _ _ = return Nothing

    rwMiddle :: Node O O -> LiveFact -> M (Maybe (Graph Node O O))
    rwMiddle (Assign v _) fact
      | S.member v fact = return Nothing
      | otherwise       = return (Just GNil)
    rwMiddle (Stmt _) _ = return Nothing

    rwLast :: Node O C -> FactBase LiveFact -> M (Maybe (Graph Node O C))
    rwLast _ _ = return Nothing

-- execution {{{1
type M = CheckingFuelMonad (SimpleUniqueMonad)

runM :: M a -> a
runM = runSimpleUniqueMonad . runWithFuel infiniteFuel

runLiveness :: (Label, Graph Node C C) -> M (Graph Node C C, FactBase LiveFact)
runLiveness (entry, graph) = do
  (graph', facts, _) <- analyzeAndRewriteBwd bwd (JustC [entry]) graph mapEmpty
  return (graph', facts)
  where bwd = BwdPass {
      bp_lattice  = liveLattice
    , bp_transfer = liveTransfer 
    , bp_rewrite  = liveRewrite
    }

main :: IO ()
main = putStrLn $ show $ runM (example >>= runLiveness)

-- example graph {{{1
example :: M (Label, Graph Node C C)
example = do
  l1 <- freshLabel
  l2 <- freshLabel
  l3 <- freshLabel
  let
    block1 = mkBlock l1 [
        Assign "x" (ExConst 1),
        Assign "y" (ExConst 2),
        Assign "z" (ExConst 3)
      ]
      (Cond (ExOp (ExVar "y") Equals (ExConst 0)) l2 l3)

    block2 = mkBlock l2 [
        Assign "y" (ExVar "x")
      ]
      (Branch l3)

    block3 = mkBlock l3 [
        Stmt (ExCall "f" [ExVar "y"])
      ] Return

    graph = foldl (|*><*|) emptyClosedGraph [block1, block2, block3]
  return (l1, graph)
  where
    splice = (H.<*>)
    mkBlock lbl ms l = mkFirst (Lbl lbl) `splice` mkMiddles ms `splice` mkLast l


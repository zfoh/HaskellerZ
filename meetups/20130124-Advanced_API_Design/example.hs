{-# LANGUAGE GADTs #-}
module Main where

-- imports {{{1
import Control.Applicative ((<$>), (<*>))
import Data.Maybe          (fromMaybe)

import Compiler.Hoopl hiding ((<*>))

import qualified Compiler.Hoopl as H
import qualified Data.Set       as S

-- nodes {{{1
type Variable   = String
type Value      = Int

data Expr =
    ExVar String
  | ExConst Value
  | ExOp Expr Operation Expr

data Operation = Plus | Minus | Equals

data Node e x where
  Lbl    :: Label                  -> Node C O
  Assign :: Variable -> Expr       -> Node O O
  Branch :: Label                  -> Node O C
  Cond   :: Expr -> Label -> Label -> Node O C
  Return ::                           Node O C

instance NonLocal Node where
  entryLabel (Lbl l)        = l
  successors (Branch l)     = [l]
  successors (Cond _ l1 l2) = [l1, l2]
  successors Return         = []

-- analysis {{{1
type LiveFact = S.Set Variable

liveLattice :: DataflowLattice LiveFact
liveLattice = DataflowLattice {
      fact_name = "Live Analysis"
    , fact_bot  = S.empty
    , fact_join = add
  }
  where
    add _ (OldFact old) (NewFact new) = (c, u)
      where
        u = new `S.union` old
        c = changeIf (S.size u > S.size old)

liveness :: BwdTransfer Node LiveFact
liveness = mkBTransfer3 firstLive middleLive lastLive
  where
    firstLive :: Node C O -> LiveFact -> LiveFact
    firstLive (Lbl _) = id

    middleLive :: Node O O -> LiveFact -> LiveFact
    middleLive (Assign v e) = addUses e . S.delete v

    lastLive :: Node O C -> FactBase LiveFact -> LiveFact
    lastLive (Branch l)     = fact l
    lastLive (Cond e tl el) = addUses e . (S.union <$> fact tl <*> fact el)
    lastLive Return         = const $ fact_bot liveLattice

    fact :: Label -> FactBase LiveFact -> LiveFact
    fact l = fromMaybe (fact_bot liveLattice) . lookupFact l

    addUses :: Expr -> LiveFact -> LiveFact
    addUses (ExVar v)   = S.insert v
    addUses (ExConst _) = id
    addUses (ExOp lhs _ rhs) = addUses lhs . addUses rhs

-- execution {{{1
type M = CheckingFuelMonad (SimpleUniqueMonad)

runLiveness :: (Label, Graph Node C C) -> M (FactBase LiveFact)
runLiveness (entry, graph) = do
  (_, facts, _) <- analyzeAndRewriteBwd bwd (JustC [entry]) graph mapEmpty
  return facts
  where bwd = BwdPass {
      bp_lattice  = liveLattice
    , bp_transfer = liveness
    , bp_rewrite  = noBwdRewrite
    }

main :: IO ()
main = print $ runSimpleUniqueMonad $ runWithFuel infiniteFuel (example >>= runLiveness)

-- example graph {{{1
example :: M (Label, Graph Node C C)
example = do
  l1 <- freshLabel
  l2 <- freshLabel
  l3 <- freshLabel
  l4 <- freshLabel

  let
    block1 =  mkFirst (Lbl l1)
     `splice` mkMiddles [Assign "x" (ExConst 23)]
     `splice` mkLast (Branch l2)

    block2 =  mkFirst (Lbl l2)
     `splice` mkMiddles []
     `splice` mkLast (Cond (ExOp (ExVar "x") Equals (ExConst 42)) l3 l4)

    block3 =  mkFirst (Lbl l3)
     `splice` mkMiddles [Assign "x" (ExConst 42)]
     `splice` mkLast (Branch l2)

    block4 =  mkFirst (Lbl l4)
     `splice` mkMiddles [Assign "x" (ExConst 0)]
     `splice` mkLast Return

    graph = foldl (|*><*|) emptyClosedGraph [block1, block2, block3, block4]

  return (l1, graph)
  where splice = (H.<*>)


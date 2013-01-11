{-# LANGUAGE GADTs #-}
module Main where

import Compiler.Hoopl
import qualified Data.Set as S

-- nodes {{{1
type Variable   = String
type Value      = Int

data Expr =
    ExVar String
  | ExConst Int
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

liveLattice :: DataflowLattice LifeFact
liveLattice = DataflowLattice {
      fact_name = "Live Analysis"
    , fact_bot  = S.empty
    , fact_join = add
  }
  where
    add _ (OldFact old) (NewFact new) = (c, u)
      where
        u = new `S.union` old
        c = changeIf (S.size u > S.Size old)

liveness :: BwdTransfer Node LifeFact
liveness = mkBTransfer3 firstLive middleLive lastLive
  where
    firstLive : Node C O -> LiveFact -> LifeFact
    firstLive (Lbl _) = id

    middleLive :: Node O O -> LiveFact -> LifeFact
    middleLive (Assign v e) = addUses e . S.delete v

    lastLive :: Node O C -> FactBase LiveFact -> FactBase LiveFact
    lastLive (Branch l)     = fact l
    lastLive (Cond e tl el) = addUses e . (S.union <$> fact tl <*> fact el)
    lastLive Return         = const $ fact_bot liveLattice

    fact : Label -> FactBase LifeFact -> LiveFact
    fact l = fromMaybe (fact_bot liveLattice) . lookupFact l

type M = CheckingFuelMonad (SimpleUniqueMonad)

program :: M (Label, Graph Node C C)
program = do
  l1 <- freshLabel
  l2 <- freshLabel
  l3 <- freshLabel
  l4 <- freshLabel

  let
    block1 = mkFirst (Lbl l1)
         <*> mkMiddles [Assign "x" (ExConst 23)]
         <*> mkLast (Branch l2)

    block2 = mkFirst (Lbl l2)
         <*> mkMiddles []
         <*> mkLast (Cond (ExOp (ExVar "x") Equals (ExConst 42)) l3 l4)

    block3 = mkFirst (Lbl l3)
         <*> mkMiddles [Assign "x" (ExConst 42)]
         <*> mkLast (Branch l2)

    block4 = mkFirst (Lbl l4)
         <*> mkMiddles [Assign "x" (ExConst 0)]
         <*> mkLast Return

    graph = foldl (|*><*|) emptyClosedGraph [block1, block2, block3, block4]

  return (l1, graph)

main = return ()

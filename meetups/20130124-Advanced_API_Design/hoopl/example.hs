{-# LANGUAGE GADTs #-}
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

data Operation =
  Plus | Minus | Equals

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

instance Show (Node e x) where
  show (Lbl lbl)      = printf "\n%s:\n" (show lbl)
  show (Assign v e)   = printf "%s = %s;" v (show e)
  show (Branch lbl)   = printf "goto %s;" (show lbl)
  show (Cond e tl el) = printf "if %s then goto %s else goto %s;" (show e) (show tl) (show el)
  show (Return)       = "return;"

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

    trLast :: Node O C -> FactBase LiveFact -> LiveFact
    trLast (Branch l)     = fact l
    trLast (Cond e tl el) = addUses e . (S.union <$> fact tl <*> fact el)
    trLast Return         = const $ fact_bot liveLattice

    fact :: Label -> FactBase LiveFact -> LiveFact
    fact l = fromMaybe (fact_bot liveLattice) . lookupFact l

    addUses :: Expr -> LiveFact -> LiveFact
    addUses (ExVar v)   = S.insert v
    addUses (ExConst _) = id
    addUses (ExOp lhs _ rhs) = addUses lhs . addUses rhs

liveRewrite :: BwdRewrite M Node LiveFact
liveRewrite = mkBRewrite3 rwFirst rwMiddle rwLast
  where
    rwFirst :: Node C O -> LiveFact -> M (Maybe (Graph Node C O))
    rwFirst _ _ = return Nothing

    rwMiddle :: Node O O -> LiveFact -> M (Maybe (Graph Node O O))
    rwMiddle (Assign v _) fact
      | S.member v fact = return Nothing
      | otherwise       = return (Just GNil)

    rwLast :: Node O C -> FactBase LiveFact -> M (Maybe (Graph Node O C))
    rwLast _ _ = return Nothing

-- execution {{{1
type M = CheckingFuelMonad (SimpleUniqueMonad)

runLiveness :: (Label, Graph Node C C) -> M String
runLiveness (entry, graph) = do
  (graph', facts, _) <- analyzeAndRewriteBwd bwd (JustC [entry]) graph mapEmpty
  return (showGraph show graph' ++ "\n" ++ showFactBase facts)
  where bwd = BwdPass {
      bp_lattice  = liveLattice
    , bp_transfer = liveTransfer 
    , bp_rewrite  = liveRewrite
    }

main :: IO ()
main = putStrLn $ runSimpleUniqueMonad $ runWithFuel infiniteFuel (example >>= runLiveness)

-- example graph {{{1
example :: M (Label, Graph Node C C)
example = do
  l1 <- freshLabel
  l2 <- freshLabel
  l3 <- freshLabel
  l4 <- freshLabel

  let
    block1 = mkBlock
      (Lbl l1)                        
      [ 
        Assign "x" (ExConst 23)      
      ]
      (Branch l2)                     

    block2 = mkBlock
      (Lbl l2)
      []
      (Cond (ExOp (ExVar "x") Equals (ExConst 42)) l3 l4)

    block3 = mkBlock
      (Lbl l3)
      [
        Assign "x" (ExConst 42)
      ]
      (Branch l2)

    block4 = mkBlock
      (Lbl l4)
      [
        Assign "x" (ExConst 0)
      ]
      Return

    graph = foldl (|*><*|) emptyClosedGraph [block1, block2, block3, block4]

  return (l1, graph)
  where
    splice = (H.<*>)
    mkBlock f ms l = mkFirst f `splice` mkMiddles ms `splice` mkLast l


module Supercompiler where

import Data.Foldable
import Data.List
import Data.Ord

import Data
import DataUtil
import Driving
import Folding
import Generalize
import Generator

supercompile :: Task -> Task
supercompile (e, p) = residuate $ simplify $ foldTree $ buildFTree (addPropagation $ driveMachine p) e

simplify :: Graph Conf -> Graph Conf
simplify (Node e (Decompose ts)) = Node e (Decompose $ map simplify ts)
simplify (Node e (Variants cs))  = Node e (Variants [(c, simplify t) | (c, t) <- cs])
simplify (Node e (Transient t)) 
  | isBase e t                   = Node e $ Transient $ simplify t
  | otherwise                    = simplify t
simplify t                       = t

buildFTree :: Machine Conf -> Conf -> Tree Conf
buildFTree m e = bft m nameSupply e  
  
bft :: Machine Conf -> NameSupply -> Conf -> Tree Conf
bft d ns e 
  | whistle e,
    (Call f es)     <- e,
    (v:ns')         <- ns,
    (ev, es')       <- extract v es = bft d ns' $ Let (v, ev) $ Call f es'
  | Stop            <- d ns e       = Node e Stop
  | (Transient nd)  <- d ns e       = Node e $ Transient $ bft d ns nd
  | (Variants vs)   <- d ns e       = Node e $ Variants $ map (\(c, ec) -> (c, bft d (unused c ns) ec)) vs 
  | (Decompose nds) <- d ns e       = Node e $ Decompose $ map (bft d ns) nds
  
  where extract :: Name -> [Expr] -> (Expr, [Expr])
        extract v es = (e, es' ++ (Var v:es'')) where
          e = maximumBy (comparing weight) es
          (es', _:es'') = break (e ==) es
          weight e = if isVar e then 0 else size e

        whistle = sizeWhistle

sizeWhistle :: Expr -> Bool
sizeWhistle e
  | (Call _ es) <- e = not (all isVar es) && size e > sizeBound
  | otherwise        = False
  where sizeBound = 179

generalizeNode :: Machine Conf -> NameSupply -> Node Conf -> Node Conf
generalizeNode = generalizeNode' []
  where generalizeNode' :: [Node Conf] -> Machine Conf -> NameSupply -> Node Conf -> Node Conf
        generalizeNode' _ _ _ nd@(Node _ Stop)                = nd
        generalizeNode' p d ns nd@(Node t (Transient e)) 
          | (Just _) <- asum $ map (renaming t . nodeLabel) p = Node t $ Transient $ Node (nodeLabel e) Stop
          | isCall t,
            (Just e') <- find ((<: t) . nodeLabel)  p,
            (ns', t') <- abstract ns t (nodeLabel e')         = generalizeNode' p d ns' $ bft d ns' t'
          | otherwise                                         = Node t $ Transient $ generalizeNode' (nd:p) d ns e
        generalizeNode' p d ns (Node t (Variants vs))         = Node t $ Variants $ map mapper vs
          where mapper (pt, v) = (pt, generalizeNode' p d (unused pt ns) v)
        generalizeNode' p d ns (Node t (Decompose es))        = Node t $ Decompose $ map (generalizeNode' p d ns) es

abstract :: NameSupply -> Expr -> Expr -> (NameSupply, Expr)
abstract ns e e' 
  | (ns', gen)    <- generalize ns e e',
    (e'', sub, _) <- gen                 = (ns', foldr Let e'' sub)

addPropagation :: Machine Conf -> Machine Conf
addPropagation m ns e = propagateContract (m ns e)

propagateContract :: Step Conf -> Step Conf
propagateContract (Variants vs) =
  Variants [(c, e // [(v, Ctr cn $ map Var vs)]) |
            (c@(Contract v (Pat cn vs)), e) <- vs]
propagateContract step = step
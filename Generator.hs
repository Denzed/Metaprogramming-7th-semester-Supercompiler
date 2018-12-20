module Generator where

import Data
import DataUtil

residuate :: Graph Conf -> Task
residuate tree = (expr, program) where
  (expr, program, _) = res nameSupply [] tree

--- generation of residual program
res :: NameSupply -> [(Conf, Conf)] -> Graph Conf -> (Conf, Program, NameSupply)
res ns mp (Node e Stop) = (e, Program [], ns)

res ns mp (Node (Ctr cname _) (Decompose ts)) = (Ctr cname args, p1, ns1) where
  (args, p1, ns1) = res' ns mp ts

res ns mp (Node (Let (v, _) _) (Decompose ts)) = (Let (v, e1) e2, p1, ns1) where
  ([e1, e2], p1, ns1) = res' ns mp ts

res ns mp (Node (e :@: e') (Decompose (var:es))) = (foldl (:@:) v es', p, ns') where 
  (Node v@(Var _) _) = var
  (es', p, ns') = res' ns mp es      

res (n:ns) mp (Node e (Transient t)) = (call, Program ((FDef f1 vs body):fs), ns1) where
  vs = vnames e
  f1 = "ff" ++ (tail n)
  call = Call f1 $ map Var vs
  (body, Program fs, ns1) = res ns ((e, call) : mp) t

res ns mp (Node (Lmb v e) (Transient t)) = (Lmb v e', p, ns') where
  (e', p, ns') = res ns mp t

res ns mp (Node e (Variants cs)) = (e', p, ns') where
  (Contract v _, _) = head cs
  (ps, es)          = unzip $ map (\(Contract _ p, ep) -> (p, ep)) cs
  (es', p, ns')     = res' ns mp es
  e'                = Case (Var v) $ zip ps es' 

res ns mp (Node e (Fold (Node base _) ren)) = (call, Program [], ns) where
  call = baseCall // [(x, Var y) | (x, y) <- ren]
  Just baseCall = lookup base mp

-- proceeds a list of trees
-- the main goal is to handle name supply
res' :: NameSupply -> [(Conf, Conf)] -> [Graph Conf] -> ([Conf], Program, NameSupply)
res' ns mp ts = foldl f ([], Program [], ns) ts where
  f (cs, Program fs, ns1) t = (cs ++ [g], Program (fs ++ fs1), ns2) where
    (g, Program fs1, ns2) = res ns1 mp t

isBase e1 (Node _ (Decompose ts)) = or $ map (isBase e1) ts
isBase e1 (Node _ (Variants cs)) = or $ map (isBase e1 . snd) cs
isBase e1 (Node _ (Transient t)) = isBase e1 t
isBase e1 (Node _ (Fold (Node e2 _) _)) = e1 == e2
isBase e1 (Node e2 Stop) = False

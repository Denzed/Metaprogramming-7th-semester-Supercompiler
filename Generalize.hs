module Generalize(
  generalize, (<:)
) where

import Control.Monad
import Data.List
import Data.Foldable
import Data.Maybe

import Data
import DataUtil
import Driving

infixl 3 <:

-- Homemorphic embedding
(<:) :: Expr -> Expr -> Bool
e <: e' = any ((`embd` e') . (e //) . zip vs . map Var) $ replicateM (length vs) vs'
  where vs  = vnames e
        vs' = vnames e'

embd :: Expr -> Expr -> Bool
embd e e' = e `coup` e' || e `dive` e'

dive :: Expr -> Expr -> Bool 
e `dive` (Call _ es)   = any (e `embd`) es
e `dive` (Ctr _ es)    = any (e `embd`) es
e `dive` (Lmb _ e')    = e `embd` e'
e `dive` (e1 :@: e2)   = e `embd` e1 || e `embd` e2
e `dive` (Case e' css) = e `embd` e' || any ((e `embd`) . snd) css
_ `dive` _             = False

coup :: Expr -> Expr -> Bool
(Var v)      `coup` (Var v')        = v == v'
(Call f es)  `coup` (Call f' es')   = f == f' && all id (zipWith embd es es')
(Ctr c es)   `coup` (Ctr c' es')    = c == c' && all id (zipWith embd es es')
(e1 :@: e2)  `coup` (e1' :@: e2')   = (e1 `coup` e1') && (e2 `embd` e2')
(Case e css) `coup` (Case e' css')  = e `embd` e' && all id (zipWith cmpCase css css')
  where cmpCase (p1, e1) (p2, e2) 
          | (Just sub) <- patSub p2 p1 = e1 `embd` (e2 // sub)
          | otherwise                  = False

patSub :: Pat -> Pat -> Maybe Subst
patSub (Pat c vs) (Pat c' vs')
  | c == c', 
    (length vs) == (length vs') = fmap (map $ fmap Var) $ f (zip vs vs', [])
  | otherwise                   = Nothing
  where f (x:_, _) = Nothing
        f (_, ps)  = g gs1 gs2
          where gs1 = groupBy (\(a, b) (c, d) -> a == c) $ sortBy h $ nub $ catMaybes ps
                gs2 = groupBy (\(a, b) (c, d) -> b == d) $ sortBy j $ nub $ catMaybes ps
                h (a, b) (c, d) = compare a c
                j (a, b) (c, d) = compare b d
        g xs ys = if all ((== 1) . length) xs && all ((== 1) . length) ys
                  then Just (concat xs) 
                  else Nothing

-- Generalization via embedding
type Generalization =  (Expr,   Subst, Subst)
type Generalizations = ([Expr], Subst, Subst)

extractApps :: Expr -> [Expr]
extractApps e = helper [] e
  where helper :: [Expr] -> Expr -> [Expr]
        helper as (e :@: e') = helper (e':as) e
        helper as e          = (e:as)

generalizeList :: NameSupply -> [Expr] -> [Expr] -> (NameSupply, Generalizations)
generalizeList ns es es' = foldr folder (ns, ([], [], [])) $ zip es es'
  where folder :: (Expr, Expr) -> (NameSupply, Generalizations) -> (NameSupply, Generalizations)
        folder (e, e') (ns', (es'', sub, sub')) 
          | (ns'', (e'', subo, subo')) <- generalize ns' e e' = (ns'', ((e'':es''), subo ++ sub, subo' ++ sub'))

generalize :: NameSupply -> Expr -> Expr -> (NameSupply, Generalization)
generalize ns (Ctr c es) (Ctr c' es')
  | c == c',
    (length es) == (length es'),
    (ns', gen)        <- generalizeList ns es es',
    (es'', sub, sub') <- gen                       = (ns', (Ctr c es'', sub, sub'))
generalize ns (Call f es) (Call f' es') 
  | f == f',
    (length es) == (length es'),
    (ns', gen)        <- generalizeList ns es es',
    (es'', sub, sub') <- gen                       = (ns', (Call f es'', sub, sub'))
generalize ns e e'                              
  | es      <- extractApps e,
    es'     <- extractApps e',
    (length es) == (length es')                    = generalizeApps ns es es'
  | (v:ns') <- ns                                  = (ns', (Var v, [(v, e)], [(v, e')]))
  where generalizeApps :: NameSupply -> [Expr] -> [Expr] -> (NameSupply, Generalization)
        generalizeApps ns (e:es) (e':es')
          | (Lmb v b)                       <- e,
            (Lmb v' b')                     <- e',
            (v'':ns')                       <- ns,
            ges                             <- (b:es),
            ges'                            <- (b' // [(v', Var v'')]:es'),
            (ns'', ((b'':es''), sub, sub')) <- generalizeList ns' ges ges',
            gen                             <- (foldl (:@:) e es'', sub, sub') = (ns'', gen)
          | (Var v)                         <- e,
            (Var v')                        <- e',
            (v == v'), -- let's ignore that they can be unequal for now
            (ns', (es'', sub, sub'))        <- generalizeList ns es es',
            gen                             <- (foldl (:@:) e es'', sub, sub') = (ns', gen)
          | (Case e css)                    <- e,
            (Case e' css')                  <- e',
            (length css) == (length css'),
            ps                              <- map fst css,
            ps'                             <- map fst css',
            (Just pSub)                     <- sequenceA $ zipWith patSub ps ps',
            (ns', ((e'':es''), sub, sub'))  <- generalizeList ns (e:es) (e':es'),
            csse                            <- map snd css,
            csse'                           <- zipWith (//) (map snd css') pSub,
            (ns'', (css'', subcs, subcs'))  <- generalizeList ns' csse csse',
            csso                            <- zip ps css'',
            base                            <- (Case e'' csso),
            subo                            <- sub ++ subcs,
            subo'                           <- sub' ++ subcs',
            eo                              <- foldl (:@:) base es,
            gen                             <- (eo, subo, subo')               = (ns', gen)
          | (v:ns')                         <- ns,
            gen                             <- (Var v, [(v, e)], [(v, e')])    = (ns', gen)
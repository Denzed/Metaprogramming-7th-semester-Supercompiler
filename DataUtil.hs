module DataUtil(
  isValue,isCall,isVar,size,
  fDef,
  (//), renaming, vnames,nameSupply,
  nodeLabel,isRepeated,unused
  ) where

import Data
import Data.Maybe
import Data.Char
import Data.List

isValue :: Expr -> Bool
isValue (Ctr _ args) = and $ map isValue args
isValue (Lmb _ _)    = True
isValue _            = False

isCall :: Expr -> Bool
isCall (Call _ _) = True
isCall _          = False

isVar :: Expr -> Bool
isVar (Var _) = True
isVar _       = False

fDef :: Program -> Name -> FDef
fDef (Program fs) fname = head [f | f@(FDef x _ _) <- fs, x == fname]

(//) :: Expr -> Subst -> Expr
(Var x) // sub           = maybe (Var x) id (lookup x sub)
(Ctr name args) // sub   = Ctr name (map (// sub) args)
(Call name args) // sub  = Call name (map (// sub) args)
(Lmb x e) // sub         = Lmb x (e // (filter ((x /=) . fst) sub))
(e1 :@: e2) // sub       = (e1 // sub) :@: e2
(Case e css) // sub      = Case (e // sub) $ map removeBounded css
  where removeBounded (p@(Pat _ vs), e') = (p, e' // (filter (flip notElem vs . fst) sub))
(Let (x, e1) e2) // sub  = Let (x, (e1 // sub)) (e2 // (filter ((x /=) . fst) sub))

nameSupply :: NameSupply
nameSupply = ["v" ++ (show i) | i <- [1 ..] ]

unused :: Contract -> NameSupply -> NameSupply
unused (Contract _ (Pat _ vs)) = (\\ vs)

vnames :: Expr -> [Name]
vnames = nub . vnames'

vnames' :: Expr -> [Name]
vnames' (Var v)          = [v]
vnames' (Ctr _ args)     = concat $ map vnames' args
vnames' (Call _ args)    = concat $ map vnames' args
vnames' (Lmb _ e)        = vnames' e
vnames' (e1 :@: e2)      = vnames' e1 ++ vnames' e2
vnames' (Case e css)     = vnames' e ++ concatMap (vnames' . snd) css
vnames' (Let (_, e1) e2) = vnames' e1 ++ vnames' e2

isRepeated :: Name -> Expr -> Bool
isRepeated vn e = (length $ filter (== vn) (vnames' e)) > 1

renaming :: Expr -> Expr -> Maybe Renaming
renaming e1 e2 = f $ partition isNothing $ renaming' (e1, e2) where
  f (x:_, _) = Nothing
  f (_, ps)  = g gs1 gs2
    where gs1 = groupBy (\(a, b) (c, d) -> a == c) $ sortBy h $ nub $ catMaybes ps
          gs2 = groupBy (\(a, b) (c, d) -> b == d) $ sortBy j $ nub $ catMaybes ps
          h (a, b) (c, d) = compare a c
          j (a, b) (c, d) = compare b d
  g xs ys = if all ((== 1) . length) xs && all ((== 1) . length) ys
            then Just (concat xs) 
            else Nothing

renaming' :: (Expr, Expr) -> [Maybe (Name, Name)]
renaming' ((Var x), (Var y))                  = [Just (x, y)]
renaming' ((Ctr n1 args1), (Ctr n2 args2)) 
  | n1 == n2                                  = concat $ map renaming' $ zip args1 args2
renaming' ((Call n1 args1), (Call n2 args2)) 
  | n1 == n2                                  = concat $ map renaming' $ zip args1 args2
renaming' ((Lmb v e), (Lmb v' e'))            = renaming' (e, e' // [(v, Var v')])
renaming' ((e1 :@: e2), (e1' :@: e2'))        = renaming' (e1, e1') ++ renaming' (e2, e2')
renaming' ((Case e css), (Case e' css'))      
  | (length css) == (length css')             = renaming' (e, e') ++ concatMap renamingCs' (zip css css')
  where renamingCs' (((Pat c nc), ec), ((Pat c' nc'), ec')) 
          | c == c',
            (length nc) == (length nc')       = renaming' (ec, ec' // zip nc (map Var nc'))
renaming' (Let (v, e1) e2, Let (v', e1') e2') = renaming' (e1, e1') ++ renaming' (e2, e2' // [(v, Var v')])
renaming' _                                   = [Nothing]

size :: Expr -> Integer
size (Var _)          = 1
size (Ctr _ args)     = 1 + sum (map size args)
size (Call _ args)    = 1 + sum (map size args)
size (Lmb _ e)        = 1 + size e
size (e1 :@: e2)      = (size  e1) + (size e2)
size (Case e css)     = 1 + (size e) + sum (map ((1 + ) . size . snd) css)
size (Let (_, e1) e2) = 1 + (size e1) + (size e2)

nodeLabel :: Node a -> a
nodeLabel (Node l _) = l

step :: Node a -> Step (Graph a)
step (Node _ s) = s

module Driving where

import Data
import DataUtil

buildTree :: Machine Conf -> Conf -> Tree Conf
buildTree m e = bt m nameSupply e

bt :: Machine Conf -> NameSupply -> Conf -> Tree Conf
bt m ns c = case m ns c of
  Decompose ds -> Node c $ Decompose (map (bt m ns) ds)
  Transient e -> Node c $ Transient (bt m ns e)
  Stop -> Node c Stop
  Variants cs -> Node c $ Variants [(c, bt m (unused c ns) e) | (c, e) <- cs]

driveMachine :: Program -> Machine Conf
driveMachine p = drive 
  where drive :: Machine Conf
        drive ns (Var _)               = Stop
        drive ns (Ctr _ [])            = Stop
        drive ns (Ctr _ args)          = Decompose args
        drive ns (Call name args)      = Transient $ e // (zip vs args) 
          where FDef _ vs e = fDef p name
        drive ns (Lmb _ e)             = Transient e
        drive ns ((Lmb x e1) :@: e2)   = Transient $ e1 // [(x, e2)]
        drive ns (v@(Var _) :@: e2)    = Decompose [v, e2]
        drive ns (e1 :@: e2)             
          | (Transient e1') <- pe1     = Transient $ e1' :@: e2
          | (Variants vs)   <- pe1     = Variants $ map (fmap (:@: e2)) vs
          | (Decompose e1s) <- pe1     = Decompose $ e1s ++ [e2] -- unlike Case below the only feasible 
                                                                 -- source of Decompose is application
          where pe1 = drive ns e1
        drive ns (Case (Ctr c es) css) = Transient $ matchCase css
          where matchCase ((Pat c' vs, e):css') 
                  | c == c' && (length es) == (length vs) = e // zip vs es
                  | otherwise                             = matchCase css'
                matchCase []                              = undefined -- we don't want this situation to happen
        drive ns (Case (Var x) css)    = Variants $ map (scrutinize ns x) css
        drive ns (Case e css)
          | (Transient e') <- pe       = Transient (Case e' css)
          | (Variants vs)  <- pe       = Variants $ map (fmap $ flip Case css) vs
          | otherwise                  = Stop -- we get Decompose from Ctr, :@: and Let,
                                              -- but Case must be injected differently into them
                                              -- so let's just stop 
          where pe = drive ns e
        drive ns (Let (_, t1) t2)      = Decompose [t1, t2]
  

scrutinize :: NameSupply -> Name -> (Pat, Expr) -> (Contract, Expr)
scrutinize ns v ((Pat cn cvs), e) =
  (Contract v (Pat cn fresh), e // sub) where
    fresh = take (length cvs) ns
    sub = zip cvs (map Var fresh)

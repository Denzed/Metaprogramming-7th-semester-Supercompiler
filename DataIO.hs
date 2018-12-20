module DataIO where

import Data
import DataUtil
import Data.Maybe
import Data.Char

import Data.List

-- SHOW
printTree t = unlines $ take 1000 $ pprintTree "" "" t

pprintTree :: String -> String -> Graph Conf -> [String]
pprintTree indent msg (Node expr next) = make next where
  make (Fold _ ren) = (indent ++ msg) : [indent ++ "|__" ++  (show expr) ++ "__↑" ++ (show ren)]
  make Stop = (indent ++ msg) : [indent ++ "|__" ++  (show expr)]
  make (Transient t) = (indent ++ msg) : (indent ++ "|__" ++ show expr) : (pprintTree (indent ++ " ") "|" t)
  make (Decompose ts) = (indent ++ msg) :  (indent ++ "|__" ++ show expr): (concat (map (pprintTree (indent ++ " ") "|") ts))
  make (Variants cs) =
    (indent ++ msg) :  (indent ++ "|__" ++  show expr) : (concat (map (\(x, t) -> pprintTree (indent ++ " ") ("?" ++ show x) t) cs))

instance Show Expr where
  show (Var n)               = n
  show (Ctr n es)            = n ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
  show (Call n es)           = (fn n) ++ "(" ++ (intercalate ", " (map show es)) ++ ")"
  show e@(Lmb _ _)           = "λ(" ++ (intercalate ", " (reverse args)) ++ "). " ++ (show body) 
    where (args, body) = collectLambdas [] e

          collectLambdas ns (Lmb n e) = collectLambdas (n:ns) e
          collectLambdas ns e         = (ns, e)
  show (e1 :@: e2@(_ :@: _)) = show e1 ++ " (" ++ show e2 ++ ")"
  show (e1 :@: e2)           = show e1 ++ " " ++ show e2
  show (Case e css)          = "case " ++ show e ++ " of " ++ intercalate "; " (map (\(p, e') -> show p ++ " -> " ++ show e') css)
  show (Let (v, e1) e2)      = "let " ++ v ++ " = " ++ (show e1) ++ " in " ++ (show e2)
  
fn :: String -> String
fn (_:s:ss) = (toLower s) : ss

instance Show FDef where
  show (FDef n args body) = (fn n) ++ "(" ++ intercalate ", " args ++ ") = " ++ (show body) ++ ";"

instance Show Pat where
  show (Pat cn vs) = cn ++ "(" ++ intercalate "," vs ++ ")"

instance Show Contract where
  show (Contract n p) = n ++ " == " ++ (show p)

instance Show Program where
  show (Program fs) = intercalate "\n" $ (map show fs)

instance Show a => Show (Step a) where
  show (Transient a) = "=> " ++ (show a)
  show (Variants vs) = intercalate "\n" $ map (\(c, e) -> (show c) ++ " => " ++ (show e)) vs
  show Stop = "!"
  show (Decompose ds) = show ds
  show (Fold e _) = "↑" ++ (show e)

-- Latex
pprintLTree :: Graph Conf -> String
pprintLTree (Node expr next) = make next where
  make (Fold _ _) = "node[conf]{" ++ (show expr) ++ "}"
  make Stop = "node[conf]{" ++ (show expr) ++ "}"
  make (Transient t) = "node[conf]{" ++ (show expr) ++ "}\nchild[->]{" ++ (pprintLTree t) ++ "}"
  make (Decompose ts) = "node[conf]{" ++ (show expr) ++ "}" ++
    (concat (map (\t -> "\nchild[->]{" ++ (pprintLTree t) ++ "}") ts))
  make (Variants [(x1, t1), (x2, t2)]) =
    "node[conf]{" ++ (show expr) ++ "}" ++
      ("\nchild[->]{" ++ (pprintLTree t1) ++ "\nedge from parent node[left,label,xshift=-5mm]{" ++ (show x1) ++ "}}") ++
      ("\nchild[->]{" ++ (pprintLTree t2) ++ "\nedge from parent node[right,label,xshift=5mm]{" ++ (show x2) ++ "}}")

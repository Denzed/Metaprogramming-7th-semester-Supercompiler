module Data where

infixl 2 :@:

type Name = String
data Expr = Var Name 
          | Ctr Name [Expr] 
          | Call Name [Expr] 
          | Lmb Name Expr
          | Expr :@: Expr
          | Case Expr [(Pat, Expr)]
          | Let (Name, Expr) Expr
          deriving (Eq)
data Pat = Pat Name [Name]
         deriving (Eq)
data FDef = FDef Name [Name] Expr 
          deriving (Eq)
data Program = Program [FDef]
             deriving (Eq)

type Renaming = [(Name, Name)]
type Subst = [(Name, Expr)]
type NameSupply = [Name]

type Conf = Expr
type Value = Expr
type Task = (Conf, Program)
type Env = [(Name, Value)]

data Contract = Contract Name Pat
data Step a = Transient a 
            | Variants [(Contract, a)] 
            | Stop 
            | Decompose [a] 
            | Fold a Renaming
data Graph a = Node a (Step (Graph a))
type Tree a = Graph a
type Node a = Tree a

type Machine a = NameSupply -> a -> Step a

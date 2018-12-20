import Data
import DataIO
import Supercompiler

addFunc = 
  (FDef "add" ["x", "y"] $ Case (Var "x") [
    (Pat "Z" []    , Var "y"),
    (Pat "S" ["x'"], Call "add" [Var "x'", Ctr "S" [Var "y"]])
  ])

addProgram :: Program 
addProgram = Program [addFunc]

addTask :: Task
addTask = 
  (
    Let ("one", Ctr "S" [Ctr "Z" []]) $
    Let ("two", Ctr "S" [Var "one"]) $
    Call "add" [Var "one", Var "two"],
    addProgram
  )
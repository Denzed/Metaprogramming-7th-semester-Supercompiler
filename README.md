# A simple (fi-)(fun-)ctional language supercompiler

Based on SC Mini supercompiler for easier implementation of basic methods

Also, [this article](https://pdfs.semanticscholar.org/ecef/6babf02aa25276101d9b55909219ee8b7a4e.pdf) was a lot of help

## Language definition

```.
    Prog := E_0 where f_1 = E_1 ... f_k = E_k
    E := x
       | Cons E_1 ... E_n
       | \x. E
       | E_1 E_2
       | f
       | let x = E_1 in E_2
       | Case E_0 of p_1 => E_1
                   | p_2 => E_2
                   ...
                   | p_n => E_n
    p := Cons x_1 ... x_n
    f \in F
    Cons always has fixed arity
```

## Usage

```Haskell
data Program = Program [FDef] -- Program is a set of function definitions
             deriving (Eq)

type Task = (Conf, Program)   -- Task is a Program and an Expr for compilation

supercompile :: Task -> Task  -- does all the work, see Supercompiler.hs
supercompile (e, p) = residuate $ simplify $ foldTree $ buildFTree (addPropagation $ driveMachine p) e
```

Basically, you just have to import `Test.hs`, which has a simple test and imports everything else.

Each class is an instance of `Show`, so no additional printing is required.
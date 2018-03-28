> module Calculus.SyntaxTree where

> import           Calculus.Calculus
> import           Data.Tree         as T
> import           Data.Tree.Pretty  as P

> -- | Pretty prints expressions as trees
> printExpr :: FunExpr -> IO ()
> printExpr = putStrLn . drawVerticalTree . makeTree

> -- | Pretty prints the steps taken when canonifying an expression
> prettyCan :: FunExpr -> IO ()
> prettyCan e =
>   let t  = makeTree e
>       e' = canonify e
>       t' = makeTree e'
>     in if t == t' then putStrLn $ drawVerticalTree t
>                   else do
>                       putStrLn $ drawVerticalTree t
>                       prettyCan e'

> -- Possible generalization, make it work on lists of Expr
> -- | Pretty prints syntactic checking of equality
> prettyEqual :: FunExpr -> FunExpr -> IO Bool
> prettyEqual e1 e2 = if e1 == e2 then
>   do
>     putStrLn "It's equal!"
>     putStrLn $ drawVerticalForest [makeTree e1, makeTree e2]
>     return True
>   else do
>     putStrLn "Not equal -> Simplifying"
>     putStrLn $ drawVerticalForest [makeTree e1, makeTree e2]
>     let c1 = canonify e1
>         c2 = canonify e2
>       in if c1 == e1 && c2 == e2 then putStrLn "Can't simplify no more" >> return False
>                                    else prettyEqual c1 c2

> -- Parse an expression as a Tree of Strings
> makeTree :: FunExpr -> T.Tree String
> makeTree (e1 :+ e2)  = T.Node "+" [makeTree e1, makeTree e2]
> makeTree (e1 :- e2)  = T.Node "-" [makeTree e1, makeTree e2]
> makeTree (e1 :* e2)  = T.Node "*" [makeTree e1, makeTree e2]
> makeTree (e1 :/ e2)  = T.Node "Div" [makeTree e1, makeTree e2]
> makeTree (Exp :. e)  = T.Node "Exp" [makeTree e]
> makeTree (e1 :. e2)  = T.Node "o" [makeTree e1, makeTree e2]
> makeTree Id          = T.Node "Id" []
> --makeTree (Lambda s e) = T.Node ("Lambda " ++ s) [makeTree e]
> --makeTree (Func s)     = T.Node s []
> --makeTree (Delta e)   = T.Node "Delta" [makeTree e]
> makeTree (D e)       = T.Node "D" [makeTree e]
> --makeTree (e1 :$ e2)   = T.Node "$" [makeTree e1, makeTree e2]
> makeTree (Const num) = T.Node (show (floor num)) [] -- | Note the use of floor
> makeTree Exp     = T.Node "Exp" []
> makeTree e           = error $ show e

> -- Staged for removal
> equals :: FunExpr -> FunExpr -> Bool
> -- Addition is commutative
> equals (e1 :+ e2) (e3 :+ e4) = (canonify (e1 :+ e2) == canonify (e3 :+ e4)) ||
>                                (canonify (e1 :+ e2) == canonify (e4 :+ e3))
> -- | Addition is associative
> -- equals (e1 :+ (e2 :+ e3))    = undefined
> -- Multiplication is commutative
> equals (e1 :* e2) (e3 :* e4) = (canonify (e1 :* e2) == canonify (e3 :* e4)) ||
>                                (canonify (e1 :* e2) == canonify (e4 :* e3))
> equals e1 e2 = canonify e1 == canonify e2

> canonify :: FunExpr -> FunExpr
> -- | Addition
> -- | e + 0 = e
> canonify (e :+ Const 0)       = canonify e
> canonify (Const 0 :+ e)       = canonify e
> -- | Lifting
> canonify (Const x :+ Const y) = Const (x + y)
> canonify (e1 :+ e2)           = canonify e1 :+ canonify e2

> -- | Subtraction
> -- | e - 0 = e
> canonify (e :- Const 0)       = canonify e
> -- | 0 - b = -b
> --canonify (Const 0 :- b)       = negate (canonify b)
> -- | Lifting
> canonify (Const a :- Const b) = Const (a - b)
> canonify (e1 :- e2)           = canonify e1 :- canonify e2
>
> -- | Multiplication
> -- | e * 0 = 0 (Kills the tree immediately)
> canonify (_ :* Const 0)       = Const 0
> canonify (Const 0 :* _)       = Const 0
> -- | e * 1 = e
> canonify (e :* Const 1)       = canonify e
> canonify (Const 1 :* e)       = canonify e
> -- | Lifting
> canonify (Const a :* Const b) = Const (a * b)
> -- | Propagate
> canonify (e1 :* e2)           = canonify e1 :* canonify e2
>
> -- | Division
> canonify (Const a :/ Const b) = Const (a / b)
> canonify (e1 :/ e2)           = canonify e1 :/ canonify e2
>
> -- | Lambda
> --canonify (Lambda p b)         = Lambda p (canonify b)
>
> -- | Function
> --canonify (Func string)        = Func string
>
> -- | Application
> --canonify (e1 :$ e2)           = canonify e1 :$ canonify e2
>
> -- | Delta
> --canonify (Delta e)            = Delta $ canonify e
>
> -- | Derivative
> canonify (D e)                = derive e
>
> -- | Catch all
> canonify (Const x)            = Const x
> canonify Id                   = Id
> canonify (e1 :. e2)           = canonify e1 :. canonify e2
> canonify e                    = error $ show e


> -- | "Proofs"
> syntacticProofOfComForMultiplication :: FunExpr -> FunExpr -> IO Bool
> syntacticProofOfComForMultiplication e1 e2 = prettyEqual (e1 :* e2) (e2 :* e1)
>
> syntacticProofOfAssocForMultiplication :: FunExpr -> FunExpr -> FunExpr -> IO Bool
> syntacticProofOfAssocForMultiplication e1 e2 e3 = prettyEqual (e1 :* (e2 :* e3))
>                                                               ((e1 :* e2) :* e3)
>
> syntacticProofOfDistForMultiplication :: FunExpr -> FunExpr -> FunExpr -> IO Bool
> syntacticProofOfDistForMultiplication e1 e2 e3 = prettyEqual (e1 :* (e2 :+ e3))
>                                                               ((e1 :* e2) :+ (e1 :* e3))
>
> {- syntacticProofOfIdentityForMultiplication :: FunExpr -> IO Bool -}
> {- syntacticProofOfIdentityForMultiplication e = -}
> {-   putStrLn "[*] Checking right identity" >> -}
> {-     prettyEqual e (1 :* e) >> -}
> {-       putStrLn "[*] Checking left identity" >> -}
> {-         prettyEqual e (e :* 1) -}
>
> {- syntacticProofOfPropertyOf0ForMultiplication :: FunExpr -> IO Bool -}
> {- syntacticProofOfPropertyOf0ForMultiplication e = -}
> {-   prettyEqual (e :* 0) 0 -}
>
> -- | Fails since default implementation of negate x for Num is 0 - x
> {- syntacticProofOfPropertyOfNegationForMultiplication :: FunExpr -> IO Bool -}
> {- syntacticProofOfPropertyOfNegationForMultiplication e = -}
> {-   prettyEqual (Const (-1) :* e)  (negate e) -}
>
> syntacticProofOfComForAddition :: FunExpr -> FunExpr -> IO Bool
> syntacticProofOfComForAddition e1 e2 = prettyEqual (e1 :+ e2) (e2 :+ e1)
>
> syntacticProofOfAssocForAddition :: FunExpr -> FunExpr -> FunExpr -> IO Bool
> syntacticProofOfAssocForAddition e1 e2 e3 = prettyEqual (e1 :+ (e2 :+ e3))
>                                                         ((e1 :+ e2) :+ e3)
>
> test :: FunExpr -> FunExpr -> IO Bool
> test b c = prettyEqual b (a :* c)
>   where
>     a = b :/ c
>
>
> syntacticProofOfIdentityForAddition :: FunExpr -> IO Bool
> syntacticProofOfIdentityForAddition e = putStrLn "[*] Checking right identity" >>
>   prettyEqual e (0 :+ e) >>
>     putStrLn "[*] Checking left identity" >>
>       prettyEqual e (e :+ 0)
>
> -- | Dummy expressions
> --eT = D (Func "sin") :+ Func "cos" :$ (Const 2 :+ Const 3) :* (Const 3 :+ Const 2 :* Delta (Const 1 :+ Const 2)) :/ (Const 5 :- (Const 4 :+ Const 8)) :+ Lambda "x" (Const 2)
> e1 = Const 1
> e2 = Const 2
> e3 = Const 3
> e4 = (Const 1 :+ Const 2) :* (Const 3 :+ Const 4)
> e5 = (Const 1 :+ Const 2) :* (Const 4 :+ Const 3)
> e6 = Const 2 :+ Const 3 :* Const 8 :* Const 19

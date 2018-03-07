module Calculus.SyntaxTree where

import Calculus.Calculus
import Data.Tree as T
import Data.Tree.Pretty as P
import VecTest.Vector


v1 = V2 (Const 2) (Const 2)
v2 = V2 (Const 3) (Const 4)
v3 = v1 + v2
v4 = vmap canonify v3

main :: IO Bool
main = prettyEqual e4 e5

-- | Pretty prints expressions as trees
printExpr :: Expr -> IO ()
printExpr = putStrLn . drawVerticalTree . makeTree

printVector :: Vector2 -> IO ()
printVector = putStrLn . drawVerticalTree . vectorTree

-- | Implement vectors with deep embedding for richer syntax trees
vectorTree :: Vector2 -> Tree String
vectorTree (V2 x y) = Node "V" [
                                 Node "x" [makeTree x]
                               , Node "y" [makeTree y]
                               ]

-- | Pretty prints the steps taken when canonifying an expression
prettyCan :: Expr -> IO ()
prettyCan e =  do
  let t  = makeTree e
      e' = canonify e
      t' = makeTree e'
    in if t == t' then (putStrLn $ drawVerticalTree t)
                  else do
                      (putStrLn $ drawVerticalTree t)
                      prettyCan e'

-- Possible generalization, make it work on lists of Expr
-- | Pretty prints syntactic checking of equality
prettyEqual :: Expr -> Expr -> IO Bool
prettyEqual e1 e2 = case (e1 == e2) of
  True -> do 
    putStrLn "It's equal!" 
    putStrLn $ drawVerticalForest [makeTree e1, makeTree e2]
    return True
  False -> do
    putStrLn "Not equal -> Simplifying"
    putStrLn $ drawVerticalForest [makeTree e1, makeTree e2]
    let c1 = canonify e1
        c2 = canonify e2
      in if (c1 == e1 && c2 == e2) then putStrLn "Can't simplify no more" >> return False
                                   else prettyEqual c1 c2

-- Parse an expression as a Tree of Strings
makeTree :: Expr -> T.Tree String
makeTree (e1 :+ e2)   = T.Node ("+") [(makeTree e1), (makeTree e2)]
makeTree (e1 :- e2)   = T.Node ("-") [(makeTree e1), (makeTree e2)]
makeTree (e1 :* e2)   = T.Node ("*") [(makeTree e1), (makeTree e2)]
makeTree (e1 :/ e2)   = T.Node ("Div") [(makeTree e1), (makeTree e2)]
makeTree (e1 :. e2)   = T.Node ("o") [(makeTree e1), (makeTree e2)]
makeTree (Var v)      = T.Node v []
makeTree (Lambda s e) = T.Node ("Lambda " ++ s) [makeTree e]
makeTree (Func s)     = T.Node s []
makeTree (Delta e)    = T.Node "Delta" [makeTree e]
makeTree (D e)        = T.Node "D" [makeTree e]
makeTree (e1 :$ e2)   = T.Node ("$") [(makeTree e1), (makeTree e2)]
makeTree e            = T.Node (show e) []

equals :: Expr -> Expr -> Bool
-- Addition is commutative
equals (e1 :+ e2) (e3 :+ e4) = (canonify (e1 :+ e2) == canonify (e3 :+ e4)) || 
                               (canonify (e1 :+ e2) == canonify (e4 :+ e3))
-- | Addition is associative
-- equals (e1 :+ (e2 :+ e3))    = undefined
-- Multiplication is commutative
equals (e1 :* e2) (e3 :* e4) = (canonify (e1 :* e2) == canonify (e3 :* e4)) || 
                               (canonify (e1 :* e2) == canonify (e4 :* e3))
equals e1 e2 = canonify e1 == canonify e2

canonify :: Expr -> Expr
-- | Addition
-- | e + 0 = e
canonify (e :+ (Const 0)) = canonify e
canonify ((Const 0) :+ e) = canonify e
-- | Lifting
canonify (Const x :+ Const y) = Const (x + y)
canonify (e1 :+ e2) = canonify e1 :+ canonify e2

-- | Subtraction
-- | e - 0 = e
canonify (e :- Const 0) = canonify e
-- | 0 - b = -b
canonify (Const 0 :- b) = (negate (canonify b))
-- | Lifting
canonify (Const a :- Const b) = Const (a - b)
canonify (e1 :- e2) = canonify e1 :- canonify e2

-- | Multiplication
-- | e * 0 = 0 (Kills the tree immediately)
-- canonify (_ :* Const 0) = Const 0
-- canonify (Const 0 :* _) = Const 0
-- | e * 1 = e
canonify (e :* (Const 1)) = canonify e
canonify ((Const 1) :* e) = canonify e
-- | Lifting
canonify (Const a :* Const b) = Const (a * b)
-- | Propagate
canonify (e1 :* e2) = canonify e1 :* canonify e2

-- | Division
canonify (Const a :/ Const b) = Const (a / b)
canonify (e1 :/ e2) = canonify e1 :/ canonify e2

-- | Lambda
canonify (Lambda p b) = (Lambda p (canonify b))

-- | Function
canonify (Func string) = Func string

-- | Application
canonify (e1 :$ e2) = canonify e1 :$ canonify e2

-- | Delta
canonify (Delta e) = Delta $ canonify e

-- | Derivative
canonify (D e) = derive e

-- | Catch all
canonify (Const x) = Const x
canonify e = error $ show e


-- | "Proofs"
syntacticProofOfComForMultiplication :: Expr -> Expr -> IO Bool
syntacticProofOfComForMultiplication e1 e2 = prettyEqual (e1 :* e2) (e2 :* e1)

syntacticProofOfAssocForMultiplication :: Expr -> Expr -> Expr -> IO Bool
syntacticProofOfAssocForMultiplication e1 e2 e3 = prettyEqual (e1 :* (e2 :* e3))
                                                              ((e1 :* e2) :* e3)

syntacticProofOfDistForMultiplication :: Expr -> Expr -> Expr -> IO Bool
syntacticProofOfDistForMultiplication e1 e2 e3 = prettyEqual (e1 :* (e2 :+ e3))
                                                              ((e1 :* e2) :+ (e1 :* e3))

syntacticProofOfIdentityForMultiplication :: Expr -> IO Bool
syntacticProofOfIdentityForMultiplication e = 
  putStrLn "[*] Checking right identity" >> 
    prettyEqual e (1 :* e) >>
      putStrLn "[*] Checking left identity" >>
        prettyEqual e (e :* 1) 

syntacticProofOfPropertyOf0ForMultiplication :: Expr -> IO Bool
syntacticProofOfPropertyOf0ForMultiplication e = 
  prettyEqual (e :* 0) 0

-- | Fails since default implementation of negate x for Num is 0 - x
syntacticProofOfPropertyOfNegationForMultiplication :: Expr -> IO Bool
syntacticProofOfPropertyOfNegationForMultiplication e = 
  prettyEqual (Const (-1) :* e)  (negate e)

syntacticProofOfComForAddition :: Expr -> Expr -> IO Bool
syntacticProofOfComForAddition e1 e2 = prettyEqual (e1 :+ e2) (e2 :+ e1)

syntacticProofOfAssocForAddition :: Expr -> Expr -> Expr -> IO Bool
syntacticProofOfAssocForAddition e1 e2 e3 = prettyEqual (e1 :+ (e2 :+ e3))
                                                        ((e1 :+ e2) :+ e3)

test :: Expr -> Expr -> IO Bool
test b c = prettyEqual b (a :* c)
  where
    a = b :/ c


syntacticProofOfIdentityForAddition :: Expr -> IO Bool
syntacticProofOfIdentityForAddition e = putStrLn "[*] Checking right identity" >> 
  prettyEqual e (0 :+ e) >>
    putStrLn "[*] Checking left identity" >>
      prettyEqual e (e :+ 0) 

-- | Dummy expressions
eT = (D (Func "sin")) :+ Func "cos" :$ (Const 2 :+ Const 3) :* ((Const 3 :+ Const 2) :* (Delta (Const 1 :+ Const 2))) :/ (Const 5 :- (Const 4 :+ Const 8)) :+ (Lambda "x" (Const 2))
e1 = Const 1
e2 = Const 2
e3 = Const 3
e4 = (Const 1 :+ Const 2) :* (Const 3 :+ Const 4)
e5 = (Const 1 :+ Const 2) :* (Const 4 :+ Const 3)
e6 = (Const 2 :+ Const 3 :* Const 8 :* Const 19)

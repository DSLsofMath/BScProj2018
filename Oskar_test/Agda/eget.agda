
module eget where

data Nat : Set where
  -- zero är ett värde hos typen
  zero : Nat
  -- suc, när den fått ett värde, är ett annat värde hos typen
  suc  : Nat -> Nat

-- Börja med att introducera axiomet a + b = b + a

data _+_ (A : Set) (B : Set) : Set where
  add : A -> B -> A + B

data _==_ : Nat -> Nat -> Set where
  addCom : {n : Nat} -> (n + n) == (n + n)

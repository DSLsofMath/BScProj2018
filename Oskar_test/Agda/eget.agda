
module eget where

data Nat : Set where
  -- zero är ett värde hos typen
  zero : Nat
  -- suc, när den fått ett värde, är ett annat värde hos typen
  suc  : Nat -> Nat

-- Börja med att introducera axiomet a + b = b + a

-- data _+_ (A : Set) (B : Set) : Set where
  -- add : A -> B -> A + B

_+_ : Nat -> Nat -> Nat
a + zero = a
a + suc b = suc (a + b)

-- Equal if equal
data _==_ : Nat -> Nat -> Set where
  refl : (n : Nat) -> n == n

sym : {a b : Nat} -> (a == b) -> (b == a)
sym (refl n) = refl n

trans : {a b c : Nat} -> (a == c) -> (b == c) -> (a == b)
trans (refl n) (refl .n) = refl n

cong : {a b : Nat} -> (f : Nat -> Nat) -> (a == b) -> (f a) == (f b)
cong f (refl n) = refl (f n)

assoc-+ : (a b c : Nat) -> ((a + b) + c) == (a + (b + c))
assoc-+ a b zero    = refl (a + b)
assoc-+ a b (suc c) = cong suc (assoc-+ a b c)

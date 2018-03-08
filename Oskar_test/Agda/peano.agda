
module peano where

-- Skapar typen ℕ, som har typen Set
data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero + zero = zero
zero + n      = n
(suc n) + m = suc (n + m)


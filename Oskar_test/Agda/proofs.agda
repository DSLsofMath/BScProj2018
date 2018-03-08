
module proofs where

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero + zero = zero
zero + n      = n
(suc n) + m = suc (n + m)

-- ℕ är en typ medan Set är en sort.
-- Så typkonstruktorn tar ett tal (värde) och ger en typ
data _even : ℕ → Set where
  -- Axiom. ZERO är ett värde, av typen zero even
  -- Alltså ett bevis att noll är jämnt
  ZERO : zero even
  -- Regel. STEP är en funktion som i slutändan ger ett värde, för NåGoT _even har typen Set
  -- Så NåGoT som har typen NåGoT _evan är ett värde

  -- x är nåt värde av typen N. När det värdet fixerats, så ger man ett värde av typen x even
  -- alltså ett bevis för att x är jämnt, och så får man ett bevis för att suc (suc x) är jämnt
  STEP : (x : ℕ) → x even → suc (suc x) even

proof₀ : suc (suc zero) even
proof₀ = STEP zero ZERO
                    
proof₁ : suc (suc (suc (suc zero))) even
proof₁ = STEP (suc (suc zero)) (STEP zero ZERO)
                                                  
proofₐ : suc (suc (suc (suc zero))) even
proofₐ = STEP (suc (suc zero)) proof₀

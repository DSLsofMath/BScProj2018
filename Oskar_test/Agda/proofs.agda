
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

-- Samma som Set → A → A, fast första parametern är namngiven
-- A har typen Set, så A är en typ
proof₂′ : (A : Set) → A → A
proof₂′ _ x = x

-- Funkar ej, varför???
--proof₂ : (suc (suc zero)) even → (suc (suc zero)) even
--proof₂ = proof₂′ ℕ

-- P och Q har båda typen Set, så de är typer. Så vad som åker in i
-- funktionen är värden (bevis)q
data _∧_ (P : Set) (Q : Set) : Set where
 ∧-intro : P → Q → (P ∧ Q)


--proof₃  : (P : Set) → (Q : Set) → P ∧ Q → P
--proof₃ _ _ (∧-intro p _) = p
-- Vad är skillnaden på dessa?
proof₃ : {P Q : Set} → (P ∧ Q) → P
proof₃ (∧-intro p q) = p
-- Hittade: den senare behöver man inte skicka med typerna som argument
-- Implicta typer

-- Ge den ett värde av typ P, som i sin tur har typen Set
-- Och ett värde av typ Q, som har typen Set
-- Tillbaks får man ett värde
--data _⇔_  (a : Set)  (b : Set) : Set
--   ⇔-intro a b : (a → b) ∧ (b → a)

--_⇔_ :  (P : Set)  →  (Q : Set) → Set
--a ⇔ b = (a → b) ∧ (b → a)

_⇔_ : (P : Set) → (Q : Set) → Set
a ⇔ b = (a → b) ∧ (b → a)
-- Fattar inte riktigt vad denna betyder

∧-comm' : ( P : Set) → (Q : Set) → P ∧ Q → Q ∧ P
∧-comm' _ _ (∧-intro p q) = ∧-intro q p

∧-comm : {P Q : Set} → (P ∧ Q) ⇔ (Q ∧ P)
∧-comm = ∧-intro (∧-comm' {P} {Q}) (∧-comm' {Q} {P})
-- Fattar inte detta heller



module Chalmers where

-- Bool är en typ, som har typen "sorten" Set
data Bool : Set where
  -- Konstruktorer med sina typer
  -- De är (efter att ha fått alla sina argument) värden av typen Bool
  true  : Bool
  false : Bool
-- Alla konstruktorer *till en typ* är möjliga värden hos den typen.

-- Ett värde av typ Bool blir ett annat värde av typ Bool
not : Bool -> Bool
not true = false
not false = true

-- En typ Nat av sorten Set
data Nat : Set where
  -- zero är ett värde hos typen
  zero : Nat
  -- suc, när den fått ett värde, är ett annat värde hos typen
  suc  : Nat -> Nat

-- En funktion som tar två värden av typen Nat, och ger ett annat värde av typen Nat
_+_ : Nat -> Nat -> Nat
zero + m = m
suc n + m = suc (n + m)

_*_ : Nat -> Nat -> Nat
zero * m = zero
suc n * m = m + (m * n)

_or_ : Bool -> Bool -> Bool
false or x = x
true or _ = true

-- {A : Set} säger att A är en typ av sort Set.
if_then_else : {A : Set} -> Bool -> A -> A -> A
if true  then x else _ = x
if false then _ else y = y

infixr 40 _::_
-- Typen List har sorten Set
-- Men List måste ges en typ, som namnges A, som är av sorten Set
data List (A : Set) : Set where
  []    : List A
  _::_ : A -> List A -> List A

-- Först ges funktionen en typ som binds till namnet A. Typen ska ha sorten Set
-- Sedan tar ett värde av typen A och ett värde av typen A returneras
identity : (A : Set) -> A -> A
identity A x = x

-- Här är samma, fast typargumentet är obundet.
identity' : Set -> Bool -> Bool
identity' A b = b
-- Notationen (A : Set) ska alltså ses som att argumentet är av typen Set,
-- och att det kallas A.

-- (A : Set) betyder att det argumentet binds till A, för parametern (A : Set)


zero' : Nat
zero' = identity Nat zero


apply : (A : Set)(B : A -> Set) -> ((x : A) -> B x) -> (a : A) -> B a
apply _ _ f a = f a

apply' : (A : Set) -> (B : A -> Set) -> ((x : A) -> B x) -> (a : A) -> B a
apply' A B f a = f a
-- x är ett värde av typen A
-- a är ett värde av typen A
-- A är en typ av sorten Set
-- B är en funktion från typen A till Set, alltså blir B matat en typ av sorten Set

-- f är en funktion som tar ett värde x av typ A, och ger ett nytt värde av typ B x.
-- B x är en typ av sort Set. Typen beror på värdet det fick
-- a är ett värde av typ A

-- (x : A)(y : B) -> C är kortform för (x : A) -> (y : B) -> C
-- (x y : A) -> B är kortform för (x : A) -> (y : A) -> B

-- {x : A} -> B är samma som (x : A) -> B, förutom att i den första är typargumnetet
-- implicit. Dvs, programmeraren behöver inte skicka med det typargumenet, utan
-- typcheckarn hittar det automatiskt.

id : {A : Set} -> A -> A
id x = x

id' : (A : Set) -> A -> A
id' A x = x

-- Likheten mellan dessa är att A är en typ som blivit namngiven och därmed kan användas
-- i typsignaturen
-- Skillnaden är att i den senare behöver typen som argument inte skickas med explicit.
-- Behöver inte = får inte, i detta fall


true' : Bool
true' = id true

silly : {A : Set}{x : A} -> A
silly {_}{x} = x

false' : Bool
false' = silly {x = false}

silly' : {A : Set} -> {x : A} -> A
silly'{_}{x} = x

-- silly {x = false} betyder att det implicita argumentet med namn x får värdet false
-- silly {Bool} skulle ge det första implicita argument, A, Bool.

one : Nat
one = identity _ (suc zero)

-- identity : (A : Set) -> A -> A
-- identity A x = x

-- Här är typargumentet explicit. Men vi låter typcheckarn fylla i det åt oss med _

map : {A B : Set} -> (A -> B) -> List A -> List B
map f [] = []
map f (x :: xs) = f x :: map f xs

map' : (A : Set) -> (B : Set) -> (A -> B) -> List A -> List B
map' _ _ f []          = []
map' _ _ f (x :: xs) = f x :: map f xs

add : (A : Set) -> List A -> List A -> List A
add _ xs [] = xs
add _ [] ys = ys
add A (x :: xs) ys = x :: (add A xs ys)

_++_ : {A : Set} -> List A -> List A -> List A
[] ++ ys = ys
xs ++ [] = xs
(x :: xs) ++ ys = x :: (xs ++ ys)


data Vec (A : Set) : Nat -> Set where
  []     : Vec A zero
  _::_ : {n : Nat} -> A -> Vec A n -> Vec A (suc n)

-- Här är Vec paramteriserad av typen A (som har sort Set)
-- och Vec är indexerad över naturliga tal, eftersom Vec A : Nat -> Set
-- dvs ge ett värde (av typ Nat) och få en typ (av sort Set)

head : {A : Set} -> {n : Nat} -> Vec A (suc n) -> A
head (h :: _) = h

vmap : {A : Set} -> {B : Set} -> {n : Nat} -> (A -> B)  -> Vec A n -> Vec B n
vmap f [] = []
vmap f (x :: xs) = f x :: vmap f xs

data Vec₂ (A : Set) : Nat -> Set where
  nil     : Vec₂ A zero
  cons : (n : Nat) -> A -> Vec₂ A n -> Vec₂ A (suc n)

vmap₂ : {A B : Set} -> (n : Nat) -> (A -> B) -> Vec₂ A n -> Vec₂ B n
vmap₂ .zero f nil = nil
vmap₂ .(suc n) f (cons n x xs) = cons n (f x) (vmap₂ n f xs)

-- . anger att kommer från typcheckning och inte mönstermatchning


-- Det "generella" att bild av en viss funktion från t.ex. char till double
-- och det "specifika" att 2 tillhör den funktionen
data Image_∋_ {A B : Set}(f : A -> B) : B -> Set where
  im : (x : A) -> Image f ∋ f x

func : Nat -> Nat
func = suc

proof : Image func ∋ (suc zero)
proof = im zero

inv : {A B : Set}(f : A -> B)(y : B) -> Image f ∋ y -> A
inv f .(f x) (im x) = x


data    False : Set where
record True  : Set where

trivial : True
trivial = _

isTrue : Bool -> Set
isTrue true = True
isTrue false = False

_<_ : Nat -> Nat -> Bool
_ < zero = false
zero < suc n = true
suc m < suc n = m < n

length : {A : Set} -> List A -> Nat
length [] = zero
length (x :: xs) = suc (length xs)

lookup : {A : Set}(xs : List A)(n : Nat) -> isTrue (n < length xs) -> A
lookup [] n ()
lookup (x :: xs) zero p = x
lookup (x :: xs) (suc n) p = lookup xs n p

-- Det tredje argumentet är det värde (bevis) av typen (påståendet) isTrue (n < length xs) gäller
-- om n < length xs inte är lika med true, blir isTrue lika med False.
-- Man ska alltså ge ett värde av typ som inte kan ha några värden

data _==_ {A : Set}(x : A) : A -> Set where
  refl : x == x

infix 4 _==_

-- "Ge mig två värden (som är naturliga tal, dvs av typen Nat) och jag blir
-- ett bevis (dvs typ, av sorten Set) att det första är mindre/lika med det andra
-- Värden av mig är bevis
data _≤_ : Nat -> Nat -> Set where
  -- Ge mig (implcit) ett tal, så får du ett bevis att 0 är mindre/lika med det talet
  leq-zero : {n : Nat} -> zero ≤ n
  leq-suc  : {m n : Nat} -> m ≤ n -> suc m ≤ suc n

leq-trans : {a b c : Nat} -> a ≤ b -> b ≤ c -> a ≤ c
leq-trans leq-zero _ = leq-zero
leq-trans (leq-suc p) (leq-suc q) = leq-suc (leq-trans p q)

refl' : {A : Set} -> (a : A) -> a == a
refl' _ = refl

sym : {A : Set} -> {a b : A} -> a == b -> b == a
sym refl = refl

trans : {A : Set} -> {a b c : A} -> a == b -> b == c -> a == c
trans refl refl = refl

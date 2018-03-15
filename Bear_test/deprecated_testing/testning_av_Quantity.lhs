
Testobjekt

Önskat resultat

Resultat

Analys

//----------------------------------------------

Testobjekt
*Dimensions.Quantity> :info quantityAdd
quantityAdd ::
  Num v => Quantity d v -> Quantity d v -> Quantity d v
        -- Defined at Quantity.lhs:94:3

Önskat resultat
v1 och v2 ska adderas. Om det är olika dimension ska den kasta fel.

Resultat

*Dimensions.Quantity> quantityAdd (Quantity V.time 4) (Quantity V.time 5)
9 s
*Dimensions.Quantity> quantityAdd (Quantity V.time 4) (Quantity V.length 5)
9 s
*Dimensions.Quantity> quantityAdd (Quantity V.length 4) (Quantity V.time 5)
9 m

*Dimensions.Quantity> quantityAdd (Quantity V.length 1) (Quantity V.time (-1))
0 m


Analys
Det verkar som om dimensionen i det andra argumentet kastas bort. Annars ser det bra ut.

//----------------------------------------
Testobjekt
quantityMul
  :: Num v =>
     Quantity d1 v -> Quantity d2 v -> Quantity (Mul d1 d2) v

Önskat resultat
Dimensionerna ska multipliceras. Värderna ska multipliceras.

Resultat
*Dimensions.Quantity> quantityMul (Quantity V.time 4) (Quantity V.time 2)

<interactive>:47:1:
    Couldn't match type ‘Mul d1 d2’ with ‘Mul d10 d20’
    NB: ‘Mul’ is a type function, and may not be injective
    The type variables ‘d10’, ‘d20’ are ambiguous
    Expected type: Quantity (Mul d1 d2) v
      Actual type: Quantity (Mul d10 d20) v
    When checking that ‘it’ has the inferred type
      it :: forall (d1 :: Dim) v (d2 :: Dim).
            Num v =>
            Quantity (Mul d1 d2) v
    Probable cause: the inferred type is ambiguous


Analys
   Frågar oskar.


//------------------------------------
Efter att ha snackat med oskar så bör jag först gå igenom:
ValueLevel, Sedan TypeLevel, och sist QuantityLevel.


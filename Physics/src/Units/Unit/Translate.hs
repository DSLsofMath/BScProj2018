
module Units.Unit.Translate
(
)
where

import Prelude hiding (length)
import Data.List hiding (length)

import Units.Helper
import Units.Unit.Syntax as S
import Units.Unit.Canonical as C

------------------------------------------------------------
-- Snack

-- Flera olika syntaktiska former kan ge samma kanoniska form
-- Tolkningen blir att "samma" enhet kan skrivas på flera sätt

-- T.ex. m/(m/s) är samma som s, där bara s är den
-- kanoniska formen

-- Att gå andra hållet är också möjligt
-- Vilken syntaktisk form som då väljs är godtyckligt,
-- då kanonisk enbart har betydelsen, medan syntaktiskt
-- även har någon slags form.

-- Unit.Syntax är syntax
-- Unit.Canonical är semantik

-- Men rätt så utförlig semantik. Mer utförlig än
-- 5+2=7 som vi sett i DSLsofMath

------------------------------------------------------------
-- Från syntaktisk till kanonisk

synToCan :: S.Unit -> C.Unit
synToCan S.One = C.one
synToCan (S.Length n) = C.Unit [(C.Length, n)]
synToCan (S.Time n)   = C.Unit [(C.Time, n)]
synToCan (S.Mass n)   = C.Unit [(C.Mass, n)]
synToCan (u1 :*: u2)  = synToCan u1 * synToCan u2
synToCan (u1 :/: u2)  = synToCan u1 / synToCan u2


weirdSyn :: S.Unit
weirdSyn = ((S.Length 2) :*: (S.Length (-5))) :/: (S.Mass 1 :/: S.Time 3 :/: (S.Time 2 :*: S.Mass 7))

------------------------------------------------------------
-- Från kanonisk till syntaktisk

canToSyn :: C.Unit -> S.Unit
canToSyn (C.Unit []) = S.One
canToSyn (C.Unit ((C.Length, n):rest)) = (S.Length n) :*: canToSyn (Unit rest)
canToSyn (C.Unit ((C.Time, n):rest)) = (S.Time n) :*: canToSyn (Unit rest)
canToSyn (C.Unit ((C.Mass, n):rest)) = (S.Mass n) :*: canToSyn (Unit rest)

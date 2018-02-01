
module Algebra
(
)
where

-- s, v, t
data Dis = Dis
data Vel = Vel
data Tim = Tim

data Mul a b
data Div a b

axiom = undefined

mulIntro :: a -> b -> Mul a b
mulIntro = axiom

divIntro :: a -> b -> Div a b
divIntro = axiom

mulCom :: Mul a b -> Mul b a
mulCom = axiom

mulDivElim1 :: Mul a (Div b a) -> b
mulDivElim1 = axiom

mulDivElim2 :: Mul (Div b a) a -> b
mulDivElim2 bDaMa = let aMbDa = mulCom bDaMa
                    in  mulDivElim1 aMbDa

mulDivElim3 :: Div (Mul a b) b -> a
mulDivElim3 = undefined

mulDivElim4 :: Div (Mul a b) a -> b
mulDivElim4 = undefined



data Equ lhs rhs = Equ lhs rhs

mult :: (Equ lhs rhs) -> f -> Equ (Mul lhs f) (Mul rhs f)
mult = axiom

divv :: (Equ lhs rhs) -> d -> Equ (Div lhs d) (Div rhs d)
divv = axiom



eqIntro :: a -> b -> Equ a b
eqIntro = axiom

eqElim :: Equ a b -> (a, b)
eqElim = axiom


vEsDt :: Equ Vel (Div Dis Tim)
vEsDt = axiom

sEvMt :: Equ Dis (Mul Vel Tim)
sEvMt = let vMtE'sDt'Mt = mult vEsDt Tim
            (vMt, sDtMt) = eqElim vMtE'sDt'Mt
            s = mulDivElim2 sDtMt
        in eqIntro s vMt

-- TODO: Lösa ovanstående så att man inte kan fuska
-- genom att byta vMt mot vad som helst

tEsDv :: Equ Tim (Div Dis Vel)
tEsDv = let sDvE'vMt'Dv = divv sEvMt Vel
            (sDv, vMtDv) = eqElim sDvE'vMt'Dv
            t = mulDivElim4 vMtDv
        in eqIntro t sDv
            


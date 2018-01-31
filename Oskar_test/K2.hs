
module K2
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


data Equ lhs rhs = Equ lhs rhs

mult :: (Equ lhs rhs) -> f -> Equ (Mul lhs f) (Mul rhs f)
mult = axiom


op :: (Equ a a) -> (a -> t) -> (Equ t t)
op = axiom


eqIntro :: a -> b -> Equ a b
eqIntro = axiom

eqElim :: Equ a b -> (a, b)
eqElim = axiom


sEvMt :: Equ Dis (Mul Vel Tim)
sEvMt = let vMtE'sDt'Mt = mult vEsDt Tim
            (vMt, sDtMt) = eqElim vMtE'sDt'Mt
            s = mulDivElim2 sDtMt
        in eqIntro s vMt

vEsDt :: Equ Vel (Div Dis Tim)
vEsDt = axiom


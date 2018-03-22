

> import Calculus.Calculus


> inc1 :: Int -> Int
> inc1 a = a + 1

Raketen ökar sin hastighet från 0 till 100 m/s under 10s.

rocket_v :: Expr
rocket_v = (Const 10) :* (Var "t")


Hur lång sträcka har raketen färdats?
Integrera, sedan ta från t0 till t = 10


Vad är raketens acceleration?


helpers:

*Calculus.Calculus> deriveEx ((Const 10) :* (Var "x")) "x"
((0.0 * x) + (10.0 * 1.0))

*Calculus.Calculus> valToReal $ eval [("x",1)] $ deriveEx ((Const 10) :* (Var "x")) "x"
10.0

*Calculus.Calculus> simplify $ deriveEx ((Const 10) :* (Var "x")) "x"
((0.0 * x) + 10.0)


Vi kan inte intergrera på 10 * x
Vi kan integrera 10+x 

*Calculus.Calculus> integrateEx ((Const 10) :* (Var "x")) "x" 0
(((10.0 * (((x * x) / 2.0) + 0.0)) - *** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at Calculus.lhs:452:14 in main:Calculus.Calculus

*Calculus.Calculus> integrateEx ((Const 10) :+ (Var "x")) "x" 0
((((10.0 * x) + 0.0) + (((x * x) / 2.0) + 0.0)) + 0.0)




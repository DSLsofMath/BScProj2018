
module Tree
(
)
where

-- v = dx/dt är grunden
-- x är position, s är sträcka
-- Men dx=ds eftersom förändring av sträcka blir samma
-- som förändring av position

-- v = dx/dt <=>
-- 1 * dx = v * dt <=>
-- integ 1 dx i->f = integ v dt i->f
-- Gränserna är av samma typ som "d...". Tydligare blir
-- integ 1 dx xi->xf = integ v dt ti->tf <=>
-- v är i det generellera fallet en funktion av tid
-- Båda sidorna är sträcka (slutposition-startposition)
-- xf-xi = integ v dt ti->tf

-- Med i->f är det en bestämd integral
-- Det betyder att +C tar ut varandra, så jag har C=0



data Function r = Polynomial [r] -- r0+r1*t+r2*t^2+...
                | Exponential r  -- r*e^t
                deriving (Eq, Show)

integ :: (Fractional r ) => Function r -> Function r
integ (Polynomial as) = Polynomial (integ' as)
  where
    integ' [] = undefined
    integ' as = 0:(integ'' as 1)
      where
        integ'' (a:[]) n = [a / n]
        integ'' (a:as) n = (a / n):(integ'' as (n+1))
integ exp = exp

data Position r = Position (Function r)
                deriving (Eq, Show)

data Velocity r = Velocity (Function r)
                deriving (Eq, Show)

data Time r     = Time (Function r)
                deriving (Eq, Show)

-- Hur härldes det välbekanta sambandet s=v*t?
-- Funkar bara om v konstant, dvs v är *en konstant*

v :: (Fractional r) => Velocity r
v = Velocity (Function 

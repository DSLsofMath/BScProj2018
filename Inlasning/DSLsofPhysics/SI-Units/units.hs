



type Meter = Double

type Second = Double

m1 :: Meter
m1 = 3.0

s1 :: Second
s1 = 3.5


inc1 :: Meter -> Meter
inc1 m = m + 1


data Meters = Mm Double 
    deriving(Show)

data Seconds = Ss Double
    deriving(Show)

minc :: Meters -> Meters
minc (Mm m) = Mm (m+1)

--freefall :: Height -> Gravitation -> Velocity -> Second

--freefall h g v = 


--s = v0 * t + v^2 * t / 2
stv v0 t a = v0 * t + a * (t^2) / 2



data Units = M
    | S
    | Units :*: Units
    | Units :/: Units
    deriving(Show)


--stvd :: Units -> Units -> Units
svtd u1 u2 = u1 :*: u2

tostr :: Units -> String
tostr M = "m"
tostr S = "s"
tostr (u1 :*: u2) = (tostr u1) ++ "*" ++ (tostr u2)
tostr (u1 :/: u2) = (tostr u1) ++ "/" ++ (tostr u2)

-- We got putStrLn $ tostr $ M :/: S -> m/s

-- We want m/(s^2), or m/s^2 first.




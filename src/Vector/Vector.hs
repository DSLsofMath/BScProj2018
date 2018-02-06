module Vector where

-- Might just complicate things more
data Coordinate a = Coord a a

-- | Är vektorer definierade från origo, eller fritt i rummet?

-- | Vektorer som vet sin "vinkel" och "storlek"?

-- | Could also only have V3 and leave the the third
-- | argument blank for V2

-- | Vectors are only used in reference to the the object


-- Stupid name
data Vector numb =
      Scalar numb
    | V2 (numb, numb)
    | V3 (numb, numb, numb)
    -- General vector type, with unlimited dimensions. And for each dimension
    | V [numb]
    deriving Show

data VectorOP numb =
    Magnitude (Vector numb)
  | Addition  (Vector numb) (Vector numb)
  | ScalarMult numb (Vector numb)
  | DotProd   (Vector numb) (Vector numb)
  | CrossProd (Vector numb) (Vector numb)

-- Floating type constraints because of my use of sqrt below.

addition :: (Floating t) => Vector t -> Vector t -> VectorOP t
addition = Addition

scalarMult :: (Floating t) => t -> Vector t -> VectorOP t
scalarMult = ScalarMult

dotProd :: (Floating t) => Vector t -> Vector t -> VectorOP t
dotProd = DotProd

magnitude :: (Floating t) => Vector t -> VectorOP t
magnitude = Magnitude

instance Vector Num where
  (Scalar i1) (+) (Scalar i2) = Scalar (i1 + i2)

-- Är monader OK?
-- Matriser?

-- Tallinje för att visa längd på vektorerna

-- | ~Laws~
-- | Langrange's formula: a x (b x c) = b(a * c) - c(a * b)
-- | Cross product is anticommutative
-- | Jacobi identity: a x (b x c) + b x (c x a) + c x (a x b) = 0

-- Jag saknar agda...

-- Check dimensions?

lift :: (Floating t) => Vector t -> Vector t
lift (V2 (x, y)) = V3 (x, y, 0)

xUnit :: (Floating t) => Vector t
xUnit = V2 (1, 0)

eval :: (Floating t) => VectorOP t -> IO (Vector t)
eval (Magnitude (V3 (x, y, z))) =
  return $ Scalar $ sqrt $ x** 2 + y ** 2 + z ** 2
eval (Addition (V3 (x1, y1, z1)) (V3 (x2, y2, z2))) =
  return $ V3 (x1 + x2, y1 + y2, z1 + z2)
eval (ScalarMult s (V3 (x, y, z))) =
  return $ V3 (x * s, y * s, z * s)
eval (DotProd (V3 (x1, y1, z1)) (V3 (x2, y2, z2))) =
  return $ Scalar $ (x1 + x2) * (y1 + y2) * (z1 + z2)
eval (CrossProd (V3 (x1, y1, z1)) (V3 (x2, y2, z2))) =
  return $ V3 (y1 * z2 - z1 * y2,
               z1 * x2 - x1 * z2,
               x1 * y2 - y1 * x2)

{-
eval (Magnitude (V coords)) =
  return $ Scalar $ sqrt . sum $ map (**2) coords

-- Can add general vectors of different dimensions
eval (Addition (V []) (V (y:ys)))  = return $ V ys
eval (Addition (V (x:xs)) (V []))  = return $ V xs
eval (Addition (V (x:xs)) (V (y:ys)))  = do
  (V test) <- eval $ Addition (V xs) (V ys)
  return $ V $ (x + y) : test

eval (ScalarMult s (V coords)) = return $ V [(x * s, y) | (x,y) <- coords]

eval (DotProd (V c1) (V c2)) =
  let l1 = map (\(f,s) -> (s -f)) c1
      l2 = map (\(f,s) -> (s -f)) c2
   in return $ Scalar $ sum $ zipWith (+) l1 l2

eval (CrossProd v1@(V c1) v2@(V c2)) = case length c1 of
                                         3 ->
                                           do a <- eval $ magnitude v1
                                              b <- eval $ magnitude v2
                                              --return a * b * sin()
                                              return $ V []
                                         _ -> error "Wrong dimensions"

-}
eval _                       = error "Not defined"

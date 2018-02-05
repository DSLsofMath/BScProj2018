module Vector where

-- Might just complicate things more
data Coordinate a = Coord a a

-- | Är vektorer definierade från origo, eller fritt i rummet?

-- | Vektorer som vet sin "vinkel" och "storlek"?

-- | Could also only have V3 and leave the the third
-- | argument blank for V2
--
-- Stupid name
data Vector numb =
      Scalar numb
    | V2 (numb, numb) (numb, numb)
    | V3 (numb, numb, numb) (numb, numb, numb)
    -- General vector type, with unlimited dimensions. And for each dimension
    -- a start and end point.
    | V [(numb, numb)] -- Int for specifying dimension (maybe)

data VectorOP numb =
    Magnitude (Vector numb)
  | Addition (Vector numb) (Vector numb)
  | ScalarMult numb (Vector numb)
  | DotProd (Vector numb) (Vector numb)
  | CrossProd (Vector numb) (Vector numb)
-- | Transpose?

-- Floating type constraints because of my use of sqrt below.

addition :: (Floating t) => Vector t -> Vector t -> VectorOP t
addition = Addition


scalarMult :: (Floating t) => t -> Vector t -> VectorOP t
scalarMult = ScalarMult

dotProd :: (Floating t) => Vector t -> Vector t -> VectorOP t
dotProd = DotProd

magnitude :: (Floating t) => Vector t -> VectorOP t
magnitude = Magnitude

-- Är monader OK?
-- Matriser?

-- Tallinje för att visa längd på vektorerna

-- | ~Laws~
-- | Langrange's formula: a x (b x c) = b(a * c) - c(a * b)
-- | Cross product is anticommutative
-- | Jacobi identity: a x (b x c) + b x (c x a) + c x (a x b) = 0

-- Jag saknar agda...

-- Check dimensions?

eval :: (Floating t) => VectorOP t -> IO (Vector t)
eval (Magnitude (V2 (x1, y1) (x2, y2))) =
  let xDiff = (x2 - x1)
      yDiff = (y2 - y1)
   in return $ Scalar $ sqrt $ xDiff ** 2 + yDiff ** 2
-- Kanske mer pedagogiskt med separata funktioner
eval (Magnitude (V coords)) = return $ Scalar $ sqrt . sum $
  map (\(fst, snd) -> (snd - fst) ** 2) coords
eval (Addition (V c1) (V c2))           =
  let list = map (\(fst, snd) -> snd - fst) c2
   in return $ V  [(x + t, y) | (x, y) <- c1, t <- list]

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

eval _                                  = error "Not defined"


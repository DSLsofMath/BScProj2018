> {-# LANGUAGE GADTs #-}
> module Body.Forces where

> import Vector.Vector as V
> import Dimensions.ValueLevel as Val

A force is a vector with a dimension.

> data Force = Force Vector2 Dim

> instance Show Force where
>   show (Force vec dim) = show (V.magnitude vec) ++ " " ++ show dim

We can add forces together:


> data ForceOps where
>   Add   :: Force  -> Force -> ForceOps
>   Scale :: Scalar -> Force -> ForceOps

> velocityVector :: Force
> velocityVector = Force (V2 0 0) (Val.length `Val.div` time)

> add :: Force -> Force -> ForceOps
> add = Add

> eval :: ForceOps -> IO Force
> eval (Add (Force vec1 dim1) (Force vec2 dim2)) = case dim1 == dim2 of
>   True  -> return $ Force (V.add vec1 vec2) dim1
>   False -> error "Dimensions are different"
> eval (Scale s (Force vec dim)) = return $ Force (V.scale s vec) dim




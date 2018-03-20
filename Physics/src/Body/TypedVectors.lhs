> {-# LANGUAGE GADTs #-}
> module Body.TypedVectors where

In order to do some physics we have to imbue our vectors with a dimension.

> import Vector.Vector as V
> import Dimensions.ValueLevel as Val
> import Dimensions.TypeLevel as T
> import Dimensions.Quantity as Q

> data TypedVector = TV Vector2 Val.Dim

> data MassVector = MV (Scalar -> Scalar)

> velocity :: TypedVector
> velocity = TV (V2 0 0) (Val.length `Val.div` Val.time)

> instance Show TypedVector where
>   show (TV vec dim) = show (V.magnitude vec) ++ " " ++ show dim

We can add vectors together

> data TVOps where
>   Add        :: TypedVector  -> TypedVector -> TVOps
>   Scale      :: Scalar       -> TypedVector -> TVOps


> add :: TypedVector -> TypedVector -> TVOps
> add = Add

> eval :: TVOps -> IO TypedVector
> eval (Add (TV vec1 dim1) (TV vec2 dim2)) = case dim1 == dim2 of
>   True  -> return $ TV (V.add vec1 vec2) dim1
>   False -> error "Dimensions are different"
> eval (Scale s (TV vec dim)) = return $ TV (V.scale s vec) dim




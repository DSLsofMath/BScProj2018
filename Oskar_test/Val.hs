
module Val
(
)
where

data Val v = Val v
           | Label String
           
instance (Show v) => Show (Val v) where
  show (Val v)   = show v
  show (Label s) = s

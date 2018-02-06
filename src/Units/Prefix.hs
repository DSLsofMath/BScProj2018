
module Prefix 
( Prefix(..)
)
where

data Prefix = P'  -- piko
            | N'  -- nano
            | U'  -- mikro
            | M'  -- milli
            | One -- "enhets-prefixet"
            | K   -- Kilo
            | M   -- Mega
            | G   -- Giga
            | T   -- Tera
            | P   -- Peta
            deriving (Eq, Ord)

-- Idé: evaluerare för SI, evaluerare för något annat ...

instance Show Prefix where
  show P' = "p"
  show N' = "n"
  show U' = "u"
  show M' = "m"
  show One = ""
  show K = "K"
  show M = "M"
  show G = "G"
  show T = "T"
  show P = "P"

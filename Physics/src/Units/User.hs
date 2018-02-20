
module Units.User where

import Prelude hiding (length)

import Units.Quantity
import Units.TypeLevel

--bredd :: Quantity Length Double
bredd = 0.3 # length
hojd = 0.2 # length
djup = 0.02 # length

bottenYta = bredd *# hojd


test = bredd +# bottenYta
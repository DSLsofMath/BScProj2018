
import Dimensions.TypeLevel
import Dimensions.Quantity
import Prelude hiding (length)

fTime = 1.0 # time

fVo = 3.0 # velocity

fAcc = 9.8 # acceleration

--freefall
fLength = fVo *# fTime +# fAcc *# fTime *# fTime /# (2.0 # one)




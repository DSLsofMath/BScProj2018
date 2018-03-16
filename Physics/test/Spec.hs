
import Vector.Vector as V
import Dimensions.ValueLevel.Test as DVL
import Dimensions.Quantity.Test as DQ

main :: IO ()
main = do
  V.runTests
  DVL.runTests
  DQ.runTests

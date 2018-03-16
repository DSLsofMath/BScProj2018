
import Vector.Vector as V
import Dimensions.ValueLevel.Test as DVL

main :: IO ()
main = do
  V.runTests
  DVL.runTests

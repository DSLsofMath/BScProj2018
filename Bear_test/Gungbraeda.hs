
import Dimensions.TypeLevel
import Dimensions.Quantity
import Prelude hiding (length)

balk_M = 1.0 # mass
m1 = 2.0 # mass
m2 = 5.0 # mass
d = 0.75 # length
balk_L = 5.0 # length
two = 2.0 # one

-- Direkt implikation:

balk_left_L = (balk_L /# two) +# d
balk_right_L = balk_L -# balk_left_L

-- Vi vill räkna ut vridmomenten?
-- Eller snarare ställa upp ekv:

m1_vrid = m1 *# balk_left_L 

balk_L_vrid = ((balk_left_L /# balk_L) *# balk_M) *# (balk_left_L /# two)


balk_H_vrid = ((balk_right_L /# balk_L) *# balk_M) *# (balk_right_L /# two)


-- m2_vrid = m2 * x

-- VL = HL
-- m1_vrid + balk_L_vrid = m2_vrid + balk_H_vrid
-- m1_vrid + balk_L_vrid - balk_H_vrid  = m2_vrid
-- (m1_vrid + balk_L_vrid - balk_H_vrid) / m2  = x

x = (m1_vrid +# balk_L_vrid -# balk_H_vrid) /# m2

-- säkerhetskoll:
m2_vrid = m2 *# x

vL = m1_vrid +# balk_L_vrid
hL = m2_vrid +# balk_H_vrid



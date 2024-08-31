########################################
# Local declaration with type
local x::T = 1
#----------
1   (= slot₂/tmp 1)
2   core.isa
3   slot₂/tmp
4   TestMod.T
5   (call %₂ %₃ %₄)
6   (gotoifnot %₅ label₈)
7   (goto label₁₅)
8   core.typeassert
9   top.convert
10  TestMod.T
11  slot₂/tmp
12  (call %₉ %₁₀ %₁₁)
13  TestMod.T
14  (= slot₂/tmp (call %₈ %₁₂ %₁₃))
15  slot₂/tmp
16  (= slot₁/x %₁₅)
17  (return 1)

########################################
# const
const xx = 10
#----------
1   (const TestMod.xx)
2   10
3   (= TestMod.xx %₂)
4   (return %₂)

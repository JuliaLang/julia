########################################
# Local declaration with type
local x::T = 1
#---------------------
1   (= slot₂/tmp 1)
2   slot₂/tmp
3   TestMod.T
4   (call core.isa %₂ %₃)
5   (gotoifnot %₄ label₇)
6   (goto label₁₂)
7   TestMod.T
8   slot₂/tmp
9   (call top.convert %₇ %₈)
10  TestMod.T
11  (= slot₂/tmp (call core.typeassert %₉ %₁₀))
12  slot₂/tmp
13  (= slot₁/x %₁₂)
14  (return 1)

########################################
# const
const xx = 10
#---------------------
1   (const TestMod.xx)
2   (= TestMod.xx 10)
3   (return 10)

########################################
# Typed const
const xx::T = 10
#---------------------
1   TestMod.T
2   (call core.set_binding_type! TestMod :xx %₁)
3   (const TestMod.xx)
4   (call core.get_binding_type TestMod :xx)
5   (= slot₁/tmp 10)
6   slot₁/tmp
7   (call core.isa %₆ %₄)
8   (gotoifnot %₇ label₁₀)
9   (goto label₁₂)
10  slot₁/tmp
11  (= slot₁/tmp (call top.convert %₄ %₁₀))
12  slot₁/tmp
13  (= TestMod.xx %₁₂)
14  (return 10)

########################################
# Global assignment
xx = 10
#---------------------
1   (call core.get_binding_type TestMod :xx)
2   (= slot₁/tmp 10)
3   slot₁/tmp
4   (call core.isa %₃ %₁)
5   (gotoifnot %₄ label₇)
6   (goto label₉)
7   slot₁/tmp
8   (= slot₁/tmp (call top.convert %₁ %₇))
9   slot₁/tmp
10  (= TestMod.xx %₉)
11  (return 10)

########################################
# Typed global assignment
global xx::T = 10
#---------------------
1   TestMod.T
2   (call core.set_binding_type! TestMod :xx %₁)
3   (global TestMod.xx)
4   (call core.get_binding_type TestMod :xx)
5   (= slot₁/tmp 10)
6   slot₁/tmp
7   (call core.isa %₆ %₄)
8   (gotoifnot %₇ label₁₀)
9   (goto label₁₂)
10  slot₁/tmp
11  (= slot₁/tmp (call top.convert %₄ %₁₀))
12  slot₁/tmp
13  (= TestMod.xx %₁₂)
14  (return 10)


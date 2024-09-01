########################################
# Local declaration with type
local x::T = 1
#---------------------
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
#---------------------
1   (const TestMod.xx)
2   (= TestMod.xx 10)
3   (return 10)

########################################
# Typed const
const xx::T = 10
#---------------------
1   core.set_binding_type!
2   TestMod.T
3   (call %₁ TestMod :xx %₂)
4   (const TestMod.xx)
5   core.get_binding_type
6   (call %₅ TestMod :xx)
7   (= slot₁/tmp 10)
8   core.isa
9   slot₁/tmp
10  (call %₈ %₉ %₆)
11  (gotoifnot %₁₀ label₁₃)
12  (goto label₁₆)
13  top.convert
14  slot₁/tmp
15  (= slot₁/tmp (call %₁₃ %₆ %₁₄))
16  slot₁/tmp
17  (= TestMod.xx %₁₆)
18  (return 10)

########################################
# Global assignment
xx = 10
#---------------------
1   core.get_binding_type
2   (call %₁ TestMod :xx)
3   (= slot₁/tmp 10)
4   core.isa
5   slot₁/tmp
6   (call %₄ %₅ %₂)
7   (gotoifnot %₆ label₉)
8   (goto label₁₂)
9   top.convert
10  slot₁/tmp
11  (= slot₁/tmp (call %₉ %₂ %₁₀))
12  slot₁/tmp
13  (= TestMod.xx %₁₂)
14  (return 10)

########################################
# Typed global assignment
global xx::T = 10
#---------------------
1   core.set_binding_type!
2   TestMod.T
3   (call %₁ TestMod :xx %₂)
4   (global TestMod.xx)
5   core.get_binding_type
6   (call %₅ TestMod :xx)
7   (= slot₁/tmp 10)
8   core.isa
9   slot₁/tmp
10  (call %₈ %₉ %₆)
11  (gotoifnot %₁₀ label₁₃)
12  (goto label₁₆)
13  top.convert
14  slot₁/tmp
15  (= slot₁/tmp (call %₁₃ %₆ %₁₄))
16  slot₁/tmp
17  (= TestMod.xx %₁₆)
18  (return 10)


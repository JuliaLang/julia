########################################
# Local declaration with type
local x::T = 1
#---------------------
1   (newvar slot₁/x)
2   1
3   (= slot₂/tmp %₂)
4   slot₂/tmp
5   TestMod.T
6   (call core.isa %₄ %₅)
7   (gotoifnot %₆ label₉)
8   (goto label₁₄)
9   TestMod.T
10  slot₂/tmp
11  (call top.convert %₉ %₁₀)
12  TestMod.T
13  (= slot₂/tmp (call core.typeassert %₁₁ %₁₂))
14  slot₂/tmp
15  (= slot₁/x %₁₄)
16  (return %₂)

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

########################################
# Error: x declared twice
begin
    local x::T = 1
    local x::S = 1
end
#---------------------
LoweringError:
begin
    local x::T = 1
    local x::S = 1
#         └──┘ ── multiple type declarations found for `x`
end

########################################
# Error: Const not supported on locals
const local x = 1
#---------------------
LoweringError:
const local x = 1
#           ╙ ── unsupported `const` declaration on local variable

########################################
# Error: Const not supported on locals
let
    const x = 1
end
#---------------------
LoweringError:
let
    const x = 1
#         ╙ ── unsupported `const` declaration on local variable
end

########################################
# Error: global type decls only allowed at top level
function f()
    global x::Int = 1
end
#---------------------
LoweringError:
function f()
    global x::Int = 1
#          └────┘ ── type declarations for global variables must be at top level, not inside a function
end


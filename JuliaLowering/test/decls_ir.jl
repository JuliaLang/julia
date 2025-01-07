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
# Type decl on function argument
function f(x)
    x::Int = 1
    x = 2.0
    x
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃ core.Any)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/x slot₃/tmp(!read) slot₄/tmp(!read)]
    1   1
    2   (= slot₃/tmp %₁)
    3   slot₃/tmp
    4   TestMod.Int
    5   (call core.isa %₃ %₄)
    6   (gotoifnot %₅ label₈)
    7   (goto label₁₃)
    8   TestMod.Int
    9   slot₃/tmp
    10  (call top.convert %₈ %₉)
    11  TestMod.Int
    12  (= slot₃/tmp (call core.typeassert %₁₀ %₁₁))
    13  slot₃/tmp
    14  (= slot₂/x %₁₃)
    15  2.0
    16  (= slot₄/tmp %₁₅)
    17  slot₄/tmp
    18  TestMod.Int
    19  (call core.isa %₁₇ %₁₈)
    20  (gotoifnot %₁₉ label₂₂)
    21  (goto label₂₇)
    22  TestMod.Int
    23  slot₄/tmp
    24  (call top.convert %₂₂ %₂₃)
    25  TestMod.Int
    26  (= slot₄/tmp (call core.typeassert %₂₄ %₂₅))
    27  slot₄/tmp
    28  (= slot₂/x %₂₇)
    29  slot₂/x
    30  (return %₂₉)
8   TestMod.f
9   (return %₈)

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


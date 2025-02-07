########################################
# Local declaration with type
local x::T = 1
#---------------------
1   (newvar slot₁/x)
2   1
3   TestMod.T
4   (= slot₂/tmp %₂)
5   slot₂/tmp
6   (call core.isa %₅ %₃)
7   (gotoifnot %₆ label₉)
8   (goto label₁₂)
9   slot₂/tmp
10  (call top.convert %₃ %₉)
11  (= slot₂/tmp (call core.typeassert %₁₀ %₃))
12  slot₂/tmp
13  (= slot₁/x %₁₂)
14  (return %₂)

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
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/x slot₃/tmp(!read) slot₄/tmp(!read)]
    1   1
    2   TestMod.Int
    3   (= slot₃/tmp %₁)
    4   slot₃/tmp
    5   (call core.isa %₄ %₂)
    6   (gotoifnot %₅ label₈)
    7   (goto label₁₁)
    8   slot₃/tmp
    9   (call top.convert %₂ %₈)
    10  (= slot₃/tmp (call core.typeassert %₉ %₂))
    11  slot₃/tmp
    12  (= slot₂/x %₁₁)
    13  2.0
    14  TestMod.Int
    15  (= slot₄/tmp %₁₃)
    16  slot₄/tmp
    17  (call core.isa %₁₆ %₁₄)
    18  (gotoifnot %₁₇ label₂₀)
    19  (goto label₂₃)
    20  slot₄/tmp
    21  (call top.convert %₁₄ %₂₀)
    22  (= slot₄/tmp (call core.typeassert %₂₁ %₁₄))
    23  slot₄/tmp
    24  (= slot₂/x %₂₃)
    25  slot₂/x
    26  (return %₂₅)
9   TestMod.f
10  (return %₉)

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


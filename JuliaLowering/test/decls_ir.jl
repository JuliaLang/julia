########################################
# Local declaration with type
begin
    local x::T = 1
end
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
# Error: Local declarations outside a scope are disallowed
# See https://github.com/JuliaLang/julia/issues/57483
local x
#---------------------
LoweringError:
local x
└─────┘ ── local declarations have no effect outside a scope

########################################
# Local declaration allowed in tail position
begin
    local x
end
#---------------------
1   (newvar slot₁/x)
2   (return core.nothing)

########################################
# Local declaration allowed in value position
# TODO: This may be a bug in flisp lowering - should we reconsider this?
let
    y = local x
end
#---------------------
1   (newvar slot₁/x)
2   core.nothing
3   (= slot₂/y %₂)
4   (return %₂)

########################################
# Global declaration allowed in tail position
global x
#---------------------
1   (global TestMod.x)
2   latestworld
3   (return core.nothing)

########################################
# Global declaration allowed in tail position, nested
begin
    global x
end
#---------------------
1   (global TestMod.x)
2   latestworld
3   (return core.nothing)

########################################
# Error: Global declaration not allowed in tail position in functions
function f()
    global x
end
#---------------------
LoweringError:
function f()
    global x
#          ╙ ── global declaration doesn't read the variable and can't return a value
end

########################################
# Error: Global declaration not allowed in value position
y = global x
#---------------------
LoweringError:
y = global x
#          ╙ ── global declaration doesn't read the variable and can't return a value

########################################
# const
const xx = 10
#---------------------
1   10
2   (constdecl TestMod.xx %₁)
3   latestworld
4   (return %₁)

########################################
# Typed const
const xx::T = 10
#---------------------
1   TestMod.T
2   (= slot₁/tmp 10)
3   slot₁/tmp
4   (call core.isa %₃ %₁)
5   (gotoifnot %₄ label₇)
6   (goto label₁₀)
7   slot₁/tmp
8   (call top.convert %₁ %₇)
9   (= slot₁/tmp (call core.typeassert %₈ %₁))
10  slot₁/tmp
11  (constdecl TestMod.xx %₁₀)
12  latestworld
13  (return %₁₀)

########################################
# Error: Const tuple
const xxx,xxxx,xxxxx = 10,20,30
#---------------------
LoweringError:
const xxx,xxxx,xxxxx = 10,20,30
#    └─────────────┘ ── Lowering TODO: `const` tuple assignment desugaring

########################################
# Const in chain: only first is const
const c0 = v0 = v1 = 123
#---------------------
1   123
2   (constdecl TestMod.c0 %₁)
3   latestworld
4   (globaldecl TestMod.v0)
5   latestworld
6   (call core.get_binding_type TestMod :v0)
7   (= slot₁/tmp %₁)
8   slot₁/tmp
9   (call core.isa %₈ %₆)
10  (gotoifnot %₉ label₁₂)
11  (goto label₁₄)
12  slot₁/tmp
13  (= slot₁/tmp (call top.convert %₆ %₁₂))
14  slot₁/tmp
15  (call core.setglobal! TestMod :v0 %₁₄)
16  (globaldecl TestMod.v1)
17  latestworld
18  (call core.get_binding_type TestMod :v1)
19  (= slot₂/tmp %₁)
20  slot₂/tmp
21  (call core.isa %₂₀ %₁₈)
22  (gotoifnot %₂₁ label₂₄)
23  (goto label₂₆)
24  slot₂/tmp
25  (= slot₂/tmp (call top.convert %₁₈ %₂₄))
26  slot₂/tmp
27  (call core.setglobal! TestMod :v1 %₂₆)
28  (return %₁)

########################################
# Global assignment
xx = 10
#---------------------
1   (globaldecl TestMod.xx)
2   latestworld
3   (call core.get_binding_type TestMod :xx)
4   (= slot₁/tmp 10)
5   slot₁/tmp
6   (call core.isa %₅ %₃)
7   (gotoifnot %₆ label₉)
8   (goto label₁₁)
9   slot₁/tmp
10  (= slot₁/tmp (call top.convert %₃ %₉))
11  slot₁/tmp
12  (call core.setglobal! TestMod :xx %₁₁)
13  (return 10)

########################################
# Typed global assignment
global xx::T = 10
#---------------------
1   (globaldecl TestMod.xx TestMod.T)
2   latestworld
3   (global TestMod.xx)
4   latestworld
5   (globaldecl TestMod.xx)
6   latestworld
7   (call core.get_binding_type TestMod :xx)
8   (= slot₁/tmp 10)
9   slot₁/tmp
10  (call core.isa %₉ %₇)
11  (gotoifnot %₁₀ label₁₃)
12  (goto label₁₅)
13  slot₁/tmp
14  (= slot₁/tmp (call top.convert %₇ %₁₃))
15  slot₁/tmp
16  (call core.setglobal! TestMod :xx %₁₅)
17  (return 10)

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
└───────────────┘ ── unsupported `const local` declaration

########################################
# Error: Const not supported on locals
let
    const x = 1
end
#---------------------
LoweringError:
let
    const x = 1
#        └────┘ ── unsupported `const` declaration on local variable
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
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
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
10  latestworld
11  TestMod.f
12  (return %₁₁)

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


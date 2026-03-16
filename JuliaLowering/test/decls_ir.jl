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
5   (call core.isa slot₂/tmp %₃)
6   (gotoifnot %₅ label₈)
7   (goto label₁₀)
8   (call top.convert %₃ slot₂/tmp)
9   (= slot₂/tmp (call core.typeassert %₈ %₃))
10  slot₂/tmp
11  (= slot₁/x %₁₀)
12  (return %₂)

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
1   (call core.declare_global TestMod :x false)
2   latestworld
3   (return core.nothing)

########################################
# Global declaration allowed in tail position, nested
begin
    global x
end
#---------------------
1   (call core.declare_global TestMod :x false)
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
2   (call core.declare_const TestMod :xx %₁)
3   latestworld
4   (return %₁)

########################################
# Typed const
const xx::T = 10
#---------------------
1   TestMod.T
2   (= slot₁/tmp 10)
3   (call core.isa slot₁/tmp %₁)
4   (gotoifnot %₃ label₆)
5   (goto label₈)
6   (call top.convert %₁ slot₁/tmp)
7   (= slot₁/tmp (call core.typeassert %₆ %₁))
8   slot₁/tmp
9   (call core.declare_const TestMod :xx %₈)
10  latestworld
11  (return %₈)

########################################
# Const tuple
const xxx,xxxx,xxxxx = 10,20,30
#---------------------
1   10
2   (call core.declare_const TestMod :xxx %₁)
3   latestworld
4   20
5   (call core.declare_const TestMod :xxxx %₄)
6   latestworld
7   30
8   (call core.declare_const TestMod :xxxxx %₇)
9   latestworld
10  (call core.tuple 10 20 30)
11  (return %₁₀)

########################################
# Const in chain: only first is const
const c0 = v0 = v1 = 123
#---------------------
1   123
2   (call core.declare_const TestMod :c0 %₁)
3   latestworld
4   (call core.declare_global TestMod :v0 true)
5   latestworld
6   (call core.get_binding_type TestMod :v0)
7   (= slot₁/tmp %₁)
8   (call core.isa slot₁/tmp %₆)
9   (gotoifnot %₈ label₁₁)
10  (goto label₁₂)
11  (= slot₁/tmp (call top.convert %₆ slot₁/tmp))
12  slot₁/tmp
13  (call core.setglobal! TestMod :v0 %₁₂)
14  (call core.declare_global TestMod :v1 true)
15  latestworld
16  (call core.get_binding_type TestMod :v1)
17  (= slot₂/tmp %₁)
18  (call core.isa slot₂/tmp %₁₆)
19  (gotoifnot %₁₈ label₂₁)
20  (goto label₂₂)
21  (= slot₂/tmp (call top.convert %₁₆ slot₂/tmp))
22  slot₂/tmp
23  (call core.setglobal! TestMod :v1 %₂₂)
24  (return %₁)

########################################
# Global assignment
xx = 10
#---------------------
1   (call core.declare_global TestMod :xx true)
2   latestworld
3   (call core.get_binding_type TestMod :xx)
4   (= slot₁/tmp 10)
5   (call core.isa slot₁/tmp %₃)
6   (gotoifnot %₅ label₈)
7   (goto label₉)
8   (= slot₁/tmp (call top.convert %₃ slot₁/tmp))
9   slot₁/tmp
10  (call core.setglobal! TestMod :xx %₉)
11  (return 10)

########################################
# Typed global assignment
global xx::T = 10
#---------------------
1   (call core.declare_global TestMod :xx false)
2   latestworld
3   TestMod.T
4   (call core.declare_global TestMod :xx true %₃)
5   latestworld
6   (call core.declare_global TestMod :xx true)
7   latestworld
8   (call core.get_binding_type TestMod :xx)
9   (= slot₁/tmp 10)
10  (call core.isa slot₁/tmp %₈)
11  (gotoifnot %₁₀ label₁₃)
12  (goto label₁₄)
13  (= slot₁/tmp (call top.convert %₈ slot₁/tmp))
14  slot₁/tmp
15  (call core.setglobal! TestMod :xx %₁₄)
16  (return 10)

########################################
# Error: local with two type declarations
begin
    local x::T = 1
    local x::S = 1
end
#---------------------
LoweringError:
begin
    local x::T = 1
    local x::S = 1
#        └───────┘ ── multiple type declarations found for `x`
end

########################################
# Error: local with two type declarations, requiring scope resolution
begin
    local x::Int = 1
    x::Int = 1
end
#---------------------
LoweringError:
begin
    local x::Int = 1
    x::Int = 1
#   └────────┘ ── multiple type declarations found for `x`
end

########################################
# multiple type declarations is OK for globals
begin
    global x::Int
    x::Int = 1
end
#---------------------
1   TestMod.Int
2   (call core.declare_global TestMod :x true %₁)
3   latestworld
4   (call core.declare_global TestMod :x false)
5   latestworld
6   TestMod.Int
7   (call core.declare_global TestMod :x true %₆)
8   latestworld
9   (call core.declare_global TestMod :x true)
10  latestworld
11  (call core.get_binding_type TestMod :x)
12  (= slot₁/tmp 1)
13  (call core.isa slot₁/tmp %₁₁)
14  (gotoifnot %₁₃ label₁₆)
15  (goto label₁₇)
16  (= slot₁/tmp (call top.convert %₁₁ slot₁/tmp))
17  slot₁/tmp
18  (call core.setglobal! TestMod :x %₁₇)
19  (return 1)

########################################
# Error: Const not supported on locals
const local x = 1
#---------------------
LoweringError:
const local x = 1
#    └──────────┘ ── unsupported `const local` declaration

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
# Error: Const not supported in function scope
function (); global g; const g = 1; end
#---------------------
LoweringError:
function (); global g; const g = 1; end
#                      └─────────┘ ── unsupported `const` inside function

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
    slots: [slot₁/#self#(!read) slot₂/x slot₃/tmp(!read) slot₄/tmp(!read) slot₅/x(!read)]
    1   (= slot₅/x slot₂/x)
    2   1
    3   TestMod.Int
    4   (= slot₃/tmp %₂)
    5   (call core.isa slot₃/tmp %₃)
    6   (gotoifnot %₅ label₈)
    7   (goto label₁₀)
    8   (call top.convert %₃ slot₃/tmp)
    9   (= slot₃/tmp (call core.typeassert %₈ %₃))
    10  slot₃/tmp
    11  (= slot₅/x %₁₀)
    12  2.0
    13  TestMod.Int
    14  (= slot₄/tmp %₁₂)
    15  (call core.isa slot₄/tmp %₁₃)
    16  (gotoifnot %₁₅ label₁₈)
    17  (goto label₂₀)
    18  (call top.convert %₁₃ slot₄/tmp)
    19  (= slot₄/tmp (call core.typeassert %₁₈ %₁₃))
    20  slot₄/tmp
    21  (= slot₅/x %₂₀)
    22  slot₅/x
    23  (return %₂₂)
10  latestworld
11  TestMod.f
12  (return %₁₁)

########################################
# Error: global type decls only allowed at top level
function f()
    global x::Int
end
#---------------------
LoweringError:
function f()
    global x::Int
#          └────┘ ── type declarations for global variables must be at top level, not inside a function
end

########################################
# Error: global type decls only allowed at top level (=)
function f()
    global x::Int = 1
end
#---------------------
LoweringError:
function f()
    global x::Int = 1
#         └─────────┘ ── type declarations for global variables must be at top level, not inside a function
end

########################################
# Error: global type decls only allowed at top level, requiring scope resolution
function f()
    global x
    x::Int = 1
end
#---------------------
LoweringError:
function f()
    global x
    x::Int = 1
#   └────────┘ ── type declarations for global variables must be at top level, not inside a function
end

########################################
# FIXME: Error: global type decls only allowed at top level (.=)
function f()
    global x::Int .= 1
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   (call core.declare_global TestMod :x false)
4   latestworld
5   TestMod.f
6   (call core.Typeof %₅)
7   (call core.svec %₆)
8   (call core.svec)
9   SourceLocation::1:10
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read)]
    1   TestMod.x
    2   TestMod.Int
    3   (call core.typeassert %₁ %₂)
    4   (call top.broadcasted top.identity 1)
    5   (call top.materialize! %₃ %₄)
    6   (return %₅)
12  latestworld
13  TestMod.f
14  (return %₁₃)

########################################
# FIXME: Error: global type decls only allowed at top level (+=)
function f()
    global x::Int += 1
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   (call core.declare_global TestMod :x false)
4   latestworld
5   (call core.declare_global TestMod :x true)
6   latestworld
7   TestMod.f
8   (call core.Typeof %₇)
9   (call core.svec %₈)
10  (call core.svec)
11  SourceLocation::1:10
12  (call core.svec %₉ %₁₀ %₁₁)
13  --- method core.nothing %₁₂
    slots: [slot₁/#self#(!read) slot₂/tmp(!read)]
    1   TestMod.+
    2   TestMod.x
    3   TestMod.Int
    4   (call core.typeassert %₂ %₃)
    5   (call %₁ %₄ 1)
    6   (call core.get_binding_type TestMod :x)
    7   (= slot₂/tmp %₅)
    8   (call core.isa slot₂/tmp %₆)
    9   (gotoifnot %₈ label₁₁)
    10  (goto label₁₂)
    11  (= slot₂/tmp (call top.convert %₆ slot₂/tmp))
    12  slot₂/tmp
    13  (call core.setglobal! TestMod :x %₁₂)
    14  (return %₅)
14  latestworld
15  TestMod.f
16  (return %₁₅)

########################################
# FIXME: Error: global type decls only allowed at top level (.+=)
function f()
    global x::Int .+= 1
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   (call core.declare_global TestMod :x false)
4   latestworld
5   TestMod.f
6   (call core.Typeof %₅)
7   (call core.svec %₆)
8   (call core.svec)
9   SourceLocation::1:10
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read)]
    1   TestMod.x
    2   TestMod.+
    3   TestMod.Int
    4   (call core.typeassert %₁ %₃)
    5   (call top.broadcasted %₂ %₄ 1)
    6   (call top.materialize! %₁ %₅)
    7   (return %₆)
12  latestworld
13  TestMod.f
14  (return %₁₃)

########################################
# Error: global type decls only allowed at top level (tuple)
function f()
    global (x::Int, y) = 1,2
end
#---------------------
LoweringError:
function f()
    global (x::Int, y) = 1,2
#         └────────────────┘ ── type declarations for global variables must be at top level, not inside a function
end

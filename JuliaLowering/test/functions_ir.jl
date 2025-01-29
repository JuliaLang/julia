########################################
# Function declaration with no methods
function f
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (return %₂)

########################################
# Functions with placeholder arg
function f(x, _, y)
    x + y
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃ core.Any core.Any core.Any)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/x slot₃/_(!read) slot₄/y]
    1   TestMod.+
    2   (call %₁ slot₂/x slot₄/y)
    3   (return %₂)
9   TestMod.f
10  (return %₉)

########################################
# Functions with argument types only, no name
function f(::T, x)
    x
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   TestMod.T
5   (call core.svec %₃ %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/_(!read) slot₃/x]
    1   slot₃/x
    2   (return %₁)
10  TestMod.f
11  (return %₁₀)

########################################
# Functions argument types
function f(x, y::T)
    body
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   TestMod.T
5   (call core.svec %₃ core.Any %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/y(!read)]
    1   TestMod.body
    2   (return %₁)
10  TestMod.f
11  (return %₁₀)

########################################
# Functions with slurp of Any
function f(x, ys...)
    body
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.apply_type core.Vararg core.Any)
5   (call core.svec %₃ core.Any %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/ys(!read)]
    1   TestMod.body
    2   (return %₁)
10  TestMod.f
11  (return %₁₀)

########################################
# Functions with slurp of T
function f(x, ys::T...)
    body
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   TestMod.T
5   (call core.apply_type core.Vararg %₄)
6   (call core.svec %₃ core.Any %₅)
7   (call core.svec)
8   SourceLocation::1:10
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/ys(!read)]
    1   TestMod.body
    2   (return %₁)
11  TestMod.f
12  (return %₁₁)

########################################
# Error: Function with slurp not in last position arg
function f(xs..., y)
    body
end
#---------------------
LoweringError:
function f(xs..., y)
#          └───┘ ── `...` may only be used for the last function argument
    body
end

########################################
# Basic static parameters
function f(::T, ::U, ::V) where T where {U,V}
    (T,U,V)
end
#---------------------
1   (method TestMod.f)
2   (= slot₂/U (call core.TypeVar :U))
3   (= slot₃/V (call core.TypeVar :V))
4   (= slot₁/T (call core.TypeVar :T))
5   TestMod.f
6   (call core.Typeof %₅)
7   slot₁/T
8   slot₂/U
9   slot₃/V
10  (call core.svec %₆ %₇ %₈ %₉)
11  slot₂/U
12  slot₃/V
13  slot₁/T
14  (call core.svec %₁₁ %₁₂ %₁₃)
15  SourceLocation::1:10
16  (call core.svec %₁₀ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(!read) slot₂/_(!read) slot₃/_(!read) slot₄/_(!read)]
    1   static_parameter₃
    2   static_parameter₁
    3   static_parameter₂
    4   (call core.tuple %₁ %₂ %₃)
    5   (return %₄)
18  TestMod.f
19  (return %₁₈)

########################################
# Static parameter with bounds and used with apply_type in argument
function f(::S{T}) where X <: T <: Y
    T
end
#---------------------
1   (method TestMod.f)
2   TestMod.X
3   TestMod.Y
4   (= slot₁/T (call core.TypeVar :T %₂ %₃))
5   TestMod.f
6   (call core.Typeof %₅)
7   TestMod.S
8   slot₁/T
9   (call core.apply_type %₇ %₈)
10  (call core.svec %₆ %₉)
11  slot₁/T
12  (call core.svec %₁₁)
13  SourceLocation::1:10
14  (call core.svec %₁₀ %₁₂ %₁₃)
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(!read) slot₂/_(!read)]
    1   static_parameter₁
    2   (return %₁)
16  TestMod.f
17  (return %₁₆)

########################################
# Return types
function f(x)::Int
    if x
        42.0
    end
    0xff
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
    slots: [slot₁/#self#(!read) slot₂/x slot₃/tmp(!read)]
    1   TestMod.Int
    2   (gotoifnot slot₂/x label₃)
    3   (= slot₃/tmp 0xff)
    4   slot₃/tmp
    5   (call core.isa %₄ %₁)
    6   (gotoifnot %₅ label₈)
    7   (goto label₁₁)
    8   slot₃/tmp
    9   (call top.convert %₁ %₈)
    10  (= slot₃/tmp (call core.typeassert %₉ %₁))
    11  slot₃/tmp
    12  (return %₁₁)
9   TestMod.f
10  (return %₉)

########################################
# Callable type
function (::T)(x)
    x
end
#---------------------
1   TestMod.T
2   (call core.svec %₁ core.Any)
3   (call core.svec)
4   SourceLocation::1:10
5   (call core.svec %₂ %₃ %₄)
6   --- method core.nothing %₅
    slots: [slot₁/#self#(!read) slot₂/x]
    1   slot₂/x
    2   (return %₁)
7   (return core.nothing)

########################################
# Callable type with instance
function (y::T)(x)
    (y, x)
end
#---------------------
1   TestMod.T
2   (call core.svec %₁ core.Any)
3   (call core.svec)
4   SourceLocation::1:10
5   (call core.svec %₂ %₃ %₄)
6   --- method core.nothing %₅
    slots: [slot₁/y slot₂/x]
    1   (call core.tuple slot₁/y slot₂/x)
    2   (return %₁)
7   (return core.nothing)

########################################
# `where` params used in callable object type
function (x::X1{T})() where T
    T
end
#---------------------
1   (= slot₁/T (call core.TypeVar :T))
2   TestMod.X1
3   slot₁/T
4   (call core.apply_type %₂ %₃)
5   (call core.svec %₄)
6   slot₁/T
7   (call core.svec %₆)
8   SourceLocation::1:10
9   (call core.svec %₅ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/x(!read)]
    1   static_parameter₁
    2   (return %₁)
11  (return core.nothing)

########################################
# Function with module ref in name
function A.f()
end
#---------------------
1   TestMod.A
2   (call top.getproperty %₁ :f)
3   (call core.Typeof %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read)]
    1   (return core.nothing)
9   (return core.nothing)

########################################
# Error: Invalid function name ccall
function ccall()
end
#---------------------
LoweringError:
function ccall()
#        └───┘ ── Invalid function name
end

########################################
# Error: Invalid function name ccall
function A.ccall()
end
#---------------------
LoweringError:
function A.ccall()
#        └─────┘ ── Invalid function name
end

########################################
# Error: Invalid dotop function name
function (.+)(x,y)
end
#---------------------
LoweringError:
function (.+)(x,y)
#        └───────┘ ── Cannot define function using `.` broadcast syntax
end

########################################
# Error: Invalid function name
function f[](x,y)
end
#---------------------
LoweringError:
function f[](x,y)
#        └─┘ ── Invalid function name
end

########################################
# Simple positional args with defaults
function f(x::T, y::S=1, z::U=2)
    (x,y)
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   TestMod.T
5   (call core.svec %₃ %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call slot₁/#self# slot₂/x 1 2)
    2   (return %₁)
10  TestMod.f
11  (call core.Typeof %₁₀)
12  TestMod.T
13  TestMod.S
14  (call core.svec %₁₁ %₁₂ %₁₃)
15  (call core.svec)
16  SourceLocation::1:10
17  (call core.svec %₁₄ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
    slots: [slot₁/#self#(called) slot₂/x slot₃/y]
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
19  TestMod.f
20  (call core.Typeof %₁₉)
21  TestMod.T
22  TestMod.S
23  TestMod.U
24  (call core.svec %₂₀ %₂₁ %₂₂ %₂₃)
25  (call core.svec)
26  SourceLocation::1:10
27  (call core.svec %₂₄ %₂₅ %₂₆)
28  --- method core.nothing %₂₇
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y slot₄/z(!read)]
    1   (call core.tuple slot₂/x slot₃/y)
    2   (return %₁)
29  TestMod.f
30  (return %₂₉)

########################################
# Default positional args which depend on other args
function f(x=1, y=x)
    (x,y)
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(called)]
    1   (call slot₁/#self# 1)
    2   (return %₁)
9   TestMod.f
10  (call core.Typeof %₉)
11  (call core.svec %₁₀ core.Any)
12  (call core.svec)
13  SourceLocation::1:10
14  (call core.svec %₁₁ %₁₂ %₁₃)
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call slot₁/#self# slot₂/x slot₂/x)
    2   (return %₁)
16  TestMod.f
17  (call core.Typeof %₁₆)
18  (call core.svec %₁₇ core.Any core.Any)
19  (call core.svec)
20  SourceLocation::1:10
21  (call core.svec %₁₈ %₁₉ %₂₀)
22  --- method core.nothing %₂₁
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y]
    1   (call core.tuple slot₂/x slot₃/y)
    2   (return %₁)
23  TestMod.f
24  (return %₂₃)

########################################
# Default positional args with missing arg names (implicit placeholders)
function f(::Int, y=1, z=2)
    (y, z)
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   TestMod.Int
5   (call core.svec %₃ %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(called) slot₂/_]
    1   (call slot₁/#self# slot₂/_ 1 2)
    2   (return %₁)
10  TestMod.f
11  (call core.Typeof %₁₀)
12  TestMod.Int
13  (call core.svec %₁₁ %₁₂ core.Any)
14  (call core.svec)
15  SourceLocation::1:10
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(called) slot₂/_ slot₃/y]
    1   (call slot₁/#self# slot₂/_ slot₃/y 2)
    2   (return %₁)
18  TestMod.f
19  (call core.Typeof %₁₈)
20  TestMod.Int
21  (call core.svec %₁₉ %₂₀ core.Any core.Any)
22  (call core.svec)
23  SourceLocation::1:10
24  (call core.svec %₂₁ %₂₂ %₂₃)
25  --- method core.nothing %₂₄
    slots: [slot₁/#self#(!read) slot₂/_(!read) slot₃/y slot₄/z]
    1   (call core.tuple slot₃/y slot₄/z)
    2   (return %₁)
26  TestMod.f
27  (return %₂₆)

########################################
# Default positional args with placeholders
function f(_::Int, x=1)
    x
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   TestMod.Int
5   (call core.svec %₃ %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(called) slot₂/_]
    1   (call slot₁/#self# slot₂/_ 1)
    2   (return %₁)
10  TestMod.f
11  (call core.Typeof %₁₀)
12  TestMod.Int
13  (call core.svec %₁₁ %₁₂ core.Any)
14  (call core.svec)
15  SourceLocation::1:10
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(!read) slot₂/_(!read) slot₃/x]
    1   slot₃/x
    2   (return %₁)
18  TestMod.f
19  (return %₁₈)

########################################
# Positional args with defaults and `where` clauses
function f(x::T, y::S=1, z::U=2) where {T,S<:T,U<:S}
    (x,y,z)
end
#---------------------
1   (method TestMod.f)
2   (= slot₂/T (call core.TypeVar :T))
3   slot₂/T
4   (= slot₁/S (call core.TypeVar :S %₃))
5   slot₁/S
6   (= slot₃/U (call core.TypeVar :U %₅))
7   TestMod.f
8   (call core.Typeof %₇)
9   slot₂/T
10  (call core.svec %₈ %₉)
11  slot₂/T
12  (call core.svec %₁₁)
13  SourceLocation::1:10
14  (call core.svec %₁₀ %₁₂ %₁₃)
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call slot₁/#self# slot₂/x 1 2)
    2   (return %₁)
16  TestMod.f
17  (call core.Typeof %₁₆)
18  slot₂/T
19  slot₁/S
20  (call core.svec %₁₇ %₁₈ %₁₉)
21  slot₂/T
22  slot₁/S
23  (call core.svec %₂₁ %₂₂)
24  SourceLocation::1:10
25  (call core.svec %₂₀ %₂₃ %₂₄)
26  --- method core.nothing %₂₅
    slots: [slot₁/#self#(called) slot₂/x slot₃/y]
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
27  TestMod.f
28  (call core.Typeof %₂₇)
29  slot₂/T
30  slot₁/S
31  slot₃/U
32  (call core.svec %₂₈ %₂₉ %₃₀ %₃₁)
33  slot₂/T
34  slot₁/S
35  slot₃/U
36  (call core.svec %₃₃ %₃₄ %₃₅)
37  SourceLocation::1:10
38  (call core.svec %₃₂ %₃₆ %₃₇)
39  --- method core.nothing %₃₈
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y slot₄/z]
    1   (call core.tuple slot₂/x slot₃/y slot₄/z)
    2   (return %₁)
40  TestMod.f
41  (return %₄₀)

########################################
# Positional args and type parameters with transitive dependencies
# See https://github.com/JuliaLang/julia/issues/49275 - the first method
# generated here for only `x` should contain zero type parameters.
function f(x, y::S=[1], z::U=2) where {T, S<:AbstractVector{T}, U}
    (x, y, z, T, S, U)
end
#---------------------
1   (method TestMod.f)
2   (= slot₂/T (call core.TypeVar :T))
3   TestMod.AbstractVector
4   slot₂/T
5   (call core.apply_type %₃ %₄)
6   (= slot₁/S (call core.TypeVar :S %₅))
7   (= slot₃/U (call core.TypeVar :U))
8   TestMod.f
9   (call core.Typeof %₈)
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:10
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call top.vect 1)
    2   (call slot₁/#self# slot₂/x %₁ 2)
    3   (return %₂)
15  TestMod.f
16  (call core.Typeof %₁₅)
17  slot₁/S
18  (call core.svec %₁₆ core.Any %₁₇)
19  slot₂/T
20  slot₁/S
21  (call core.svec %₁₉ %₂₀)
22  SourceLocation::1:10
23  (call core.svec %₁₈ %₂₁ %₂₂)
24  --- method core.nothing %₂₃
    slots: [slot₁/#self#(called) slot₂/x slot₃/y]
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
25  TestMod.f
26  (call core.Typeof %₂₅)
27  slot₁/S
28  slot₃/U
29  (call core.svec %₂₆ core.Any %₂₇ %₂₈)
30  slot₂/T
31  slot₁/S
32  slot₃/U
33  (call core.svec %₃₀ %₃₁ %₃₂)
34  SourceLocation::1:10
35  (call core.svec %₂₉ %₃₃ %₃₄)
36  --- method core.nothing %₃₅
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y slot₄/z]
    1   static_parameter₁
    2   static_parameter₂
    3   static_parameter₃
    4   (call core.tuple slot₂/x slot₃/y slot₄/z %₁ %₂ %₃)
    5   (return %₄)
37  TestMod.f
38  (return %₃₇)

########################################
# Default positional args are allowed before trailing slurp with no default
function f(x=1, ys...)
    ys
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(called)]
    1   (call slot₁/#self# 1)
    2   (return %₁)
9   TestMod.f
10  (call core.Typeof %₉)
11  (call core.apply_type core.Vararg core.Any)
12  (call core.svec %₁₀ core.Any %₁₁)
13  (call core.svec)
14  SourceLocation::1:10
15  (call core.svec %₁₂ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/ys]
    1   slot₃/ys
    2   (return %₁)
17  TestMod.f
18  (return %₁₇)

########################################
# Error: Default positional args after a slurp
function f(x=1, ys..., z=2)
    ys
end
#---------------------
LoweringError:
function f(x=1, ys..., z=2)
#              └────┘ ── `...` may only be used for the last function argument
    ys
end

########################################
# Positional arg with slurp and default
function f(xs...=1)
    xs
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(called)]
    1   (call slot₁/#self# 1)
    2   (return %₁)
9   TestMod.f
10  (call core.Typeof %₉)
11  (call core.apply_type core.Vararg core.Any)
12  (call core.svec %₁₀ %₁₁)
13  (call core.svec)
14  SourceLocation::1:10
15  (call core.svec %₁₂ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/xs]
    1   slot₂/xs
    2   (return %₁)
17  TestMod.f
18  (return %₁₇)

########################################
# Positional arg with slurp and splatted default value
function f(xs...=(1,2)...)
    xs
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#]
    1   (call core.tuple 1 2)
    2   (call core._apply_iterate top.iterate slot₁/#self# %₁)
    3   (return %₂)
9   TestMod.f
10  (call core.Typeof %₉)
11  (call core.apply_type core.Vararg core.Any)
12  (call core.svec %₁₀ %₁₁)
13  (call core.svec)
14  SourceLocation::1:10
15  (call core.svec %₁₂ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/xs]
    1   slot₂/xs
    2   (return %₁)
17  TestMod.f
18  (return %₁₇)

########################################
# Trivial function argument destructuring
function f(x, (y,z), w)
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃ core.Any core.Any core.Any)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/destructured_arg slot₄/w(!read) slot₅/iterstate slot₆/y(!read) slot₇/z(!read)]
    1   (call top.indexed_iterate slot₃/destructured_arg 1)
    2   (= slot₆/y (call core.getfield %₁ 1))
    3   (= slot₅/iterstate (call core.getfield %₁ 2))
    4   slot₅/iterstate
    5   (call top.indexed_iterate slot₃/destructured_arg 2 %₄)
    6   (= slot₇/z (call core.getfield %₅ 1))
    7   (return core.nothing)
9   TestMod.f
10  (return %₉)

########################################
# Function argument destructuring combined with splats, types and and defaults
function f((x,)::T...=rhs)
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(called)]
    1   TestMod.rhs
    2   (call slot₁/#self# %₁)
    3   (return %₂)
9   TestMod.f
10  (call core.Typeof %₉)
11  TestMod.T
12  (call core.apply_type core.Vararg %₁₁)
13  (call core.svec %₁₀ %₁₂)
14  (call core.svec)
15  SourceLocation::1:10
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(!read) slot₂/destructured_arg slot₃/x(!read)]
    1   (call top.indexed_iterate slot₂/destructured_arg 1)
    2   (= slot₃/x (call core.getfield %₁ 1))
    3   (return core.nothing)
18  TestMod.f
19  (return %₁₈)

########################################
# Function argument destructuring combined with splats, types and and defaults
function f(x=default_x)::T
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(called)]
    1   TestMod.default_x
    2   (call slot₁/#self# %₁)
    3   (return %₂)
9   TestMod.f
10  (call core.Typeof %₉)
11  (call core.svec %₁₀ core.Any)
12  (call core.svec)
13  SourceLocation::1:10
14  (call core.svec %₁₁ %₁₂ %₁₃)
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/tmp(!read)]
    1   TestMod.T
    2   (= slot₃/tmp core.nothing)
    3   slot₃/tmp
    4   (call core.isa %₃ %₁)
    5   (gotoifnot %₄ label₇)
    6   (goto label₁₀)
    7   slot₃/tmp
    8   (call top.convert %₁ %₇)
    9   (= slot₃/tmp (call core.typeassert %₈ %₁))
    10  slot₃/tmp
    11  (return %₁₀)
16  TestMod.f
17  (return %₁₆)

########################################
# Duplicate destructured placeholders ok
function f((_,), (_,))
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃ core.Any core.Any)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/destructured_arg slot₃/destructured_arg]
    1   (call top.indexed_iterate slot₂/destructured_arg 1)
    2   (call core.getfield %₁ 1)
    3   (call top.indexed_iterate slot₃/destructured_arg 1)
    4   (call core.getfield %₃ 1)
    5   (return core.nothing)
9   TestMod.f
10  (return %₉)

########################################
# Slot flags
function f(@nospecialize(x), g, y)
    g() + y
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃ core.Any core.Any core.Any)
5   (call core.svec)
6   SourceLocation::1:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/x(nospecialize,!read) slot₃/g(called) slot₄/y]
    1   TestMod.+
    2   (call slot₃/g)
    3   (call %₁ %₂ slot₄/y)
    4   (return %₃)
9   TestMod.f
10  (return %₉)

########################################
# Binding docs to functions
"""
some docs
"""
function f()
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃)
5   (call core.svec)
6   SourceLocation::4:10
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read)]
    1   (return core.nothing)
9   TestMod.f
10  (call JuliaLowering.bind_docs! %₉ "some docs\n" %₇)
11  TestMod.f
12  (return %₁₁)

########################################
# Binding docs to callable type
"""
some docs
"""
function (x::T)()
end
#---------------------
1   TestMod.T
2   (call core.svec %₁)
3   (call core.svec)
4   SourceLocation::4:10
5   (call core.svec %₂ %₃ %₄)
6   --- method core.nothing %₅
    slots: [slot₁/x(!read)]
    1   (return core.nothing)
7   TestMod.T
8   (call JuliaLowering.bind_docs! %₇ "some docs\n" %₅)
9   (return core.nothing)

########################################
# Keyword function with defaults.
# Order of methods
# 1. #f_kw_simple#0(x, y, ::typeof(f_kw_simple), a, b)  (body)
# 2. Core.kwcall(kws, ::typeof(f_kw_simple))
# 3. Core.kwcall(kws, ::typeof(f_kw_simple), a)
# 4. Core.kwcall(kws, ::typeof(f_kw_simple), a, b)      (kwcall body)
# 5. f_kw_simple()
# 6. f_kw_simple(a)
# 7. f_kw_simple(a, b)
function f_kw_simple(a::Int=1, b::Float64=1.0; x::Char='a', y::Bool=true)
    (a, b, x, y)
end
#---------------------
1   (method TestMod.#f_kw_simple#0)
2   (method TestMod.f_kw_simple)
3   TestMod.#f_kw_simple#0
4   (call core.Typeof %₃)
5   TestMod.Char
6   TestMod.Bool
7   TestMod.f_kw_simple
8   (call core.Typeof %₇)
9   TestMod.Int
10  TestMod.Float64
11  (call core.svec %₄ %₅ %₆ %₈ %₉ %₁₀)
12  (call core.svec)
13  SourceLocation::1:10
14  (call core.svec %₁₁ %₁₂ %₁₃)
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y slot₄/#self#(!read) slot₅/a slot₆/b]
    1   (call core.tuple slot₅/a slot₆/b slot₂/x slot₃/y)
    2   (return %₁)
16  (call core.typeof core.kwcall)
17  TestMod.f_kw_simple
18  (call core.Typeof %₁₇)
19  (call core.svec %₁₆ core.NamedTuple %₁₈)
20  (call core.svec)
21  SourceLocation::1:10
22  (call core.svec %₁₉ %₂₀ %₂₁)
23  --- method core.nothing %₂₂
    slots: [slot₁/#self#(called) slot₂/kws slot₃/#self#]
    1   (call slot₁/#self# slot₂/kws slot₃/#self# 1 1.0)
    2   (return %₁)
24  (call core.typeof core.kwcall)
25  TestMod.f_kw_simple
26  (call core.Typeof %₂₅)
27  TestMod.Int
28  (call core.svec %₂₄ core.NamedTuple %₂₆ %₂₇)
29  (call core.svec)
30  SourceLocation::1:10
31  (call core.svec %₂₈ %₂₉ %₃₀)
32  --- method core.nothing %₃₁
    slots: [slot₁/#self#(called) slot₂/kws slot₃/#self# slot₄/a]
    1   (call slot₁/#self# slot₂/kws slot₃/#self# slot₄/a 1.0)
    2   (return %₁)
33  (call core.typeof core.kwcall)
34  TestMod.f_kw_simple
35  (call core.Typeof %₃₄)
36  TestMod.Int
37  TestMod.Float64
38  (call core.svec %₃₃ core.NamedTuple %₃₅ %₃₆ %₃₇)
39  (call core.svec)
40  SourceLocation::1:10
41  (call core.svec %₃₈ %₃₉ %₄₀)
42  --- method core.nothing %₄₁
    slots: [slot₁/#self#(!read) slot₂/kws slot₃/#self# slot₄/a slot₅/b slot₆/if_val(!read) slot₇/if_val(!read)]
    1   (call core.isdefined slot₂/kws :x)
    2   (gotoifnot %₁ label₁₃)
    3   (call core.getfield slot₂/kws :x)
    4   TestMod.Char
    5   (call core.isa %₃ %₄)
    6   (gotoifnot %₅ label₈)
    7   (goto label₁₁)
    8   TestMod.Char
    9   (new core.TypeError :keyword argument :x %₈ %₃)
    10  (call core.throw %₉)
    11  (= slot₆/if_val %₃)
    12  (goto label₁₄)
    13  (= slot₆/if_val 'a')
    14  slot₆/if_val
    15  (call core.isdefined slot₂/kws :y)
    16  (gotoifnot %₁₅ label₂₇)
    17  (call core.getfield slot₂/kws :y)
    18  TestMod.Bool
    19  (call core.isa %₁₇ %₁₈)
    20  (gotoifnot %₁₉ label₂₂)
    21  (goto label₂₅)
    22  TestMod.Bool
    23  (new core.TypeError :keyword argument :y %₂₂ %₁₇)
    24  (call core.throw %₂₃)
    25  (= slot₇/if_val %₁₇)
    26  (goto label₂₈)
    27  (= slot₇/if_val true)
    28  slot₇/if_val
    29  (call top.keys slot₂/kws)
    30  (call core.tuple :x :y)
    31  (call top.diff_names %₂₉ %₃₀)
    32  (call top.isempty %₃₁)
    33  (gotoifnot %₃₂ label₃₅)
    34  (goto label₃₆)
    35  (call top.kwerr slot₂/kws slot₃/#self# slot₄/a slot₅/b)
    36  TestMod.#f_kw_simple#0
    37  (call %₃₆ %₁₄ %₂₈ slot₃/#self# slot₄/a slot₅/b)
    38  (return %₃₇)
43  TestMod.f_kw_simple
44  (call core.Typeof %₄₃)
45  (call core.svec %₄₄)
46  (call core.svec)
47  SourceLocation::1:10
48  (call core.svec %₄₅ %₄₆ %₄₇)
49  --- method core.nothing %₄₈
    slots: [slot₁/#self#(called)]
    1   (call slot₁/#self# 1 1.0)
    2   (return %₁)
50  TestMod.f_kw_simple
51  (call core.Typeof %₅₀)
52  TestMod.Int
53  (call core.svec %₅₁ %₅₂)
54  (call core.svec)
55  SourceLocation::1:10
56  (call core.svec %₅₃ %₅₄ %₅₅)
57  --- method core.nothing %₅₆
    slots: [slot₁/#self#(called) slot₂/a]
    1   (call slot₁/#self# slot₂/a 1.0)
    2   (return %₁)
58  TestMod.f_kw_simple
59  (call core.Typeof %₅₈)
60  TestMod.Int
61  TestMod.Float64
62  (call core.svec %₅₉ %₆₀ %₆₁)
63  (call core.svec)
64  SourceLocation::1:10
65  (call core.svec %₆₂ %₆₃ %₆₄)
66  --- method core.nothing %₆₅
    slots: [slot₁/#self# slot₂/a slot₃/b]
    1   TestMod.#f_kw_simple#0
    2   (call %₁ 'a' true slot₁/#self# slot₂/a slot₃/b)
    3   (return %₂)
67  TestMod.f_kw_simple
68  (return %₆₇)

########################################
# Error: argument unpacking in keywords
function f_invalid_kw(; (x,y)=10)
    (x, y)
end
#---------------------
LoweringError:
function f_invalid_kw(; (x,y)=10)
#                       └───┘ ── Invalid keyword name
    (x, y)
end


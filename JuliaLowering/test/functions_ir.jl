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
#          └───┘ ── `...` may only be used for the last positional argument
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
# Static parameter which is used only in the bounds of another static parameter
# See https://github.com/JuliaLang/julia/issues/49275
function f(x, y::S) where {T, S<:AbstractVector{T}}
    (T,S)
end
#---------------------
1   (method TestMod.f)
2   (= slot₂/T (call core.TypeVar :T))
3   TestMod.AbstractVector
4   slot₂/T
5   (call core.apply_type %₃ %₄)
6   (= slot₁/S (call core.TypeVar :S %₅))
7   TestMod.f
8   (call core.Typeof %₇)
9   slot₁/S
10  (call core.svec %₈ core.Any %₉)
11  slot₂/T
12  slot₁/S
13  (call core.svec %₁₁ %₁₂)
14  SourceLocation::1:10
15  (call core.svec %₁₀ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/y(!read)]
    1   static_parameter₁
    2   static_parameter₂
    3   (call core.tuple %₁ %₂)
    4   (return %₃)
17  TestMod.f
18  (return %₁₇)

########################################
# Error: Static parameter which is unused
function f(::T) where {T,S}
    (T,S)
end
#---------------------
LoweringError:
function f(::T) where {T,S}
#                        ╙ ── Method definition declares type variable but does not use it in the type of any function parameter
    (T,S)
end

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
# Error: Default positional args before non-default arg
function f(x=1, ys, z=2)
    ys
end
#---------------------
LoweringError:
function f(x=1, ys, z=2)
#          └─┘ ── optional positional arguments must occur at end
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
# Function return without arguments
function f()
    return
    after_return # <- distinguish output from implicit return
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
    slots: [slot₁/#self#(!read)]
    1   (return core.nothing)
    2   TestMod.after_return
    3   (return %₂)
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
1   (method TestMod.f_kw_simple)
2   (method TestMod.#f_kw_simple#0)
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
    1   (meta :nkw 2)
    2   (call core.tuple slot₅/a slot₆/b slot₂/x slot₃/y)
    3   (return %₂)
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
    slots: [slot₁/#self#(!read) slot₂/kws slot₃/#self# slot₄/a slot₅/b slot₆/kwtmp slot₇/x(!read) slot₈/y(!read)]
    1   (newvar slot₇/x)
    2   (newvar slot₈/y)
    3   (call core.isdefined slot₂/kws :x)
    4   (gotoifnot %₃ label₁₅)
    5   (call core.getfield slot₂/kws :x)
    6   TestMod.Char
    7   (call core.isa %₅ %₆)
    8   (gotoifnot %₇ label₁₀)
    9   (goto label₁₃)
    10  TestMod.Char
    11  (new core.TypeError :keyword argument :x %₁₀ %₅)
    12  (call core.throw %₁₁)
    13  (= slot₆/kwtmp %₅)
    14  (goto label₁₆)
    15  (= slot₆/kwtmp 'a')
    16  slot₆/kwtmp
    17  (call core.isdefined slot₂/kws :y)
    18  (gotoifnot %₁₇ label₂₉)
    19  (call core.getfield slot₂/kws :y)
    20  TestMod.Bool
    21  (call core.isa %₁₉ %₂₀)
    22  (gotoifnot %₂₁ label₂₄)
    23  (goto label₂₇)
    24  TestMod.Bool
    25  (new core.TypeError :keyword argument :y %₂₄ %₁₉)
    26  (call core.throw %₂₅)
    27  (= slot₆/kwtmp %₁₉)
    28  (goto label₃₀)
    29  (= slot₆/kwtmp true)
    30  slot₆/kwtmp
    31  (call top.keys slot₂/kws)
    32  (call core.tuple :x :y)
    33  (call top.diff_names %₃₁ %₃₂)
    34  (call top.isempty %₃₃)
    35  (gotoifnot %₃₄ label₃₇)
    36  (goto label₃₈)
    37  (call top.kwerr slot₂/kws slot₃/#self# slot₄/a slot₅/b)
    38  TestMod.#f_kw_simple#0
    39  (call %₃₈ %₁₆ %₃₀ slot₃/#self# slot₄/a slot₅/b)
    40  (return %₃₉)
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
# Keyword slurping - simple forwarding of all kws
function f_kw_slurp_simple(; all_kws...)
    all_kws
end
#---------------------
1   (method TestMod.f_kw_slurp_simple)
2   (method TestMod.#f_kw_slurp_simple#0)
3   TestMod.#f_kw_slurp_simple#0
4   (call core.Typeof %₃)
5   (call top.pairs core.NamedTuple)
6   TestMod.f_kw_slurp_simple
7   (call core.Typeof %₆)
8   (call core.svec %₄ %₅ %₇)
9   (call core.svec)
10  SourceLocation::1:10
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read) slot₂/all_kws slot₃/#self#(!read)]
    1   (meta :nkw 1)
    2   slot₂/all_kws
    3   (return %₂)
13  (call core.typeof core.kwcall)
14  TestMod.f_kw_slurp_simple
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₃ core.NamedTuple %₁₅)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/kws slot₃/#self# slot₄/all_kws(!read)]
    1   (newvar slot₄/all_kws)
    2   (call top.pairs slot₂/kws)
    3   TestMod.#f_kw_slurp_simple#0
    4   (call %₃ %₂ slot₃/#self#)
    5   (return %₄)
21  TestMod.f_kw_slurp_simple
22  (call core.Typeof %₂₁)
23  (call core.svec %₂₂)
24  (call core.svec)
25  SourceLocation::1:10
26  (call core.svec %₂₃ %₂₄ %₂₅)
27  --- method core.nothing %₂₆
    slots: [slot₁/#self#]
    1   TestMod.#f_kw_slurp_simple#0
    2   (call core.NamedTuple)
    3   (call top.pairs %₂)
    4   (call %₁ %₃ slot₁/#self#)
    5   (return %₄)
28  TestMod.f_kw_slurp_simple
29  (return %₂₈)

########################################
# Keyword slurping
function f_kw_slurp(; x=x_default, non_x_kws...)
    all_kws
end
#---------------------
1   (method TestMod.f_kw_slurp)
2   (method TestMod.#f_kw_slurp#0)
3   TestMod.#f_kw_slurp#0
4   (call core.Typeof %₃)
5   (call top.pairs core.NamedTuple)
6   TestMod.f_kw_slurp
7   (call core.Typeof %₆)
8   (call core.svec %₄ core.Any %₅ %₇)
9   (call core.svec)
10  SourceLocation::1:10
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/non_x_kws(!read) slot₄/#self#(!read)]
    1   (meta :nkw 2)
    2   TestMod.all_kws
    3   (return %₂)
13  (call core.typeof core.kwcall)
14  TestMod.f_kw_slurp
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₃ core.NamedTuple %₁₅)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/kws slot₃/#self# slot₄/kwtmp slot₅/non_x_kws(!read) slot₆/x(!read)]
    1   (newvar slot₅/non_x_kws)
    2   (newvar slot₆/x)
    3   (call core.isdefined slot₂/kws :x)
    4   (gotoifnot %₃ label₈)
    5   (call core.getfield slot₂/kws :x)
    6   (= slot₄/kwtmp %₅)
    7   (goto label₁₀)
    8   TestMod.x_default
    9   (= slot₄/kwtmp %₈)
    10  slot₄/kwtmp
    11  (call core.tuple :x)
    12  (call core.apply_type core.NamedTuple %₁₁)
    13  (call top.structdiff slot₂/kws %₁₂)
    14  (call top.pairs %₁₃)
    15  TestMod.#f_kw_slurp#0
    16  (call %₁₅ %₁₀ %₁₄ slot₃/#self#)
    17  (return %₁₆)
21  TestMod.f_kw_slurp
22  (call core.Typeof %₂₁)
23  (call core.svec %₂₂)
24  (call core.svec)
25  SourceLocation::1:10
26  (call core.svec %₂₃ %₂₄ %₂₅)
27  --- method core.nothing %₂₆
    slots: [slot₁/#self#]
    1   TestMod.#f_kw_slurp#0
    2   TestMod.x_default
    3   (call core.NamedTuple)
    4   (call top.pairs %₃)
    5   (call %₁ %₂ %₄ slot₁/#self#)
    6   (return %₅)
28  TestMod.f_kw_slurp
29  (return %₂₈)

########################################
# Static parameters used in keywords, with and without the static parameter
# being present in positional argument types.
# 
# Here the wrong type for `b` will get a `TypeError` but `A` will need to rely
# on a MethodError.
function f_kw_sparams(x::X; a::A=a_def, b::X=b_def) where {X,A}
    (X,A)
end
#---------------------
1   (method TestMod.f_kw_sparams)
2   (method TestMod.#f_kw_sparams#0)
3   (= slot₂/X (call core.TypeVar :X))
4   (= slot₁/A (call core.TypeVar :A))
5   TestMod.#f_kw_sparams#0
6   (call core.Typeof %₅)
7   slot₁/A
8   slot₂/X
9   TestMod.f_kw_sparams
10  (call core.Typeof %₉)
11  slot₂/X
12  (call core.svec %₆ %₇ %₈ %₁₀ %₁₁)
13  slot₂/X
14  slot₁/A
15  (call core.svec %₁₃ %₁₄)
16  SourceLocation::1:10
17  (call core.svec %₁₂ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
    slots: [slot₁/#self#(!read) slot₂/a(!read) slot₃/b(!read) slot₄/#self#(!read) slot₅/x(!read)]
    1   (meta :nkw 2)
    2   static_parameter₁
    3   static_parameter₂
    4   (call core.tuple %₂ %₃)
    5   (return %₄)
19  (= slot₄/X (call core.TypeVar :X))
20  (= slot₃/A (call core.TypeVar :A))
21  (call core.typeof core.kwcall)
22  TestMod.f_kw_sparams
23  (call core.Typeof %₂₂)
24  slot₄/X
25  (call core.svec %₂₁ core.NamedTuple %₂₃ %₂₄)
26  slot₄/X
27  (call core.svec %₂₆)
28  SourceLocation::1:10
29  (call core.svec %₂₅ %₂₇ %₂₈)
30  --- method core.nothing %₂₉
    slots: [slot₁/#self#(!read) slot₂/kws slot₃/#self# slot₄/x slot₅/kwtmp slot₆/a(!read) slot₇/b(!read)]
    1   (newvar slot₆/a)
    2   (newvar slot₇/b)
    3   (call core.isdefined slot₂/kws :a)
    4   (gotoifnot %₃ label₈)
    5   (call core.getfield slot₂/kws :a)
    6   (= slot₅/kwtmp %₅)
    7   (goto label₁₀)
    8   TestMod.a_def
    9   (= slot₅/kwtmp %₈)
    10  slot₅/kwtmp
    11  (call core.isdefined slot₂/kws :b)
    12  (gotoifnot %₁₁ label₂₃)
    13  (call core.getfield slot₂/kws :b)
    14  static_parameter₁
    15  (call core.isa %₁₃ %₁₄)
    16  (gotoifnot %₁₅ label₁₈)
    17  (goto label₂₁)
    18  static_parameter₁
    19  (new core.TypeError :keyword argument :b %₁₈ %₁₃)
    20  (call core.throw %₁₉)
    21  (= slot₅/kwtmp %₁₃)
    22  (goto label₂₅)
    23  TestMod.b_def
    24  (= slot₅/kwtmp %₂₃)
    25  slot₅/kwtmp
    26  (call top.keys slot₂/kws)
    27  (call core.tuple :a :b)
    28  (call top.diff_names %₂₆ %₂₇)
    29  (call top.isempty %₂₈)
    30  (gotoifnot %₂₉ label₃₂)
    31  (goto label₃₃)
    32  (call top.kwerr slot₂/kws slot₃/#self# slot₄/x)
    33  TestMod.#f_kw_sparams#0
    34  (call %₃₃ %₁₀ %₂₅ slot₃/#self# slot₄/x)
    35  (return %₃₄)
31  (= slot₆/X (call core.TypeVar :X))
32  (= slot₅/A (call core.TypeVar :A))
33  TestMod.f_kw_sparams
34  (call core.Typeof %₃₃)
35  slot₆/X
36  (call core.svec %₃₄ %₃₅)
37  slot₆/X
38  (call core.svec %₃₇)
39  SourceLocation::1:10
40  (call core.svec %₃₆ %₃₈ %₃₉)
41  --- method core.nothing %₄₀
    slots: [slot₁/#self# slot₂/x]
    1   TestMod.#f_kw_sparams#0
    2   TestMod.a_def
    3   TestMod.b_def
    4   (call %₁ %₂ %₃ slot₁/#self# slot₂/x)
    5   (return %₄)
42  TestMod.f_kw_sparams
43  (return %₄₂)

########################################
# Error: Static parameter which is unused in keyword body arg types
function f_kw_sparams(x::X; a::A) where {X,Y,A}
    (X,A)
end
#---------------------
LoweringError:
function f_kw_sparams(x::X; a::A) where {X,Y,A}
#                                          ╙ ── Method definition declares type variable but does not use it in the type of any function parameter
    (X,A)
end

########################################
# Error: argument unpacking in keywords
function f_kw_destruct(; (x,y)=10)
end
#---------------------
LoweringError:
function f_kw_destruct(; (x,y)=10)
#                        └───┘ ── Invalid keyword name
end

########################################
# Error: keyword slurping combined with a default
function f_kw_slurp_default(; kws...=def)
end
#---------------------
LoweringError:
function f_kw_slurp_default(; kws...=def)
#                             └────────┘ ── keyword argument with `...` cannot have a default value
end

########################################
# Error: keyword slurping combined with type
function f_kw_slurp_type(; kws::T...)
end
#---------------------
LoweringError:
function f_kw_slurp_type(; kws::T...)
#                          └───────┘ ── keyword argument with `...` may not be given a type
end

########################################
# Error: keyword slurping on non-final argument
function f_kw_slurp_not_last(; kws..., x=1)
end
#---------------------
LoweringError:
function f_kw_slurp_not_last(; kws..., x=1)
#                              └────┘ ── `...` may only be used for the last keyword argument
end

########################################
# Fully generated function
@generated function f_only_generated(x, y)
    generator_code(x,y)
end
#---------------------
1   (method TestMod.f_only_generated)
2   (method TestMod.#f_only_generated@generator#0)
3   TestMod.#f_only_generated@generator#0
4   (call core.Typeof %₃)
5   (call core.svec %₄ JuliaLowering.MacroContext core.Any core.Any core.Any)
6   (call core.svec)
7   SourceLocation::1:21
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/__context__(!read) slot₃/#self#(!read) slot₄/x(nospecialize) slot₅/y(nospecialize)]
    1   TestMod.generator_code
    2   (call %₁ slot₄/x slot₅/y)
    3   (return %₂)
10  TestMod.f_only_generated
11  (call core.Typeof %₁₀)
12  (call core.svec %₁₁ core.Any core.Any)
13  (call core.svec)
14  SourceLocation::1:21
15  (call core.svec %₁₂ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/y(!read)]
    1   (meta :generated (new JuliaLowering.GeneratedFunctionStub TestMod.#f_only_generated@generator#0 SourceRef(SourceFile("@generated function f_only_generated(x, y)\n    generator_code(x,y)\nend", 0, nothing, 1, [1, 44, 68, 71]), 1, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"macrocall", 0x0000), 0x00000046, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"@", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"MacroName", 0x0000), 0x00000009, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"function", 0x0000), 0x0000003b, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"function", 0x0001), 0x00000008, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"call", 0x0000), 0x00000016, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000010, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"(", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K",", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K")", 0x0001), 0x00000001, nothing)]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"block", 0x0000), 0x00000019, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x00000005, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"call", 0x0000), 0x00000013, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x0000000e, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"(", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K",", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K")", 0x0001), 0x00000001, nothing)]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x00000001, nothing)]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"end", 0x0001), 0x00000003, nothing)])])) (call core.svec :#self# :x :y) (call core.svec)))
    2   (meta :generated_only)
    3   (return core.nothing)
17  TestMod.f_only_generated
18  (return %₁₇)

########################################
# Partially generated function with `if @generated`
function f_partially_generated(x, y)
    nongen_stuff = bothgen(x, y)
    if @generated
        quote
            maybe_gen_stuff = some_gen_stuff(x, y)
        end
    else
        maybe_gen_stuff = some_nongen_stuff(x, y)
    end
    (nongen_stuff, maybe_gen_stuff)
end
#---------------------
1   (method TestMod.f_partially_generated)
2   (method TestMod.#f_partially_generated@generator#0)
3   TestMod.#f_partially_generated@generator#0
4   (call core.Typeof %₃)
5   (call core.svec %₄ JuliaLowering.MacroContext core.Any core.Any core.Any)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/__context__(!read) slot₃/#self#(!read) slot₄/x(nospecialize,!read) slot₅/y(nospecialize,!read)]
    1   (call JuliaLowering.interpolate_ast (inert (block (= maybe_gen_stuff (call some_gen_stuff x y)))))
    2   (call core.tuple %₁)
    3   (call JuliaLowering.interpolate_ast (inert (block (block (= nongen_stuff (call bothgen x y)) ($ (block (call JuliaLowering.interpolate_ast (inert (block (= maybe_gen_stuff (call some_gen_stuff x y))))))) (tuple-p nongen_stuff maybe_gen_stuff)))) %₂)
    4   (return %₃)
10  TestMod.f_partially_generated
11  (call core.Typeof %₁₀)
12  (call core.svec %₁₁ core.Any core.Any)
13  (call core.svec)
14  SourceLocation::1:10
15  (call core.svec %₁₂ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y slot₄/maybe_gen_stuff slot₅/nongen_stuff]
    1   (meta :generated (new JuliaLowering.GeneratedFunctionStub TestMod.#f_partially_generated@generator#0 SourceRef(SourceFile("function f_partially_generated(x, y)\n    nongen_stuff = bothgen(x, y)\n    if @generated\n        quote\n            maybe_gen_stuff = some_gen_stuff(x, y)\n        end\n    else\n        maybe_gen_stuff = some_nongen_stuff(x, y)\n    end\n    (nongen_stuff, maybe_gen_stuff)\nend", 0, nothing, 1, [1, 38, 71, 89, 103, 154, 166, 175, 225, 233, 269, 272]), 1, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"function", 0x0000), 0x0000010f, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"function", 0x0001), 0x00000008, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"call", 0x0000), 0x0000001b, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000015, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"(", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K",", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K")", 0x0001), 0x00000001, nothing)]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"block", 0x0000), 0x000000e8, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x00000005, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"=", 0x0000), 0x0000001c, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x0000000c, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"=", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"call", 0x0000), 0x0000000d, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000007, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"(", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K",", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K")", 0x0001), 0x00000001, nothing)])]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x00000005, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"if", 0x0000), 0x0000009d, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"if", 0x0001), 0x00000002, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"macrocall", 0x0000), 0x0000000a, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"@", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"MacroName", 0x0000), 0x00000009, nothing)]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"block", 0x0000), 0x00000052, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x00000009, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"quote", 0x0000), 0x00000044, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"block", 0x0000), 0x00000044, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"quote", 0x0001), 0x00000005, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x0000000d, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"=", 0x0000), 0x00000026, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x0000000f, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"=", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"call", 0x0000), 0x00000014, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x0000000e, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"(", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K",", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K")", 0x0001), 0x00000001, nothing)])]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x00000009, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"end", 0x0001), 0x00000003, nothing)])]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x00000005, nothing)]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"else", 0x0001), 0x00000004, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"block", 0x0000), 0x00000037, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x00000009, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"=", 0x0000), 0x00000029, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x0000000f, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"=", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"call", 0x0000), 0x00000017, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000011, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"(", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K",", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K")", 0x0001), 0x00000001, nothing)])]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x00000005, nothing)]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"end", 0x0001), 0x00000003, nothing)]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x00000005, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"tuple", 0x0020), 0x0000001f, JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}[JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"(", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x0000000c, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K",", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Whitespace", 0x0001), 0x00000001, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"Identifier", 0x0000), 0x0000000f, nothing), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K")", 0x0001), 0x00000001, nothing)]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"NewlineWs", 0x0001), 0x00000001, nothing)]), JuliaSyntax.GreenNode{JuliaSyntax.SyntaxHead}(JuliaSyntax.SyntaxHead(K"end", 0x0001), 0x00000003, nothing)])) (call core.svec :#self# :x :y) (call core.svec)))
    2   TestMod.bothgen
    3   (= slot₅/nongen_stuff (call %₂ slot₂/x slot₃/y))
    4   TestMod.some_nongen_stuff
    5   (= slot₄/maybe_gen_stuff (call %₄ slot₂/x slot₃/y))
    6   slot₅/nongen_stuff
    7   slot₄/maybe_gen_stuff
    8   (call core.tuple %₆ %₇)
    9   (return %₈)
17  TestMod.f_partially_generated
18  (return %₁₇)


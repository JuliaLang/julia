########################################
# Function declaration with no methods
function f
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (return %₃)

########################################
# Functions with placeholder arg
function f(x, _, y)
    x + y
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄ core.Any core.Any core.Any)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/x slot₃/#arg2#(!read) slot₄/y]
    1   TestMod.+
    2   (call %₁ slot₂/x slot₄/y)
    3   (return %₂)
10  latestworld
11  TestMod.f
12  (return %₁₁)

########################################
# Functions with argument types only, no name
function f(::T, x)
    x
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   TestMod.T
6   (call core.svec %₄ %₅ core.Any)
7   (call core.svec)
8   SourceLocation::1:10
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read) slot₂/#arg1#(!read) slot₃/x]
    1   slot₃/x
    2   (return %₁)
11  latestworld
12  TestMod.f
13  (return %₁₂)

########################################
# Functions argument types
function f(x, y::T)
    body
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   TestMod.T
6   (call core.svec %₄ core.Any %₅)
7   (call core.svec)
8   SourceLocation::1:10
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/y(!read)]
    1   TestMod.body
    2   (return %₁)
11  latestworld
12  TestMod.f
13  (return %₁₂)

########################################
# Functions with slurp of Any
function f(x, ys...)
    body
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.apply_type core.Vararg core.Any)
6   (call core.svec %₄ core.Any %₅)
7   (call core.svec)
8   SourceLocation::1:10
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/ys(!read)]
    1   TestMod.body
    2   (return %₁)
11  latestworld
12  TestMod.f
13  (return %₁₂)

########################################
# Functions with slurp of T
function f(x, ys::T...)
    body
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   TestMod.T
6   (call core.apply_type core.Vararg %₅)
7   (call core.svec %₄ core.Any %₆)
8   (call core.svec)
9   SourceLocation::1:10
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/ys(!read)]
    1   TestMod.body
    2   (return %₁)
12  latestworld
13  TestMod.f
14  (return %₁₃)

########################################
# Error: Function with slurp not in last position arg
function f(xs..., y)
    body
end
#---------------------
LoweringError:
function f(xs..., y)
#          └───┘ ── `...` may only be used on the final parameter
    body
end

########################################
# Basic static parameters
function f(::T, ::U, ::V) where T where {U,V}
    (T,U,V)
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   (= slot₁/U (call core.TypeVar :U))
4   (= slot₂/V (call core.TypeVar :V))
5   (= slot₃/T (call core.TypeVar :T))
6   TestMod.f
7   (call core.Typeof %₆)
8   slot₃/T
9   slot₁/U
10  slot₂/V
11  (call core.svec %₇ %₈ %₉ %₁₀)
12  slot₁/U
13  slot₂/V
14  slot₃/T
15  (call core.svec %₁₂ %₁₃ %₁₄)
16  SourceLocation::1:10
17  (call core.svec %₁₁ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
    slots: [slot₁/#self#(!read) slot₂/#arg1#(!read) slot₃/#arg2#(!read) slot₄/#arg3#(!read)]
    1   static_parameter₃
    2   static_parameter₁
    3   static_parameter₂
    4   (call core.tuple %₁ %₂ %₃)
    5   (return %₄)
19  latestworld
20  TestMod.f
21  (return %₂₀)

########################################
# Static parameter with bounds and used with apply_type in argument
function f(::S{T}) where X <: T <: Y
    T
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.X
4   TestMod.Y
5   (= slot₁/T (call core.TypeVar :T %₃ %₄))
6   TestMod.f
7   (call core.Typeof %₆)
8   TestMod.S
9   slot₁/T
10  (call core.apply_type %₈ %₉)
11  (call core.svec %₇ %₁₀)
12  slot₁/T
13  (call core.svec %₁₂)
14  SourceLocation::1:10
15  (call core.svec %₁₁ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/#arg1#(!read)]
    1   static_parameter₁
    2   (return %₁)
17  latestworld
18  TestMod.f
19  (return %₁₈)

########################################
# Static parameter with lower bound
function f(::S{T}) where T >: X
    T
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.X
4   (= slot₁/T (call core.TypeVar :T %₃ core.Any))
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
    slots: [slot₁/#self#(!read) slot₂/#arg1#(!read)]
    1   static_parameter₁
    2   (return %₁)
16  latestworld
17  TestMod.f
18  (return %₁₇)

########################################
# Static parameter which is used only in the bounds of another static parameter
# See https://github.com/JuliaLang/julia/issues/49275
function f(x, y::S) where {T, S<:AbstractVector{T}}
    (T,S)
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   (= slot₁/T (call core.TypeVar :T))
4   TestMod.AbstractVector
5   slot₁/T
6   (call core.apply_type %₄ %₅)
7   (= slot₂/S (call core.TypeVar :S %₆))
8   TestMod.f
9   (call core.Typeof %₈)
10  slot₂/S
11  (call core.svec %₉ core.Any %₁₀)
12  slot₁/T
13  slot₂/S
14  (call core.svec %₁₂ %₁₃)
15  SourceLocation::1:10
16  (call core.svec %₁₁ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/y(!read)]
    1   static_parameter₁
    2   static_parameter₂
    3   (call core.tuple %₁ %₂)
    4   (return %₃)
18  latestworld
19  TestMod.f
20  (return %₁₉)

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
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/x slot₃/tmp(!read)]
    1   TestMod.Int
    2   (gotoifnot slot₂/x label₃)
    3   (= slot₃/tmp 0xff)
    4   (call core.isa slot₃/tmp %₁)
    5   (gotoifnot %₄ label₇)
    6   (goto label₉)
    7   (call top.convert %₁ slot₃/tmp)
    8   (= slot₃/tmp (call core.typeassert %₇ %₁))
    9   slot₃/tmp
    10  (return %₉)
10  latestworld
11  TestMod.f
12  (return %₁₁)

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
7   latestworld
8   (return core.nothing)

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
7   latestworld
8   (return core.nothing)

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
11  latestworld
12  (return core.nothing)

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
9   latestworld
10  (return core.nothing)

########################################
# Error: Invalid dotop function name
function (.+)(x,y)
end
#---------------------
LoweringError:
function (.+)(x,y)
#         └┘ ── invalid function name
end

########################################
# Error: Invalid function name
function f[](x,y)
end
#---------------------
LoweringError:
function f[](x,y)
#        └─┘ ── invalid function name
end

########################################
# Simple positional args with defaults
function f(x::T, y::S=1, z::U=2)
    (x,y)
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   TestMod.T
6   (call core.svec %₄ %₅)
7   (call core.svec)
8   SourceLocation::1:10
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call slot₁/#self# slot₂/x 1 2)
    2   (return %₁)
11  latestworld
12  TestMod.f
13  (call core.Typeof %₁₂)
14  TestMod.T
15  TestMod.S
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  (call core.svec)
18  SourceLocation::1:10
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(called) slot₂/x slot₃/y]
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
21  latestworld
22  TestMod.f
23  (call core.Typeof %₂₂)
24  TestMod.T
25  TestMod.S
26  TestMod.U
27  (call core.svec %₂₃ %₂₄ %₂₅ %₂₆)
28  (call core.svec)
29  SourceLocation::1:10
30  (call core.svec %₂₇ %₂₈ %₂₉)
31  --- method core.nothing %₃₀
    slots: [slot₁/#self#(called) slot₂/x slot₃/y slot₄/z(!read)]
    1   (call core.tuple slot₂/x slot₃/y)
    2   (return %₁)
32  latestworld
33  TestMod.f
34  (return %₃₃)

########################################
# Default positional args which depend on other args
function f(x=1, y=x)
    (x,y)
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(called)]
    1   (call slot₁/#self# 1)
    2   (return %₁)
10  latestworld
11  TestMod.f
12  (call core.Typeof %₁₁)
13  (call core.svec %₁₂ core.Any)
14  (call core.svec)
15  SourceLocation::1:10
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call slot₁/#self# slot₂/x slot₂/x)
    2   (return %₁)
18  latestworld
19  TestMod.f
20  (call core.Typeof %₁₉)
21  (call core.svec %₂₀ core.Any core.Any)
22  (call core.svec)
23  SourceLocation::1:10
24  (call core.svec %₂₁ %₂₂ %₂₃)
25  --- method core.nothing %₂₄
    slots: [slot₁/#self#(called) slot₂/x slot₃/y]
    1   (call core.tuple slot₂/x slot₃/y)
    2   (return %₁)
26  latestworld
27  TestMod.f
28  (return %₂₇)

########################################
# Default positional args with missing arg names (implicit placeholders)
function f(::Int, y=1, z=2)
    (y, z)
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   TestMod.Int
6   (call core.svec %₄ %₅)
7   (call core.svec)
8   SourceLocation::1:10
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(called) slot₂/#arg1#]
    1   (call slot₁/#self# slot₂/#arg1# 1 2)
    2   (return %₁)
11  latestworld
12  TestMod.f
13  (call core.Typeof %₁₂)
14  TestMod.Int
15  (call core.svec %₁₃ %₁₄ core.Any)
16  (call core.svec)
17  SourceLocation::1:10
18  (call core.svec %₁₅ %₁₆ %₁₇)
19  --- method core.nothing %₁₈
    slots: [slot₁/#self#(called) slot₂/#arg1# slot₃/y]
    1   (call slot₁/#self# slot₂/#arg1# slot₃/y 2)
    2   (return %₁)
20  latestworld
21  TestMod.f
22  (call core.Typeof %₂₁)
23  TestMod.Int
24  (call core.svec %₂₂ %₂₃ core.Any core.Any)
25  (call core.svec)
26  SourceLocation::1:10
27  (call core.svec %₂₄ %₂₅ %₂₆)
28  --- method core.nothing %₂₇
    slots: [slot₁/#self#(called) slot₂/#arg1# slot₃/y slot₄/z]
    1   (call core.tuple slot₃/y slot₄/z)
    2   (return %₁)
29  latestworld
30  TestMod.f
31  (return %₃₀)

########################################
# Default positional args with placeholders
function f(_::Int, x=1)
    x
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   TestMod.Int
6   (call core.svec %₄ %₅)
7   (call core.svec)
8   SourceLocation::1:10
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(called) slot₂/#arg1#]
    1   (call slot₁/#self# slot₂/#arg1# 1)
    2   (return %₁)
11  latestworld
12  TestMod.f
13  (call core.Typeof %₁₂)
14  TestMod.Int
15  (call core.svec %₁₃ %₁₄ core.Any)
16  (call core.svec)
17  SourceLocation::1:10
18  (call core.svec %₁₅ %₁₆ %₁₇)
19  --- method core.nothing %₁₈
    slots: [slot₁/#self#(called) slot₂/#arg1# slot₃/x]
    1   slot₃/x
    2   (return %₁)
20  latestworld
21  TestMod.f
22  (return %₂₁)

########################################
# Positional args with defaults and `where` clauses
function f(x::T, y::S=1, z::U=2) where {T,S<:T,U<:S}
    (x,y,z)
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   (= slot₁/T (call core.TypeVar :T))
4   slot₁/T
5   (= slot₂/S (call core.TypeVar :S %₄))
6   slot₂/S
7   (= slot₃/U (call core.TypeVar :U %₆))
8   TestMod.f
9   (call core.Typeof %₈)
10  slot₁/T
11  (call core.svec %₉ %₁₀)
12  slot₁/T
13  (call core.svec %₁₂)
14  SourceLocation::1:10
15  (call core.svec %₁₁ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call slot₁/#self# slot₂/x 1 2)
    2   (return %₁)
17  latestworld
18  TestMod.f
19  (call core.Typeof %₁₈)
20  slot₁/T
21  slot₂/S
22  (call core.svec %₁₉ %₂₀ %₂₁)
23  slot₁/T
24  slot₂/S
25  (call core.svec %₂₃ %₂₄)
26  SourceLocation::1:10
27  (call core.svec %₂₂ %₂₅ %₂₆)
28  --- method core.nothing %₂₇
    slots: [slot₁/#self#(called) slot₂/x slot₃/y]
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
29  latestworld
30  TestMod.f
31  (call core.Typeof %₃₀)
32  slot₁/T
33  slot₂/S
34  slot₃/U
35  (call core.svec %₃₁ %₃₂ %₃₃ %₃₄)
36  slot₁/T
37  slot₂/S
38  slot₃/U
39  (call core.svec %₃₆ %₃₇ %₃₈)
40  SourceLocation::1:10
41  (call core.svec %₃₅ %₃₉ %₄₀)
42  --- method core.nothing %₄₁
    slots: [slot₁/#self#(called) slot₂/x slot₃/y slot₄/z]
    1   (call core.tuple slot₂/x slot₃/y slot₄/z)
    2   (return %₁)
43  latestworld
44  TestMod.f
45  (return %₄₄)

########################################
# Positional args and type parameters with transitive dependencies
# See https://github.com/JuliaLang/julia/issues/49275 - the first method
# generated here for only `x` should contain zero type parameters.
function f(x, y::S=[1], z::U=2) where {T, S<:AbstractVector{T}, U}
    (x, y, z, T, S, U)
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   (= slot₁/T (call core.TypeVar :T))
4   TestMod.AbstractVector
5   slot₁/T
6   (call core.apply_type %₄ %₅)
7   (= slot₂/S (call core.TypeVar :S %₆))
8   (= slot₃/U (call core.TypeVar :U))
9   TestMod.f
10  (call core.Typeof %₉)
11  (call core.svec %₁₀ core.Any)
12  (call core.svec)
13  SourceLocation::1:10
14  (call core.svec %₁₁ %₁₂ %₁₃)
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call top.vect 1)
    2   (call slot₁/#self# slot₂/x %₁ 2)
    3   (return %₂)
16  latestworld
17  TestMod.f
18  (call core.Typeof %₁₇)
19  slot₂/S
20  (call core.svec %₁₈ core.Any %₁₉)
21  slot₁/T
22  slot₂/S
23  (call core.svec %₂₁ %₂₂)
24  SourceLocation::1:10
25  (call core.svec %₂₀ %₂₃ %₂₄)
26  --- method core.nothing %₂₅
    slots: [slot₁/#self#(called) slot₂/x slot₃/y]
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
27  latestworld
28  TestMod.f
29  (call core.Typeof %₂₈)
30  slot₂/S
31  slot₃/U
32  (call core.svec %₂₉ core.Any %₃₀ %₃₁)
33  slot₁/T
34  slot₂/S
35  slot₃/U
36  (call core.svec %₃₃ %₃₄ %₃₅)
37  SourceLocation::1:10
38  (call core.svec %₃₂ %₃₆ %₃₇)
39  --- method core.nothing %₃₈
    slots: [slot₁/#self#(called) slot₂/x slot₃/y slot₄/z]
    1   static_parameter₁
    2   static_parameter₂
    3   static_parameter₃
    4   (call core.tuple slot₂/x slot₃/y slot₄/z %₁ %₂ %₃)
    5   (return %₄)
40  latestworld
41  TestMod.f
42  (return %₄₁)

########################################
# Default positional args are allowed before trailing slurp with no default
function f(x=1, ys...)
    ys
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(called)]
    1   (call slot₁/#self# 1)
    2   (return %₁)
10  latestworld
11  TestMod.f
12  (call core.Typeof %₁₁)
13  (call core.apply_type core.Vararg core.Any)
14  (call core.svec %₁₂ core.Any %₁₃)
15  (call core.svec)
16  SourceLocation::1:10
17  (call core.svec %₁₄ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
    slots: [slot₁/#self#(called) slot₂/x(!read) slot₃/ys]
    1   slot₃/ys
    2   (return %₁)
19  latestworld
20  TestMod.f
21  (return %₂₀)

########################################
# Error: Default positional args before non-default arg
function f(x=1, ys, z=2)
    ys
end
#---------------------
LoweringError:
function f(x=1, ys, z=2)
#               └┘ ── all function parameters after an optional parameter must also be optional
    ys
end

########################################
# Positional arg with slurp and default
function f(xs...=1)
    xs
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(called)]
    1   (call slot₁/#self# 1)
    2   (return %₁)
10  latestworld
11  TestMod.f
12  (call core.Typeof %₁₁)
13  (call core.apply_type core.Vararg core.Any)
14  (call core.svec %₁₂ %₁₃)
15  (call core.svec)
16  SourceLocation::1:10
17  (call core.svec %₁₄ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
    slots: [slot₁/#self#(called) slot₂/xs]
    1   slot₂/xs
    2   (return %₁)
19  latestworld
20  TestMod.f
21  (return %₂₀)

########################################
# Positional arg with slurp and splatted default value
function f(xs...=(1,2)...)
    xs
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#]
    1   (call core.tuple 1 2)
    2   (call core._apply_iterate top.iterate slot₁/#self# %₁)
    3   (return %₂)
10  latestworld
11  TestMod.f
12  (call core.Typeof %₁₁)
13  (call core.apply_type core.Vararg core.Any)
14  (call core.svec %₁₂ %₁₃)
15  (call core.svec)
16  SourceLocation::1:10
17  (call core.svec %₁₄ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
    slots: [slot₁/#self# slot₂/xs]
    1   slot₂/xs
    2   (return %₁)
19  latestworld
20  TestMod.f
21  (return %₂₀)

########################################
# Trivial function argument destructuring
function f(x, (y,z), w)
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄ core.Any core.Any core.Any)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/destructured_arg slot₄/w(!read) slot₅/iterstate(single_assign) slot₆/y(!read,single_assign) slot₇/z(!read,single_assign)]
    1   (call top.indexed_iterate slot₃/destructured_arg 1)
    2   (= slot₆/y (call core.getfield %₁ 1))
    3   (= slot₅/iterstate (call core.getfield %₁ 2))
    4   slot₅/iterstate
    5   (call top.indexed_iterate slot₃/destructured_arg 2 %₄)
    6   (= slot₇/z (call core.getfield %₅ 1))
    7   (return core.nothing)
10  latestworld
11  TestMod.f
12  (return %₁₁)

########################################
# Function argument destructuring combined with splats, types and and defaults
function f((x,)::T...=rhs)
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(called)]
    1   TestMod.rhs
    2   (call slot₁/#self# %₁)
    3   (return %₂)
10  latestworld
11  TestMod.f
12  (call core.Typeof %₁₁)
13  TestMod.T
14  (call core.apply_type core.Vararg %₁₃)
15  (call core.svec %₁₂ %₁₄)
16  (call core.svec)
17  SourceLocation::1:10
18  (call core.svec %₁₅ %₁₆ %₁₇)
19  --- method core.nothing %₁₈
    slots: [slot₁/#self#(called) slot₂/destructured_arg slot₃/x(!read,single_assign)]
    1   (call top.indexed_iterate slot₂/destructured_arg 1)
    2   (= slot₃/x (call core.getfield %₁ 1))
    3   (return core.nothing)
20  latestworld
21  TestMod.f
22  (return %₂₁)

########################################
# Error: multiple destructuring in destructured arg
function f(_,(x...,y...)); end
#---------------------
LoweringError:
function f(_,(x...,y...)); end
#            └─────────┘ ── multiple `...` in destructured parameter is ambiguous

########################################
# Error: multiple destructuring in var-destructured arg
function f(_,(x...,y...)...); end
#---------------------
LoweringError:
function f(_,(x...,y...)...); end
#            └─────────┘ ── multiple `...` in destructured parameter is ambiguous

########################################
# Error: type in destructured arg
function f(_,(x,y::Int)); end
#---------------------
LoweringError:
function f(_,(x,y::Int)); end
#               └────┘ ── cannot have type in destructured argument

########################################
# Error: type in destructured arg, nested
function f(_,(x,(y,(z::Int,)))...); end
#---------------------
LoweringError:
function f(_,(x,(y,(z::Int,)))...); end
#                   └────┘ ── cannot have type in destructured argument

########################################
# Error: type on splat
function (_,((a,b)...)::Int); end
#---------------------
LoweringError:
function (_,((a,b)...)::Int); end
#           └─────────────┘ ── expected identifier or `identifier::type`

########################################
# Error: destructured arg with kw
function f(_,(x,y=1)); end
#---------------------
LoweringError:
function f(_,(x,y=1)); end
#               └─┘ ── expected identifier or tuple

########################################
# Error: destructured arg with ;kw
function f(_,(;x,y=1)); end
#---------------------
LoweringError:
function f(_,(;x,y=1)); end
#                └─┘ ── expected identifier

########################################
# Error: destructured arg with other lhs-likes after ; (call)
function f(_,(;x,y())); end
#---------------------
LoweringError:
function f(_,(;x,y())); end
#                └─┘ ── expected identifier

########################################
# Error: destructured arg with other lhs-likes after ; (ref)
function f(_,(;x,y())); end
#---------------------
LoweringError:
function f(_,(;x,y())); end
#                └─┘ ── expected identifier

########################################
# Error: destructured arg with other lhs-likes after ; (tuple)
function f(_,(;x,(y,z))); end
#---------------------
LoweringError:
function f(_,(;x,(y,z))); end
#                └───┘ ── expected identifier

########################################
# Error: destructured arg with other lhs-likes after ; (...)
function f(_,(;x,y...)); end
#---------------------
LoweringError:
function f(_,(;x,y...)); end
#                └──┘ ── expected identifier

########################################
# Error: destructuring mixed tuple
function f(_,(x,;y=1)); end
#---------------------
LoweringError:
function f(_,(x,;y=1)); end
#               └──┘ ── cannot mix tuple `(a,b,c)` and named tuple `(;a,b,c)` syntax

########################################
# Error: ref in arg tuple (flisp allows this)
function f(_,(_,x[])); end
#---------------------
LoweringError:
function f(_,(_,x[])); end
#               └─┘ ── expected identifier or tuple

########################################
# Error: call in arg tuple (flisp allows this; args ignored)
function f(_,(_,x(y))); end
#---------------------
LoweringError:
function f(_,(_,x(y))); end
#               └──┘ ── expected identifier or tuple

########################################
# Error: curly in arg tuple (flisp allows this)
function f(_,(_,x{y})); end
#---------------------
LoweringError:
function f(_,(_,x{y})); end
#               └──┘ ── expected identifier or tuple

########################################
# Error: splat on non-final default positional arg
function f(x=1...,y=2); end
#---------------------
LoweringError:
function f(x=1...,y=2); end
#            └──┘ ── splat only allowed on final positional default arg

########################################
# Error: splat on non-final default positional arg 2
function f(x=(1,2)...,y=(3,4)...); end
#---------------------
LoweringError:
function f(x=(1,2)...,y=(3,4)...); end
#            └──────┘ ── splat only allowed on final positional default arg

########################################
# Function argument destructuring combined with splats, types and and defaults
function f(x=default_x)::T
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(called)]
    1   TestMod.default_x
    2   (call slot₁/#self# %₁)
    3   (return %₂)
10  latestworld
11  TestMod.f
12  (call core.Typeof %₁₁)
13  (call core.svec %₁₂ core.Any)
14  (call core.svec)
15  SourceLocation::1:10
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(called) slot₂/x(!read) slot₃/tmp(!read)]
    1   TestMod.T
    2   (= slot₃/tmp core.nothing)
    3   (call core.isa slot₃/tmp %₁)
    4   (gotoifnot %₃ label₆)
    5   (goto label₈)
    6   (call top.convert %₁ slot₃/tmp)
    7   (= slot₃/tmp (call core.typeassert %₆ %₁))
    8   slot₃/tmp
    9   (return %₈)
18  latestworld
19  TestMod.f
20  (return %₁₉)

########################################
# Duplicate positional placeholders ok
function f(_, _); end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄ core.Any core.Any)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/#arg1#(!read) slot₃/#arg2#(!read)]
    1   (return core.nothing)
10  latestworld
11  TestMod.f
12  (return %₁₁)

########################################
# Duplicate destructured placeholders ok
function f((_,), (_,))
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄ core.Any core.Any)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/destructured_arg slot₃/destructured_arg]
    1   (call top.indexed_iterate slot₂/destructured_arg 1)
    2   (call core.getfield %₁ 1)
    3   (call top.indexed_iterate slot₃/destructured_arg 1)
    4   (call core.getfield %₃ 1)
    5   (return core.nothing)
10  latestworld
11  TestMod.f
12  (return %₁₁)

########################################
# Slot flags
function f(@nospecialize(x), g, y)
    g() + y
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄ core.Any core.Any core.Any)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/x(nospecialize,!read) slot₃/g(called) slot₄/y]
    1   TestMod.+
    2   (call slot₃/g)
    3   (call %₁ %₂ slot₄/y)
    4   (return %₃)
10  latestworld
11  TestMod.f
12  (return %₁₁)

########################################
# Function return without arguments
function f()
    return
    after_return # <- distinguish output from implicit return
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read)]
    1   (return core.nothing)
    2   TestMod.after_return
    3   (return %₂)
10  latestworld
11  TestMod.f
12  (return %₁₁)

########################################
# Function return in value position is allowed
function f()
    x = return 1
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/x(!read,single_assign)]
    1   (return 1)
    2   (= slot₂/x core.nothing)
10  latestworld
11  TestMod.f
12  (return %₁₁)

########################################
# Binding docs to functions
"""
some docs
"""
function f()
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄)
6   (call core.svec)
7   SourceLocation:nothing:4:0
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read)]
    1   (return core.nothing)
10  latestworld
11  TestMod.f
12  (= slot₁/val %₁₁)
13  (call JuliaLowering.interpolate_ast SyntaxTree (inert_syntaxtree f))
14  (call Base.Docs.Binding TestMod %₁₃)
15  (call Core.svec "some docs\n")
16  (call Dict{Symbol, Any} :path => "none" :linenumber => 1 :module => TestMod)
17  (call Base.Docs.docstr %₁₅ %₁₆)
18  TestMod.Union
19  TestMod.Tuple
20  (call core.apply_type %₁₉)
21  (call core.apply_type %₁₈ %₂₀)
22  (call Base.Docs.doc! TestMod %₁₄ %₁₇ %₂₁)
23  slot₁/val
24  (return %₂₃)

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
4   SourceLocation:nothing:4:0
5   (call core.svec %₂ %₃ %₄)
6   --- method core.nothing %₅
    slots: [slot₁/x(!read)]
    1   (return core.nothing)
7   latestworld
8   (= slot₁/val core.nothing)
9   (call JuliaLowering.interpolate_ast SyntaxTree (inert_syntaxtree T))
10  (call Base.Docs.Binding TestMod %₉)
11  (call Core.svec "some docs\n")
12  (call Dict{Symbol, Any} :path => "none" :linenumber => 1 :module => TestMod)
13  (call Base.Docs.docstr %₁₁ %₁₂)
14  TestMod.Union
15  TestMod.Tuple
16  (call core.apply_type %₁₅)
17  (call core.apply_type %₁₄ %₁₆)
18  (call Base.Docs.doc! TestMod %₁₀ %₁₃ %₁₇)
19  slot₁/val
20  (return %₁₉)

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
2   latestworld
3   (method TestMod.#f_kw_simple#0)
4   latestworld
5   TestMod.#f_kw_simple#0
6   (call core.Typeof %₅)
7   TestMod.Char
8   TestMod.Bool
9   TestMod.f_kw_simple
10  (call core.Typeof %₉)
11  TestMod.Int
12  TestMod.Float64
13  (call core.svec %₆ %₇ %₈ %₁₀ %₁₁ %₁₂)
14  (call core.svec)
15  SourceLocation::1:10
16  (call core.svec %₁₃ %₁₄ %₁₅)
17  --- method core.nothing %₁₆
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y slot₄/#self#(called) slot₅/a slot₆/b]
    1   (meta :nkw 2)
    2   (call core.tuple slot₅/a slot₆/b slot₂/x slot₃/y)
    3   (return %₂)
18  latestworld
19  (call core.typeof core.kwcall)
20  TestMod.f_kw_simple
21  (call core.Typeof %₂₀)
22  (call core.svec %₁₉ core.NamedTuple %₂₁)
23  (call core.svec)
24  SourceLocation::1:10
25  (call core.svec %₂₂ %₂₃ %₂₄)
26  --- method core.nothing %₂₅
    slots: [slot₁/#self#(called) slot₂/kws slot₃/#self#(called)]
    1   (call slot₁/#self# slot₂/kws slot₃/#self# 1 1.0)
    2   (return %₁)
27  latestworld
28  (call core.typeof core.kwcall)
29  TestMod.f_kw_simple
30  (call core.Typeof %₂₉)
31  TestMod.Int
32  (call core.svec %₂₈ core.NamedTuple %₃₀ %₃₁)
33  (call core.svec)
34  SourceLocation::1:10
35  (call core.svec %₃₂ %₃₃ %₃₄)
36  --- method core.nothing %₃₅
    slots: [slot₁/#self#(called) slot₂/kws slot₃/#self#(called) slot₄/a]
    1   (call slot₁/#self# slot₂/kws slot₃/#self# slot₄/a 1.0)
    2   (return %₁)
37  latestworld
38  (call core.typeof core.kwcall)
39  TestMod.f_kw_simple
40  (call core.Typeof %₃₉)
41  TestMod.Int
42  TestMod.Float64
43  (call core.svec %₃₈ core.NamedTuple %₄₀ %₄₁ %₄₂)
44  (call core.svec)
45  SourceLocation::1:10
46  (call core.svec %₄₃ %₄₄ %₄₅)
47  --- method core.nothing %₄₆
    slots: [slot₁/#self#(called) slot₂/kws slot₃/#self#(called) slot₄/a slot₅/b slot₆/kwtmp slot₇/x(!read) slot₈/y(!read)]
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
48  latestworld
49  TestMod.f_kw_simple
50  (call core.Typeof %₄₉)
51  (call core.svec %₅₀)
52  (call core.svec)
53  SourceLocation::1:10
54  (call core.svec %₅₁ %₅₂ %₅₃)
55  --- method core.nothing %₅₄
    slots: [slot₁/#self#(called)]
    1   (call slot₁/#self# 1 1.0)
    2   (return %₁)
56  latestworld
57  TestMod.f_kw_simple
58  (call core.Typeof %₅₇)
59  TestMod.Int
60  (call core.svec %₅₈ %₅₉)
61  (call core.svec)
62  SourceLocation::1:10
63  (call core.svec %₆₀ %₆₁ %₆₂)
64  --- method core.nothing %₆₃
    slots: [slot₁/#self#(called) slot₂/a]
    1   (call slot₁/#self# slot₂/a 1.0)
    2   (return %₁)
65  latestworld
66  TestMod.f_kw_simple
67  (call core.Typeof %₆₆)
68  TestMod.Int
69  TestMod.Float64
70  (call core.svec %₆₇ %₆₈ %₆₉)
71  (call core.svec)
72  SourceLocation::1:10
73  (call core.svec %₇₀ %₇₁ %₇₂)
74  --- method core.nothing %₇₃
    slots: [slot₁/#self#(called) slot₂/a slot₃/b]
    1   TestMod.#f_kw_simple#0
    2   (call %₁ 'a' true slot₁/#self# slot₂/a slot₃/b)
    3   (return %₂)
75  latestworld
76  TestMod.f_kw_simple
77  (return %₇₆)

########################################
# Error: Duplicate keyword placeholder name
function f_kw_placeholders(; _=1, _=2); end
#---------------------
LoweringError:
function f_kw_placeholders(; _=1, _=2); end
#                                 ╙ ── function argument name not unique

########################################
# Keyword slurping - simple forwarding of all kws
function f_kw_slurp_simple(; all_kws...)
    all_kws
end
#---------------------
1   (method TestMod.f_kw_slurp_simple)
2   latestworld
3   (method TestMod.#f_kw_slurp_simple#0)
4   latestworld
5   TestMod.#f_kw_slurp_simple#0
6   (call core.Typeof %₅)
7   (call top.pairs core.NamedTuple)
8   TestMod.f_kw_slurp_simple
9   (call core.Typeof %₈)
10  (call core.svec %₆ %₇ %₉)
11  (call core.svec)
12  SourceLocation::1:10
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/all_kws slot₃/#self#]
    1   (meta :nkw 1)
    2   slot₂/all_kws
    3   (return %₂)
15  latestworld
16  (call core.typeof core.kwcall)
17  TestMod.f_kw_slurp_simple
18  (call core.Typeof %₁₇)
19  (call core.svec %₁₆ core.NamedTuple %₁₈)
20  (call core.svec)
21  SourceLocation::1:10
22  (call core.svec %₁₉ %₂₀ %₂₁)
23  --- method core.nothing %₂₂
    slots: [slot₁/#self#(!read) slot₂/kws slot₃/#self# slot₄/all_kws(!read)]
    1   (newvar slot₄/all_kws)
    2   (call top.pairs slot₂/kws)
    3   TestMod.#f_kw_slurp_simple#0
    4   (call %₃ %₂ slot₃/#self#)
    5   (return %₄)
24  latestworld
25  TestMod.f_kw_slurp_simple
26  (call core.Typeof %₂₅)
27  (call core.svec %₂₆)
28  (call core.svec)
29  SourceLocation::1:10
30  (call core.svec %₂₇ %₂₈ %₂₉)
31  --- method core.nothing %₃₀
    slots: [slot₁/#self#]
    1   TestMod.#f_kw_slurp_simple#0
    2   (call core.NamedTuple)
    3   (call top.pairs %₂)
    4   (call %₁ %₃ slot₁/#self#)
    5   (return %₄)
32  latestworld
33  TestMod.f_kw_slurp_simple
34  (return %₃₃)

########################################
# Keyword slurping
function f_kw_slurp(; x=x_default, non_x_kws...)
    all_kws
end
#---------------------
1   (method TestMod.f_kw_slurp)
2   latestworld
3   (method TestMod.#f_kw_slurp#0)
4   latestworld
5   TestMod.#f_kw_slurp#0
6   (call core.Typeof %₅)
7   (call top.pairs core.NamedTuple)
8   TestMod.f_kw_slurp
9   (call core.Typeof %₈)
10  (call core.svec %₆ core.Any %₇ %₉)
11  (call core.svec)
12  SourceLocation::1:10
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/non_x_kws(!read) slot₄/#self#]
    1   (meta :nkw 2)
    2   TestMod.all_kws
    3   (return %₂)
15  latestworld
16  (call core.typeof core.kwcall)
17  TestMod.f_kw_slurp
18  (call core.Typeof %₁₇)
19  (call core.svec %₁₆ core.NamedTuple %₁₈)
20  (call core.svec)
21  SourceLocation::1:10
22  (call core.svec %₁₉ %₂₀ %₂₁)
23  --- method core.nothing %₂₂
    slots: [slot₁/#self#(!read) slot₂/kws slot₃/#self# slot₄/kwtmp slot₅/x(!read) slot₆/non_x_kws(!read)]
    1   (newvar slot₅/x)
    2   (newvar slot₆/non_x_kws)
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
24  latestworld
25  TestMod.f_kw_slurp
26  (call core.Typeof %₂₅)
27  (call core.svec %₂₆)
28  (call core.svec)
29  SourceLocation::1:10
30  (call core.svec %₂₇ %₂₈ %₂₉)
31  --- method core.nothing %₃₀
    slots: [slot₁/#self#]
    1   TestMod.#f_kw_slurp#0
    2   TestMod.x_default
    3   (call core.NamedTuple)
    4   (call top.pairs %₃)
    5   (call %₁ %₂ %₄ slot₁/#self#)
    6   (return %₅)
32  latestworld
33  TestMod.f_kw_slurp
34  (return %₃₃)

########################################
# Keyword slurping with defaults depending on keyword names
# This tests the case where use_ssa_kw_temps=false because a keyword default
# depends on another keyword name. The slurp argument should not be included
# in kw_val_vars to avoid creating an unwanted global binding.
function f_kw_slurp_dep(; a=1, b=a, kws...)
    (a, b, kws)
end
#---------------------
1   (method TestMod.f_kw_slurp_dep)
2   latestworld
3   (method TestMod.#f_kw_slurp_dep#0)
4   latestworld
5   TestMod.#f_kw_slurp_dep#0
6   (call core.Typeof %₅)
7   (call top.pairs core.NamedTuple)
8   TestMod.f_kw_slurp_dep
9   (call core.Typeof %₈)
10  (call core.svec %₆ core.Any core.Any %₇ %₉)
11  (call core.svec)
12  SourceLocation::1:10
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/a slot₃/b slot₄/kws slot₅/#self#]
    1   (meta :nkw 3)
    2   (call core.tuple slot₂/a slot₃/b slot₄/kws)
    3   (return %₂)
15  latestworld
16  (call core.typeof core.kwcall)
17  TestMod.f_kw_slurp_dep
18  (call core.Typeof %₁₇)
19  (call core.svec %₁₆ core.NamedTuple %₁₈)
20  (call core.svec)
21  SourceLocation::1:10
22  (call core.svec %₁₉ %₂₀ %₂₁)
23  --- method core.nothing %₂₂
    slots: [slot₁/#self#(!read) slot₂/kws slot₃/#self# slot₄/kwtmp slot₅/a(single_assign) slot₆/b(single_assign)]
    1   (call core.isdefined slot₂/kws :a)
    2   (gotoifnot %₁ label₆)
    3   (call core.getfield slot₂/kws :a)
    4   (= slot₄/kwtmp %₃)
    5   (goto label₇)
    6   (= slot₄/kwtmp 1)
    7   slot₄/kwtmp
    8   (= slot₅/a %₇)
    9   (call core.isdefined slot₂/kws :b)
    10  (gotoifnot %₉ label₁₄)
    11  (call core.getfield slot₂/kws :b)
    12  (= slot₄/kwtmp %₁₁)
    13  (goto label₁₆)
    14  slot₅/a
    15  (= slot₄/kwtmp %₁₄)
    16  slot₄/kwtmp
    17  (= slot₆/b %₁₆)
    18  (call core.tuple :a :b)
    19  (call core.apply_type core.NamedTuple %₁₈)
    20  (call top.structdiff slot₂/kws %₁₉)
    21  (call top.pairs %₂₀)
    22  TestMod.#f_kw_slurp_dep#0
    23  (call %₂₂ slot₅/a slot₆/b %₂₁ slot₃/#self#)
    24  (return %₂₃)
24  latestworld
25  TestMod.f_kw_slurp_dep
26  (call core.Typeof %₂₅)
27  (call core.svec %₂₆)
28  (call core.svec)
29  SourceLocation::1:10
30  (call core.svec %₂₇ %₂₈ %₂₉)
31  --- method core.nothing %₃₀
    slots: [slot₁/#self# slot₂/a(single_assign) slot₃/b(single_assign) slot₄/kws(single_assign)]
    1   1
    2   (= slot₂/a %₁)
    3   slot₂/a
    4   (= slot₃/b %₃)
    5   (call core.NamedTuple)
    6   (call top.pairs %₅)
    7   (= slot₄/kws %₆)
    8   TestMod.#f_kw_slurp_dep#0
    9   (call %₈ slot₂/a slot₃/b slot₄/kws slot₁/#self#)
    10  (return %₉)
32  latestworld
33  TestMod.f_kw_slurp_dep
34  (return %₃₃)

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
2   latestworld
3   (method TestMod.#f_kw_sparams#0)
4   latestworld
5   (= slot₁/X (call core.TypeVar :X))
6   (= slot₂/A (call core.TypeVar :A))
7   TestMod.#f_kw_sparams#0
8   (call core.Typeof %₇)
9   slot₂/A
10  slot₁/X
11  TestMod.f_kw_sparams
12  (call core.Typeof %₁₁)
13  slot₁/X
14  (call core.svec %₈ %₉ %₁₀ %₁₂ %₁₃)
15  slot₁/X
16  slot₂/A
17  (call core.svec %₁₅ %₁₆)
18  SourceLocation::1:10
19  (call core.svec %₁₄ %₁₇ %₁₈)
20  --- method core.nothing %₁₉
    slots: [slot₁/#self#(!read) slot₂/a(!read) slot₃/b(!read) slot₄/#self# slot₅/x(!read)]
    1   (meta :nkw 2)
    2   static_parameter₁
    3   static_parameter₂
    4   (call core.tuple %₂ %₃)
    5   (return %₄)
21  latestworld
22  (= slot₃/X (call core.TypeVar :X))
23  (= slot₄/A (call core.TypeVar :A))
24  (call core.typeof core.kwcall)
25  TestMod.f_kw_sparams
26  (call core.Typeof %₂₅)
27  slot₃/X
28  (call core.svec %₂₄ core.NamedTuple %₂₆ %₂₇)
29  slot₃/X
30  (call core.svec %₂₉)
31  SourceLocation::1:10
32  (call core.svec %₂₈ %₃₀ %₃₁)
33  --- method core.nothing %₃₂
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
34  latestworld
35  (= slot₅/X (call core.TypeVar :X))
36  (= slot₆/A (call core.TypeVar :A))
37  TestMod.f_kw_sparams
38  (call core.Typeof %₃₇)
39  slot₅/X
40  (call core.svec %₃₈ %₃₉)
41  slot₅/X
42  (call core.svec %₄₁)
43  SourceLocation::1:10
44  (call core.svec %₄₀ %₄₂ %₄₃)
45  --- method core.nothing %₄₄
    slots: [slot₁/#self# slot₂/x]
    1   TestMod.#f_kw_sparams#0
    2   TestMod.a_def
    3   TestMod.b_def
    4   (call %₁ %₂ %₃ slot₁/#self# slot₂/x)
    5   (return %₄)
46  latestworld
47  TestMod.f_kw_sparams
48  (return %₄₇)

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
#                        └───┘ ── expected identifier or `identifier::type`
end

########################################
# Error: keyword slurping combined with a default
function f_kw_slurp_default(; kws...=def)
end
#---------------------
LoweringError:
function f_kw_slurp_default(; kws...=def)
#                             └────┘ ── expected identifier or `identifier::type`
end

########################################
# Error: keyword slurping combined with type
function f_kw_slurp_type(; kws::T...)
end
#---------------------
LoweringError:
function f_kw_slurp_type(; kws::T...)
#                          └────┘ ── keyword parameter with `...` may not be given a type
end

########################################
# Error: keyword slurping on non-final argument
function f_kw_slurp_not_last(; kws..., x=1)
end
#---------------------
LoweringError:
function f_kw_slurp_not_last(; kws..., x=1)
#                              └────┘ ── `...` may only be used for the final keyword parameter
end

########################################
# Fully generated function
@generated function f_only_generated(x, y)
    generator_code(x,y)
end
#---------------------
1   (method TestMod.f_only_generated)
2   latestworld
3   (method TestMod.#f_only_generated@generator#0)
4   latestworld
5   TestMod.#f_only_generated@generator#0
6   (call core.Typeof %₅)
7   (call core.svec %₆ JuliaLowering.MacroContext core.Any core.Any core.Any)
8   (call core.svec)
9   SourceLocation::1:21
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read) slot₂/__context__(!read) slot₃/#self#(!read) slot₄/x(nospecialize) slot₅/y(nospecialize)]
    1   TestMod.generator_code
    2   (call %₁ slot₄/x slot₅/y)
    3   (call core.tuple %₂)
    4   (call JuliaLowering.interpolate_ast SyntaxTree (inert_syntaxtree (block ($ (block (call generator_code x y))))) %₃)
    5   (return %₄)
12  latestworld
13  TestMod.f_only_generated
14  (call core.Typeof %₁₃)
15  (call core.svec %₁₄ core.Any core.Any)
16  (call core.svec)
17  SourceLocation::1:21
18  (call core.svec %₁₅ %₁₆ %₁₇)
19  --- method core.nothing %₁₈
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/y(!read)]
    1   (meta :generated (new JuliaLowering.GeneratedFunctionStub false TestMod.#f_only_generated@generator#0 SourceRef::1:1 (call core.svec :#self# :x :y) (call core.svec)))
    2   (meta :generated_only)
    3   (return core.nothing)
20  latestworld
21  TestMod.f_only_generated
22  (return %₂₁)

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
2   latestworld
3   (method TestMod.#f_partially_generated@generator#0)
4   latestworld
5   TestMod.#f_partially_generated@generator#0
6   (call core.Typeof %₅)
7   (call core.svec %₆ JuliaLowering.MacroContext core.Any core.Any core.Any)
8   (call core.svec)
9   SourceLocation::1:10
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read) slot₂/__context__(!read) slot₃/#self#(!read) slot₄/x(nospecialize,!read) slot₅/y(nospecialize,!read)]
    1   (call JuliaLowering.interpolate_ast SyntaxTree (inert_syntaxtree (block (= maybe_gen_stuff (call some_gen_stuff x y)))))
    2   (call core.tuple %₁)
    3   (call JuliaLowering.interpolate_ast SyntaxTree (inert_syntaxtree (block (block (= nongen_stuff (call bothgen x y)) ($ (block (call JuliaLowering.interpolate_ast SyntaxTree (inert_syntaxtree (block (= maybe_gen_stuff (call some_gen_stuff x y))))))) (tuple-p nongen_stuff maybe_gen_stuff)))) %₂)
    4   (return %₃)
12  latestworld
13  TestMod.f_partially_generated
14  (call core.Typeof %₁₃)
15  (call core.svec %₁₄ core.Any core.Any)
16  (call core.svec)
17  SourceLocation::1:10
18  (call core.svec %₁₅ %₁₆ %₁₇)
19  --- method core.nothing %₁₈
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y slot₄/maybe_gen_stuff(single_assign) slot₅/nongen_stuff(single_assign)]
    1   (meta :generated (new JuliaLowering.GeneratedFunctionStub false TestMod.#f_partially_generated@generator#0 SourceRef::1:37 (call core.svec :#self# :x :y) (call core.svec)))
    2   TestMod.bothgen
    3   (= slot₅/nongen_stuff (call %₂ slot₂/x slot₃/y))
    4   TestMod.some_nongen_stuff
    5   (= slot₄/maybe_gen_stuff (call %₄ slot₂/x slot₃/y))
    6   slot₅/nongen_stuff
    7   slot₄/maybe_gen_stuff
    8   (call core.tuple %₆ %₇)
    9   (return %₈)
20  latestworld
21  TestMod.f_partially_generated
22  (return %₂₁)

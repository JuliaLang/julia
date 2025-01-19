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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/x slot₃/_(!read) slot₄/y]
    1   TestMod.+
    2   (call %₁ slot₂/x slot₄/y)
    3   (return %₂)
8   TestMod.f
9   (return %₈)

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
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 1 =#)))))
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/_(!read) slot₃/x]
    1   slot₃/x
    2   (return %₁)
9   TestMod.f
10  (return %₉)

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
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 1 =#)))))
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/y(!read)]
    1   TestMod.body
    2   (return %₁)
9   TestMod.f
10  (return %₉)

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
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 1 =#)))))
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/ys(!read)]
    1   TestMod.body
    2   (return %₁)
9   TestMod.f
10  (return %₉)

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
8   (call core.svec %₆ %₇ :($(QuoteNode(:(#= line 1 =#)))))
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/ys(!read)]
    1   TestMod.body
    2   (return %₁)
10  TestMod.f
11  (return %₁₀)

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
15  (call core.svec %₁₀ %₁₄ :($(QuoteNode(:(#= line 1 =#)))))
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/_(!read) slot₃/_(!read) slot₄/_(!read)]
    1   static_parameter₃
    2   static_parameter₁
    3   static_parameter₂
    4   (call core.tuple %₁ %₂ %₃)
    5   (return %₄)
17  TestMod.f
18  (return %₁₇)

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
13  (call core.svec %₁₀ %₁₂ :($(QuoteNode(:(#= line 1 =#)))))
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/_(!read)]
    1   static_parameter₁
    2   (return %₁)
15  TestMod.f
16  (return %₁₅)

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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
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
8   TestMod.f
9   (return %₈)

########################################
# Callable type
function (::T)(x)
    x
end
#---------------------
1   TestMod.T
2   (call core.svec %₁ core.Any)
3   (call core.svec)
4   (call core.svec %₂ %₃ :($(QuoteNode(:(#= line 1 =#)))))
5   --- method core.nothing %₄
    slots: [slot₁/#self#(!read) slot₂/x]
    1   slot₂/x
    2   (return %₁)
6   (return core.nothing)

########################################
# Callable type with instance
function (y::T)(x)
    (y, x)
end
#---------------------
1   TestMod.T
2   (call core.svec %₁ core.Any)
3   (call core.svec)
4   (call core.svec %₂ %₃ :($(QuoteNode(:(#= line 1 =#)))))
5   --- method core.nothing %₄
    slots: [slot₁/y slot₂/x]
    1   (call core.tuple slot₁/y slot₂/x)
    2   (return %₁)
6   (return core.nothing)

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
8   (call core.svec %₅ %₇ :($(QuoteNode(:(#= line 1 =#)))))
9   --- method core.nothing %₈
    slots: [slot₁/x(!read)]
    1   static_parameter₁
    2   (return %₁)
10  (return core.nothing)

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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read)]
    1   (return core.nothing)
8   (return core.nothing)

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
# Keyword calls
f(x; a=1, b=2)
#---------------------
1   TestMod.f
2   (call core.tuple :a :b)
3   (call core.apply_type core.NamedTuple %₂)
4   (call core.tuple 1 2)
5   (call %₃ %₄)
6   TestMod.x
7   (call core.kwcall %₅ %₁ %₆)
8   (return %₇)

########################################
# Keyword call with only splats for kws
f(; ks1..., ks2...)
#---------------------
1   TestMod.f
2   (call core.NamedTuple)
3   TestMod.ks1
4   (call top.merge %₂ %₃)
5   TestMod.ks2
6   (call top.merge %₄ %₅)
7   (call top.isempty %₆)
8   (gotoifnot %₇ label₁₁)
9   (call %₁)
10  (return %₉)
11  (call core.kwcall %₆ %₁)
12  (return %₁₁)

########################################
# Error: Call with repeated keywords
f(x; a=1, a=2)
#---------------------
LoweringError:
f(x; a=1, a=2)
#         ╙ ── Repeated keyword argument name

########################################
# literal_pow lowering
x^42
#---------------------
1   TestMod.^
2   TestMod.x
3   (call core.apply_type top.Val 42)
4   (call %₃)
5   (call top.literal_pow %₁ %₂ %₄)
6   (return %₅)

########################################
# almost but not quite literal_pow lowering :)
x^42.0
#---------------------
1   TestMod.^
2   TestMod.x
3   (call %₁ %₂ 42.0)
4   (return %₃)

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
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 1 =#)))))
8   --- method core.nothing %₇
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call slot₁/#self# slot₂/x 1 2)
    2   (return %₁)
9   TestMod.f
10  (call core.Typeof %₉)
11  TestMod.T
12  TestMod.S
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  (call core.svec)
15  (call core.svec %₁₃ %₁₄ :($(QuoteNode(:(#= line 1 =#)))))
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(called) slot₂/x slot₃/y]
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
17  TestMod.f
18  (call core.Typeof %₁₇)
19  TestMod.T
20  TestMod.S
21  TestMod.U
22  (call core.svec %₁₈ %₁₉ %₂₀ %₂₁)
23  (call core.svec)
24  (call core.svec %₂₂ %₂₃ :($(QuoteNode(:(#= line 1 =#)))))
25  --- method core.nothing %₂₄
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y slot₄/z(!read)]
    1   (call core.tuple slot₂/x slot₃/y)
    2   (return %₁)
26  TestMod.f
27  (return %₂₆)

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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(called)]
    1   (call slot₁/#self# 1)
    2   (return %₁)
8   TestMod.f
9   (call core.Typeof %₈)
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  (call core.svec %₁₀ %₁₁ :($(QuoteNode(:(#= line 1 =#)))))
13  --- method core.nothing %₁₂
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call slot₁/#self# slot₂/x slot₂/x)
    2   (return %₁)
14  TestMod.f
15  (call core.Typeof %₁₄)
16  (call core.svec %₁₅ core.Any core.Any)
17  (call core.svec)
18  (call core.svec %₁₆ %₁₇ :($(QuoteNode(:(#= line 1 =#)))))
19  --- method core.nothing %₁₈
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y]
    1   (call core.tuple slot₂/x slot₃/y)
    2   (return %₁)
20  TestMod.f
21  (return %₂₀)

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
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 1 =#)))))
8   --- method core.nothing %₇
    slots: [slot₁/#self#(called) slot₂/_]
    1   (call slot₁/#self# slot₂/_ 1 2)
    2   (return %₁)
9   TestMod.f
10  (call core.Typeof %₉)
11  TestMod.Int
12  (call core.svec %₁₀ %₁₁ core.Any)
13  (call core.svec)
14  (call core.svec %₁₂ %₁₃ :($(QuoteNode(:(#= line 1 =#)))))
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(called) slot₂/_ slot₃/y]
    1   (call slot₁/#self# slot₂/_ slot₃/y 2)
    2   (return %₁)
16  TestMod.f
17  (call core.Typeof %₁₆)
18  TestMod.Int
19  (call core.svec %₁₇ %₁₈ core.Any core.Any)
20  (call core.svec)
21  (call core.svec %₁₉ %₂₀ :($(QuoteNode(:(#= line 1 =#)))))
22  --- method core.nothing %₂₁
    slots: [slot₁/#self#(!read) slot₂/_(!read) slot₃/y slot₄/z]
    1   (call core.tuple slot₃/y slot₄/z)
    2   (return %₁)
23  TestMod.f
24  (return %₂₃)

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
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 1 =#)))))
8   --- method core.nothing %₇
    slots: [slot₁/#self#(called) slot₂/_]
    1   (call slot₁/#self# slot₂/_ 1)
    2   (return %₁)
9   TestMod.f
10  (call core.Typeof %₉)
11  TestMod.Int
12  (call core.svec %₁₀ %₁₁ core.Any)
13  (call core.svec)
14  (call core.svec %₁₂ %₁₃ :($(QuoteNode(:(#= line 1 =#)))))
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(!read) slot₂/_(!read) slot₃/x]
    1   slot₃/x
    2   (return %₁)
16  TestMod.f
17  (return %₁₆)

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
13  (call core.svec %₁₀ %₁₂ :($(QuoteNode(:(#= line 1 =#)))))
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call slot₁/#self# slot₂/x 1 2)
    2   (return %₁)
15  TestMod.f
16  (call core.Typeof %₁₅)
17  slot₂/T
18  slot₁/S
19  (call core.svec %₁₆ %₁₇ %₁₈)
20  slot₂/T
21  slot₁/S
22  (call core.svec %₂₀ %₂₁)
23  (call core.svec %₁₉ %₂₂ :($(QuoteNode(:(#= line 1 =#)))))
24  --- method core.nothing %₂₃
    slots: [slot₁/#self#(called) slot₂/x slot₃/y]
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
25  TestMod.f
26  (call core.Typeof %₂₅)
27  slot₂/T
28  slot₁/S
29  slot₃/U
30  (call core.svec %₂₆ %₂₇ %₂₈ %₂₉)
31  slot₂/T
32  slot₁/S
33  slot₃/U
34  (call core.svec %₃₁ %₃₂ %₃₃)
35  (call core.svec %₃₀ %₃₄ :($(QuoteNode(:(#= line 1 =#)))))
36  --- method core.nothing %₃₅
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y slot₄/z]
    1   (call core.tuple slot₂/x slot₃/y slot₄/z)
    2   (return %₁)
37  TestMod.f
38  (return %₃₇)

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
12  (call core.svec %₁₀ %₁₁ :($(QuoteNode(:(#= line 1 =#)))))
13  --- method core.nothing %₁₂
    slots: [slot₁/#self#(called) slot₂/x]
    1   (call top.vect 1)
    2   (call slot₁/#self# slot₂/x %₁ 2)
    3   (return %₂)
14  TestMod.f
15  (call core.Typeof %₁₄)
16  slot₁/S
17  (call core.svec %₁₅ core.Any %₁₆)
18  slot₂/T
19  slot₁/S
20  (call core.svec %₁₈ %₁₉)
21  (call core.svec %₁₇ %₂₀ :($(QuoteNode(:(#= line 1 =#)))))
22  --- method core.nothing %₂₁
    slots: [slot₁/#self#(called) slot₂/x slot₃/y]
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
23  TestMod.f
24  (call core.Typeof %₂₃)
25  slot₁/S
26  slot₃/U
27  (call core.svec %₂₄ core.Any %₂₅ %₂₆)
28  slot₂/T
29  slot₁/S
30  slot₃/U
31  (call core.svec %₂₈ %₂₉ %₃₀)
32  (call core.svec %₂₇ %₃₁ :($(QuoteNode(:(#= line 1 =#)))))
33  --- method core.nothing %₃₂
    slots: [slot₁/#self#(!read) slot₂/x slot₃/y slot₄/z]
    1   static_parameter₁
    2   static_parameter₂
    3   static_parameter₃
    4   (call core.tuple slot₂/x slot₃/y slot₄/z %₁ %₂ %₃)
    5   (return %₄)
34  TestMod.f
35  (return %₃₄)

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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(called)]
    1   (call slot₁/#self# 1)
    2   (return %₁)
8   TestMod.f
9   (call core.Typeof %₈)
10  (call core.apply_type core.Vararg core.Any)
11  (call core.svec %₉ core.Any %₁₀)
12  (call core.svec)
13  (call core.svec %₁₁ %₁₂ :($(QuoteNode(:(#= line 1 =#)))))
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/ys]
    1   slot₃/ys
    2   (return %₁)
15  TestMod.f
16  (return %₁₅)

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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(called)]
    1   (call slot₁/#self# 1)
    2   (return %₁)
8   TestMod.f
9   (call core.Typeof %₈)
10  (call core.apply_type core.Vararg core.Any)
11  (call core.svec %₉ %₁₀)
12  (call core.svec)
13  (call core.svec %₁₁ %₁₂ :($(QuoteNode(:(#= line 1 =#)))))
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/xs]
    1   slot₂/xs
    2   (return %₁)
15  TestMod.f
16  (return %₁₅)

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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#]
    1   (call core.tuple 1 2)
    2   (call core._apply_iterate top.iterate slot₁/#self# %₁)
    3   (return %₂)
8   TestMod.f
9   (call core.Typeof %₈)
10  (call core.apply_type core.Vararg core.Any)
11  (call core.svec %₉ %₁₀)
12  (call core.svec)
13  (call core.svec %₁₁ %₁₂ :($(QuoteNode(:(#= line 1 =#)))))
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/xs]
    1   slot₂/xs
    2   (return %₁)
15  TestMod.f
16  (return %₁₅)

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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/destructured_arg_2 slot₄/w(!read) slot₅/iterstate slot₆/y(!read) slot₇/z(!read)]
    1   (call top.indexed_iterate slot₃/destructured_arg_2 1)
    2   (= slot₆/y (call core.getfield %₁ 1))
    3   (= slot₅/iterstate (call core.getfield %₁ 2))
    4   slot₅/iterstate
    5   (call top.indexed_iterate slot₃/destructured_arg_2 2 %₄)
    6   (= slot₇/z (call core.getfield %₅ 1))
    7   (return core.nothing)
8   TestMod.f
9   (return %₈)

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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(called)]
    1   TestMod.rhs
    2   (call slot₁/#self# %₁)
    3   (return %₂)
8   TestMod.f
9   (call core.Typeof %₈)
10  TestMod.T
11  (call core.apply_type core.Vararg %₁₀)
12  (call core.svec %₉ %₁₁)
13  (call core.svec)
14  (call core.svec %₁₂ %₁₃ :($(QuoteNode(:(#= line 1 =#)))))
15  --- method core.nothing %₁₄
    slots: [slot₁/#self#(!read) slot₂/destructured_arg_1 slot₃/x(!read)]
    1   (call top.indexed_iterate slot₂/destructured_arg_1 1)
    2   (= slot₃/x (call core.getfield %₁ 1))
    3   (return core.nothing)
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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/destructured_arg_1 slot₃/destructured_arg_2]
    1   (call top.indexed_iterate slot₂/destructured_arg_1 1)
    2   (call core.getfield %₁ 1)
    3   (call top.indexed_iterate slot₃/destructured_arg_2 1)
    4   (call core.getfield %₃ 1)
    5   (return core.nothing)
8   TestMod.f
9   (return %₈)

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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/x(nospecialize,!read) slot₃/g(called) slot₄/y]
    1   TestMod.+
    2   (call slot₃/g)
    3   (call %₁ %₂ slot₄/y)
    4   (return %₃)
8   TestMod.f
9   (return %₈)

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
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 4 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read)]
    1   (return core.nothing)
8   TestMod.f
9   (call JuliaLowering.bind_docs! %₈ "some docs\n" %₆)
10  TestMod.f
11  (return %₁₀)

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
4   (call core.svec %₂ %₃ :($(QuoteNode(:(#= line 4 =#)))))
5   --- method core.nothing %₄
    slots: [slot₁/x(!read)]
    1   (return core.nothing)
6   TestMod.T
7   (call JuliaLowering.bind_docs! %₆ "some docs\n" %₄)
8   (return core.nothing)

########################################
# Error: infix call without enough arguments
@ast_ [K"call"(syntax_flags=JuliaSyntax.INFIX_FLAG)
    "x"::K"Identifier"
]
#---------------------
LoweringError:
#= line 1 =# - Postfix/infix operators must have at least two positional arguments

########################################
# Error: postfix call without enough arguments
@ast_ [K"call"(syntax_flags=JuliaSyntax.POSTFIX_OP_FLAG)
    "x"::K"Identifier"
]
#---------------------
LoweringError:
#= line 1 =# - Postfix/infix operators must have at least two positional arguments

########################################
# Error: Call with no function name
@ast_ [K"call"]
#---------------------
LoweringError:
#= line 1 =# - Call expressions must have a function name

########################################
# Simple broadcast
x .* y .+ f.(z)
#---------------------
1   TestMod.+
2   TestMod.*
3   TestMod.x
4   TestMod.y
5   (call top.broadcasted %₂ %₃ %₄)
6   TestMod.f
7   TestMod.z
8   (call top.broadcasted %₆ %₇)
9   (call top.broadcasted %₁ %₅ %₈)
10  (call top.materialize %₉)
11  (return %₁₀)

########################################
# Broadcast with unary function calls
.+x
#---------------------
1   TestMod.+
2   TestMod.x
3   (call top.broadcasted %₁ %₂)
4   (call top.materialize %₃)
5   (return %₄)

########################################
# Broadcast with short circuit operators
x .&& y .|| z
#---------------------
1   TestMod.x
2   TestMod.y
3   (call top.broadcasted top.andand %₁ %₂)
4   TestMod.z
5   (call top.broadcasted top.oror %₃ %₄)
6   (call top.materialize %₅)
7   (return %₆)

########################################
# Scalar comparison chain
x < y < z
#---------------------
1   TestMod.<
2   TestMod.x
3   TestMod.y
4   (call %₁ %₂ %₃)
5   (gotoifnot %₄ label₁₁)
6   TestMod.<
7   TestMod.y
8   TestMod.z
9   (call %₆ %₇ %₈)
10  (return %₉)
11  (return false)

########################################
# Broadcasted comparison chain
x .< y .< z
#---------------------
1   TestMod.<
2   TestMod.x
3   TestMod.y
4   (call top.broadcasted %₁ %₂ %₃)
5   TestMod.<
6   TestMod.y
7   TestMod.z
8   (call top.broadcasted %₅ %₆ %₇)
9   (call top.broadcasted top.& %₄ %₈)
10  (call top.materialize %₉)
11  (return %₁₀)

########################################
# Mixed scalar / broadcasted comparison chain
a < b < c .< d .< e
#---------------------
1   TestMod.<
2   TestMod.a
3   TestMod.b
4   (call %₁ %₂ %₃)
5   (gotoifnot %₄ label₁₁)
6   TestMod.<
7   TestMod.b
8   TestMod.c
9   (= slot₁/if_val (call %₆ %₇ %₈))
10  (goto label₁₂)
11  (= slot₁/if_val false)
12  slot₁/if_val
13  TestMod.<
14  TestMod.c
15  TestMod.d
16  (call top.broadcasted %₁₃ %₁₄ %₁₅)
17  (call top.broadcasted top.& %₁₂ %₁₆)
18  TestMod.<
19  TestMod.d
20  TestMod.e
21  (call top.broadcasted %₁₈ %₁₉ %₂₀)
22  (call top.broadcasted top.& %₁₇ %₂₁)
23  (call top.materialize %₂₂)
24  (return %₂₃)

########################################
# Mixed scalar / broadcasted comparison chain
a .< b .< c < d < e
#---------------------
1   TestMod.<
2   TestMod.a
3   TestMod.b
4   (call top.broadcasted %₁ %₂ %₃)
5   TestMod.<
6   TestMod.b
7   TestMod.c
8   (call top.broadcasted %₅ %₆ %₇)
9   (call top.broadcasted top.& %₄ %₈)
10  TestMod.<
11  TestMod.c
12  TestMod.d
13  (call %₁₀ %₁₁ %₁₂)
14  (gotoifnot %₁₃ label₂₀)
15  TestMod.<
16  TestMod.d
17  TestMod.e
18  (= slot₁/if_val (call %₁₅ %₁₆ %₁₇))
19  (goto label₂₁)
20  (= slot₁/if_val false)
21  slot₁/if_val
22  (call top.broadcasted top.& %₉ %₂₁)
23  (call top.materialize %₂₂)
24  (return %₂₃)

########################################
# Comparison chain fused with other broadcasting
x .+ (a .< b .< c)
#---------------------
1   TestMod.+
2   TestMod.x
3   TestMod.<
4   TestMod.a
5   TestMod.b
6   (call top.broadcasted %₃ %₄ %₅)
7   TestMod.<
8   TestMod.b
9   TestMod.c
10  (call top.broadcasted %₇ %₈ %₉)
11  (call top.broadcasted top.& %₆ %₁₀)
12  (call top.broadcasted %₁ %₂ %₁₁)
13  (call top.materialize %₁₂)
14  (return %₁₃)

########################################
# Broadcast with literal_pow
x.^3
#---------------------
1   TestMod.^
2   TestMod.x
3   (call core.apply_type top.Val 3)
4   (call %₃)
5   (call top.broadcasted top.literal_pow %₁ %₂ %₄)
6   (call top.materialize %₅)
7   (return %₆)

########################################
# Broadcast with keywords
f.(x, y, z = 1; w = 2)
#---------------------
1   top.broadcasted_kwsyntax
2   (call core.tuple :z :w)
3   (call core.apply_type core.NamedTuple %₂)
4   (call core.tuple 1 2)
5   (call %₃ %₄)
6   TestMod.f
7   TestMod.x
8   TestMod.y
9   (call core.kwcall %₅ %₁ %₆ %₇ %₈)
10  (call top.materialize %₉)
11  (return %₁₀)

########################################
# Broadcast with unary dot syntax
(.+)(x,y)
#---------------------
1   TestMod.+
2   TestMod.x
3   TestMod.y
4   (call top.broadcasted %₁ %₂ %₃)
5   (call top.materialize %₄)
6   (return %₅)

########################################
# Trivial in-place broadcast update
x .= y
#---------------------
1   TestMod.x
2   TestMod.y
3   (call top.broadcasted top.identity %₂)
4   (call top.materialize! %₁ %₃)
5   (return %₄)

########################################
# Fused in-place broadcast update
x .= y .+ z
#---------------------
1   TestMod.x
2   TestMod.+
3   TestMod.y
4   TestMod.z
5   (call top.broadcasted %₂ %₃ %₄)
6   (call top.materialize! %₁ %₅)
7   (return %₆)

########################################
# In-place broadcast update with property assignment on left hand side
x.prop .= y
#---------------------
1   TestMod.x
2   (call top.dotgetproperty %₁ :prop)
3   TestMod.y
4   (call top.broadcasted top.identity %₃)
5   (call top.materialize! %₂ %₄)
6   (return %₅)

########################################
# In-place broadcast update with ref on left hand side
x[i,end] .= y
#---------------------
1   TestMod.x
2   TestMod.i
3   (call top.lastindex %₁ 2)
4   (call top.dotview %₁ %₂ %₃)
5   TestMod.y
6   (call top.broadcasted top.identity %₅)
7   (call top.materialize! %₄ %₆)
8   (return %₇)


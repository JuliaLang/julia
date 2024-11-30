########################################
# Functions with placeholder arg
function f(x, _, y)
    x + y
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂ core.Any core.Any core.Any)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    1   TestMod.+
    2   (call %₁ slot₂/x slot₄/y)
    3   (return %₂)
7   (return %₁)

########################################
# Functions with argument types only, no name
function f(::T, x)
    x
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   TestMod.T
4   (call core.svec %₂ %₃ core.Any)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    1   slot₃/x
    2   (return %₁)
8   (return %₁)

########################################
# Functions argument types
function f(x, y::T)
    body
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   TestMod.T
4   (call core.svec %₂ core.Any %₃)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    1   TestMod.body
    2   (return %₁)
8   (return %₁)

########################################
# Functions with slurp of Any
function f(x, ys...)
    body
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.apply_type core.Vararg core.Any)
4   (call core.svec %₂ core.Any %₃)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    1   TestMod.body
    2   (return %₁)
8   (return %₁)

########################################
# Functions with slurp of T
function f(x, ys::T...)
    body
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   TestMod.T
4   (call core.apply_type core.Vararg %₃)
5   (call core.svec %₂ core.Any %₄)
6   (call core.svec)
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 1 =#)))))
8   --- method core.nothing %₇
    1   TestMod.body
    2   (return %₁)
9   (return %₁)

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
1   (= slot₂/U (call core.TypeVar :U))
2   (= slot₃/V (call core.TypeVar :V))
3   (= slot₁/T (call core.TypeVar :T))
4   (method :f)
5   (call core.Typeof %₄)
6   slot₁/T
7   slot₂/U
8   slot₃/V
9   (call core.svec %₅ %₆ %₇ %₈)
10  slot₂/U
11  slot₃/V
12  slot₁/T
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  (call core.svec %₉ %₁₃ :($(QuoteNode(:(#= line 1 =#)))))
15  --- method core.nothing %₁₄
    1   static_parameter₃
    2   static_parameter₁
    3   static_parameter₂
    4   (call core.tuple %₁ %₂ %₃)
    5   (return %₄)
16  (return %₄)

########################################
# Static parameter with bounds and used with apply_type in argument
function f(::S{T}) where X <: T <: Y
    T
end
#---------------------
1   TestMod.X
2   TestMod.Y
3   (= slot₁/T (call core.TypeVar :T %₁ %₂))
4   (method :f)
5   (call core.Typeof %₄)
6   TestMod.S
7   slot₁/T
8   (call core.apply_type %₆ %₇)
9   (call core.svec %₅ %₈)
10  slot₁/T
11  (call core.svec %₁₀)
12  (call core.svec %₉ %₁₁ :($(QuoteNode(:(#= line 1 =#)))))
13  --- method core.nothing %₁₂
    1   static_parameter₁
    2   (return %₁)
14  (return %₄)

########################################
# Return types
function f(x)::Int
    if x
        42.0
    end
    0xff
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂ core.Any)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
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
7   (return %₁)

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
    1   slot₂/x
    2   (return %₁)
6   (return %₁)

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
    1   (call core.tuple slot₁/y slot₂/x)
    2   (return %₁)
6   (return %₁)

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
    1   (return core.nothing)
8   (return %₂)

########################################
# Error: Invalid function name
function ccall()
end
#---------------------
LoweringError:
function ccall()
#        └───┘ ── Invalid function name
end

########################################
# Error: Invalid function name
function A.ccall()
end
#---------------------
LoweringError:
function A.ccall()
#        └─────┘ ── Invalid function name
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
1   (method :f)
2   (call core.Typeof %₁)
3   TestMod.T
4   (call core.svec %₂ %₃)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    1   (call slot₁/#self# slot₂/x 1 2)
    2   (return %₁)
8   (call core.Typeof %₁)
9   TestMod.T
10  TestMod.S
11  (call core.svec %₈ %₉ %₁₀)
12  (call core.svec)
13  (call core.svec %₁₁ %₁₂ :($(QuoteNode(:(#= line 1 =#)))))
14  --- method core.nothing %₁₃
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
15  (call core.Typeof %₁)
16  TestMod.T
17  TestMod.S
18  TestMod.U
19  (call core.svec %₁₅ %₁₆ %₁₇ %₁₈)
20  (call core.svec)
21  (call core.svec %₁₉ %₂₀ :($(QuoteNode(:(#= line 1 =#)))))
22  --- method core.nothing %₂₁
    1   (call core.tuple slot₂/x slot₃/y)
    2   (return %₁)
23  (return %₁)

########################################
# Default positional args which depend on other args
function f(x=1, y=x)
    (x,y)
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    1   (call slot₁/#self# 1)
    2   (return %₁)
7   (call core.Typeof %₁)
8   (call core.svec %₇ core.Any)
9   (call core.svec)
10  (call core.svec %₈ %₉ :($(QuoteNode(:(#= line 1 =#)))))
11  --- method core.nothing %₁₀
    1   (call slot₁/#self# slot₂/x slot₂/x)
    2   (return %₁)
12  (call core.Typeof %₁)
13  (call core.svec %₁₂ core.Any core.Any)
14  (call core.svec)
15  (call core.svec %₁₃ %₁₄ :($(QuoteNode(:(#= line 1 =#)))))
16  --- method core.nothing %₁₅
    1   (call core.tuple slot₂/x slot₃/y)
    2   (return %₁)
17  (return %₁)

########################################
# Default positional args with missing arg names (implicit placeholders)
function f(::Int, y=1, z=2)
    (y, z)
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   TestMod.Int
4   (call core.svec %₂ %₃)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    1   (call slot₁/#self# slot₂/_ 1 2)
    2   (return %₁)
8   (call core.Typeof %₁)
9   TestMod.Int
10  (call core.svec %₈ %₉ core.Any)
11  (call core.svec)
12  (call core.svec %₁₀ %₁₁ :($(QuoteNode(:(#= line 1 =#)))))
13  --- method core.nothing %₁₂
    1   (call slot₁/#self# slot₂/_ slot₃/y 2)
    2   (return %₁)
14  (call core.Typeof %₁)
15  TestMod.Int
16  (call core.svec %₁₄ %₁₅ core.Any core.Any)
17  (call core.svec)
18  (call core.svec %₁₆ %₁₇ :($(QuoteNode(:(#= line 1 =#)))))
19  --- method core.nothing %₁₈
    1   (call core.tuple slot₃/y slot₄/z)
    2   (return %₁)
20  (return %₁)

########################################
# Default positional args with placeholders
function f(_::Int, x=1)
    x
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   TestMod.Int
4   (call core.svec %₂ %₃)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    1   (call slot₁/#self# slot₂/_ 1)
    2   (return %₁)
8   (call core.Typeof %₁)
9   TestMod.Int
10  (call core.svec %₈ %₉ core.Any)
11  (call core.svec)
12  (call core.svec %₁₀ %₁₁ :($(QuoteNode(:(#= line 1 =#)))))
13  --- method core.nothing %₁₂
    1   slot₃/x
    2   (return %₁)
14  (return %₁)

########################################
# Positional args with defaults and `where` clauses
function f(x::T, y::S=1, z::U=2) where {T,S<:T,U<:S}
    (x,y,z)
end
#---------------------
1   (= slot₂/T (call core.TypeVar :T))
2   slot₂/T
3   (= slot₁/S (call core.TypeVar :S %₂))
4   slot₁/S
5   (= slot₃/U (call core.TypeVar :U %₄))
6   (method :f)
7   (call core.Typeof %₆)
8   slot₂/T
9   (call core.svec %₇ %₈)
10  slot₂/T
11  (call core.svec %₁₀)
12  (call core.svec %₉ %₁₁ :($(QuoteNode(:(#= line 1 =#)))))
13  --- method core.nothing %₁₂
    1   (call slot₁/#self# slot₂/x 1 2)
    2   (return %₁)
14  (call core.Typeof %₆)
15  slot₂/T
16  slot₁/S
17  (call core.svec %₁₄ %₁₅ %₁₆)
18  slot₂/T
19  slot₁/S
20  (call core.svec %₁₈ %₁₉)
21  (call core.svec %₁₇ %₂₀ :($(QuoteNode(:(#= line 1 =#)))))
22  --- method core.nothing %₂₁
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
23  (call core.Typeof %₆)
24  slot₂/T
25  slot₁/S
26  slot₃/U
27  (call core.svec %₂₃ %₂₄ %₂₅ %₂₆)
28  slot₂/T
29  slot₁/S
30  slot₃/U
31  (call core.svec %₂₈ %₂₉ %₃₀)
32  (call core.svec %₂₇ %₃₁ :($(QuoteNode(:(#= line 1 =#)))))
33  --- method core.nothing %₃₂
    1   (call core.tuple slot₂/x slot₃/y slot₄/z)
    2   (return %₁)
34  (return %₆)

########################################
# Positional args and type parameters with transitive dependencies
# See https://github.com/JuliaLang/julia/issues/49275 - the first method
# generated here for only `x` should contain zero type parameters.
function f(x, y::S=[1], z::U=2) where {T, S<:AbstractVector{T}, U}
    (x, y, z, T, S, U)
end
#---------------------
1   (= slot₂/T (call core.TypeVar :T))
2   TestMod.AbstractVector
3   slot₂/T
4   (call core.apply_type %₂ %₃)
5   (= slot₁/S (call core.TypeVar :S %₄))
6   (= slot₃/U (call core.TypeVar :U))
7   (method :f)
8   (call core.Typeof %₇)
9   (call core.svec %₈ core.Any)
10  (call core.svec)
11  (call core.svec %₉ %₁₀ :($(QuoteNode(:(#= line 1 =#)))))
12  --- method core.nothing %₁₁
    1   (call top.vect 1)
    2   (call slot₁/#self# slot₂/x %₁ 2)
    3   (return %₂)
13  (call core.Typeof %₇)
14  slot₁/S
15  (call core.svec %₁₃ core.Any %₁₄)
16  slot₂/T
17  slot₁/S
18  (call core.svec %₁₆ %₁₇)
19  (call core.svec %₁₅ %₁₈ :($(QuoteNode(:(#= line 1 =#)))))
20  --- method core.nothing %₁₉
    1   (call slot₁/#self# slot₂/x slot₃/y 2)
    2   (return %₁)
21  (call core.Typeof %₇)
22  slot₁/S
23  slot₃/U
24  (call core.svec %₂₁ core.Any %₂₂ %₂₃)
25  slot₂/T
26  slot₁/S
27  slot₃/U
28  (call core.svec %₂₅ %₂₆ %₂₇)
29  (call core.svec %₂₄ %₂₈ :($(QuoteNode(:(#= line 1 =#)))))
30  --- method core.nothing %₂₉
    1   static_parameter₁
    2   static_parameter₂
    3   static_parameter₃
    4   (call core.tuple slot₂/x slot₃/y slot₄/z %₁ %₂ %₃)
    5   (return %₄)
31  (return %₇)

########################################
# Default positional args are allowed before trailing slurp with no default
function f(x=1, ys...)
    ys
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    1   (call slot₁/#self# 1)
    2   (return %₁)
7   (call core.Typeof %₁)
8   (call core.apply_type core.Vararg core.Any)
9   (call core.svec %₇ core.Any %₈)
10  (call core.svec)
11  (call core.svec %₉ %₁₀ :($(QuoteNode(:(#= line 1 =#)))))
12  --- method core.nothing %₁₁
    1   slot₃/ys
    2   (return %₁)
13  (return %₁)

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
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    1   (call slot₁/#self# 1)
    2   (return %₁)
7   (call core.Typeof %₁)
8   (call core.apply_type core.Vararg core.Any)
9   (call core.svec %₇ %₈)
10  (call core.svec)
11  (call core.svec %₉ %₁₀ :($(QuoteNode(:(#= line 1 =#)))))
12  --- method core.nothing %₁₁
    1   slot₂/xs
    2   (return %₁)
13  (return %₁)

########################################
# Positional arg with slurp and splatted default value
function f(xs...=(1,2)...)
    xs
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    1   (call core.tuple 1 2)
    2   (call core._apply_iterate top.iterate slot₁/#self# %₁)
    3   (return %₂)
7   (call core.Typeof %₁)
8   (call core.apply_type core.Vararg core.Any)
9   (call core.svec %₇ %₈)
10  (call core.svec)
11  (call core.svec %₉ %₁₀ :($(QuoteNode(:(#= line 1 =#)))))
12  --- method core.nothing %₁₁
    1   slot₂/xs
    2   (return %₁)
13  (return %₁)

########################################
# Trivial function argument destructuring
function f(x, (y,z), w)
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂ core.Any core.Any core.Any)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    1   (call top.indexed_iterate slot₃/destructured_arg_2 1)
    2   (= slot₆/y (call core.getfield %₁ 1))
    3   (= slot₅/iterstate (call core.getfield %₁ 2))
    4   slot₅/iterstate
    5   (call top.indexed_iterate slot₃/destructured_arg_2 2 %₄)
    6   (= slot₇/z (call core.getfield %₅ 1))
    7   (return core.nothing)
7   (return %₁)

########################################
# Function argument destructuring combined with splats, types and and defaults
function f((x,)::T...=rhs)
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    1   TestMod.rhs
    2   (call slot₁/#self# %₁)
    3   (return %₂)
7   (call core.Typeof %₁)
8   TestMod.T
9   (call core.apply_type core.Vararg %₈)
10  (call core.svec %₇ %₉)
11  (call core.svec)
12  (call core.svec %₁₀ %₁₁ :($(QuoteNode(:(#= line 1 =#)))))
13  --- method core.nothing %₁₂
    1   (call top.indexed_iterate slot₂/destructured_arg_1 1)
    2   (= slot₃/x (call core.getfield %₁ 1))
    3   (return core.nothing)
14  (return %₁)

########################################
# Duplicate destructured placeholders ok
function f((_,), (_,))
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂ core.Any core.Any)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    1   (call top.indexed_iterate slot₂/destructured_arg_1 1)
    2   (call core.getfield %₁ 1)
    3   (call top.indexed_iterate slot₃/destructured_arg_2 1)
    4   (call core.getfield %₃ 1)
    5   (return core.nothing)
7   (return %₁)

########################################
# Functions with @nospecialize argument metadata
function f(@nospecialize(x))
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂ core.Any)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    1   (return core.nothing)
7   (return %₁)

########################################
# Binding docs to functions
"""
some docs
"""
function f()
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 4 =#)))))
6   --- method core.nothing %₅
    1   (return core.nothing)
7   (call JuliaLowering.bind_docs! %₁ "some docs\n" %₅)
8   (return %₁)


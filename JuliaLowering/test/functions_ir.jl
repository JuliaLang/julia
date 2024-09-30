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
6   --- method :f %₅
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
7   --- method :f %₆
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
7   --- method :f %₆
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
7   --- method :f %₆
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
8   --- method :f %₇
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
6   --- method :f %₅
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


########################################
# Simple call
f(x, y)
#---------------------
1   TestMod.f
2   TestMod.x
3   TestMod.y
4   (call %₁ %₂ %₃)
5   (return %₄)

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

########################################
# <: as a function call
x <: y
#---------------------
1   TestMod.<:
2   TestMod.x
3   TestMod.y
4   (call %₁ %₂ %₃)
5   (return %₄)

########################################
# >: as a function call
x >: y
#---------------------
1   TestMod.>:
2   TestMod.x
3   TestMod.y
4   (call %₁ %₂ %₃)
5   (return %₄)

########################################
# --> as a function call
x --> y
#---------------------
1   TestMod.-->
2   TestMod.x
3   TestMod.y
4   (call %₁ %₂ %₃)
5   (return %₄)

########################################
# basic ccall
ccall(:strlen, Csize_t, (Cstring,), "asdfg")
#---------------------
1   TestMod.Cstring
2   (call top.cconvert %₁ "asdfg")
3   (call top.unsafe_convert %₁ %₂)
4   (foreigncall :strlen TestMod.Csize_t (call core.svec TestMod.Cstring) 0 :ccall %₃ %₂)
5   (return %₄)

########################################
# ccall with library name as a global var
ccall((:strlen, libc), Csize_t, (Cstring,), "asdfg")
#---------------------
1   TestMod.Cstring
2   (call top.cconvert %₁ "asdfg")
3   TestMod.libc
4   (call core.tuple :strlen %₃)
5   (call top.unsafe_convert %₁ %₂)
6   (foreigncall %₄ TestMod.Csize_t (call core.svec TestMod.Cstring) 0 :ccall %₅ %₂)
7   (return %₆)

########################################
# ccall with a calling convention
ccall(:foo, stdcall, Csize_t, ())
#---------------------
1   (foreigncall :foo TestMod.Csize_t (call core.svec) 0 :stdcall)
2   (return %₁)

########################################
# ccall with Any args become core.Any and don't need conversion or GC roots
ccall(:foo, stdcall, Csize_t, (Any,), x)
#---------------------
1   core.Any
2   TestMod.x
3   (foreigncall :foo TestMod.Csize_t (call core.svec core.Any) 0 :stdcall %₂)
4   (return %₃)

########################################
# ccall with variable as function name (must eval to a pointer)
ccall(ptr, Csize_t, (Cstring,), "asdfg")
#---------------------
1   TestMod.Cstring
2   (call top.cconvert %₁ "asdfg")
3   TestMod.ptr
4   (call top.unsafe_convert %₁ %₂)
5   (foreigncall %₃ TestMod.Csize_t (call core.svec TestMod.Cstring) 0 :ccall %₄ %₂)
6   (return %₅)

########################################
# ccall with varargs
ccall(:printf, Cint, (Cstring, Cstring...), "%s = %s\n", "2 + 2", "5")
#---------------------
1   TestMod.Cstring
2   TestMod.Cstring
3   (call top.cconvert %₁ "%s = %s\n")
4   (call top.cconvert %₂ "2 + 2")
5   (call top.cconvert %₂ "5")
6   (call top.unsafe_convert %₁ %₃)
7   (call top.unsafe_convert %₂ %₄)
8   (call top.unsafe_convert %₂ %₅)
9   (foreigncall :printf TestMod.Cint (call core.svec TestMod.Cstring TestMod.Cstring TestMod.Cstring) 1 :ccall %₆ %₇ %₈ %₃ %₄ %₅)
10  (return %₉)

########################################
# Error: ccall with too few arguments
ccall(:foo, Csize_t)
#---------------------
LoweringError:
ccall(:foo, Csize_t)
└──────────────────┘ ── too few arguments to ccall

########################################
# Error: ccall with calling conv and too few arguments
ccall(:foo, thiscall, Csize_t)
#---------------------
LoweringError:
ccall(:foo, thiscall, Csize_t)
└────────────────────────────┘ ── too few arguments to ccall with calling convention specified

########################################
# Error: ccall without tuple for argument types
ccall(:foo, Csize_t, Cstring)
#---------------------
LoweringError:
ccall(:foo, Csize_t, Cstring)
#                    └─────┘ ── ccall argument types must be a tuple; try `(T,)`

########################################
# Error: ccall without tuple for argument types
ccall(:foo, (Csize_t,), "arg")
#---------------------
LoweringError:
ccall(:foo, (Csize_t,), "arg")
#           └────────┘ ── ccall argument types must be a tuple; try `(T,)` and check if you specified a correct return type

########################################
# Error: ccall with library name which is a local variable
let libc = "libc"
    ccall((:strlen, libc), Csize_t, (Cstring,), "asdfg")
end
#---------------------
LoweringError:
let libc = "libc"
    ccall((:strlen, libc), Csize_t, (Cstring,), "asdfg")
#         └─────────────┘ ── ccall function name and library expression cannot reference local variables
end

########################################
# Error: ccall with return type which is a local variable
let Csize_t = 1
    ccall(:strlen, Csize_t, (Cstring,), "asdfg")
end
#---------------------
LoweringError:
let Csize_t = 1
    ccall(:strlen, Csize_t, (Cstring,), "asdfg")
#                  └─────┘ ── ccall return type cannot reference local variables
end

########################################
# Error: ccall with argument type which is a local variable
let Cstring = 1
    ccall(:strlen, Csize_t, (Cstring,), "asdfg")
end
#---------------------
LoweringError:
let Cstring = 1
    ccall(:strlen, Csize_t, (Cstring,), "asdfg")
#                            └─────┘ ── ccall argument types cannot reference local variables
end

########################################
# Error: ccall with too few arguments
ccall(:strlen, Csize_t, (Cstring,))
#---------------------
LoweringError:
ccall(:strlen, Csize_t, (Cstring,))
└─────────────────────────────────┘ ── Too few arguments in ccall compared to argument types

########################################
# Error: ccall with too many arguments
ccall(:strlen, Csize_t, (Cstring,), "asdfg", "blah")
#---------------------
LoweringError:
ccall(:strlen, Csize_t, (Cstring,), "asdfg", "blah")
└──────────────────────────────────────────────────┘ ── More arguments than types in ccall

########################################
# Error: ccall varargs with too few args
ccall(:foo, Csize_t, (Cstring...,), "asdfg")
#---------------------
LoweringError:
ccall(:foo, Csize_t, (Cstring...,), "asdfg")
#                     └────────┘ ── C ABI prohibits vararg without one required argument

########################################
# Error: ccall with multiple varargs
ccall(:foo, Csize_t, (Cstring..., Cstring...), "asdfg", "blah")
#---------------------
LoweringError:
ccall(:foo, Csize_t, (Cstring..., Cstring...), "asdfg", "blah")
#                     └────────┘ ── only the trailing ccall argument type should have `...`


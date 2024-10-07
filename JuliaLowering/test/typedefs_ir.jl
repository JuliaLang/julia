using JuliaLowering: JuliaLowering, @ast, @chk
using JuliaSyntax

function var"@atomic"(__context__::JuliaLowering.MacroContext, ex)
    @chk kind(ex) == JuliaSyntax.K"Identifier" || kind(ex) == JuliaSyntax.K"::" (ex,
                                                        "Expected identifier or declaration")
    @ast __context__ ex [K"atomic" ex]
end

#*******************************************************************************
########################################
# where expression without type bounds
A where X
#---------------------
1   (call core.TypeVar :X)
2   (= slot₁/X %₁)
3   slot₁/X
4   TestMod.A
5   (call core.UnionAll %₃ %₄)
6   (return %₅)

########################################
# where expression with upper bound
A where X <: UB
#---------------------
1   TestMod.UB
2   (call core.TypeVar :X %₁)
3   (= slot₁/X %₂)
4   slot₁/X
5   TestMod.A
6   (call core.UnionAll %₄ %₅)
7   (return %₆)

########################################
# where expression with lower bound
A where X >: LB
#---------------------
1   TestMod.X
2   (call core.TypeVar :LB %₁ core.Any)
3   (= slot₁/LB %₂)
4   slot₁/LB
5   TestMod.A
6   (call core.UnionAll %₄ %₅)
7   (return %₆)

########################################
# where expression with both bounds
A where LB <: X <: UB
#---------------------
1   TestMod.LB
2   TestMod.UB
3   (call core.TypeVar :X %₁ %₂)
4   (= slot₁/X %₃)
5   slot₁/X
6   TestMod.A
7   (call core.UnionAll %₅ %₆)
8   (return %₇)

########################################
# where expression with braces
A where {X, Y<:X}
#---------------------
1   (call core.TypeVar :X)
2   (= slot₁/X %₁)
3   slot₁/X
4   slot₁/X
5   (call core.TypeVar :Y %₄)
6   (= slot₂/Y %₅)
7   slot₂/Y
8   TestMod.A
9   (call core.UnionAll %₇ %₈)
10  (call core.UnionAll %₃ %₉)
11  (return %₁₀)

########################################
# Equivalent nested where expression without braces
A where Y<:X where X
#---------------------
1   (call core.TypeVar :X)
2   (= slot₁/X %₁)
3   slot₁/X
4   slot₁/X
5   (call core.TypeVar :Y %₄)
6   (= slot₂/Y %₅)
7   slot₂/Y
8   TestMod.A
9   (call core.UnionAll %₇ %₈)
10  (call core.UnionAll %₃ %₉)
11  (return %₁₀)

########################################
# Error: bad type bounds
A where f()
#---------------------
LoweringError:
A where f()
#       └─┘ ── expected type name or type bounds

########################################
# Error: bad type bounds
A where X < Y < Z
#---------------------
LoweringError:
A where X < Y < Z
#       └───────┘ ── invalid type bounds

########################################
# Error: bad type bounds
A where X <: f() <: Z
#---------------------
LoweringError:
A where X <: f() <: Z
#            └─┘ ── expected type name

########################################
# Error: bad type bounds
A where f() <: Y
#---------------------
LoweringError:
A where f() <: Y
#       └─┘ ── expected type name

########################################
# Error: bad type bounds
A where Y >: f()
#---------------------
LoweringError:
A where Y >: f()
#            └─┘ ── expected type name

########################################
# Simple abstract type definition
abstract type A end
#---------------------
1   (call core.svec)
2   (call core._abstracttype TestMod :A %₁)
3   (= slot₁/A %₂)
4   (call core._setsuper! %₂ core.Any)
5   (call core._typebody! %₂)
6   (global TestMod.A)
7   (const TestMod.A)
8   (isdefined TestMod.A)
9   (gotoifnot %₈ label₁₄)
10  TestMod.A
11  (call core._equiv_typedef %₁₀ %₂)
12  (gotoifnot %₁₁ label₁₄)
13  (goto label₁₅)
14  (= TestMod.A %₂)
15  (return core.nothing)

########################################
# Abstract type definition with supertype
abstract type A <: B end
#---------------------
1   (call core.svec)
2   (call core._abstracttype TestMod :A %₁)
3   (= slot₁/A %₂)
4   TestMod.B
5   (call core._setsuper! %₂ %₄)
6   (call core._typebody! %₂)
7   (global TestMod.A)
8   (const TestMod.A)
9   (isdefined TestMod.A)
10  (gotoifnot %₉ label₁₅)
11  TestMod.A
12  (call core._equiv_typedef %₁₁ %₂)
13  (gotoifnot %₁₂ label₁₅)
14  (goto label₁₆)
15  (= TestMod.A %₂)
16  (return core.nothing)

########################################
# Abstract type definition with multiple typevars
abstract type A{X, Y <: X} end
#---------------------
1   (= slot₂/X (call core.TypeVar :X))
2   slot₂/X
3   (= slot₃/Y (call core.TypeVar :Y %₂))
4   slot₂/X
5   slot₃/Y
6   (call core.svec %₄ %₅)
7   (call core._abstracttype TestMod :A %₆)
8   (= slot₁/A %₇)
9   (call core._setsuper! %₇ core.Any)
10  (call core._typebody! %₇)
11  (global TestMod.A)
12  (const TestMod.A)
13  (isdefined TestMod.A)
14  (gotoifnot %₁₃ label₁₉)
15  TestMod.A
16  (call core._equiv_typedef %₁₅ %₇)
17  (gotoifnot %₁₆ label₁₉)
18  (goto label₂₀)
19  (= TestMod.A %₇)
20  (return core.nothing)

########################################
# Error: Abstract type definition with bad signature
abstract type A() end
#---------------------
LoweringError:
abstract type A() end
#             └─┘ ── invalid type signature

########################################
# Error: Abstract type definition with bad signature
abstract type A(){T} end
#---------------------
LoweringError:
abstract type A(){T} end
#             └────┘ ── invalid type signature

########################################
# Error: Abstract type definition with bad signature
abstract type A() <: B end
#---------------------
LoweringError:
abstract type A() <: B end
#            └───────┘ ── invalid type signature

########################################
# Error: Abstract type definition in function scope
function f()
    abstract type A end
end
#---------------------
LoweringError:
function f()
    abstract type A end
#   └─────────────────┘ ── this syntax is only allowed in top level code
end

########################################
# Simple primitive type definition
primitive type P 8 end
#---------------------
1   (call core.svec)
2   (call core._primitivetype TestMod :P %₁ 8)
3   (= slot₁/P %₂)
4   (call core._setsuper! %₂ core.Any)
5   (call core._typebody! %₂)
6   (global TestMod.P)
7   (const TestMod.P)
8   (isdefined TestMod.P)
9   (gotoifnot %₈ label₁₄)
10  TestMod.P
11  (call core._equiv_typedef %₁₀ %₂)
12  (gotoifnot %₁₁ label₁₄)
13  (goto label₁₅)
14  (= TestMod.P %₂)
15  (return core.nothing)

########################################
# Complex primitive type definition
primitive type P{X,Y} <: Z 32 end
#---------------------
1   (= slot₂/X (call core.TypeVar :X))
2   (= slot₃/Y (call core.TypeVar :Y))
3   slot₂/X
4   slot₃/Y
5   (call core.svec %₃ %₄)
6   (call core._primitivetype TestMod :P %₅ 32)
7   (= slot₁/P %₆)
8   TestMod.Z
9   (call core._setsuper! %₆ %₈)
10  (call core._typebody! %₆)
11  (global TestMod.P)
12  (const TestMod.P)
13  (isdefined TestMod.P)
14  (gotoifnot %₁₃ label₁₉)
15  TestMod.P
16  (call core._equiv_typedef %₁₅ %₆)
17  (gotoifnot %₁₆ label₁₉)
18  (goto label₂₀)
19  (= TestMod.P %₆)
20  (return core.nothing)

########################################
# Primitive type definition with computed size (should this be allowed??)
primitive type P P_nbits() end
#---------------------
1   (call core.svec)
2   TestMod.P_nbits
3   (call %₂)
4   (call core._primitivetype TestMod :P %₁ %₃)
5   (= slot₁/P %₄)
6   (call core._setsuper! %₄ core.Any)
7   (call core._typebody! %₄)
8   (global TestMod.P)
9   (const TestMod.P)
10  (isdefined TestMod.P)
11  (gotoifnot %₁₀ label₁₆)
12  TestMod.P
13  (call core._equiv_typedef %₁₂ %₄)
14  (gotoifnot %₁₃ label₁₆)
15  (goto label₁₇)
16  (= TestMod.P %₄)
17  (return core.nothing)

########################################
# Basic struct
struct X
    a
    b::T
    c
end
#---------------------
1   (global TestMod.X)
2   (const TestMod.X)
3   (call core.svec)
4   (call core.svec :a :b :c)
5   (call core.svec)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 3)
7   (= slot₁/X %₆)
8   (call core._setsuper! %₆ core.Any)
9   (isdefined TestMod.X)
10  (gotoifnot %₉ label₂₀)
11  TestMod.X
12  (call core._equiv_typedef %₁₁ %₆)
13  (gotoifnot %₁₂ label₁₇)
14  TestMod.X
15  (= slot₁/X %₁₄)
16  (goto label₁₉)
17  slot₁/X
18  (= TestMod.X %₁₇)
19  (goto label₂₂)
20  slot₁/X
21  (= TestMod.X %₂₀)
22  slot₁/X
23  TestMod.T
24  (call core.svec core.Any %₂₃ core.Any)
25  (call core._typebody! %₂₂ %₂₄)
26  (return core.nothing)

########################################
# Struct with supertype and type params
struct X{U, S <: V <: T} <: Z
end
#---------------------
1   (global TestMod.X)
2   (const TestMod.X)
3   (= slot₂/U (call core.TypeVar :U))
4   TestMod.S
5   TestMod.T
6   (= slot₃/V (call core.TypeVar :V %₄ %₅))
7   slot₂/U
8   slot₃/V
9   (call core.svec %₇ %₈)
10  (call core.svec)
11  (call core.svec)
12  (call core._structtype TestMod :X %₉ %₁₀ %₁₁ false 0)
13  (= slot₄/X %₁₂)
14  TestMod.Z
15  (call core._setsuper! %₁₂ %₁₄)
16  (isdefined TestMod.X)
17  (gotoifnot %₁₆ label₃₇)
18  TestMod.X
19  (call core._equiv_typedef %₁₈ %₁₂)
20  (gotoifnot %₁₉ label₃₄)
21  TestMod.X
22  (= slot₄/X %₂₁)
23  TestMod.X
24  (call top.getproperty %₂₃ :body)
25  (call top.getproperty %₂₄ :body)
26  (call top.getproperty %₂₅ :parameters)
27  (call top.indexed_iterate %₂₆ 1)
28  (= slot₂/U (call core.getfield %₂₇ 1))
29  (= slot₁/iterstate (call core.getfield %₂₇ 2))
30  slot₁/iterstate
31  (call top.indexed_iterate %₂₆ 2 %₃₀)
32  (= slot₃/V (call core.getfield %₃₁ 1))
33  (goto label₃₆)
34  slot₄/X
35  (= TestMod.X %₃₄)
36  (goto label₃₉)
37  slot₄/X
38  (= TestMod.X %₃₇)
39  slot₄/X
40  (call core.svec)
41  (call core._typebody! %₃₉ %₄₀)
42  (return core.nothing)

########################################
# Error: Struct not at top level
function f()
    struct X
    end
end
#---------------------
LoweringError:
function f()
#   ┌───────
    struct X
    end
#─────┘ ── this syntax is only allowed in top level code
end


########################################
# where expression without type bounds
A where X
#---------------------
1   (call core.TypeVar :X)
2   (= slot₁/X %₁)
3   TestMod.A
4   (call core.UnionAll slot₁/X %₃)
5   (return %₄)

########################################
# where expression with upper bound
A where X <: UB
#---------------------
1   TestMod.UB
2   (call core.TypeVar :X %₁)
3   (= slot₁/X %₂)
4   TestMod.A
5   (call core.UnionAll slot₁/X %₄)
6   (return %₅)

########################################
# where expression with lower bound
A where X >: LB
#---------------------
1   TestMod.LB
2   (call core.TypeVar :X %₁ core.Any)
3   (= slot₁/X %₂)
4   TestMod.A
5   (call core.UnionAll slot₁/X %₄)
6   (return %₅)

########################################
# where expression with both bounds
A where LB <: X <: UB
#---------------------
1   TestMod.LB
2   TestMod.UB
3   (call core.TypeVar :X %₁ %₂)
4   (= slot₁/X %₃)
5   TestMod.A
6   (call core.UnionAll slot₁/X %₅)
7   (return %₆)

########################################
# where expression with braces
A where {X, Y<:X}
#---------------------
1   (call core.TypeVar :X)
2   (= slot₁/X %₁)
3   (call core.TypeVar :Y slot₁/X)
4   (= slot₂/Y %₃)
5   TestMod.A
6   (call core.UnionAll slot₂/Y %₅)
7   (call core.UnionAll slot₁/X %₆)
8   (return %₇)

########################################
# Equivalent nested where expression without braces
A where Y<:X where X
#---------------------
1   (call core.TypeVar :X)
2   (= slot₁/X %₁)
3   (call core.TypeVar :Y slot₁/X)
4   (= slot₂/Y %₃)
5   TestMod.A
6   (call core.UnionAll slot₂/Y %₅)
7   (call core.UnionAll slot₁/X %₆)
8   (return %₇)

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
#       └───────┘ ── expected `lb <: type_name <: ub` or `ub >: type_name >: lb`

########################################
# Error: bad type bounds
A where X <: f() <: Z
#---------------------
LoweringError:
A where X <: f() <: Z
#            └─┘ ── expected identifier

########################################
# Error: bad type bounds
A where f() <: Y
#---------------------
LoweringError:
A where f() <: Y
#       └─┘ ── expected identifier

########################################
# Simple type application
X{A,B,C}
#---------------------
1   TestMod.X
2   TestMod.A
3   TestMod.B
4   TestMod.C
5   (call core.apply_type %₁ %₂ %₃ %₄)
6   (return %₅)

########################################
# Type with implicit where param upper bound
X{<:A}
#---------------------
1   TestMod.A
2   (call core.TypeVar :#T1 %₁)
3   TestMod.X
4   (call core.apply_type %₃ %₂)
5   (call core.UnionAll %₂ %₄)
6   (return %₅)

########################################
# Type with implicit where param lower bound
X{>:A}
#---------------------
1   TestMod.A
2   (call core.TypeVar :#T1 %₁ core.Any)
3   TestMod.X
4   (call core.apply_type %₃ %₂)
5   (call core.UnionAll %₂ %₄)
6   (return %₅)

########################################
# Type with several implicit where params
X{S, <:A, T, >:B}
#---------------------
1   TestMod.A
2   (call core.TypeVar :#T1 %₁)
3   TestMod.B
4   (call core.TypeVar :#T2 %₃ core.Any)
5   TestMod.X
6   TestMod.S
7   TestMod.T
8   (call core.apply_type %₅ %₆ %₂ %₇ %₄)
9   (call core.UnionAll %₄ %₈)
10  (call core.UnionAll %₂ %₉)
11  (return %₁₀)

########################################
# Error: parameters in type application
X{S, T; W}
#---------------------
LoweringError:
X{S, T; W}
#     └─┘ ── unexpected semicolon

########################################
# Error: assignment in type application
X{S, T=w}
#---------------------
LoweringError:
X{S, T=w}
#   └──┘ ── assignment is not allowed in type parameter list

########################################
# Simple abstract type definition
abstract type A end
#---------------------
1   (call core.svec)
2   (call core._abstracttype TestMod :A %₁)
3   (= slot₁/A %₂)
4   (call core._setsuper! %₂ core.Any)
5   (call core._typebody! slot₁/A)
6   (call core.declare_global TestMod :A false)
7   latestworld
8   (call core.isdefinedglobal TestMod :A false)
9   (gotoifnot %₈ label₁₄)
10  TestMod.A
11  (call core._equiv_typedef %₁₀ %₂)
12  (gotoifnot %₁₁ label₁₄)
13  (goto label₁₆)
14  (call core.declare_const TestMod :A %₂)
15  latestworld
16  (return core.nothing)

########################################
# Abstract type definition with supertype
abstract type A <: B end
#---------------------
1   (call core.svec)
2   (call core._abstracttype TestMod :A %₁)
3   (= slot₁/A %₂)
4   TestMod.B
5   (call core._setsuper! %₂ %₄)
6   (call core._typebody! slot₁/A)
7   (call core.declare_global TestMod :A false)
8   latestworld
9   (call core.isdefinedglobal TestMod :A false)
10  (gotoifnot %₉ label₁₅)
11  TestMod.A
12  (call core._equiv_typedef %₁₁ %₂)
13  (gotoifnot %₁₂ label₁₅)
14  (goto label₁₇)
15  (call core.declare_const TestMod :A %₂)
16  latestworld
17  (return core.nothing)

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
10  (call core._typebody! slot₁/A)
11  (call core.declare_global TestMod :A false)
12  latestworld
13  (call core.isdefinedglobal TestMod :A false)
14  (gotoifnot %₁₃ label₁₉)
15  TestMod.A
16  (call core._equiv_typedef %₁₅ %₇)
17  (gotoifnot %₁₆ label₁₉)
18  (goto label₂₁)
19  (call core.declare_const TestMod :A %₇)
20  latestworld
21  (return core.nothing)

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
#             └─┘ ── expected identifier

########################################
# Error: Abstract type definition with bad signature
abstract type A() <: B end
#---------------------
LoweringError:
abstract type A() <: B end
#             └─┘ ── expected identifier

########################################
# Error: Abstract type definition in function scope
function f()
    abstract type A end
end
#---------------------
LoweringError:
function f()
    abstract type A end
#   └─────────────────┘ ── this syntax is only allowed at top level
end

########################################
# Simple primitive type definition
primitive type P 8 end
#---------------------
1   (call core.svec)
2   (call core._primitivetype TestMod :P %₁ 8)
3   (= slot₁/P %₂)
4   (call core._setsuper! %₂ core.Any)
5   (call core._typebody! slot₁/P)
6   (call core.declare_global TestMod :P false)
7   latestworld
8   (call core.isdefinedglobal TestMod :P false)
9   (gotoifnot %₈ label₁₄)
10  TestMod.P
11  (call core._equiv_typedef %₁₀ %₂)
12  (gotoifnot %₁₁ label₁₄)
13  (goto label₁₆)
14  (call core.declare_const TestMod :P %₂)
15  latestworld
16  (return core.nothing)

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
10  (call core._typebody! slot₁/P)
11  (call core.declare_global TestMod :P false)
12  latestworld
13  (call core.isdefinedglobal TestMod :P false)
14  (gotoifnot %₁₃ label₁₉)
15  TestMod.P
16  (call core._equiv_typedef %₁₅ %₆)
17  (gotoifnot %₁₆ label₁₉)
18  (goto label₂₁)
19  (call core.declare_const TestMod :P %₆)
20  latestworld
21  (return core.nothing)

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
7   (call core._typebody! slot₁/P)
8   (call core.declare_global TestMod :P false)
9   latestworld
10  (call core.isdefinedglobal TestMod :P false)
11  (gotoifnot %₁₀ label₁₆)
12  TestMod.P
13  (call core._equiv_typedef %₁₂ %₄)
14  (gotoifnot %₁₃ label₁₆)
15  (goto label₁₈)
16  (call core.declare_const TestMod :P %₄)
17  latestworld
18  (return core.nothing)

########################################
# Empty struct
struct X
end
#---------------------
1   (= slot₁/X (call core.TypeVar :X))
2   (call core.svec)
3   (call core.svec)
4   (call core.svec)
5   (call core.svec)
6   (call core.svec %₂ %₃ %₄ false 0 core.Any %₅)
7   (call core.isdefinedglobal TestMod :X false)
8   (gotoifnot %₇ label₁₂)
9   TestMod.X
10  (= slot₂/if_val %₉)
11  (goto label₁₃)
12  (= slot₂/if_val core.nothing)
13  slot₂/if_val
14  slot₁/X
15  (call core.svec %₁₄)
16  (call core.svec %₆)
17  (call core.svec %₁₃)
18  (call core.resolve_typegroup TestMod %₁₅ %₁₆ %₁₇)
19  (= slot₁/X (call core.getfield %₁₈ 1))
20  slot₁/X
21  (call core.declare_const TestMod :X %₂₀)
22  latestworld
23  TestMod.X
24  SourceLocation::1:1
25  (call top._defaultctors %₂₃ %₂₄)
26  latestworld
27  (return core.nothing)

########################################
# Empty struct with empty ctor
struct X
    X() = new()
end
#---------------------
1   (= slot₁/X (call core.TypeVar :X))
2   (call core.svec)
3   (call core.svec)
4   (call core.svec)
5   (call core.svec)
6   (call core.svec %₂ %₃ %₄ false 0 core.Any %₅)
7   (call core.isdefinedglobal TestMod :X false)
8   (gotoifnot %₇ label₁₂)
9   TestMod.X
10  (= slot₂/if_val %₉)
11  (goto label₁₃)
12  (= slot₂/if_val core.nothing)
13  slot₂/if_val
14  slot₁/X
15  (call core.svec %₁₄)
16  (call core.svec %₆)
17  (call core.svec %₁₃)
18  (call core.resolve_typegroup TestMod %₁₅ %₁₆ %₁₇)
19  (= slot₁/X (call core.getfield %₁₈ 1))
20  slot₁/X
21  (call core.declare_const TestMod :X %₂₀)
22  latestworld
23  TestMod.X
24  (call core.apply_type_or_typeapp core.Type %₂₃)
25  (call core.svec %₂₄)
26  (call core.svec)
27  SourceLocation::2:5
28  (call core.svec %₂₅ %₂₆ %₂₇)
29  --- method core.nothing %₂₈
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁)
    3   (return %₂)
30  latestworld
31  (return core.nothing)

########################################
# Basic struct
struct X
    a
    b::T
    c
end
#---------------------
1   (= slot₁/X (call core.TypeVar :X))
2   (call core.svec)
3   (call core.svec :a :b :c)
4   (call core.svec)
5   TestMod.T
6   (call core.svec core.Any %₅ core.Any)
7   (call core.svec %₂ %₃ %₄ false 3 core.Any %₆)
8   (call core.isdefinedglobal TestMod :X false)
9   (gotoifnot %₈ label₁₃)
10  TestMod.X
11  (= slot₂/if_val %₁₀)
12  (goto label₁₄)
13  (= slot₂/if_val core.nothing)
14  slot₂/if_val
15  slot₁/X
16  (call core.svec %₁₅)
17  (call core.svec %₇)
18  (call core.svec %₁₄)
19  (call core.resolve_typegroup TestMod %₁₆ %₁₇ %₁₈)
20  (= slot₁/X (call core.getfield %₁₉ 1))
21  slot₁/X
22  (call core.declare_const TestMod :X %₂₁)
23  latestworld
24  TestMod.X
25  SourceLocation::1:1
26  (call top._defaultctors %₂₄ %₂₅)
27  latestworld
28  (return core.nothing)

########################################
# Struct with supertype and type params
struct X{U, S <: V <: T} <: Z
end
#---------------------
1   (= slot₁/X (call core.TypeVar :X))
2   (= slot₂/U (call core.TypeVar :U))
3   TestMod.S
4   TestMod.T
5   (= slot₃/V (call core.TypeVar :V %₃ %₄))
6   slot₂/U
7   slot₃/V
8   (call core.svec %₆ %₇)
9   (call core.svec)
10  (call core.svec)
11  TestMod.Z
12  (call core.svec)
13  (call core.svec %₈ %₉ %₁₀ false 0 %₁₁ %₁₂)
14  (call core.isdefinedglobal TestMod :X false)
15  (gotoifnot %₁₄ label₁₉)
16  TestMod.X
17  (= slot₄/if_val %₁₆)
18  (goto label₂₀)
19  (= slot₄/if_val core.nothing)
20  slot₄/if_val
21  slot₁/X
22  (call core.svec %₂₁)
23  (call core.svec %₁₃)
24  (call core.svec %₂₀)
25  (call core.resolve_typegroup TestMod %₂₂ %₂₃ %₂₄)
26  (= slot₁/X (call core.getfield %₂₅ 1))
27  slot₁/X
28  (call core.declare_const TestMod :X %₂₇)
29  latestworld
30  TestMod.X
31  SourceLocation::1:1
32  (call top._defaultctors %₃₀ %₃₁)
33  latestworld
34  (return core.nothing)

########################################
# Struct with const and atomic fields
struct X
    const a
    @atomic b
    const @atomic c
end
#---------------------
1   (= slot₁/X (call core.TypeVar :X))
2   (call core.svec)
3   (call core.svec :a :b :c)
4   (call core.svec 1 :const 2 :atomic 3 :atomic 3 :const)
5   (call core.svec core.Any core.Any core.Any)
6   (call core.svec %₂ %₃ %₄ false 3 core.Any %₅)
7   (call core.isdefinedglobal TestMod :X false)
8   (gotoifnot %₇ label₁₂)
9   TestMod.X
10  (= slot₂/if_val %₉)
11  (goto label₁₃)
12  (= slot₂/if_val core.nothing)
13  slot₂/if_val
14  slot₁/X
15  (call core.svec %₁₄)
16  (call core.svec %₆)
17  (call core.svec %₁₃)
18  (call core.resolve_typegroup TestMod %₁₅ %₁₆ %₁₇)
19  (= slot₁/X (call core.getfield %₁₈ 1))
20  slot₁/X
21  (call core.declare_const TestMod :X %₂₀)
22  latestworld
23  TestMod.X
24  SourceLocation::1:1
25  (call top._defaultctors %₂₃ %₂₄)
26  latestworld
27  (return core.nothing)

########################################
# Documented struct
"""
X docs
"""
struct X
    "field a docs"
    a
    "field b docs"
    b
end
#---------------------
1   (newvar slot₁/val)
2   (gotoifnot true label₃₀)
3   (= slot₂/X (call core.TypeVar :X))
4   (call core.svec)
5   (call core.svec :a :b)
6   (call core.svec)
7   (call core.svec core.Any core.Any)
8   (call core.svec %₄ %₅ %₆ false 2 core.Any %₇)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₃/if_val %₁₁)
13  (goto label₁₅)
14  (= slot₃/if_val core.nothing)
15  slot₃/if_val
16  slot₂/X
17  (call core.svec %₁₆)
18  (call core.svec %₈)
19  (call core.svec %₁₅)
20  (call core.resolve_typegroup TestMod %₁₇ %₁₈ %₁₉)
21  (= slot₂/X (call core.getfield %₂₀ 1))
22  slot₂/X
23  (call core.declare_const TestMod :X %₂₂)
24  latestworld
25  TestMod.X
26  SourceLocation:none:1:0
27  (call top._defaultctors %₂₅ %₂₆)
28  latestworld
29  (= slot₁/val core.nothing)
30  (call Base.Docs.Binding TestMod :X)
31  (call Core.svec "X docs\n")
32  (call Pair{Symbol, Any} :a "field a docs")
33  (call Pair{Symbol, Any} :b "field b docs")
34  (call Dict{Symbol, Any} %₃₂ %₃₃)
35  (call Pair :fields %₃₄)
36  (call Dict{Symbol, Any} :path => "none" :linenumber => 1 :module => TestMod %₃₅)
37  (call Base.Docs.docstr %₃₁ %₃₆)
38  TestMod.Union
39  (call core.apply_type %₃₈)
40  (call Base.Docs.doc! TestMod %₃₀ %₃₇ %₃₉)
41  (gotoifnot true label₄₄)
42  slot₁/val
43  (return %₄₂)
44  (return core.nothing)

########################################
# Struct with outer constructor
struct X{U}
    x::U
end
#---------------------
1   (= slot₁/X (call core.TypeVar :X))
2   (= slot₂/U (call core.TypeVar :U))
3   slot₂/U
4   (call core.svec %₃)
5   (call core.svec :x)
6   (call core.svec)
7   slot₂/U
8   (call core.svec %₇)
9   (call core.svec %₄ %₅ %₆ false 1 core.Any %₈)
10  (call core.isdefinedglobal TestMod :X false)
11  (gotoifnot %₁₀ label₁₅)
12  TestMod.X
13  (= slot₃/if_val %₁₂)
14  (goto label₁₆)
15  (= slot₃/if_val core.nothing)
16  slot₃/if_val
17  slot₁/X
18  (call core.svec %₁₇)
19  (call core.svec %₉)
20  (call core.svec %₁₆)
21  (call core.resolve_typegroup TestMod %₁₈ %₁₉ %₂₀)
22  (= slot₁/X (call core.getfield %₂₁ 1))
23  slot₁/X
24  (call core.declare_const TestMod :X %₂₃)
25  latestworld
26  TestMod.X
27  SourceLocation::1:1
28  (call top._defaultctors %₂₆ %₂₇)
29  latestworld
30  (return core.nothing)

########################################
# Struct with outer constructor where one typevar is constrained by the other
# See https://github.com/JuliaLang/julia/issues/27269)
struct X{T, S <: Vector{T}}
    v::Vector{S}
end
#---------------------
1   (= slot₁/X (call core.TypeVar :X))
2   (= slot₂/T (call core.TypeVar :T))
3   TestMod.Vector
4   slot₂/T
5   (call core.apply_type_or_typeapp %₃ %₄)
6   (= slot₃/S (call core.TypeVar :S %₅))
7   slot₂/T
8   slot₃/S
9   (call core.svec %₇ %₈)
10  (call core.svec :v)
11  (call core.svec)
12  TestMod.Vector
13  slot₃/S
14  (call core.apply_type_or_typeapp %₁₂ %₁₃)
15  (call core.svec %₁₄)
16  (call core.svec %₉ %₁₀ %₁₁ false 1 core.Any %₁₅)
17  (call core.isdefinedglobal TestMod :X false)
18  (gotoifnot %₁₇ label₂₂)
19  TestMod.X
20  (= slot₄/if_val %₁₉)
21  (goto label₂₃)
22  (= slot₄/if_val core.nothing)
23  slot₄/if_val
24  slot₁/X
25  (call core.svec %₂₄)
26  (call core.svec %₁₆)
27  (call core.svec %₂₃)
28  (call core.resolve_typegroup TestMod %₂₅ %₂₆ %₂₇)
29  (= slot₁/X (call core.getfield %₂₈ 1))
30  slot₁/X
31  (call core.declare_const TestMod :X %₃₀)
32  latestworld
33  TestMod.X
34  SourceLocation::1:1
35  (call top._defaultctors %₃₃ %₃₄)
36  latestworld
37  (return core.nothing)

########################################
# User defined inner constructors and helper functions for structs without type params
struct X
    x
    f() = new(1)
    X() = f() # this X() captures `f` (in flisp, as a Box :-/ )
    X(x) = new(x)
    X(y,z)::ReallyXIPromise = new(y+z)
    """
    Docs for X constructor
    """
    X(a,b,c) = new(a)
end
#---------------------
1   (= slot₁/X (call core.TypeVar :X))
2   (call core.svec)
3   (call core.svec :x)
4   (call core.svec)
5   (call core.svec core.Any)
6   (call core.svec %₂ %₃ %₄ false 1 core.Any %₅)
7   (call core.isdefinedglobal TestMod :X false)
8   (gotoifnot %₇ label₁₂)
9   TestMod.X
10  (= slot₃/if_val %₉)
11  (goto label₁₃)
12  (= slot₃/if_val core.nothing)
13  slot₃/if_val
14  slot₁/X
15  (call core.svec %₁₄)
16  (call core.svec %₆)
17  (call core.svec %₁₃)
18  (call core.resolve_typegroup TestMod %₁₅ %₁₆ %₁₇)
19  (= slot₁/X (call core.getfield %₁₈ 1))
20  slot₁/X
21  (call core.declare_const TestMod :X %₂₀)
22  latestworld
23  (call core.svec)
24  (call core.svec)
25  (call core.svec)
26  (call JuliaLowering.eval_closure_type TestMod :#f##0 %₂₃ %₂₄ %₂₅)
27  latestworld
28  TestMod.#f##0
29  (new %₂₈)
30  (= slot₂/f %₂₉)
31  TestMod.#f##0
32  (call core.svec %₃₁)
33  (call core.svec)
34  SourceLocation::3:5
35  (call core.svec %₃₂ %₃₃ %₃₄)
36  --- method core.nothing %₃₅
    slots: [slot₁/#self#(!read)]
    1   TestMod.X
    2   (new %₁ 1)
    3   (return %₂)
37  latestworld
38  TestMod.X
39  (call core.apply_type_or_typeapp core.Type %₃₈)
40  (call core.svec %₃₉)
41  (call core.svec)
42  SourceLocation::4:5
43  (call core.svec %₄₀ %₄₁ %₄₂)
44  --- code_info
    slots: [slot₁/#ctor-self#(!read)]
    1   (captured_local 1)
    2   (call %₁)
    3   (return %₂)
45  (call core.svec slot₂/f)
46  (call JuliaLowering.replace_captured_locals %₄₄ %₄₅)
47  --- method core.nothing %₄₃ %₄₆
48  latestworld
49  TestMod.X
50  (call core.apply_type_or_typeapp core.Type %₄₉)
51  (call core.svec %₅₀ core.Any)
52  (call core.svec)
53  SourceLocation::5:5
54  (call core.svec %₅₁ %₅₂ %₅₃)
55  --- method core.nothing %₅₄
    slots: [slot₁/#ctor-self# slot₂/x]
    1   slot₁/#ctor-self#
    2   (new %₁ slot₂/x)
    3   (return %₂)
56  latestworld
57  TestMod.X
58  (call core.apply_type_or_typeapp core.Type %₅₇)
59  (call core.svec %₅₈ core.Any core.Any)
60  (call core.svec)
61  SourceLocation::6:5
62  (call core.svec %₅₉ %₆₀ %₆₁)
63  --- method core.nothing %₆₂
    slots: [slot₁/#ctor-self# slot₂/y slot₃/z slot₄/tmp(!read)]
    1   TestMod.ReallyXIPromise
    2   slot₁/#ctor-self#
    3   TestMod.+
    4   (call %₃ slot₂/y slot₃/z)
    5   (= slot₄/tmp (new %₂ %₄))
    6   (call core.isa slot₄/tmp %₁)
    7   (gotoifnot %₆ label₉)
    8   (goto label₁₁)
    9   (call top.convert %₁ slot₄/tmp)
    10  (= slot₄/tmp (call core.typeassert %₉ %₁))
    11  slot₄/tmp
    12  (return %₁₁)
64  latestworld
65  TestMod.X
66  (call core.apply_type_or_typeapp core.Type %₆₅)
67  (call core.svec %₆₆ core.Any core.Any core.Any)
68  (call core.svec)
69  SourceLocation::10:5
70  (call core.svec %₆₇ %₆₈ %₆₉)
71  --- method core.nothing %₇₀
    slots: [slot₁/#ctor-self# slot₂/a slot₃/b(!read) slot₄/c(!read)]
    1   slot₁/#ctor-self#
    2   (new %₁ slot₂/a)
    3   (return %₂)
72  latestworld
73  (return core.nothing)

########################################
# User defined inner constructors and helper functions for structs with type params
struct X{S,T}
    x
    X{A,B}() = new(1)
    X{U,V}() where {U,V} = new(1)
    f() = new{A,B}(1)
end
#---------------------
1   (= slot₁/X (call core.TypeVar :X))
2   (= slot₂/S (call core.TypeVar :S))
3   (= slot₃/T (call core.TypeVar :T))
4   slot₂/S
5   slot₃/T
6   (call core.svec %₄ %₅)
7   (call core.svec :x)
8   (call core.svec)
9   (call core.svec core.Any)
10  (call core.svec %₆ %₇ %₈ false 1 core.Any %₉)
11  (call core.isdefinedglobal TestMod :X false)
12  (gotoifnot %₁₁ label₁₆)
13  TestMod.X
14  (= slot₅/if_val %₁₃)
15  (goto label₁₇)
16  (= slot₅/if_val core.nothing)
17  slot₅/if_val
18  slot₁/X
19  (call core.svec %₁₈)
20  (call core.svec %₁₀)
21  (call core.svec %₁₇)
22  (call core.resolve_typegroup TestMod %₁₉ %₂₀ %₂₁)
23  (= slot₁/X (call core.getfield %₂₂ 1))
24  slot₁/X
25  (call core.declare_const TestMod :X %₂₄)
26  latestworld
27  TestMod.X
28  TestMod.A
29  TestMod.B
30  (call core.apply_type_or_typeapp %₂₇ %₂₈ %₂₉)
31  (call core.apply_type_or_typeapp core.Type %₃₀)
32  (call core.svec %₃₁)
33  (call core.svec)
34  SourceLocation::3:5
35  (call core.svec %₃₂ %₃₃ %₃₄)
36  --- method core.nothing %₃₅
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁ 1)
    3   (return %₂)
37  latestworld
38  (call core.TypeVar :U)
39  (call core.TypeVar :V)
40  TestMod.X
41  (call core.apply_type_or_typeapp %₄₀ %₃₈ %₃₉)
42  (call core.apply_type_or_typeapp core.Type %₄₁)
43  (call core.svec %₄₂)
44  (call core.svec %₃₈ %₃₉)
45  SourceLocation::4:5
46  (call core.svec %₄₃ %₄₄ %₄₅)
47  --- method core.nothing %₄₆
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁ 1)
    3   (return %₂)
48  latestworld
49  (call core.svec)
50  (call core.svec)
51  (call core.svec)
52  (call JuliaLowering.eval_closure_type TestMod :#f##1 %₄₉ %₅₀ %₅₁)
53  latestworld
54  TestMod.#f##1
55  (new %₅₄)
56  (= slot₄/f %₅₅)
57  TestMod.#f##1
58  (call core.svec %₅₇)
59  (call core.svec)
60  SourceLocation::5:5
61  (call core.svec %₅₈ %₅₉ %₆₀)
62  --- method core.nothing %₆₁
    slots: [slot₁/#self#(!read)]
    1   TestMod.X
    2   TestMod.A
    3   TestMod.B
    4   (call core.apply_type_or_typeapp %₁ %₂ %₃)
    5   (new %₄ 1)
    6   (return %₅)
63  latestworld
64  (return core.nothing)

########################################
# new() calls with splats; `Any` fields
struct X
    x
    y
    X(xs) = new(xs...)
end
#---------------------
1   (= slot₁/X (call core.TypeVar :X))
2   (call core.svec)
3   (call core.svec :x :y)
4   (call core.svec)
5   (call core.svec core.Any core.Any)
6   (call core.svec %₂ %₃ %₄ false 2 core.Any %₅)
7   (call core.isdefinedglobal TestMod :X false)
8   (gotoifnot %₇ label₁₂)
9   TestMod.X
10  (= slot₂/if_val %₉)
11  (goto label₁₃)
12  (= slot₂/if_val core.nothing)
13  slot₂/if_val
14  slot₁/X
15  (call core.svec %₁₄)
16  (call core.svec %₆)
17  (call core.svec %₁₃)
18  (call core.resolve_typegroup TestMod %₁₅ %₁₆ %₁₇)
19  (= slot₁/X (call core.getfield %₁₈ 1))
20  slot₁/X
21  (call core.declare_const TestMod :X %₂₀)
22  latestworld
23  TestMod.X
24  (call core.apply_type_or_typeapp core.Type %₂₃)
25  (call core.svec %₂₄ core.Any)
26  (call core.svec)
27  SourceLocation::4:5
28  (call core.svec %₂₅ %₂₆ %₂₇)
29  --- method core.nothing %₂₈
    slots: [slot₁/#ctor-self# slot₂/xs]
    1   slot₁/#ctor-self#
    2   (call core._apply_iterate top.iterate core.tuple slot₂/xs)
    3   (splatnew %₁ %₂)
    4   (return %₃)
30  latestworld
31  (return core.nothing)

########################################
# new() calls with splats; typed fields
struct X{T}
    x::T
    y::A
    X{T}(xs) where {T} = new(xs...)
end
#---------------------
1   (= slot₁/X (call core.TypeVar :X))
2   (= slot₂/T (call core.TypeVar :T))
3   slot₂/T
4   (call core.svec %₃)
5   (call core.svec :x :y)
6   (call core.svec)
7   slot₂/T
8   TestMod.A
9   (call core.svec %₇ %₈)
10  (call core.svec %₄ %₅ %₆ false 2 core.Any %₉)
11  (call core.isdefinedglobal TestMod :X false)
12  (gotoifnot %₁₁ label₁₆)
13  TestMod.X
14  (= slot₃/if_val %₁₃)
15  (goto label₁₇)
16  (= slot₃/if_val core.nothing)
17  slot₃/if_val
18  slot₁/X
19  (call core.svec %₁₈)
20  (call core.svec %₁₀)
21  (call core.svec %₁₇)
22  (call core.resolve_typegroup TestMod %₁₉ %₂₀ %₂₁)
23  (= slot₁/X (call core.getfield %₂₂ 1))
24  slot₁/X
25  (call core.declare_const TestMod :X %₂₄)
26  latestworld
27  (call core.TypeVar :T)
28  TestMod.X
29  (call core.apply_type_or_typeapp %₂₈ %₂₇)
30  (call core.apply_type_or_typeapp core.Type %₂₉)
31  (call core.svec %₃₀ core.Any)
32  (call core.svec %₂₇)
33  SourceLocation::4:5
34  (call core.svec %₃₁ %₃₂ %₃₃)
35  --- method core.nothing %₃₄
    slots: [slot₁/#ctor-self# slot₂/xs slot₃/tmp slot₄/tmp]
    1   (call core._apply_iterate top.iterate core.tuple slot₂/xs)
    2   (call core.nfields %₁)
    3   (call top.ult_int %₂ 2)
    4   (gotoifnot %₃ label₇)
    5   (call top.ArgumentError "too few arguments in `new` (expected 2)")
    6   (call core.throw %₅)
    7   (call top.ult_int 2 %₂)
    8   (gotoifnot %₇ label₁₁)
    9   (call top.ArgumentError "too many arguments in `new` (expected 2)")
    10  (call core.throw %₉)
    11  slot₁/#ctor-self#
    12  (call core.fieldtype %₁₁ 1)
    13  (= slot₃/tmp (call core.getfield %₁ 1))
    14  (call core.isa slot₃/tmp %₁₂)
    15  (gotoifnot %₁₄ label₁₇)
    16  (goto label₁₈)
    17  (= slot₃/tmp (call top.convert %₁₂ slot₃/tmp))
    18  slot₃/tmp
    19  (call core.fieldtype %₁₁ 2)
    20  (= slot₄/tmp (call core.getfield %₁ 2))
    21  (call core.isa slot₄/tmp %₁₉)
    22  (gotoifnot %₂₁ label₂₄)
    23  (goto label₂₅)
    24  (= slot₄/tmp (call top.convert %₁₉ slot₄/tmp))
    25  slot₄/tmp
    26  (new %₁₁ %₁₈ %₂₅)
    27  (return %₂₆)
36  latestworld
37  (return core.nothing)

########################################
# Error: new doesn't accept keywords
struct X
    X() = new(a=1)
end
#---------------------
LoweringError:
struct X
    X() = new(a=1)
#             └─┘ ── `new` does not accept keyword arguments
end

########################################
# Error: new doesn't accept keywords (params block)
struct X
    X() = new(; a=1)
end
#---------------------
LoweringError:
struct X
    X() = new(; a=1)
#             └───┘ ── `new` does not accept keyword arguments
end

########################################
# Error: User defined inner constructors without enough type params {A}
struct X{S,T}
    X() = new{A}()
end
#---------------------
LoweringError:
struct X{S,T}
    X() = new{A}()
#         └────┘ ── too few type parameters specified in `new{...}`
end

########################################
# Error: User defined inner constructors without enough type params {}
struct X{S,T}
    X() = new{}()
end
#---------------------
LoweringError:
struct X{S,T}
    X() = new{}()
#         └───┘ ── too few type parameters specified in `new{...}`
end

########################################
# Error: User defined inner constructors without enough type params
struct X{S,T}
    X{A}() = new()
end
#---------------------
LoweringError:
struct X{S,T}
    X{A}() = new()
#            └─┘ ── too few type parameters specified in `new`
end

########################################
# Error: User defined inner constructors with too many type params
struct X{S,T}
    X() = new{A,B,C}()
end
#---------------------
LoweringError:
struct X{S,T}
    X() = new{A,B,C}()
#         └────────┘ ── too many type parameters specified in `new{...}`
end

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
#─────┘ ── this syntax is only allowed at top level
end

########################################
# Constructor with type parameter
A{<:Real}() = A(1)
#---------------------
1   TestMod.Real
2   (call core.TypeVar :#T1 %₁)
3   TestMod.A
4   (call core.apply_type %₃ %₂)
5   (call core.UnionAll %₂ %₄)
6   (call core.TypeEqOf %₅)
7   (call core.svec %₆)
8   (call core.svec)
9   SourceLocation::1:1
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read)]
    1   TestMod.A
    2   (call %₁ 1)
    3   (return %₂)
12  latestworld
13  (return core.nothing)

########################################
# Inner constructor with local variable shadowing type parameter
# Type parameter T should NOT be boxed or captured.
# See https://github.com/aviatesk/JETLS.jl/issues/508
struct ShadowTypeParam{T}
    x::T
    function ShadowTypeParam(x)
        T = typeof(x)
        return new{T}(x)
    end
end
#---------------------
1   (= slot₁/ShadowTypeParam (call core.TypeVar :ShadowTypeParam))
2   (= slot₂/T (call core.TypeVar :T))
3   slot₂/T
4   (call core.svec %₃)
5   (call core.svec :x)
6   (call core.svec)
7   slot₂/T
8   (call core.svec %₇)
9   (call core.svec %₄ %₅ %₆ false 1 core.Any %₈)
10  (call core.isdefinedglobal TestMod :ShadowTypeParam false)
11  (gotoifnot %₁₀ label₁₅)
12  TestMod.ShadowTypeParam
13  (= slot₃/if_val %₁₂)
14  (goto label₁₆)
15  (= slot₃/if_val core.nothing)
16  slot₃/if_val
17  slot₁/ShadowTypeParam
18  (call core.svec %₁₇)
19  (call core.svec %₉)
20  (call core.svec %₁₆)
21  (call core.resolve_typegroup TestMod %₁₈ %₁₉ %₂₀)
22  (= slot₁/ShadowTypeParam (call core.getfield %₂₁ 1))
23  slot₁/ShadowTypeParam
24  (call core.declare_const TestMod :ShadowTypeParam %₂₃)
25  latestworld
26  TestMod.ShadowTypeParam
27  (call core.apply_type_or_typeapp core.Type %₂₆)
28  (call core.svec %₂₇ core.Any)
29  (call core.svec)
30  SourceLocation::3:5
31  (call core.svec %₂₈ %₂₉ %₃₀)
32  --- method core.nothing %₃₁
    slots: [slot₁/#ctor-self#(!read) slot₂/x slot₃/tmp slot₄/T(single_assign)]
    1   TestMod.typeof
    2   (= slot₄/T (call %₁ slot₂/x))
    3   TestMod.ShadowTypeParam
    4   slot₄/T
    5   (call core.apply_type_or_typeapp %₃ %₄)
    6   (call core.fieldtype %₅ 1)
    7   slot₂/x
    8   (= slot₃/tmp %₇)
    9   (call core.isa slot₃/tmp %₆)
    10  (gotoifnot %₉ label₁₂)
    11  (goto label₁₃)
    12  (= slot₃/tmp (call top.convert %₆ slot₃/tmp))
    13  slot₃/tmp
    14  (new %₅ %₁₃)
    15  (return %₁₄)
33  latestworld
34  (return core.nothing)

########################################
# Basic typegroup
typegroup
    struct A
        b::B
    end
    struct B
        a::A
    end
end
#---------------------
1   (= slot₂/A (call core.TypeVar :A))
2   (= slot₃/B (call core.TypeVar :B))
3   (call core.svec)
4   (call core.svec :b)
5   (call core.svec)
6   slot₃/B
7   (call core.svec %₆)
8   (call core.svec %₃ %₄ %₅ false 1 core.Any %₇)
9   (call core.svec)
10  (call core.svec :a)
11  (call core.svec)
12  slot₂/A
13  (call core.svec %₁₂)
14  (call core.svec %₉ %₁₀ %₁₁ false 1 core.Any %₁₃)
15  (call core.isdefinedglobal TestMod :A false)
16  (gotoifnot %₁₅ label₂₀)
17  TestMod.A
18  (= slot₄/if_val %₁₇)
19  (goto label₂₁)
20  (= slot₄/if_val core.nothing)
21  slot₄/if_val
22  (call core.isdefinedglobal TestMod :B false)
23  (gotoifnot %₂₂ label₂₇)
24  TestMod.B
25  (= slot₅/if_val %₂₄)
26  (goto label₂₈)
27  (= slot₅/if_val core.nothing)
28  slot₅/if_val
29  slot₂/A
30  slot₃/B
31  (call core.svec %₂₉ %₃₀)
32  (call core.svec %₈ %₁₄)
33  (call core.svec %₂₁ %₂₈)
34  (call core.resolve_typegroup TestMod %₃₁ %₃₂ %₃₃)
35  (call top.indexed_iterate %₃₄ 1)
36  (= slot₂/A (call core.getfield %₃₅ 1))
37  (= slot₁/iterstate (call core.getfield %₃₅ 2))
38  slot₁/iterstate
39  (call top.indexed_iterate %₃₄ 2 %₃₈)
40  (= slot₃/B (call core.getfield %₃₉ 1))
41  slot₂/A
42  (call core.declare_const TestMod :A %₄₁)
43  latestworld
44  slot₃/B
45  (call core.declare_const TestMod :B %₄₄)
46  latestworld
47  TestMod.A
48  SourceLocation::2:5
49  (call top._defaultctors %₄₇ %₄₈)
50  TestMod.B
51  SourceLocation::5:5
52  (call top._defaultctors %₅₀ %₅₁)
53  latestworld
54  (return core.nothing)

########################################
# Typegroup with supertype and apply_type_or_typeapp replacement
typegroup
    struct A <: AbstractVector{B}
        b::Union{Nothing, B}
    end
    struct B
        a::A
    end
end
#---------------------
1   (= slot₂/A (call core.TypeVar :A))
2   (= slot₃/B (call core.TypeVar :B))
3   (call core.svec)
4   (call core.svec :b)
5   (call core.svec)
6   TestMod.AbstractVector
7   slot₃/B
8   (call core.apply_type_or_typeapp %₆ %₇)
9   TestMod.Union
10  TestMod.Nothing
11  slot₃/B
12  (call core.apply_type_or_typeapp %₉ %₁₀ %₁₁)
13  (call core.svec %₁₂)
14  (call core.svec %₃ %₄ %₅ false 1 %₈ %₁₃)
15  (call core.svec)
16  (call core.svec :a)
17  (call core.svec)
18  slot₂/A
19  (call core.svec %₁₈)
20  (call core.svec %₁₅ %₁₆ %₁₇ false 1 core.Any %₁₉)
21  (call core.isdefinedglobal TestMod :A false)
22  (gotoifnot %₂₁ label₂₆)
23  TestMod.A
24  (= slot₄/if_val %₂₃)
25  (goto label₂₇)
26  (= slot₄/if_val core.nothing)
27  slot₄/if_val
28  (call core.isdefinedglobal TestMod :B false)
29  (gotoifnot %₂₈ label₃₃)
30  TestMod.B
31  (= slot₅/if_val %₃₀)
32  (goto label₃₄)
33  (= slot₅/if_val core.nothing)
34  slot₅/if_val
35  slot₂/A
36  slot₃/B
37  (call core.svec %₃₅ %₃₆)
38  (call core.svec %₁₄ %₂₀)
39  (call core.svec %₂₇ %₃₄)
40  (call core.resolve_typegroup TestMod %₃₇ %₃₈ %₃₉)
41  (call top.indexed_iterate %₄₀ 1)
42  (= slot₂/A (call core.getfield %₄₁ 1))
43  (= slot₁/iterstate (call core.getfield %₄₁ 2))
44  slot₁/iterstate
45  (call top.indexed_iterate %₄₀ 2 %₄₄)
46  (= slot₃/B (call core.getfield %₄₅ 1))
47  slot₂/A
48  (call core.declare_const TestMod :A %₄₇)
49  latestworld
50  slot₃/B
51  (call core.declare_const TestMod :B %₅₀)
52  latestworld
53  TestMod.A
54  SourceLocation::2:5
55  (call top._defaultctors %₅₃ %₅₄)
56  TestMod.B
57  SourceLocation::5:5
58  (call top._defaultctors %₅₆ %₅₇)
59  latestworld
60  (return core.nothing)

########################################
# Error: Duplicate field name in struct
struct A; x; x; end
#---------------------
LoweringError:
struct A; x; x; end
#            ╙ ── duplicate field name

########################################
# Error: Duplicate field name with different types
struct A; x::Int; x::String; end
#---------------------
LoweringError:
struct A; x::Int; x::String; end
#                 ╙ ── duplicate field name

########################################
# Error: Duplicate field name in mutable struct
mutable struct A; x; y; x; end
#---------------------
LoweringError:
mutable struct A; x; y; x; end
#                       ╙ ── duplicate field name

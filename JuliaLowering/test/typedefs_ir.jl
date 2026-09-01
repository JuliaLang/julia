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
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₁/X (call core.TypeVar :X))
4   (call core.svec)
5   (call core.svec)
6   (call core.svec)
7   (call core.svec)
8   (call core.svec %₄ %₅ %₆ false 0 core.Any %₇)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₂/if_val %₁₁)
13  (goto label₁₅)
14  (= slot₂/if_val core.nothing)
15  slot₂/if_val
16  slot₁/X
17  (call core.svec %₁₆)
18  (call core.svec %₈)
19  (call core.svec %₁₅)
20  (call core.resolve_typegroup TestMod %₁₇ %₁₈ %₁₉)
21  (= slot₁/X (call core.getfield %₂₀ 1))
22  slot₁/X
23  (call core.declare_const TestMod :X %₂₂)
24  latestworld
25  TestMod.X
26  SourceLocation::1:1
27  (call top._defaultctors %₂₅ %₂₆)
28  latestworld
29  (return core.nothing)

########################################
# Empty struct with empty ctor
struct X
    X() = new()
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₁/X (call core.TypeVar :X))
4   (call core.svec)
5   (call core.svec)
6   (call core.svec)
7   (call core.svec)
8   (call core.svec %₄ %₅ %₆ false 0 core.Any %₇)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₂/if_val %₁₁)
13  (goto label₁₅)
14  (= slot₂/if_val core.nothing)
15  slot₂/if_val
16  slot₁/X
17  (call core.svec %₁₆)
18  (call core.svec %₈)
19  (call core.svec %₁₅)
20  (call core.resolve_typegroup TestMod %₁₇ %₁₈ %₁₉)
21  (= slot₁/X (call core.getfield %₂₀ 1))
22  slot₁/X
23  (call core.declare_const TestMod :X %₂₂)
24  latestworld
25  TestMod.X
26  (call core.apply_type core.Type %₂₅)
27  (call core.svec %₂₆)
28  (call core.svec)
29  SourceLocation::2:5
30  (call core.svec %₂₇ %₂₈ %₂₉)
31  (call core.define_method TestMod core.nothing %₃₀
    --- code_info
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁)
    3   (return %₂)
32  latestworld
33  (return core.nothing)

########################################
# Basic struct
struct X
    a
    b::T
    c
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₁/X (call core.TypeVar :X))
4   (call core.svec)
5   (call core.svec :a :b :c)
6   (call core.svec)
7   TestMod.T
8   (call core.svec core.Any %₇ core.Any)
9   (call core.svec %₄ %₅ %₆ false 3 core.Any %₈)
10  (call core.isdefinedglobal TestMod :X false)
11  (gotoifnot %₁₀ label₁₅)
12  TestMod.X
13  (= slot₂/if_val %₁₂)
14  (goto label₁₆)
15  (= slot₂/if_val core.nothing)
16  slot₂/if_val
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
# Struct with supertype and type params
struct X{U, S <: V <: T} <: Z
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₁/X (call core.TypeVar :X))
4   (= slot₂/U (call core.TypeVar :U))
5   TestMod.S
6   TestMod.T
7   (= slot₃/V (call core.TypeVar :V %₅ %₆))
8   slot₂/U
9   slot₃/V
10  (call core.svec %₈ %₉)
11  (call core.svec)
12  (call core.svec)
13  TestMod.Z
14  (call core.svec)
15  (call core.svec %₁₀ %₁₁ %₁₂ false 0 %₁₃ %₁₄)
16  (call core.isdefinedglobal TestMod :X false)
17  (gotoifnot %₁₆ label₂₁)
18  TestMod.X
19  (= slot₄/if_val %₁₈)
20  (goto label₂₂)
21  (= slot₄/if_val core.nothing)
22  slot₄/if_val
23  slot₁/X
24  (call core.svec %₂₃)
25  (call core.svec %₁₅)
26  (call core.svec %₂₂)
27  (call core.resolve_typegroup TestMod %₂₄ %₂₅ %₂₆)
28  (= slot₁/X (call core.getfield %₂₇ 1))
29  slot₁/X
30  (call core.declare_const TestMod :X %₂₉)
31  latestworld
32  TestMod.X
33  SourceLocation::1:1
34  (call top._defaultctors %₃₂ %₃₃)
35  latestworld
36  (return core.nothing)

########################################
# Struct with const and atomic fields
struct X
    const a
    @atomic b
    const @atomic c
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₁/X (call core.TypeVar :X))
4   (call core.svec)
5   (call core.svec :a :b :c)
6   (call core.svec 1 :const 2 :atomic 3 :atomic 3 :const)
7   (call core.svec core.Any core.Any core.Any)
8   (call core.svec %₄ %₅ %₆ false 3 core.Any %₇)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₂/if_val %₁₁)
13  (goto label₁₅)
14  (= slot₂/if_val core.nothing)
15  slot₂/if_val
16  slot₁/X
17  (call core.svec %₁₆)
18  (call core.svec %₈)
19  (call core.svec %₁₅)
20  (call core.resolve_typegroup TestMod %₁₇ %₁₈ %₁₉)
21  (= slot₁/X (call core.getfield %₂₀ 1))
22  slot₁/X
23  (call core.declare_const TestMod :X %₂₂)
24  latestworld
25  TestMod.X
26  SourceLocation::1:1
27  (call top._defaultctors %₂₅ %₂₆)
28  latestworld
29  (return core.nothing)

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
2   (gotoifnot true label₃₂)
3   (call core.declare_global TestMod :X false)
4   latestworld
5   (= slot₂/X (call core.TypeVar :X))
6   (call core.svec)
7   (call core.svec :a :b)
8   (call core.svec)
9   (call core.svec core.Any core.Any)
10  (call core.svec %₆ %₇ %₈ false 2 core.Any %₉)
11  (call core.isdefinedglobal TestMod :X false)
12  (gotoifnot %₁₁ label₁₆)
13  TestMod.X
14  (= slot₃/if_val %₁₃)
15  (goto label₁₇)
16  (= slot₃/if_val core.nothing)
17  slot₃/if_val
18  slot₂/X
19  (call core.svec %₁₈)
20  (call core.svec %₁₀)
21  (call core.svec %₁₇)
22  (call core.resolve_typegroup TestMod %₁₉ %₂₀ %₂₁)
23  (= slot₂/X (call core.getfield %₂₂ 1))
24  slot₂/X
25  (call core.declare_const TestMod :X %₂₄)
26  latestworld
27  TestMod.X
28  SourceLocation:none:1:0
29  (call top._defaultctors %₂₇ %₂₈)
30  latestworld
31  (= slot₁/val core.nothing)
32  (call Base.Docs.Binding TestMod :X)
33  (call Core.svec "X docs\n")
34  (call Pair{Symbol, Any} :a "field a docs")
35  (call Pair{Symbol, Any} :b "field b docs")
36  (call Dict{Symbol, Any} %₃₄ %₃₅)
37  (call Pair :fields %₃₆)
38  (call Dict{Symbol, Any} :path => "none" :linenumber => 1 :module => TestMod %₃₇)
39  (call Base.Docs.docstr %₃₃ %₃₈)
40  TestMod.Union
41  (call core.apply_type %₄₀)
42  (call Base.Docs.doc! TestMod %₃₂ %₃₉ %₄₁)
43  (gotoifnot true label₄₆)
44  slot₁/val
45  (return %₄₄)
46  (return core.nothing)

########################################
# Struct with outer constructor
struct X{U}
    x::U
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₁/X (call core.TypeVar :X))
4   (= slot₂/U (call core.TypeVar :U))
5   slot₂/U
6   (call core.svec %₅)
7   (call core.svec :x)
8   (call core.svec)
9   slot₂/U
10  (call core.svec %₉)
11  (call core.svec %₆ %₇ %₈ false 1 core.Any %₁₀)
12  (call core.isdefinedglobal TestMod :X false)
13  (gotoifnot %₁₂ label₁₇)
14  TestMod.X
15  (= slot₃/if_val %₁₄)
16  (goto label₁₈)
17  (= slot₃/if_val core.nothing)
18  slot₃/if_val
19  slot₁/X
20  (call core.svec %₁₉)
21  (call core.svec %₁₁)
22  (call core.svec %₁₈)
23  (call core.resolve_typegroup TestMod %₂₀ %₂₁ %₂₂)
24  (= slot₁/X (call core.getfield %₂₃ 1))
25  slot₁/X
26  (call core.declare_const TestMod :X %₂₅)
27  latestworld
28  TestMod.X
29  SourceLocation::1:1
30  (call top._defaultctors %₂₈ %₂₉)
31  latestworld
32  (return core.nothing)

########################################
# Struct with outer constructor where one typevar is constrained by the other
# See https://github.com/JuliaLang/julia/issues/27269)
struct X{T, S <: Vector{T}}
    v::Vector{S}
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₁/X (call core.TypeVar :X))
4   (= slot₂/T (call core.TypeVar :T))
5   TestMod.Vector
6   slot₂/T
7   (call core.apply_type_or_typeapp %₅ %₆)
8   (= slot₃/S (call core.TypeVar :S %₇))
9   slot₂/T
10  slot₃/S
11  (call core.svec %₉ %₁₀)
12  (call core.svec :v)
13  (call core.svec)
14  TestMod.Vector
15  slot₃/S
16  (call core.apply_type_or_typeapp %₁₄ %₁₅)
17  (call core.svec %₁₆)
18  (call core.svec %₁₁ %₁₂ %₁₃ false 1 core.Any %₁₇)
19  (call core.isdefinedglobal TestMod :X false)
20  (gotoifnot %₁₉ label₂₄)
21  TestMod.X
22  (= slot₄/if_val %₂₁)
23  (goto label₂₅)
24  (= slot₄/if_val core.nothing)
25  slot₄/if_val
26  slot₁/X
27  (call core.svec %₂₆)
28  (call core.svec %₁₈)
29  (call core.svec %₂₅)
30  (call core.resolve_typegroup TestMod %₂₇ %₂₈ %₂₉)
31  (= slot₁/X (call core.getfield %₃₀ 1))
32  slot₁/X
33  (call core.declare_const TestMod :X %₃₂)
34  latestworld
35  TestMod.X
36  SourceLocation::1:1
37  (call top._defaultctors %₃₅ %₃₆)
38  latestworld
39  (return core.nothing)

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
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₁/X (call core.TypeVar :X))
4   (call core.svec)
5   (call core.svec :x)
6   (call core.svec)
7   (call core.svec core.Any)
8   (call core.svec %₄ %₅ %₆ false 1 core.Any %₇)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₃/if_val %₁₁)
13  (goto label₁₅)
14  (= slot₃/if_val core.nothing)
15  slot₃/if_val
16  slot₁/X
17  (call core.svec %₁₆)
18  (call core.svec %₈)
19  (call core.svec %₁₅)
20  (call core.resolve_typegroup TestMod %₁₇ %₁₈ %₁₉)
21  (= slot₁/X (call core.getfield %₂₀ 1))
22  slot₁/X
23  (call core.declare_const TestMod :X %₂₂)
24  latestworld
25  (call core.svec)
26  (call core.svec)
27  (call core.svec)
28  (call JuliaLowering.eval_closure_type TestMod :#f##0 %₂₅ %₂₆ %₂₇)
29  latestworld
30  TestMod.#f##0
31  (new %₃₀)
32  TestMod.#f##0
33  (call core.svec %₃₂)
34  (call core.svec)
35  SourceLocation::3:5
36  (call core.svec %₃₃ %₃₄ %₃₅)
37  (call core.define_method TestMod core.nothing %₃₆
    --- code_info
    slots: [slot₁/#self#(!read)]
    1   TestMod.X
    2   (new %₁ 1)
    3   (return %₂)
38  latestworld
39  (= slot₂/f %₃₁)
40  TestMod.X
41  (call core.apply_type core.Type %₄₀)
42  (call core.svec %₄₁)
43  (call core.svec)
44  SourceLocation::4:5
45  (call core.svec %₄₂ %₄₃ %₄₄)
46  --- code_info
    slots: [slot₁/#ctor-self#(!read)]
    1   (captured_local 1)
    2   (call %₁)
    3   (return %₂)
47  (call core.svec slot₂/f)
48  (call JuliaLowering.replace_captured_locals %₄₆ %₄₇)
49  (call core.define_method TestMod core.nothing %₄₅ %₄₈)
50  latestworld
51  TestMod.X
52  (call core.apply_type core.Type %₅₁)
53  (call core.svec %₅₂ core.Any)
54  (call core.svec)
55  SourceLocation::5:5
56  (call core.svec %₅₃ %₅₄ %₅₅)
57  (call core.define_method TestMod core.nothing %₅₆
    --- code_info
    slots: [slot₁/#ctor-self# slot₂/x]
    1   slot₁/#ctor-self#
    2   (new %₁ slot₂/x)
    3   (return %₂)
58  latestworld
59  TestMod.X
60  (call core.apply_type core.Type %₅₉)
61  (call core.svec %₆₀ core.Any core.Any)
62  (call core.svec)
63  SourceLocation::6:5
64  (call core.svec %₆₁ %₆₂ %₆₃)
65  (call core.define_method TestMod core.nothing %₆₄
    --- code_info
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
66  latestworld
67  TestMod.X
68  (call core.apply_type core.Type %₆₇)
69  (call core.svec %₆₈ core.Any core.Any core.Any)
70  (call core.svec)
71  SourceLocation::10:5
72  (call core.svec %₆₉ %₇₀ %₇₁)
73  (call core.define_method TestMod core.nothing %₇₂
    --- code_info
    slots: [slot₁/#ctor-self# slot₂/a slot₃/b(!read) slot₄/c(!read)]
    1   slot₁/#ctor-self#
    2   (new %₁ slot₂/a)
    3   (return %₂)
74  latestworld
75  (return core.nothing)

########################################
# User defined inner constructors and helper functions for structs with type params
struct X{S,T}
    x
    X{A,B}() = new(1)
    X{U,V}() where {U,V} = new(1)
    f() = new{A,B}(1)
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₁/X (call core.TypeVar :X))
4   (= slot₂/S (call core.TypeVar :S))
5   (= slot₃/T (call core.TypeVar :T))
6   slot₂/S
7   slot₃/T
8   (call core.svec %₆ %₇)
9   (call core.svec :x)
10  (call core.svec)
11  (call core.svec core.Any)
12  (call core.svec %₈ %₉ %₁₀ false 1 core.Any %₁₁)
13  (call core.isdefinedglobal TestMod :X false)
14  (gotoifnot %₁₃ label₁₈)
15  TestMod.X
16  (= slot₅/if_val %₁₅)
17  (goto label₁₉)
18  (= slot₅/if_val core.nothing)
19  slot₅/if_val
20  slot₁/X
21  (call core.svec %₂₀)
22  (call core.svec %₁₂)
23  (call core.svec %₁₉)
24  (call core.resolve_typegroup TestMod %₂₁ %₂₂ %₂₃)
25  (= slot₁/X (call core.getfield %₂₄ 1))
26  slot₁/X
27  (call core.declare_const TestMod :X %₂₆)
28  latestworld
29  TestMod.X
30  TestMod.A
31  TestMod.B
32  (call core.apply_type %₂₉ %₃₀ %₃₁)
33  (call core.apply_type core.Type %₃₂)
34  (call core.svec %₃₃)
35  (call core.svec)
36  SourceLocation::3:5
37  (call core.svec %₃₄ %₃₅ %₃₆)
38  (call core.define_method TestMod core.nothing %₃₇
    --- code_info
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁ 1)
    3   (return %₂)
39  latestworld
40  (call core.TypeVar :U)
41  (call core.TypeVar :V)
42  TestMod.X
43  (call core.apply_type %₄₂ %₄₀ %₄₁)
44  (call core.apply_type core.Type %₄₃)
45  (call core.svec %₄₄)
46  (call core.svec %₄₀ %₄₁)
47  SourceLocation::4:5
48  (call core.svec %₄₅ %₄₆ %₄₇)
49  (call core.define_method TestMod core.nothing %₄₈
    --- code_info
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁ 1)
    3   (return %₂)
50  latestworld
51  (call core.svec)
52  (call core.svec)
53  (call core.svec)
54  (call JuliaLowering.eval_closure_type TestMod :#f##1 %₅₁ %₅₂ %₅₃)
55  latestworld
56  TestMod.#f##1
57  (new %₅₆)
58  TestMod.#f##1
59  (call core.svec %₅₈)
60  (call core.svec)
61  SourceLocation::5:5
62  (call core.svec %₅₉ %₆₀ %₆₁)
63  (call core.define_method TestMod core.nothing %₆₂
    --- code_info
    slots: [slot₁/#self#(!read)]
    1   TestMod.X
    2   TestMod.A
    3   TestMod.B
    4   (call core.apply_type %₁ %₂ %₃)
    5   (new %₄ 1)
    6   (return %₅)
64  latestworld
65  (= slot₄/f %₅₇)
66  latestworld
67  (return core.nothing)

########################################
# new() calls with splats; `Any` fields
struct X
    x
    y
    X(xs) = new(xs...)
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₁/X (call core.TypeVar :X))
4   (call core.svec)
5   (call core.svec :x :y)
6   (call core.svec)
7   (call core.svec core.Any core.Any)
8   (call core.svec %₄ %₅ %₆ false 2 core.Any %₇)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₂/if_val %₁₁)
13  (goto label₁₅)
14  (= slot₂/if_val core.nothing)
15  slot₂/if_val
16  slot₁/X
17  (call core.svec %₁₆)
18  (call core.svec %₈)
19  (call core.svec %₁₅)
20  (call core.resolve_typegroup TestMod %₁₇ %₁₈ %₁₉)
21  (= slot₁/X (call core.getfield %₂₀ 1))
22  slot₁/X
23  (call core.declare_const TestMod :X %₂₂)
24  latestworld
25  TestMod.X
26  (call core.apply_type core.Type %₂₅)
27  (call core.svec %₂₆ core.Any)
28  (call core.svec)
29  SourceLocation::4:5
30  (call core.svec %₂₇ %₂₈ %₂₉)
31  (call core.define_method TestMod core.nothing %₃₀
    --- code_info
    slots: [slot₁/#ctor-self# slot₂/xs]
    1   slot₁/#ctor-self#
    2   (call core._apply_iterate top.iterate core.tuple slot₂/xs)
    3   (splatnew %₁ %₂)
    4   (return %₃)
32  latestworld
33  (return core.nothing)

########################################
# new() calls with splats; typed fields
struct X{T}
    x::T
    y::A
    X{T}(xs) where {T} = new(xs...)
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₁/X (call core.TypeVar :X))
4   (= slot₂/T (call core.TypeVar :T))
5   slot₂/T
6   (call core.svec %₅)
7   (call core.svec :x :y)
8   (call core.svec)
9   slot₂/T
10  TestMod.A
11  (call core.svec %₉ %₁₀)
12  (call core.svec %₆ %₇ %₈ false 2 core.Any %₁₁)
13  (call core.isdefinedglobal TestMod :X false)
14  (gotoifnot %₁₃ label₁₈)
15  TestMod.X
16  (= slot₃/if_val %₁₅)
17  (goto label₁₉)
18  (= slot₃/if_val core.nothing)
19  slot₃/if_val
20  slot₁/X
21  (call core.svec %₂₀)
22  (call core.svec %₁₂)
23  (call core.svec %₁₉)
24  (call core.resolve_typegroup TestMod %₂₁ %₂₂ %₂₃)
25  (= slot₁/X (call core.getfield %₂₄ 1))
26  slot₁/X
27  (call core.declare_const TestMod :X %₂₆)
28  latestworld
29  (call core.TypeVar :T)
30  TestMod.X
31  (call core.apply_type %₃₀ %₂₉)
32  (call core.apply_type core.Type %₃₁)
33  (call core.svec %₃₂ core.Any)
34  (call core.svec %₂₉)
35  SourceLocation::4:5
36  (call core.svec %₃₃ %₃₄ %₃₅)
37  (call core.define_method TestMod core.nothing %₃₆
    --- code_info
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
38  latestworld
39  (return core.nothing)

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
11  (call core.define_method TestMod core.nothing %₁₀
    --- code_info
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
1   (call core.declare_global TestMod :ShadowTypeParam false)
2   latestworld
3   (= slot₁/ShadowTypeParam (call core.TypeVar :ShadowTypeParam))
4   (= slot₂/T (call core.TypeVar :T))
5   slot₂/T
6   (call core.svec %₅)
7   (call core.svec :x)
8   (call core.svec)
9   slot₂/T
10  (call core.svec %₉)
11  (call core.svec %₆ %₇ %₈ false 1 core.Any %₁₀)
12  (call core.isdefinedglobal TestMod :ShadowTypeParam false)
13  (gotoifnot %₁₂ label₁₇)
14  TestMod.ShadowTypeParam
15  (= slot₃/if_val %₁₄)
16  (goto label₁₈)
17  (= slot₃/if_val core.nothing)
18  slot₃/if_val
19  slot₁/ShadowTypeParam
20  (call core.svec %₁₉)
21  (call core.svec %₁₁)
22  (call core.svec %₁₈)
23  (call core.resolve_typegroup TestMod %₂₀ %₂₁ %₂₂)
24  (= slot₁/ShadowTypeParam (call core.getfield %₂₃ 1))
25  slot₁/ShadowTypeParam
26  (call core.declare_const TestMod :ShadowTypeParam %₂₅)
27  latestworld
28  TestMod.ShadowTypeParam
29  (call core.apply_type core.Type %₂₈)
30  (call core.svec %₂₉ core.Any)
31  (call core.svec)
32  SourceLocation::3:5
33  (call core.svec %₃₀ %₃₁ %₃₂)
34  (call core.define_method TestMod core.nothing %₃₃
    --- code_info
    slots: [slot₁/#ctor-self#(!read) slot₂/x slot₃/tmp slot₄/T(single_assign)]
    1   TestMod.typeof
    2   (= slot₄/T (call %₁ slot₂/x))
    3   TestMod.ShadowTypeParam
    4   slot₄/T
    5   (call core.apply_type %₃ %₄)
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
35  latestworld
36  (return core.nothing)

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
1   (call core.declare_global TestMod :A false)
2   latestworld
3   (call core.declare_global TestMod :B false)
4   latestworld
5   (= slot₁/A (call core.TypeVar :A))
6   (= slot₂/B (call core.TypeVar :B))
7   (call core.svec)
8   (call core.svec :b)
9   (call core.svec)
10  slot₂/B
11  (call core.svec %₁₀)
12  (call core.svec %₇ %₈ %₉ false 1 core.Any %₁₁)
13  (call core.svec)
14  (call core.svec :a)
15  (call core.svec)
16  slot₁/A
17  (call core.svec %₁₆)
18  (call core.svec %₁₃ %₁₄ %₁₅ false 1 core.Any %₁₇)
19  (call core.isdefinedglobal TestMod :A false)
20  (gotoifnot %₁₉ label₂₄)
21  TestMod.A
22  (= slot₃/if_val %₂₁)
23  (goto label₂₅)
24  (= slot₃/if_val core.nothing)
25  slot₃/if_val
26  (call core.isdefinedglobal TestMod :B false)
27  (gotoifnot %₂₆ label₃₁)
28  TestMod.B
29  (= slot₄/if_val %₂₈)
30  (goto label₃₂)
31  (= slot₄/if_val core.nothing)
32  slot₄/if_val
33  slot₁/A
34  slot₂/B
35  (call core.svec %₃₃ %₃₄)
36  (call core.svec %₁₂ %₁₈)
37  (call core.svec %₂₅ %₃₂)
38  (call core.resolve_typegroup TestMod %₃₅ %₃₆ %₃₇)
39  (= slot₁/A (call core.getfield %₃₈ 1))
40  slot₁/A
41  (call core.declare_const TestMod :A %₄₀)
42  latestworld
43  (= slot₂/B (call core.getfield %₃₈ 2))
44  slot₂/B
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
1   (call core.declare_global TestMod :A false)
2   latestworld
3   (call core.declare_global TestMod :B false)
4   latestworld
5   (= slot₁/A (call core.TypeVar :A))
6   (= slot₂/B (call core.TypeVar :B))
7   (call core.svec)
8   (call core.svec :b)
9   (call core.svec)
10  TestMod.AbstractVector
11  slot₂/B
12  (call core.apply_type_or_typeapp %₁₀ %₁₁)
13  TestMod.Union
14  TestMod.Nothing
15  slot₂/B
16  (call core.apply_type_or_typeapp %₁₃ %₁₄ %₁₅)
17  (call core.svec %₁₆)
18  (call core.svec %₇ %₈ %₉ false 1 %₁₂ %₁₇)
19  (call core.svec)
20  (call core.svec :a)
21  (call core.svec)
22  slot₁/A
23  (call core.svec %₂₂)
24  (call core.svec %₁₉ %₂₀ %₂₁ false 1 core.Any %₂₃)
25  (call core.isdefinedglobal TestMod :A false)
26  (gotoifnot %₂₅ label₃₀)
27  TestMod.A
28  (= slot₃/if_val %₂₇)
29  (goto label₃₁)
30  (= slot₃/if_val core.nothing)
31  slot₃/if_val
32  (call core.isdefinedglobal TestMod :B false)
33  (gotoifnot %₃₂ label₃₇)
34  TestMod.B
35  (= slot₄/if_val %₃₄)
36  (goto label₃₈)
37  (= slot₄/if_val core.nothing)
38  slot₄/if_val
39  slot₁/A
40  slot₂/B
41  (call core.svec %₃₉ %₄₀)
42  (call core.svec %₁₈ %₂₄)
43  (call core.svec %₃₁ %₃₈)
44  (call core.resolve_typegroup TestMod %₄₁ %₄₂ %₄₃)
45  (= slot₁/A (call core.getfield %₄₄ 1))
46  slot₁/A
47  (call core.declare_const TestMod :A %₄₆)
48  latestworld
49  (= slot₂/B (call core.getfield %₄₄ 2))
50  slot₂/B
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

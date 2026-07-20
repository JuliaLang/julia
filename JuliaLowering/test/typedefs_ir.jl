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
4   (call core.svec)
5   (call core._setsuper! %₂ core.Any %₄)
6   (call core._typebody! false slot₁/A)
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
# Abstract type definition with supertype
abstract type A <: B end
#---------------------
1   (call core.svec)
2   (call core._abstracttype TestMod :A %₁)
3   (= slot₁/A %₂)
4   TestMod.B
5   (call core.svec)
6   (call core._setsuper! %₂ %₄ %₅)
7   (call core._typebody! false slot₁/A)
8   (call core.declare_global TestMod :A false)
9   latestworld
10  (call core.isdefinedglobal TestMod :A false)
11  (gotoifnot %₁₀ label₁₆)
12  TestMod.A
13  (call core._equiv_typedef %₁₂ %₂)
14  (gotoifnot %₁₃ label₁₆)
15  (goto label₁₈)
16  (call core.declare_const TestMod :A %₂)
17  latestworld
18  (return core.nothing)

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
9   slot₂/X
10  slot₃/Y
11  (call core.svec %₉ %₁₀)
12  (call core._setsuper! %₇ core.Any %₁₁)
13  (call core._typebody! false slot₁/A)
14  (call core.declare_global TestMod :A false)
15  latestworld
16  (call core.isdefinedglobal TestMod :A false)
17  (gotoifnot %₁₆ label₂₂)
18  TestMod.A
19  (call core._equiv_typedef %₁₈ %₇)
20  (gotoifnot %₁₉ label₂₂)
21  (goto label₂₄)
22  (call core.declare_const TestMod :A %₇)
23  latestworld
24  (return core.nothing)

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
4   (call core.svec)
5   (call core._setsuper! %₂ core.Any %₄)
6   (call core._typebody! false slot₁/P)
7   (call core.declare_global TestMod :P false)
8   latestworld
9   (call core.isdefinedglobal TestMod :P false)
10  (gotoifnot %₉ label₁₅)
11  TestMod.P
12  (call core._equiv_typedef %₁₁ %₂)
13  (gotoifnot %₁₂ label₁₅)
14  (goto label₁₇)
15  (call core.declare_const TestMod :P %₂)
16  latestworld
17  (return core.nothing)

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
9   slot₂/X
10  slot₃/Y
11  (call core.svec %₉ %₁₀)
12  (call core._setsuper! %₆ %₈ %₁₁)
13  (call core._typebody! false slot₁/P)
14  (call core.declare_global TestMod :P false)
15  latestworld
16  (call core.isdefinedglobal TestMod :P false)
17  (gotoifnot %₁₆ label₂₂)
18  TestMod.P
19  (call core._equiv_typedef %₁₈ %₆)
20  (gotoifnot %₁₉ label₂₂)
21  (goto label₂₄)
22  (call core.declare_const TestMod :P %₆)
23  latestworld
24  (return core.nothing)

########################################
# Primitive type definition with computed size (should this be allowed??)
primitive type P P_nbits() end
#---------------------
1   (call core.svec)
2   TestMod.P_nbits
3   (call %₂)
4   (call core._primitivetype TestMod :P %₁ %₃)
5   (= slot₁/P %₄)
6   (call core.svec)
7   (call core._setsuper! %₄ core.Any %₆)
8   (call core._typebody! false slot₁/P)
9   (call core.declare_global TestMod :P false)
10  latestworld
11  (call core.isdefinedglobal TestMod :P false)
12  (gotoifnot %₁₁ label₁₇)
13  TestMod.P
14  (call core._equiv_typedef %₁₃ %₄)
15  (gotoifnot %₁₄ label₁₇)
16  (goto label₁₉)
17  (call core.declare_const TestMod :P %₄)
18  latestworld
19  (return core.nothing)

########################################
# Empty struct
struct X
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (call core.svec)
4   (call core.svec)
5   (call core.svec)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 0)
7   (= slot₁/X %₆)
8   (call core.svec)
9   (call core._setsuper! %₆ core.Any %₈)
10  (call core.isdefinedglobal TestMod :X false)
11  (gotoifnot %₁₀ label₁₅)
12  TestMod.X
13  (= slot₂/if_val (call core._equiv_typedef %₁₂ %₆))
14  (goto label₁₆)
15  (= slot₂/if_val false)
16  slot₂/if_val
17  (gotoifnot %₁₆ label₂₁)
18  TestMod.X
19  (= slot₃/if_val %₁₈)
20  (goto label₂₂)
21  (= slot₃/if_val false)
22  slot₃/if_val
23  (gotoifnot %₁₆ label₂₄)
24  (call core.svec)
25  (call core.svec)
26  (call core._typebody! %₂₂ %₆ %₂₄ %₂₅)
27  (call core.declare_const TestMod :X %₂₆)
28  latestworld
29  TestMod.X
30  SourceLocation::1:1
31  (call top._defaultctors %₂₉ %₃₀)
32  latestworld
33  (return core.nothing)

########################################
# Empty struct with empty ctor
struct X
    X() = new()
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (call core.svec)
4   (call core.svec)
5   (call core.svec)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 0)
7   (= slot₁/X %₆)
8   (call core.svec)
9   (call core._setsuper! %₆ core.Any %₈)
10  (call core.isdefinedglobal TestMod :X false)
11  (gotoifnot %₁₀ label₁₅)
12  TestMod.X
13  (= slot₂/if_val (call core._equiv_typedef %₁₂ %₆))
14  (goto label₁₆)
15  (= slot₂/if_val false)
16  slot₂/if_val
17  (gotoifnot %₁₆ label₂₁)
18  TestMod.X
19  (= slot₃/if_val %₁₈)
20  (goto label₂₂)
21  (= slot₃/if_val false)
22  slot₃/if_val
23  (gotoifnot %₁₆ label₂₄)
24  (call core.svec)
25  (call core.svec)
26  (call core._typebody! %₂₂ %₆ %₂₄ %₂₅)
27  (call core.declare_const TestMod :X %₂₆)
28  latestworld
29  TestMod.X
30  (call core.apply_type core.Type %₂₉)
31  (call core.svec %₃₀)
32  (call core.svec)
33  SourceLocation::2:5
34  (call core.svec %₃₁ %₃₂ %₃₃)
35  --- method core.nothing %₃₄
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁)
    3   (return %₂)
36  latestworld
37  (return core.nothing)

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
3   (call core.svec)
4   (call core.svec :a :b :c)
5   (call core.svec)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 3)
7   (= slot₁/X %₆)
8   (call core.svec)
9   (call core._setsuper! %₆ core.Any %₈)
10  (call core.isdefinedglobal TestMod :X false)
11  (gotoifnot %₁₀ label₁₅)
12  TestMod.X
13  (= slot₂/if_val (call core._equiv_typedef %₁₂ %₆))
14  (goto label₁₆)
15  (= slot₂/if_val false)
16  slot₂/if_val
17  (gotoifnot %₁₆ label₂₁)
18  TestMod.X
19  (= slot₃/if_val %₁₈)
20  (goto label₂₂)
21  (= slot₃/if_val false)
22  slot₃/if_val
23  (gotoifnot %₁₆ label₂₄)
24  TestMod.T
25  (call core.svec core.Any %₂₄ core.Any)
26  (call core.svec)
27  (call core._typebody! %₂₂ %₆ %₂₅ %₂₆)
28  (call core.declare_const TestMod :X %₂₇)
29  latestworld
30  TestMod.X
31  SourceLocation::1:1
32  (call top._defaultctors %₃₀ %₃₁)
33  latestworld
34  (return core.nothing)

########################################
# Struct with supertype and type params
struct X{U, S <: V <: T} <: Z
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₃/U (call core.TypeVar :U))
4   TestMod.S
5   TestMod.T
6   (= slot₄/V (call core.TypeVar :V %₄ %₅))
7   slot₃/U
8   slot₄/V
9   (call core.svec %₇ %₈)
10  (call core.svec)
11  (call core.svec)
12  (call core._structtype TestMod :X %₉ %₁₀ %₁₁ false 0)
13  (= slot₂/X %₁₂)
14  TestMod.Z
15  slot₃/U
16  slot₄/V
17  (call core.svec %₁₅ %₁₆)
18  (call core._setsuper! %₁₂ %₁₄ %₁₇)
19  (call core.isdefinedglobal TestMod :X false)
20  (gotoifnot %₁₉ label₂₄)
21  TestMod.X
22  (= slot₅/if_val (call core._equiv_typedef %₂₁ %₁₂))
23  (goto label₂₅)
24  (= slot₅/if_val false)
25  slot₅/if_val
26  (gotoifnot %₂₅ label₃₀)
27  TestMod.X
28  (= slot₆/if_val %₂₇)
29  (goto label₃₁)
30  (= slot₆/if_val false)
31  slot₆/if_val
32  (gotoifnot %₂₅ label₄₃)
33  TestMod.X
34  (call top.getproperty %₃₃ :body)
35  (call top.getproperty %₃₄ :body)
36  (call top.getproperty %₃₅ :parameters)
37  (call top.indexed_iterate %₃₆ 1)
38  (= slot₃/U (call core.getfield %₃₇ 1))
39  (= slot₁/iterstate (call core.getfield %₃₇ 2))
40  slot₁/iterstate
41  (call top.indexed_iterate %₃₆ 2 %₄₀)
42  (= slot₄/V (call core.getfield %₄₁ 1))
43  (call core.svec)
44  slot₃/U
45  slot₄/V
46  (call core.svec %₄₄ %₄₅)
47  (call core._typebody! %₃₁ %₁₂ %₄₃ %₄₆)
48  (call core.declare_const TestMod :X %₄₇)
49  latestworld
50  TestMod.X
51  SourceLocation::1:1
52  (call top._defaultctors %₅₀ %₅₁)
53  latestworld
54  (return core.nothing)

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
3   (call core.svec)
4   (call core.svec :a :b :c)
5   (call core.svec 1 :const 2 :atomic 3 :atomic 3 :const)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 3)
7   (= slot₁/X %₆)
8   (call core.svec)
9   (call core._setsuper! %₆ core.Any %₈)
10  (call core.isdefinedglobal TestMod :X false)
11  (gotoifnot %₁₀ label₁₅)
12  TestMod.X
13  (= slot₂/if_val (call core._equiv_typedef %₁₂ %₆))
14  (goto label₁₆)
15  (= slot₂/if_val false)
16  slot₂/if_val
17  (gotoifnot %₁₆ label₂₁)
18  TestMod.X
19  (= slot₃/if_val %₁₈)
20  (goto label₂₂)
21  (= slot₃/if_val false)
22  slot₃/if_val
23  (gotoifnot %₁₆ label₂₄)
24  (call core.svec core.Any core.Any core.Any)
25  (call core.svec)
26  (call core._typebody! %₂₂ %₆ %₂₄ %₂₅)
27  (call core.declare_const TestMod :X %₂₆)
28  latestworld
29  TestMod.X
30  SourceLocation::1:1
31  (call top._defaultctors %₂₉ %₃₀)
32  latestworld
33  (return core.nothing)

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
2   (gotoifnot true label₃₆)
3   (call core.declare_global TestMod :X false)
4   latestworld
5   (call core.svec)
6   (call core.svec :a :b)
7   (call core.svec)
8   (call core._structtype TestMod :X %₅ %₆ %₇ false 2)
9   (= slot₂/X %₈)
10  (call core.svec)
11  (call core._setsuper! %₈ core.Any %₁₀)
12  (call core.isdefinedglobal TestMod :X false)
13  (gotoifnot %₁₂ label₁₇)
14  TestMod.X
15  (= slot₃/if_val (call core._equiv_typedef %₁₄ %₈))
16  (goto label₁₈)
17  (= slot₃/if_val false)
18  slot₃/if_val
19  (gotoifnot %₁₈ label₂₃)
20  TestMod.X
21  (= slot₄/if_val %₂₀)
22  (goto label₂₄)
23  (= slot₄/if_val false)
24  slot₄/if_val
25  (gotoifnot %₁₈ label₂₆)
26  (call core.svec core.Any core.Any)
27  (call core.svec)
28  (call core._typebody! %₂₄ %₈ %₂₆ %₂₇)
29  (call core.declare_const TestMod :X %₂₈)
30  latestworld
31  TestMod.X
32  SourceLocation:none:1:0
33  (call top._defaultctors %₃₁ %₃₂)
34  latestworld
35  (= slot₁/val core.nothing)
36  (call Base.Docs.Binding TestMod :X)
37  (call Core.svec "X docs\n")
38  (call Pair{Symbol, Any} :a "field a docs")
39  (call Pair{Symbol, Any} :b "field b docs")
40  (call Dict{Symbol, Any} %₃₈ %₃₉)
41  (call Pair :fields %₄₀)
42  (call Dict{Symbol, Any} :path => "none" :linenumber => 1 :module => TestMod %₄₁)
43  (call Base.Docs.docstr %₃₇ %₄₂)
44  TestMod.Union
45  (call core.apply_type %₄₄)
46  (call Base.Docs.doc! TestMod %₃₆ %₄₃ %₄₅)
47  (gotoifnot true label₅₀)
48  slot₁/val
49  (return %₄₈)
50  (return core.nothing)

########################################
# Struct with outer constructor
struct X{U}
    x::U
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₂/U (call core.TypeVar :U))
4   slot₂/U
5   (call core.svec %₄)
6   (call core.svec :x)
7   (call core.svec)
8   (call core._structtype TestMod :X %₅ %₆ %₇ false 1)
9   (= slot₁/X %₈)
10  slot₂/U
11  (call core.svec %₁₀)
12  (call core._setsuper! %₈ core.Any %₁₁)
13  (call core.isdefinedglobal TestMod :X false)
14  (gotoifnot %₁₃ label₁₈)
15  TestMod.X
16  (= slot₃/if_val (call core._equiv_typedef %₁₅ %₈))
17  (goto label₁₉)
18  (= slot₃/if_val false)
19  slot₃/if_val
20  (gotoifnot %₁₉ label₂₄)
21  TestMod.X
22  (= slot₄/if_val %₂₁)
23  (goto label₂₅)
24  (= slot₄/if_val false)
25  slot₄/if_val
26  (gotoifnot %₁₉ label₃₂)
27  TestMod.X
28  (call top.getproperty %₂₇ :body)
29  (call top.getproperty %₂₈ :parameters)
30  (call top.indexed_iterate %₂₉ 1)
31  (= slot₂/U (call core.getfield %₃₀ 1))
32  slot₂/U
33  (call core.svec %₃₂)
34  slot₂/U
35  (call core.svec %₃₄)
36  (call core._typebody! %₂₅ %₈ %₃₃ %₃₅)
37  (call core.declare_const TestMod :X %₃₆)
38  latestworld
39  TestMod.X
40  SourceLocation::1:1
41  (call top._defaultctors %₃₉ %₄₀)
42  latestworld
43  (return core.nothing)

########################################
# Struct with outer constructor where one typevar is constrained by the other
# See https://github.com/JuliaLang/julia/issues/27269)
struct X{T, S <: Vector{T}}
    v::Vector{S}
end
#---------------------
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₃/T (call core.TypeVar :T))
4   TestMod.Vector
5   slot₃/T
6   (call core.apply_type %₄ %₅)
7   (= slot₄/S (call core.TypeVar :S %₆))
8   slot₃/T
9   slot₄/S
10  (call core.svec %₈ %₉)
11  (call core.svec :v)
12  (call core.svec)
13  (call core._structtype TestMod :X %₁₀ %₁₁ %₁₂ false 1)
14  (= slot₂/X %₁₃)
15  slot₃/T
16  slot₄/S
17  (call core.svec %₁₅ %₁₆)
18  (call core._setsuper! %₁₃ core.Any %₁₇)
19  (call core.isdefinedglobal TestMod :X false)
20  (gotoifnot %₁₉ label₂₄)
21  TestMod.X
22  (= slot₅/if_val (call core._equiv_typedef %₂₁ %₁₃))
23  (goto label₂₅)
24  (= slot₅/if_val false)
25  slot₅/if_val
26  (gotoifnot %₂₅ label₃₀)
27  TestMod.X
28  (= slot₆/if_val %₂₇)
29  (goto label₃₁)
30  (= slot₆/if_val false)
31  slot₆/if_val
32  (gotoifnot %₂₅ label₄₃)
33  TestMod.X
34  (call top.getproperty %₃₃ :body)
35  (call top.getproperty %₃₄ :body)
36  (call top.getproperty %₃₅ :parameters)
37  (call top.indexed_iterate %₃₆ 1)
38  (= slot₃/T (call core.getfield %₃₇ 1))
39  (= slot₁/iterstate (call core.getfield %₃₇ 2))
40  slot₁/iterstate
41  (call top.indexed_iterate %₃₆ 2 %₄₀)
42  (= slot₄/S (call core.getfield %₄₁ 1))
43  TestMod.Vector
44  slot₄/S
45  (call core.apply_type %₄₃ %₄₄)
46  (call core.svec %₄₅)
47  slot₃/T
48  slot₄/S
49  (call core.svec %₄₇ %₄₈)
50  (call core._typebody! %₃₁ %₁₃ %₄₆ %₄₉)
51  (call core.declare_const TestMod :X %₅₀)
52  latestworld
53  TestMod.X
54  SourceLocation::1:1
55  (call top._defaultctors %₅₃ %₅₄)
56  latestworld
57  (return core.nothing)

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
3   (call core.svec)
4   (call core.svec :x)
5   (call core.svec)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 1)
7   (= slot₁/X %₆)
8   (call core.svec)
9   (call core._setsuper! %₆ core.Any %₈)
10  (call core.isdefinedglobal TestMod :X false)
11  (gotoifnot %₁₀ label₁₅)
12  TestMod.X
13  (= slot₃/if_val (call core._equiv_typedef %₁₂ %₆))
14  (goto label₁₆)
15  (= slot₃/if_val false)
16  slot₃/if_val
17  (gotoifnot %₁₆ label₂₁)
18  TestMod.X
19  (= slot₄/if_val %₁₈)
20  (goto label₂₂)
21  (= slot₄/if_val false)
22  slot₄/if_val
23  (gotoifnot %₁₆ label₂₄)
24  (call core.svec core.Any)
25  (call core.svec)
26  (call core._typebody! %₂₂ %₆ %₂₄ %₂₅)
27  (call core.declare_const TestMod :X %₂₆)
28  latestworld
29  (call core.svec)
30  (call core.svec)
31  (call core.svec)
32  (call JuliaLowering.eval_closure_type TestMod :#f##0 %₂₉ %₃₀ %₃₁)
33  latestworld
34  TestMod.#f##0
35  (new %₃₄)
36  (= slot₂/f %₃₅)
37  TestMod.#f##0
38  (call core.svec %₃₇)
39  (call core.svec)
40  SourceLocation::3:5
41  (call core.svec %₃₈ %₃₉ %₄₀)
42  --- method core.nothing %₄₁
    slots: [slot₁/#self#(!read)]
    1   TestMod.X
    2   (new %₁ 1)
    3   (return %₂)
43  latestworld
44  TestMod.X
45  (call core.apply_type core.Type %₄₄)
46  (call core.svec %₄₅)
47  (call core.svec)
48  SourceLocation::4:5
49  (call core.svec %₄₆ %₄₇ %₄₈)
50  --- code_info
    slots: [slot₁/#ctor-self#(!read)]
    1   (captured_local 1)
    2   (call %₁)
    3   (return %₂)
51  (call core.svec slot₂/f)
52  (call JuliaLowering.replace_captured_locals %₅₀ %₅₁)
53  --- method core.nothing %₄₉ %₅₂
54  latestworld
55  TestMod.X
56  (call core.apply_type core.Type %₅₅)
57  (call core.svec %₅₆ core.Any)
58  (call core.svec)
59  SourceLocation::5:5
60  (call core.svec %₅₇ %₅₈ %₅₉)
61  --- method core.nothing %₆₀
    slots: [slot₁/#ctor-self# slot₂/x]
    1   slot₁/#ctor-self#
    2   (new %₁ slot₂/x)
    3   (return %₂)
62  latestworld
63  TestMod.X
64  (call core.apply_type core.Type %₆₃)
65  (call core.svec %₆₄ core.Any core.Any)
66  (call core.svec)
67  SourceLocation::6:5
68  (call core.svec %₆₅ %₆₆ %₆₇)
69  --- method core.nothing %₆₈
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
70  latestworld
71  TestMod.X
72  (call core.apply_type core.Type %₇₁)
73  (call core.svec %₇₂ core.Any core.Any core.Any)
74  (call core.svec)
75  SourceLocation::10:5
76  (call core.svec %₇₃ %₇₄ %₇₅)
77  --- method core.nothing %₇₆
    slots: [slot₁/#ctor-self# slot₂/a slot₃/b(!read) slot₄/c(!read)]
    1   slot₁/#ctor-self#
    2   (new %₁ slot₂/a)
    3   (return %₂)
78  latestworld
79  (return core.nothing)

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
3   (= slot₃/S (call core.TypeVar :S))
4   (= slot₄/T (call core.TypeVar :T))
5   slot₃/S
6   slot₄/T
7   (call core.svec %₅ %₆)
8   (call core.svec :x)
9   (call core.svec)
10  (call core._structtype TestMod :X %₇ %₈ %₉ false 1)
11  (= slot₂/X %₁₀)
12  slot₃/S
13  slot₄/T
14  (call core.svec %₁₂ %₁₃)
15  (call core._setsuper! %₁₀ core.Any %₁₄)
16  (call core.isdefinedglobal TestMod :X false)
17  (gotoifnot %₁₆ label₂₁)
18  TestMod.X
19  (= slot₆/if_val (call core._equiv_typedef %₁₈ %₁₀))
20  (goto label₂₂)
21  (= slot₆/if_val false)
22  slot₆/if_val
23  (gotoifnot %₂₂ label₂₇)
24  TestMod.X
25  (= slot₇/if_val %₂₄)
26  (goto label₂₈)
27  (= slot₇/if_val false)
28  slot₇/if_val
29  (gotoifnot %₂₂ label₄₀)
30  TestMod.X
31  (call top.getproperty %₃₀ :body)
32  (call top.getproperty %₃₁ :body)
33  (call top.getproperty %₃₂ :parameters)
34  (call top.indexed_iterate %₃₃ 1)
35  (= slot₃/S (call core.getfield %₃₄ 1))
36  (= slot₁/iterstate (call core.getfield %₃₄ 2))
37  slot₁/iterstate
38  (call top.indexed_iterate %₃₃ 2 %₃₇)
39  (= slot₄/T (call core.getfield %₃₈ 1))
40  (call core.svec core.Any)
41  slot₃/S
42  slot₄/T
43  (call core.svec %₄₁ %₄₂)
44  (call core._typebody! %₂₈ %₁₀ %₄₀ %₄₃)
45  (call core.declare_const TestMod :X %₄₄)
46  latestworld
47  TestMod.X
48  TestMod.A
49  TestMod.B
50  (call core.apply_type %₄₇ %₄₈ %₄₉)
51  (call core.apply_type core.Type %₅₀)
52  (call core.svec %₅₁)
53  (call core.svec)
54  SourceLocation::3:5
55  (call core.svec %₅₂ %₅₃ %₅₄)
56  --- method core.nothing %₅₅
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁ 1)
    3   (return %₂)
57  latestworld
58  (call core.TypeVar :U)
59  (call core.TypeVar :V)
60  TestMod.X
61  (call core.apply_type %₆₀ %₅₈ %₅₉)
62  (call core.apply_type core.Type %₆₁)
63  (call core.svec %₆₂)
64  (call core.svec %₅₈ %₅₉)
65  SourceLocation::4:5
66  (call core.svec %₆₃ %₆₄ %₆₅)
67  --- method core.nothing %₆₆
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁ 1)
    3   (return %₂)
68  latestworld
69  (call core.svec)
70  (call core.svec)
71  (call core.svec)
72  (call JuliaLowering.eval_closure_type TestMod :#f##1 %₆₉ %₇₀ %₇₁)
73  latestworld
74  TestMod.#f##1
75  (new %₇₄)
76  (= slot₅/f %₇₅)
77  TestMod.#f##1
78  (call core.svec %₇₇)
79  (call core.svec)
80  SourceLocation::5:5
81  (call core.svec %₇₈ %₇₉ %₈₀)
82  --- method core.nothing %₈₁
    slots: [slot₁/#self#(!read)]
    1   TestMod.X
    2   TestMod.A
    3   TestMod.B
    4   (call core.apply_type %₁ %₂ %₃)
    5   (new %₄ 1)
    6   (return %₅)
83  latestworld
84  (return core.nothing)

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
3   (call core.svec)
4   (call core.svec :x :y)
5   (call core.svec)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 2)
7   (= slot₁/X %₆)
8   (call core.svec)
9   (call core._setsuper! %₆ core.Any %₈)
10  (call core.isdefinedglobal TestMod :X false)
11  (gotoifnot %₁₀ label₁₅)
12  TestMod.X
13  (= slot₂/if_val (call core._equiv_typedef %₁₂ %₆))
14  (goto label₁₆)
15  (= slot₂/if_val false)
16  slot₂/if_val
17  (gotoifnot %₁₆ label₂₁)
18  TestMod.X
19  (= slot₃/if_val %₁₈)
20  (goto label₂₂)
21  (= slot₃/if_val false)
22  slot₃/if_val
23  (gotoifnot %₁₆ label₂₄)
24  (call core.svec core.Any core.Any)
25  (call core.svec)
26  (call core._typebody! %₂₂ %₆ %₂₄ %₂₅)
27  (call core.declare_const TestMod :X %₂₆)
28  latestworld
29  TestMod.X
30  (call core.apply_type core.Type %₂₉)
31  (call core.svec %₃₀ core.Any)
32  (call core.svec)
33  SourceLocation::4:5
34  (call core.svec %₃₁ %₃₂ %₃₃)
35  --- method core.nothing %₃₄
    slots: [slot₁/#ctor-self# slot₂/xs]
    1   slot₁/#ctor-self#
    2   (call core._apply_iterate top.iterate core.tuple slot₂/xs)
    3   (splatnew %₁ %₂)
    4   (return %₃)
36  latestworld
37  (return core.nothing)

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
3   (= slot₂/T (call core.TypeVar :T))
4   slot₂/T
5   (call core.svec %₄)
6   (call core.svec :x :y)
7   (call core.svec)
8   (call core._structtype TestMod :X %₅ %₆ %₇ false 2)
9   (= slot₁/X %₈)
10  slot₂/T
11  (call core.svec %₁₀)
12  (call core._setsuper! %₈ core.Any %₁₁)
13  (call core.isdefinedglobal TestMod :X false)
14  (gotoifnot %₁₃ label₁₈)
15  TestMod.X
16  (= slot₃/if_val (call core._equiv_typedef %₁₅ %₈))
17  (goto label₁₉)
18  (= slot₃/if_val false)
19  slot₃/if_val
20  (gotoifnot %₁₉ label₂₄)
21  TestMod.X
22  (= slot₄/if_val %₂₁)
23  (goto label₂₅)
24  (= slot₄/if_val false)
25  slot₄/if_val
26  (gotoifnot %₁₉ label₃₂)
27  TestMod.X
28  (call top.getproperty %₂₇ :body)
29  (call top.getproperty %₂₈ :parameters)
30  (call top.indexed_iterate %₂₉ 1)
31  (= slot₂/T (call core.getfield %₃₀ 1))
32  slot₂/T
33  TestMod.A
34  (call core.svec %₃₂ %₃₃)
35  slot₂/T
36  (call core.svec %₃₅)
37  (call core._typebody! %₂₅ %₈ %₃₄ %₃₆)
38  (call core.declare_const TestMod :X %₃₇)
39  latestworld
40  (call core.TypeVar :T)
41  TestMod.X
42  (call core.apply_type %₄₁ %₄₀)
43  (call core.apply_type core.Type %₄₂)
44  (call core.svec %₄₃ core.Any)
45  (call core.svec %₄₀)
46  SourceLocation::4:5
47  (call core.svec %₄₄ %₄₅ %₄₆)
48  --- method core.nothing %₄₇
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
49  latestworld
50  (return core.nothing)

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
# Error: User defined inner constructors without enough type params
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
1   (call core.declare_global TestMod :ShadowTypeParam false)
2   latestworld
3   (= slot₂/T (call core.TypeVar :T))
4   slot₂/T
5   (call core.svec %₄)
6   (call core.svec :x)
7   (call core.svec)
8   (call core._structtype TestMod :ShadowTypeParam %₅ %₆ %₇ false 1)
9   (= slot₁/ShadowTypeParam %₈)
10  slot₂/T
11  (call core.svec %₁₀)
12  (call core._setsuper! %₈ core.Any %₁₁)
13  (call core.isdefinedglobal TestMod :ShadowTypeParam false)
14  (gotoifnot %₁₃ label₁₈)
15  TestMod.ShadowTypeParam
16  (= slot₃/if_val (call core._equiv_typedef %₁₅ %₈))
17  (goto label₁₉)
18  (= slot₃/if_val false)
19  slot₃/if_val
20  (gotoifnot %₁₉ label₂₄)
21  TestMod.ShadowTypeParam
22  (= slot₄/if_val %₂₁)
23  (goto label₂₅)
24  (= slot₄/if_val false)
25  slot₄/if_val
26  (gotoifnot %₁₉ label₃₂)
27  TestMod.ShadowTypeParam
28  (call top.getproperty %₂₇ :body)
29  (call top.getproperty %₂₈ :parameters)
30  (call top.indexed_iterate %₂₉ 1)
31  (= slot₂/T (call core.getfield %₃₀ 1))
32  slot₂/T
33  (call core.svec %₃₂)
34  slot₂/T
35  (call core.svec %₃₄)
36  (call core._typebody! %₂₅ %₈ %₃₃ %₃₅)
37  (call core.declare_const TestMod :ShadowTypeParam %₃₆)
38  latestworld
39  TestMod.ShadowTypeParam
40  (call core.apply_type core.Type %₃₉)
41  (call core.svec %₄₀ core.Any)
42  (call core.svec)
43  SourceLocation::3:5
44  (call core.svec %₄₁ %₄₂ %₄₃)
45  --- method core.nothing %₄₄
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
46  latestworld
47  (return core.nothing)

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
15  slot₂/A
16  slot₃/B
17  (call core.svec %₁₅ %₁₆)
18  (call core.svec %₈ %₁₄)
19  (call core.resolve_typegroup TestMod %₁₇ %₁₈)
20  (call top.indexed_iterate %₁₉ 1)
21  (= slot₂/A (call core.getfield %₂₀ 1))
22  (= slot₁/iterstate (call core.getfield %₂₀ 2))
23  slot₁/iterstate
24  (call top.indexed_iterate %₁₉ 2 %₂₃)
25  (= slot₃/B (call core.getfield %₂₄ 1))
26  slot₂/A
27  (call core.declare_const TestMod :A %₂₆)
28  latestworld
29  slot₃/B
30  (call core.declare_const TestMod :B %₂₉)
31  latestworld
32  TestMod.A
33  SourceLocation::2:5
34  (call top._defaultctors %₃₂ %₃₃)
35  TestMod.B
36  SourceLocation::5:5
37  (call top._defaultctors %₃₅ %₃₆)
38  latestworld
39  (return core.nothing)

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
21  slot₂/A
22  slot₃/B
23  (call core.svec %₂₁ %₂₂)
24  (call core.svec %₁₄ %₂₀)
25  (call core.resolve_typegroup TestMod %₂₃ %₂₄)
26  (call top.indexed_iterate %₂₅ 1)
27  (= slot₂/A (call core.getfield %₂₆ 1))
28  (= slot₁/iterstate (call core.getfield %₂₆ 2))
29  slot₁/iterstate
30  (call top.indexed_iterate %₂₅ 2 %₂₉)
31  (= slot₃/B (call core.getfield %₃₀ 1))
32  slot₂/A
33  (call core.declare_const TestMod :A %₃₂)
34  latestworld
35  slot₃/B
36  (call core.declare_const TestMod :B %₃₅)
37  latestworld
38  TestMod.A
39  SourceLocation::2:5
40  (call top._defaultctors %₃₈ %₃₉)
41  TestMod.B
42  SourceLocation::5:5
43  (call top._defaultctors %₄₁ %₄₂)
44  latestworld
45  (return core.nothing)

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

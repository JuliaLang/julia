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
#     └─┘ ── unexpected semicolon in type parameter list

########################################
# Error: assignment in type application
X{S, T=w}
#---------------------
LoweringError:
X{S, T=w}
#   └──┘ ── misplace assignment in type parameter list

########################################
# Simple abstract type definition
abstract type A end
#---------------------
1   (call core.svec)
2   (call core._abstracttype TestMod :A %₁)
3   (= slot₁/A %₂)
4   (call core._setsuper! %₂ core.Any)
5   slot₁/A
6   (call core._typebody! false %₅)
7   (global TestMod.A)
8   latestworld
9   (call core.isdefinedglobal TestMod :A false)
10  (gotoifnot %₉ label₁₅)
11  TestMod.A
12  (call core._equiv_typedef %₁₁ %₂)
13  (gotoifnot %₁₂ label₁₅)
14  (goto label₁₇)
15  (constdecl TestMod.A %₂)
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
5   (call core._setsuper! %₂ %₄)
6   slot₁/A
7   (call core._typebody! false %₆)
8   (global TestMod.A)
9   latestworld
10  (call core.isdefinedglobal TestMod :A false)
11  (gotoifnot %₁₀ label₁₆)
12  TestMod.A
13  (call core._equiv_typedef %₁₂ %₂)
14  (gotoifnot %₁₃ label₁₆)
15  (goto label₁₈)
16  (constdecl TestMod.A %₂)
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
9   (call core._setsuper! %₇ core.Any)
10  slot₁/A
11  (call core._typebody! false %₁₀)
12  (global TestMod.A)
13  latestworld
14  (call core.isdefinedglobal TestMod :A false)
15  (gotoifnot %₁₄ label₂₀)
16  TestMod.A
17  (call core._equiv_typedef %₁₆ %₇)
18  (gotoifnot %₁₇ label₂₀)
19  (goto label₂₂)
20  (constdecl TestMod.A %₇)
21  latestworld
22  (return core.nothing)

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
5   slot₁/P
6   (call core._typebody! false %₅)
7   (global TestMod.P)
8   latestworld
9   (call core.isdefinedglobal TestMod :P false)
10  (gotoifnot %₉ label₁₅)
11  TestMod.P
12  (call core._equiv_typedef %₁₁ %₂)
13  (gotoifnot %₁₂ label₁₅)
14  (goto label₁₇)
15  (constdecl TestMod.P %₂)
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
9   (call core._setsuper! %₆ %₈)
10  slot₁/P
11  (call core._typebody! false %₁₀)
12  (global TestMod.P)
13  latestworld
14  (call core.isdefinedglobal TestMod :P false)
15  (gotoifnot %₁₄ label₂₀)
16  TestMod.P
17  (call core._equiv_typedef %₁₆ %₆)
18  (gotoifnot %₁₇ label₂₀)
19  (goto label₂₂)
20  (constdecl TestMod.P %₆)
21  latestworld
22  (return core.nothing)

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
7   slot₁/P
8   (call core._typebody! false %₇)
9   (global TestMod.P)
10  latestworld
11  (call core.isdefinedglobal TestMod :P false)
12  (gotoifnot %₁₁ label₁₇)
13  TestMod.P
14  (call core._equiv_typedef %₁₃ %₄)
15  (gotoifnot %₁₄ label₁₇)
16  (goto label₁₉)
17  (constdecl TestMod.P %₄)
18  latestworld
19  (return core.nothing)

########################################
# Empty struct
struct X
end
#---------------------
1   (global TestMod.X)
2   latestworld
3   (call core.svec)
4   (call core.svec)
5   (call core.svec)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 0)
7   (= slot₁/X %₆)
8   (call core._setsuper! %₆ core.Any)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₂/if_val (call core._equiv_typedef %₁₁ %₆))
13  (goto label₁₅)
14  (= slot₂/if_val false)
15  slot₂/if_val
16  (gotoifnot %₁₅ label₂₀)
17  TestMod.X
18  (= slot₃/if_val %₁₇)
19  (goto label₂₁)
20  (= slot₃/if_val false)
21  slot₃/if_val
22  (gotoifnot %₁₅ label₂₃)
23  (call core.svec)
24  (call core._typebody! %₂₁ %₆ %₂₃)
25  (constdecl TestMod.X %₂₄)
26  latestworld
27  TestMod.X
28  (call core.apply_type core.Type %₂₇)
29  (call core.svec %₂₈)
30  (call core.svec)
31  SourceLocation::1:1
32  (call core.svec %₂₉ %₃₀ %₃₁)
33  --- method core.nothing %₃₂
    slots: [slot₁/#self#(!read)]
    1   TestMod.X
    2   (new %₁)
    3   (return %₂)
34  latestworld
35  (return core.nothing)

########################################
# Basic struct
struct X
    a
    b::T
    c
end
#---------------------
1   (global TestMod.X)
2   latestworld
3   (call core.svec)
4   (call core.svec :a :b :c)
5   (call core.svec)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 3)
7   (= slot₁/X %₆)
8   (call core._setsuper! %₆ core.Any)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₂/if_val (call core._equiv_typedef %₁₁ %₆))
13  (goto label₁₅)
14  (= slot₂/if_val false)
15  slot₂/if_val
16  (gotoifnot %₁₅ label₂₀)
17  TestMod.X
18  (= slot₃/if_val %₁₇)
19  (goto label₂₁)
20  (= slot₃/if_val false)
21  slot₃/if_val
22  (gotoifnot %₁₅ label₂₃)
23  TestMod.T
24  (call core.svec core.Any %₂₃ core.Any)
25  (call core._typebody! %₂₁ %₆ %₂₄)
26  (constdecl TestMod.X %₂₅)
27  latestworld
28  TestMod.T
29  (call core.=== core.Any %₂₈)
30  (gotoifnot %₂₉ label₃₂)
31  (goto label₄₀)
32  TestMod.X
33  (call core.apply_type core.Type %₃₂)
34  (call core.svec %₃₃ core.Any core.Any core.Any)
35  (call core.svec)
36  SourceLocation::1:1
37  (call core.svec %₃₄ %₃₅ %₃₆)
38  --- method core.nothing %₃₇
    slots: [slot₁/#ctor-self# slot₂/a slot₃/b slot₄/c slot₅/tmp]
    1   (call core.fieldtype slot₁/#ctor-self# 2)
    2   slot₃/b
    3   (= slot₅/tmp %₂)
    4   slot₅/tmp
    5   (call core.isa %₄ %₁)
    6   (gotoifnot %₅ label₈)
    7   (goto label₁₀)
    8   slot₅/tmp
    9   (= slot₅/tmp (call top.convert %₁ %₈))
    10  slot₅/tmp
    11  (new slot₁/#ctor-self# slot₂/a %₁₀ slot₄/c)
    12  (return %₁₁)
39  latestworld
40  TestMod.X
41  (call core.apply_type core.Type %₄₀)
42  TestMod.T
43  (call core.svec %₄₁ core.Any %₄₂ core.Any)
44  (call core.svec)
45  SourceLocation::1:1
46  (call core.svec %₄₃ %₄₄ %₄₅)
47  --- method core.nothing %₄₆
    slots: [slot₁/#self#(!read) slot₂/a slot₃/b slot₄/c]
    1   TestMod.X
    2   (new %₁ slot₂/a slot₃/b slot₄/c)
    3   (return %₂)
48  latestworld
49  (return core.nothing)

########################################
# Struct with supertype and type params
struct X{U, S <: V <: T} <: Z
end
#---------------------
1   (global TestMod.X)
2   latestworld
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
16  (call core.isdefinedglobal TestMod :X false)
17  (gotoifnot %₁₆ label₂₁)
18  TestMod.X
19  (= slot₅/if_val (call core._equiv_typedef %₁₈ %₁₂))
20  (goto label₂₂)
21  (= slot₅/if_val false)
22  slot₅/if_val
23  (gotoifnot %₂₂ label₂₇)
24  TestMod.X
25  (= slot₆/if_val %₂₄)
26  (goto label₂₈)
27  (= slot₆/if_val false)
28  slot₆/if_val
29  (gotoifnot %₂₂ label₄₀)
30  TestMod.X
31  (call top.getproperty %₃₀ :body)
32  (call top.getproperty %₃₁ :body)
33  (call top.getproperty %₃₂ :parameters)
34  (call top.indexed_iterate %₃₃ 1)
35  (= slot₂/U (call core.getfield %₃₄ 1))
36  (= slot₁/iterstate (call core.getfield %₃₄ 2))
37  slot₁/iterstate
38  (call top.indexed_iterate %₃₃ 2 %₃₇)
39  (= slot₃/V (call core.getfield %₃₈ 1))
40  (call core.svec)
41  (call core._typebody! %₂₈ %₁₂ %₄₀)
42  (constdecl TestMod.X %₄₁)
43  latestworld
44  slot₂/U
45  slot₃/V
46  TestMod.X
47  slot₂/U
48  slot₃/V
49  (call core.apply_type %₄₆ %₄₇ %₄₈)
50  (call core.apply_type core.Type %₄₉)
51  (call core.UnionAll %₄₅ %₅₀)
52  (call core.UnionAll %₄₄ %₅₁)
53  (call core.svec %₅₂)
54  (call core.svec)
55  SourceLocation::1:1
56  (call core.svec %₅₃ %₅₄ %₅₅)
57  --- method core.nothing %₅₆
    slots: [slot₁/#ctor-self#]
    1   (new slot₁/#ctor-self#)
    2   (return %₁)
58  latestworld
59  (return core.nothing)

########################################
# Struct with const and atomic fields
struct X
    const a
    @atomic b
    const @atomic c
end
#---------------------
1   (global TestMod.X)
2   latestworld
3   (call core.svec)
4   (call core.svec :a :b :c)
5   (call core.svec 1 :const 2 :atomic 3 :atomic 3 :const)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 3)
7   (= slot₁/X %₆)
8   (call core._setsuper! %₆ core.Any)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₂/if_val (call core._equiv_typedef %₁₁ %₆))
13  (goto label₁₅)
14  (= slot₂/if_val false)
15  slot₂/if_val
16  (gotoifnot %₁₅ label₂₀)
17  TestMod.X
18  (= slot₃/if_val %₁₇)
19  (goto label₂₁)
20  (= slot₃/if_val false)
21  slot₃/if_val
22  (gotoifnot %₁₅ label₂₃)
23  (call core.svec core.Any core.Any core.Any)
24  (call core._typebody! %₂₁ %₆ %₂₃)
25  (constdecl TestMod.X %₂₄)
26  latestworld
27  TestMod.X
28  (call core.apply_type core.Type %₂₇)
29  (call core.svec %₂₈ core.Any core.Any core.Any)
30  (call core.svec)
31  SourceLocation::1:1
32  (call core.svec %₂₉ %₃₀ %₃₁)
33  --- method core.nothing %₃₂
    slots: [slot₁/#self#(!read) slot₂/a slot₃/b slot₄/c]
    1   TestMod.X
    2   (new %₁ slot₂/a slot₃/b slot₄/c)
    3   (return %₂)
34  latestworld
35  (return core.nothing)

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
1   (global TestMod.X)
2   latestworld
3   (call core.svec)
4   (call core.svec :a :b)
5   (call core.svec)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 2)
7   (= slot₁/X %₆)
8   (call core._setsuper! %₆ core.Any)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₂/if_val (call core._equiv_typedef %₁₁ %₆))
13  (goto label₁₅)
14  (= slot₂/if_val false)
15  slot₂/if_val
16  (gotoifnot %₁₅ label₂₀)
17  TestMod.X
18  (= slot₃/if_val %₁₇)
19  (goto label₂₁)
20  (= slot₃/if_val false)
21  slot₃/if_val
22  (gotoifnot %₁₅ label₂₃)
23  (call core.svec core.Any core.Any)
24  (call core._typebody! %₂₁ %₆ %₂₃)
25  (constdecl TestMod.X %₂₄)
26  latestworld
27  TestMod.X
28  (call core.apply_type core.Type %₂₇)
29  (call core.svec %₂₈ core.Any core.Any)
30  (call core.svec)
31  SourceLocation::4:1
32  (call core.svec %₂₉ %₃₀ %₃₁)
33  --- method core.nothing %₃₂
    slots: [slot₁/#self#(!read) slot₂/a slot₃/b]
    1   TestMod.X
    2   (new %₁ slot₂/a slot₃/b)
    3   (return %₂)
34  latestworld
35  JuliaLowering.bind_docs!
36  (call core.tuple :field_docs)
37  (call core.apply_type core.NamedTuple %₃₆)
38  (call core.svec 1 "field a docs" 2 "field b docs")
39  (call core.tuple %₃₈)
40  (call %₃₇ %₃₉)
41  TestMod.X
42  SourceLocation::4:1
43  (call core.kwcall %₄₀ %₃₅ %₄₁ "X docs\n" %₄₂)
44  (return core.nothing)

########################################
# Struct with outer constructor
struct X{U}
    x::U
end
#---------------------
1   (global TestMod.X)
2   latestworld
3   (= slot₁/U (call core.TypeVar :U))
4   slot₁/U
5   (call core.svec %₄)
6   (call core.svec :x)
7   (call core.svec)
8   (call core._structtype TestMod :X %₅ %₆ %₇ false 1)
9   (= slot₂/X %₈)
10  (call core._setsuper! %₈ core.Any)
11  (call core.isdefinedglobal TestMod :X false)
12  (gotoifnot %₁₁ label₁₆)
13  TestMod.X
14  (= slot₃/if_val (call core._equiv_typedef %₁₃ %₈))
15  (goto label₁₇)
16  (= slot₃/if_val false)
17  slot₃/if_val
18  (gotoifnot %₁₇ label₂₂)
19  TestMod.X
20  (= slot₄/if_val %₁₉)
21  (goto label₂₃)
22  (= slot₄/if_val false)
23  slot₄/if_val
24  (gotoifnot %₁₇ label₃₀)
25  TestMod.X
26  (call top.getproperty %₂₅ :body)
27  (call top.getproperty %₂₆ :parameters)
28  (call top.indexed_iterate %₂₇ 1)
29  (= slot₁/U (call core.getfield %₂₈ 1))
30  slot₁/U
31  (call core.svec %₃₀)
32  (call core._typebody! %₂₃ %₈ %₃₁)
33  (constdecl TestMod.X %₃₂)
34  latestworld
35  slot₁/U
36  TestMod.X
37  slot₁/U
38  (call core.apply_type %₃₆ %₃₇)
39  (call core.apply_type core.Type %₃₈)
40  (call core.UnionAll %₃₅ %₃₉)
41  (call core.svec %₄₀ core.Any)
42  (call core.svec)
43  SourceLocation::1:1
44  (call core.svec %₄₁ %₄₂ %₄₃)
45  --- method core.nothing %₄₄
    slots: [slot₁/#ctor-self# slot₂/x slot₃/tmp]
    1   (call core.fieldtype slot₁/#ctor-self# 1)
    2   slot₂/x
    3   (= slot₃/tmp %₂)
    4   slot₃/tmp
    5   (call core.isa %₄ %₁)
    6   (gotoifnot %₅ label₈)
    7   (goto label₁₀)
    8   slot₃/tmp
    9   (= slot₃/tmp (call top.convert %₁ %₈))
    10  slot₃/tmp
    11  (new slot₁/#ctor-self# %₁₀)
    12  (return %₁₁)
46  latestworld
47  TestMod.X
48  (call core.apply_type core.Type %₄₇)
49  slot₁/U
50  (call core.svec %₄₈ %₄₉)
51  slot₁/U
52  (call core.svec %₅₁)
53  SourceLocation::1:1
54  (call core.svec %₅₀ %₅₂ %₅₃)
55  --- method core.nothing %₅₄
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.X
    2   static_parameter₁
    3   (call core.apply_type %₁ %₂)
    4   (new %₃ slot₂/x)
    5   (return %₄)
56  latestworld
57  (return core.nothing)

########################################
# Struct with outer constructor where one typevar is constrained by the other
# See https://github.com/JuliaLang/julia/issues/27269)
struct X{T, S <: Vector{T}}
    v::Vector{S}
end
#---------------------
1   (global TestMod.X)
2   latestworld
3   (= slot₃/T (call core.TypeVar :T))
4   TestMod.Vector
5   slot₃/T
6   (call core.apply_type %₄ %₅)
7   (= slot₂/S (call core.TypeVar :S %₆))
8   slot₃/T
9   slot₂/S
10  (call core.svec %₈ %₉)
11  (call core.svec :v)
12  (call core.svec)
13  (call core._structtype TestMod :X %₁₀ %₁₁ %₁₂ false 1)
14  (= slot₄/X %₁₃)
15  (call core._setsuper! %₁₃ core.Any)
16  (call core.isdefinedglobal TestMod :X false)
17  (gotoifnot %₁₆ label₂₁)
18  TestMod.X
19  (= slot₅/if_val (call core._equiv_typedef %₁₈ %₁₃))
20  (goto label₂₂)
21  (= slot₅/if_val false)
22  slot₅/if_val
23  (gotoifnot %₂₂ label₂₇)
24  TestMod.X
25  (= slot₆/if_val %₂₄)
26  (goto label₂₈)
27  (= slot₆/if_val false)
28  slot₆/if_val
29  (gotoifnot %₂₂ label₄₀)
30  TestMod.X
31  (call top.getproperty %₃₀ :body)
32  (call top.getproperty %₃₁ :body)
33  (call top.getproperty %₃₂ :parameters)
34  (call top.indexed_iterate %₃₃ 1)
35  (= slot₃/T (call core.getfield %₃₄ 1))
36  (= slot₁/iterstate (call core.getfield %₃₄ 2))
37  slot₁/iterstate
38  (call top.indexed_iterate %₃₃ 2 %₃₇)
39  (= slot₂/S (call core.getfield %₃₈ 1))
40  TestMod.Vector
41  slot₂/S
42  (call core.apply_type %₄₀ %₄₁)
43  (call core.svec %₄₂)
44  (call core._typebody! %₂₈ %₁₃ %₄₃)
45  (constdecl TestMod.X %₄₄)
46  latestworld
47  slot₃/T
48  slot₂/S
49  TestMod.X
50  slot₃/T
51  slot₂/S
52  (call core.apply_type %₄₉ %₅₀ %₅₁)
53  (call core.apply_type core.Type %₅₂)
54  (call core.UnionAll %₄₈ %₅₃)
55  (call core.UnionAll %₄₇ %₅₄)
56  (call core.svec %₅₅ core.Any)
57  (call core.svec)
58  SourceLocation::1:1
59  (call core.svec %₅₆ %₅₇ %₅₈)
60  --- method core.nothing %₅₉
    slots: [slot₁/#ctor-self# slot₂/v slot₃/tmp]
    1   (call core.fieldtype slot₁/#ctor-self# 1)
    2   slot₂/v
    3   (= slot₃/tmp %₂)
    4   slot₃/tmp
    5   (call core.isa %₄ %₁)
    6   (gotoifnot %₅ label₈)
    7   (goto label₁₀)
    8   slot₃/tmp
    9   (= slot₃/tmp (call top.convert %₁ %₈))
    10  slot₃/tmp
    11  (new slot₁/#ctor-self# %₁₀)
    12  (return %₁₁)
61  latestworld
62  TestMod.X
63  (call core.apply_type core.Type %₆₂)
64  TestMod.Vector
65  slot₂/S
66  (call core.apply_type %₆₄ %₆₅)
67  (call core.svec %₆₃ %₆₆)
68  slot₃/T
69  slot₂/S
70  (call core.svec %₆₈ %₆₉)
71  SourceLocation::1:1
72  (call core.svec %₆₇ %₇₀ %₇₁)
73  --- method core.nothing %₇₂
    slots: [slot₁/#self#(!read) slot₂/v]
    1   TestMod.X
    2   static_parameter₁
    3   static_parameter₂
    4   (call core.apply_type %₁ %₂ %₃)
    5   (new %₄ slot₂/v)
    6   (return %₅)
74  latestworld
75  (return core.nothing)

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
1   (= slot₂/f (call core.Box))
2   (global TestMod.X)
3   latestworld
4   (call core.svec)
5   (call core.svec :x)
6   (call core.svec)
7   (call core._structtype TestMod :X %₄ %₅ %₆ false 1)
8   (= slot₁/X %₇)
9   (call core._setsuper! %₇ core.Any)
10  (call core.isdefinedglobal TestMod :X false)
11  (gotoifnot %₁₀ label₁₅)
12  TestMod.X
13  (= slot₄/if_val (call core._equiv_typedef %₁₂ %₇))
14  (goto label₁₆)
15  (= slot₄/if_val false)
16  slot₄/if_val
17  (gotoifnot %₁₆ label₂₁)
18  TestMod.X
19  (= slot₅/if_val %₁₈)
20  (goto label₂₂)
21  (= slot₅/if_val false)
22  slot₅/if_val
23  (gotoifnot %₁₆ label₂₄)
24  (call core.svec core.Any)
25  (call core._typebody! %₂₂ %₇ %₂₄)
26  (constdecl TestMod.X %₂₅)
27  latestworld
28  (call core.svec)
29  (call core.svec)
30  (call JuliaLowering.eval_closure_type TestMod :#f##0 %₂₈ %₂₉)
31  latestworld
32  TestMod.#f##0
33  (new %₃₂)
34  slot₂/f
35  (call core.setfield! %₃₄ :contents %₃₃)
36  TestMod.#f##0
37  (call core.svec %₃₆)
38  (call core.svec)
39  SourceLocation::3:5
40  (call core.svec %₃₇ %₃₈ %₃₉)
41  --- method core.nothing %₄₀
    slots: [slot₁/#self#(!read)]
    1   TestMod.X
    2   (new %₁ 1)
    3   (return %₂)
42  latestworld
43  TestMod.X
44  (call core.apply_type core.Type %₄₃)
45  (call core.svec %₄₄)
46  (call core.svec)
47  SourceLocation::4:5
48  (call core.svec %₄₅ %₄₆ %₄₇)
49  --- code_info
    slots: [slot₁/#ctor-self#(!read) slot₂/f(!read)]
    1   (captured_local 1)
    2   (call core.isdefined %₁ :contents)
    3   (gotoifnot %₂ label₅)
    4   (goto label₇)
    5   (newvar slot₂/f)
    6   slot₂/f
    7   (call core.getfield %₁ :contents)
    8   (call %₇)
    9   (return %₈)
50  slot₂/f
51  (call core.svec %₅₀)
52  (call JuliaLowering.replace_captured_locals! %₄₉ %₅₁)
53  --- method core.nothing %₄₈ %₅₂
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
    6   slot₄/tmp
    7   (call core.isa %₆ %₁)
    8   (gotoifnot %₇ label₁₀)
    9   (goto label₁₃)
    10  slot₄/tmp
    11  (call top.convert %₁ %₁₀)
    12  (= slot₄/tmp (call core.typeassert %₁₁ %₁))
    13  slot₄/tmp
    14  (return %₁₃)
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
79  TestMod.X
80  (call core.apply_type core.Type %₇₉)
81  (call JuliaLowering.bind_docs! %₈₀ "Docs for X constructor\n" %₇₆)
82  (return core.nothing)

########################################
# User defined inner constructors and helper functions for structs with type params
struct X{S,T}
    x
    X{A,B}() = new(1)
    X{U,V}() where {U,V} = new(1)
    f() = new{A,B}(1)
end
#---------------------
1   (newvar slot₅/f)
2   (global TestMod.X)
3   latestworld
4   (= slot₂/S (call core.TypeVar :S))
5   (= slot₃/T (call core.TypeVar :T))
6   slot₂/S
7   slot₃/T
8   (call core.svec %₆ %₇)
9   (call core.svec :x)
10  (call core.svec)
11  (call core._structtype TestMod :X %₈ %₉ %₁₀ false 1)
12  (= slot₄/X %₁₁)
13  (call core._setsuper! %₁₁ core.Any)
14  (call core.isdefinedglobal TestMod :X false)
15  (gotoifnot %₁₄ label₁₉)
16  TestMod.X
17  (= slot₈/if_val (call core._equiv_typedef %₁₆ %₁₁))
18  (goto label₂₀)
19  (= slot₈/if_val false)
20  slot₈/if_val
21  (gotoifnot %₂₀ label₂₅)
22  TestMod.X
23  (= slot₉/if_val %₂₂)
24  (goto label₂₆)
25  (= slot₉/if_val false)
26  slot₉/if_val
27  (gotoifnot %₂₀ label₃₈)
28  TestMod.X
29  (call top.getproperty %₂₈ :body)
30  (call top.getproperty %₂₉ :body)
31  (call top.getproperty %₃₀ :parameters)
32  (call top.indexed_iterate %₃₁ 1)
33  (= slot₂/S (call core.getfield %₃₂ 1))
34  (= slot₁/iterstate (call core.getfield %₃₂ 2))
35  slot₁/iterstate
36  (call top.indexed_iterate %₃₁ 2 %₃₅)
37  (= slot₃/T (call core.getfield %₃₆ 1))
38  (call core.svec core.Any)
39  (call core._typebody! %₂₆ %₁₁ %₃₈)
40  (constdecl TestMod.X %₃₉)
41  latestworld
42  TestMod.X
43  TestMod.A
44  TestMod.B
45  (call core.apply_type %₄₂ %₄₃ %₄₄)
46  (call core.apply_type core.Type %₄₅)
47  (call core.svec %₄₆)
48  (call core.svec)
49  SourceLocation::3:5
50  (call core.svec %₄₇ %₄₈ %₄₉)
51  --- method core.nothing %₅₀
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁ 1)
    3   (return %₂)
52  latestworld
53  (= slot₆/U (call core.TypeVar :U))
54  (= slot₇/V (call core.TypeVar :V))
55  TestMod.X
56  slot₆/U
57  slot₇/V
58  (call core.apply_type %₅₅ %₅₆ %₅₇)
59  (call core.apply_type core.Type %₅₈)
60  (call core.svec %₅₉)
61  slot₆/U
62  slot₇/V
63  (call core.svec %₆₁ %₆₂)
64  SourceLocation::4:5
65  (call core.svec %₆₀ %₆₃ %₆₄)
66  --- method core.nothing %₆₅
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁ 1)
    3   (return %₂)
67  latestworld
68  (call core.svec)
69  (call core.svec)
70  (call JuliaLowering.eval_closure_type TestMod :#f##1 %₆₈ %₆₉)
71  latestworld
72  TestMod.#f##1
73  (new %₇₂)
74  (= slot₅/f %₇₃)
75  TestMod.#f##1
76  (call core.svec %₇₅)
77  (call core.svec)
78  SourceLocation::5:5
79  (call core.svec %₇₆ %₇₇ %₇₈)
80  --- method core.nothing %₇₉
    slots: [slot₁/#self#(!read)]
    1   TestMod.X
    2   TestMod.A
    3   TestMod.B
    4   (call core.apply_type %₁ %₂ %₃)
    5   (new %₄ 1)
    6   (return %₅)
81  latestworld
82  (return core.nothing)

########################################
# new() calls with splats; `Any` fields
struct X
    x
    y
    X(xs) = new(xs...)
end
#---------------------
1   (global TestMod.X)
2   latestworld
3   (call core.svec)
4   (call core.svec :x :y)
5   (call core.svec)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 2)
7   (= slot₁/X %₆)
8   (call core._setsuper! %₆ core.Any)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₂/if_val (call core._equiv_typedef %₁₁ %₆))
13  (goto label₁₅)
14  (= slot₂/if_val false)
15  slot₂/if_val
16  (gotoifnot %₁₅ label₂₀)
17  TestMod.X
18  (= slot₃/if_val %₁₇)
19  (goto label₂₁)
20  (= slot₃/if_val false)
21  slot₃/if_val
22  (gotoifnot %₁₅ label₂₃)
23  (call core.svec core.Any core.Any)
24  (call core._typebody! %₂₁ %₆ %₂₃)
25  (constdecl TestMod.X %₂₄)
26  latestworld
27  TestMod.X
28  (call core.apply_type core.Type %₂₇)
29  (call core.svec %₂₈ core.Any)
30  (call core.svec)
31  SourceLocation::4:5
32  (call core.svec %₂₉ %₃₀ %₃₁)
33  --- method core.nothing %₃₂
    slots: [slot₁/#ctor-self# slot₂/xs]
    1   slot₁/#ctor-self#
    2   (call core._apply_iterate top.iterate core.tuple slot₂/xs)
    3   (splatnew %₁ %₂)
    4   (return %₃)
34  latestworld
35  (return core.nothing)

########################################
# new() calls with splats; typed fields
struct X{T}
    x::T
    y::A
    X{T}(xs) where {T} = new(xs...)
end
#---------------------
1   (global TestMod.X)
2   latestworld
3   (= slot₁/T (call core.TypeVar :T))
4   slot₁/T
5   (call core.svec %₄)
6   (call core.svec :x :y)
7   (call core.svec)
8   (call core._structtype TestMod :X %₅ %₆ %₇ false 2)
9   (= slot₂/X %₈)
10  (call core._setsuper! %₈ core.Any)
11  (call core.isdefinedglobal TestMod :X false)
12  (gotoifnot %₁₁ label₁₆)
13  TestMod.X
14  (= slot₄/if_val (call core._equiv_typedef %₁₃ %₈))
15  (goto label₁₇)
16  (= slot₄/if_val false)
17  slot₄/if_val
18  (gotoifnot %₁₇ label₂₂)
19  TestMod.X
20  (= slot₅/if_val %₁₉)
21  (goto label₂₃)
22  (= slot₅/if_val false)
23  slot₅/if_val
24  (gotoifnot %₁₇ label₃₀)
25  TestMod.X
26  (call top.getproperty %₂₅ :body)
27  (call top.getproperty %₂₆ :parameters)
28  (call top.indexed_iterate %₂₇ 1)
29  (= slot₁/T (call core.getfield %₂₈ 1))
30  slot₁/T
31  TestMod.A
32  (call core.svec %₃₀ %₃₁)
33  (call core._typebody! %₂₃ %₈ %₃₂)
34  (constdecl TestMod.X %₃₃)
35  latestworld
36  (= slot₃/T (call core.TypeVar :T))
37  TestMod.X
38  slot₃/T
39  (call core.apply_type %₃₇ %₃₈)
40  (call core.apply_type core.Type %₃₉)
41  (call core.svec %₄₀ core.Any)
42  slot₃/T
43  (call core.svec %₄₂)
44  SourceLocation::4:5
45  (call core.svec %₄₁ %₄₃ %₄₄)
46  --- method core.nothing %₄₅
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
    14  slot₃/tmp
    15  (call core.isa %₁₄ %₁₂)
    16  (gotoifnot %₁₅ label₁₈)
    17  (goto label₂₀)
    18  slot₃/tmp
    19  (= slot₃/tmp (call top.convert %₁₂ %₁₈))
    20  slot₃/tmp
    21  (call core.fieldtype %₁₁ 2)
    22  (= slot₄/tmp (call core.getfield %₁ 2))
    23  slot₄/tmp
    24  (call core.isa %₂₃ %₂₁)
    25  (gotoifnot %₂₄ label₂₇)
    26  (goto label₂₉)
    27  slot₄/tmp
    28  (= slot₄/tmp (call top.convert %₂₁ %₂₇))
    29  slot₄/tmp
    30  (new %₁₁ %₂₀ %₂₉)
    31  (return %₃₀)
47  latestworld
48  (return core.nothing)

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
#─────┘ ── this syntax is only allowed in top level code
end


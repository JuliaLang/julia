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
#   └──┘ ── misplaced assignment in type parameter list

########################################
# Simple abstract type definition
abstract type A end
#---------------------
1   (call core.svec)
2   (call core._abstracttype TestMod :A %₁)
3   (= slot₁/A %₂)
4   (call core._setsuper! %₂ core.Any)
5   (call core._typebody! false slot₁/A)
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
10  (call core._typebody! false slot₁/A)
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
5   (call core._typebody! false slot₁/P)
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
10  (call core._typebody! false slot₁/P)
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
7   (call core._typebody! false slot₁/P)
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
25  (call core.declare_const TestMod :X %₂₄)
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
25  (call core.declare_const TestMod :X %₂₄)
26  latestworld
27  (call core.declare_global TestMod :X false)
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
26  (call core.declare_const TestMod :X %₂₅)
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
    4   (call core.isa slot₅/tmp %₁)
    5   (gotoifnot %₄ label₇)
    6   (goto label₈)
    7   (= slot₅/tmp (call top.convert %₁ slot₅/tmp))
    8   slot₅/tmp
    9   (new slot₁/#ctor-self# slot₂/a %₈ slot₄/c)
    10  (return %₉)
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
35  (= slot₃/U (call core.getfield %₃₄ 1))
36  (= slot₁/iterstate (call core.getfield %₃₄ 2))
37  slot₁/iterstate
38  (call top.indexed_iterate %₃₃ 2 %₃₇)
39  (= slot₄/V (call core.getfield %₃₈ 1))
40  (call core.svec)
41  (call core._typebody! %₂₈ %₁₂ %₄₀)
42  (call core.declare_const TestMod :X %₄₁)
43  latestworld
44  slot₃/U
45  slot₄/V
46  TestMod.X
47  slot₃/U
48  slot₄/V
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
1   (call core.declare_global TestMod :X false)
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
25  (call core.declare_const TestMod :X %₂₄)
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
1   (call core.declare_global TestMod :X false)
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
25  (call core.declare_const TestMod :X %₂₄)
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
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (= slot₂/U (call core.TypeVar :U))
4   slot₂/U
5   (call core.svec %₄)
6   (call core.svec :x)
7   (call core.svec)
8   (call core._structtype TestMod :X %₅ %₆ %₇ false 1)
9   (= slot₁/X %₈)
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
29  (= slot₂/U (call core.getfield %₂₈ 1))
30  slot₂/U
31  (call core.svec %₃₀)
32  (call core._typebody! %₂₃ %₈ %₃₁)
33  (call core.declare_const TestMod :X %₃₂)
34  latestworld
35  slot₂/U
36  TestMod.X
37  slot₂/U
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
    4   (call core.isa slot₃/tmp %₁)
    5   (gotoifnot %₄ label₇)
    6   (goto label₈)
    7   (= slot₃/tmp (call top.convert %₁ slot₃/tmp))
    8   slot₃/tmp
    9   (new slot₁/#ctor-self# %₈)
    10  (return %₉)
46  latestworld
47  TestMod.X
48  (call core.apply_type core.Type %₄₇)
49  slot₂/U
50  (call core.svec %₄₈ %₄₉)
51  slot₂/U
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
39  (= slot₄/S (call core.getfield %₃₈ 1))
40  TestMod.Vector
41  slot₄/S
42  (call core.apply_type %₄₀ %₄₁)
43  (call core.svec %₄₂)
44  (call core._typebody! %₂₈ %₁₃ %₄₃)
45  (call core.declare_const TestMod :X %₄₄)
46  latestworld
47  slot₃/T
48  slot₄/S
49  TestMod.X
50  slot₃/T
51  slot₄/S
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
    4   (call core.isa slot₃/tmp %₁)
    5   (gotoifnot %₄ label₇)
    6   (goto label₈)
    7   (= slot₃/tmp (call top.convert %₁ slot₃/tmp))
    8   slot₃/tmp
    9   (new slot₁/#ctor-self# %₈)
    10  (return %₉)
61  latestworld
62  TestMod.X
63  (call core.apply_type core.Type %₆₂)
64  TestMod.Vector
65  slot₄/S
66  (call core.apply_type %₆₄ %₆₅)
67  (call core.svec %₆₃ %₆₆)
68  slot₃/T
69  slot₄/S
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
1   (call core.declare_global TestMod :X false)
2   latestworld
3   (call core.svec)
4   (call core.svec :x)
5   (call core.svec)
6   (call core._structtype TestMod :X %₃ %₄ %₅ false 1)
7   (= slot₁/X %₆)
8   (call core._setsuper! %₆ core.Any)
9   (call core.isdefinedglobal TestMod :X false)
10  (gotoifnot %₉ label₁₄)
11  TestMod.X
12  (= slot₄/if_val (call core._equiv_typedef %₁₁ %₆))
13  (goto label₁₅)
14  (= slot₄/if_val false)
15  slot₄/if_val
16  (gotoifnot %₁₅ label₂₀)
17  TestMod.X
18  (= slot₅/if_val %₁₇)
19  (goto label₂₁)
20  (= slot₅/if_val false)
21  slot₅/if_val
22  (gotoifnot %₁₅ label₂₃)
23  (call core.svec core.Any)
24  (call core._typebody! %₂₁ %₆ %₂₃)
25  (call core.declare_const TestMod :X %₂₄)
26  latestworld
27  (= slot₂/f (call core.Box))
28  (call core.declare_global TestMod :X false)
29  latestworld
30  (call core.svec)
31  (call core.svec)
32  (call JuliaLowering.eval_closure_type TestMod :#f##0 %₃₀ %₃₁)
33  latestworld
34  TestMod.#f##0
35  (new %₃₄)
36  slot₂/f
37  (call core.setfield! %₃₆ :contents %₃₅)
38  TestMod.#f##0
39  (call core.svec %₃₈)
40  (call core.svec)
41  SourceLocation::3:5
42  (call core.svec %₃₉ %₄₀ %₄₁)
43  --- method core.nothing %₄₂
    slots: [slot₁/#self#(!read)]
    1   TestMod.X
    2   (new %₁ 1)
    3   (return %₂)
44  latestworld
45  TestMod.X
46  (call core.apply_type core.Type %₄₅)
47  (call core.svec %₄₆)
48  (call core.svec)
49  SourceLocation::4:5
50  (call core.svec %₄₇ %₄₈ %₄₉)
51  --- code_info
    slots: [slot₁/#ctor-self#(!read) slot₂/f(!read,maybe_undef)]
    1   (captured_local 1)
    2   (call core.isdefined %₁ :contents)
    3   (gotoifnot %₂ label₅)
    4   (goto label₇)
    5   (newvar slot₂/f)
    6   slot₂/f
    7   (call core.getfield %₁ :contents)
    8   (call %₇)
    9   (return %₈)
52  slot₂/f
53  (call core.svec %₅₂)
54  (call JuliaLowering.replace_captured_locals! %₅₁ %₅₃)
55  --- method core.nothing %₅₀ %₅₄
56  latestworld
57  TestMod.X
58  (call core.apply_type core.Type %₅₇)
59  (call core.svec %₅₈ core.Any)
60  (call core.svec)
61  SourceLocation::5:5
62  (call core.svec %₅₉ %₆₀ %₆₁)
63  --- method core.nothing %₆₂
    slots: [slot₁/#ctor-self# slot₂/x]
    1   slot₁/#ctor-self#
    2   (new %₁ slot₂/x)
    3   (return %₂)
64  latestworld
65  TestMod.X
66  (call core.apply_type core.Type %₆₅)
67  (call core.svec %₆₆ core.Any core.Any)
68  (call core.svec)
69  SourceLocation::6:5
70  (call core.svec %₆₇ %₆₈ %₆₉)
71  --- method core.nothing %₇₀
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
72  latestworld
73  TestMod.X
74  (call core.apply_type core.Type %₇₃)
75  (call core.svec %₇₄ core.Any core.Any core.Any)
76  (call core.svec)
77  SourceLocation::10:5
78  (call core.svec %₇₅ %₇₆ %₇₇)
79  --- method core.nothing %₇₈
    slots: [slot₁/#ctor-self# slot₂/a slot₃/b(!read) slot₄/c(!read)]
    1   slot₁/#ctor-self#
    2   (new %₁ slot₂/a)
    3   (return %₂)
80  latestworld
81  TestMod.X
82  (call core.apply_type core.Type %₈₁)
83  (call JuliaLowering.bind_docs! %₈₂ "Docs for X constructor\n" %₇₈)
84  (return core.nothing)

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
12  (call core._setsuper! %₁₀ core.Any)
13  (call core.isdefinedglobal TestMod :X false)
14  (gotoifnot %₁₃ label₁₈)
15  TestMod.X
16  (= slot₈/if_val (call core._equiv_typedef %₁₅ %₁₀))
17  (goto label₁₉)
18  (= slot₈/if_val false)
19  slot₈/if_val
20  (gotoifnot %₁₉ label₂₄)
21  TestMod.X
22  (= slot₉/if_val %₂₁)
23  (goto label₂₅)
24  (= slot₉/if_val false)
25  slot₉/if_val
26  (gotoifnot %₁₉ label₃₇)
27  TestMod.X
28  (call top.getproperty %₂₇ :body)
29  (call top.getproperty %₂₈ :body)
30  (call top.getproperty %₂₉ :parameters)
31  (call top.indexed_iterate %₃₀ 1)
32  (= slot₃/S (call core.getfield %₃₁ 1))
33  (= slot₁/iterstate (call core.getfield %₃₁ 2))
34  slot₁/iterstate
35  (call top.indexed_iterate %₃₀ 2 %₃₄)
36  (= slot₄/T (call core.getfield %₃₅ 1))
37  (call core.svec core.Any)
38  (call core._typebody! %₂₅ %₁₀ %₃₇)
39  (call core.declare_const TestMod :X %₃₈)
40  latestworld
41  (call core.declare_global TestMod :X false)
42  latestworld
43  TestMod.X
44  TestMod.A
45  TestMod.B
46  (call core.apply_type %₄₃ %₄₄ %₄₅)
47  (call core.apply_type core.Type %₄₆)
48  (call core.svec %₄₇)
49  (call core.svec)
50  SourceLocation::3:5
51  (call core.svec %₄₈ %₄₉ %₅₀)
52  --- method core.nothing %₅₁
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁ 1)
    3   (return %₂)
53  latestworld
54  (= slot₆/U (call core.TypeVar :U))
55  (= slot₇/V (call core.TypeVar :V))
56  TestMod.X
57  slot₆/U
58  slot₇/V
59  (call core.apply_type %₅₆ %₅₇ %₅₈)
60  (call core.apply_type core.Type %₅₉)
61  (call core.svec %₆₀)
62  slot₆/U
63  slot₇/V
64  (call core.svec %₆₂ %₆₃)
65  SourceLocation::4:5
66  (call core.svec %₆₁ %₆₄ %₆₅)
67  --- method core.nothing %₆₆
    slots: [slot₁/#ctor-self#]
    1   slot₁/#ctor-self#
    2   (new %₁ 1)
    3   (return %₂)
68  latestworld
69  (call core.svec)
70  (call core.svec)
71  (call JuliaLowering.eval_closure_type TestMod :#f##1 %₆₉ %₇₀)
72  latestworld
73  TestMod.#f##1
74  (new %₇₃)
75  (= slot₅/f %₇₄)
76  TestMod.#f##1
77  (call core.svec %₇₆)
78  (call core.svec)
79  SourceLocation::5:5
80  (call core.svec %₇₇ %₇₈ %₇₉)
81  --- method core.nothing %₈₀
    slots: [slot₁/#self#(!read)]
    1   TestMod.X
    2   TestMod.A
    3   TestMod.B
    4   (call core.apply_type %₁ %₂ %₃)
    5   (new %₄ 1)
    6   (return %₅)
82  latestworld
83  (return core.nothing)

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
25  (call core.declare_const TestMod :X %₂₄)
26  latestworld
27  (call core.declare_global TestMod :X false)
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
29  (= slot₂/T (call core.getfield %₂₈ 1))
30  slot₂/T
31  TestMod.A
32  (call core.svec %₃₀ %₃₁)
33  (call core._typebody! %₂₃ %₈ %₃₂)
34  (call core.declare_const TestMod :X %₃₃)
35  latestworld
36  (call core.declare_global TestMod :X false)
37  latestworld
38  (= slot₃/T (call core.TypeVar :T))
39  TestMod.X
40  slot₃/T
41  (call core.apply_type %₃₉ %₄₀)
42  (call core.apply_type core.Type %₄₁)
43  (call core.svec %₄₂ core.Any)
44  slot₃/T
45  (call core.svec %₄₄)
46  SourceLocation::4:5
47  (call core.svec %₄₃ %₄₅ %₄₆)
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
#─────┘ ── this syntax is only allowed in top level code
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
6   (call core.Typeof %₅)
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
10  (call core._setsuper! %₈ core.Any)
11  (call core.isdefinedglobal TestMod :ShadowTypeParam false)
12  (gotoifnot %₁₁ label₁₆)
13  TestMod.ShadowTypeParam
14  (= slot₃/if_val (call core._equiv_typedef %₁₃ %₈))
15  (goto label₁₇)
16  (= slot₃/if_val false)
17  slot₃/if_val
18  (gotoifnot %₁₇ label₂₂)
19  TestMod.ShadowTypeParam
20  (= slot₄/if_val %₁₉)
21  (goto label₂₃)
22  (= slot₄/if_val false)
23  slot₄/if_val
24  (gotoifnot %₁₇ label₃₀)
25  TestMod.ShadowTypeParam
26  (call top.getproperty %₂₅ :body)
27  (call top.getproperty %₂₆ :parameters)
28  (call top.indexed_iterate %₂₇ 1)
29  (= slot₂/T (call core.getfield %₂₈ 1))
30  slot₂/T
31  (call core.svec %₃₀)
32  (call core._typebody! %₂₃ %₈ %₃₁)
33  (call core.declare_const TestMod :ShadowTypeParam %₃₂)
34  latestworld
35  (call core.declare_global TestMod :ShadowTypeParam false)
36  latestworld
37  TestMod.ShadowTypeParam
38  (call core.apply_type core.Type %₃₇)
39  (call core.svec %₃₈ core.Any)
40  (call core.svec)
41  SourceLocation::3:14
42  (call core.svec %₃₉ %₄₀ %₄₁)
43  --- method core.nothing %₄₂
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
44  latestworld
45  (return core.nothing)

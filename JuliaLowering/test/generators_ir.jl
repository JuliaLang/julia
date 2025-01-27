########################################
# Simple 1D generator
(x+1 for x in xs)
#---------------------
1   --- thunk
    1   (global TestMod.#->##0)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#->##0 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#->##0)
    8   (= TestMod.#->##0 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   TestMod.#->##0
3   (call core.svec %₂ core.Any)
4   (call core.svec)
5   SourceLocation::1:2
6   (call core.svec %₃ %₄ %₅)
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.+
    2   (call %₁ slot₂/x 1)
    3   (return %₂)
8   TestMod.#->##0
9   (new %₈)
10  TestMod.xs
11  (call top.Generator %₉ %₁₀)
12  (return %₁₁)

########################################
# Product iteration
(x+y for x in xs, y in ys)
#---------------------
1   --- thunk
    1   (global TestMod.#->##1)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#->##1 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#->##1)
    8   (= TestMod.#->##1 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   TestMod.#->##1
3   (call core.svec %₂ core.Any)
4   (call core.svec)
5   SourceLocation::1:2
6   (call core.svec %₃ %₄ %₅)
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/destructured_arg slot₃/iterstate slot₄/x slot₅/y]
    1   (call top.indexed_iterate slot₂/destructured_arg 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/destructured_arg 2 %₄)
    6   (= slot₅/y (call core.getfield %₅ 1))
    7   TestMod.+
    8   slot₄/x
    9   slot₅/y
    10  (call %₇ %₈ %₉)
    11  (return %₁₀)
8   TestMod.#->##1
9   (new %₈)
10  TestMod.xs
11  TestMod.ys
12  (call top.product %₁₀ %₁₁)
13  (call top.Generator %₉ %₁₂)
14  (return %₁₃)

########################################
# Use `identity` as the Generator function when possible eg in filters
((x,y) for (x,y) in iter if f(x))
#---------------------
1   --- thunk
    1   (global TestMod.#->##2)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#->##2 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#->##2)
    8   (= TestMod.#->##2 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   TestMod.#->##2
3   (call core.svec %₂ core.Any)
4   (call core.svec)
5   SourceLocation::1:29
6   (call core.svec %₃ %₄ %₅)
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/destructured_arg slot₃/iterstate slot₄/x slot₅/y(!read)]
    1   (call top.indexed_iterate slot₂/destructured_arg 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/destructured_arg 2 %₄)
    6   (= slot₅/y (call core.getfield %₅ 1))
    7   TestMod.f
    8   slot₄/x
    9   (call %₇ %₈)
    10  (return %₉)
8   TestMod.#->##2
9   (new %₈)
10  TestMod.iter
11  (call top.Filter %₉ %₁₀)
12  (call top.Generator top.identity %₁₁)
13  (return %₁₂)

########################################
# Use of placeholders in iteration vars
(1 for _ in xs)
#---------------------
1   --- thunk
    1   (global TestMod.#->##3)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#->##3 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#->##3)
    8   (= TestMod.#->##3 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   TestMod.#->##3
3   (call core.svec %₂ core.Any)
4   (call core.svec)
5   SourceLocation::1:2
6   (call core.svec %₃ %₄ %₅)
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/_(!read)]
    1   (return 1)
8   TestMod.#->##3
9   (new %₈)
10  TestMod.xs
11  (call top.Generator %₉ %₁₀)
12  (return %₁₁)

########################################
# Error: Use of placeholders in body
(_ for _ in xs)
#---------------------
LoweringError:
(_ for _ in xs)
#╙ ── all-underscore identifiers are write-only and their values cannot be used in expressions

########################################
# 1D generator with destructuring
(body for (x,_,y) in iter)
#---------------------
1   --- thunk
    1   (global TestMod.#->##5)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#->##5 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#->##5)
    8   (= TestMod.#->##5 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   TestMod.#->##5
3   (call core.svec %₂ core.Any)
4   (call core.svec)
5   SourceLocation::1:2
6   (call core.svec %₃ %₄ %₅)
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/destructured_arg slot₃/iterstate slot₄/x(!read) slot₅/y(!read)]
    1   (call top.indexed_iterate slot₂/destructured_arg 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/destructured_arg 2 %₄)
    6   (call core.getfield %₅ 1)
    7   (= slot₃/iterstate (call core.getfield %₅ 2))
    8   slot₃/iterstate
    9   (call top.indexed_iterate slot₂/destructured_arg 3 %₈)
    10  (= slot₅/y (call core.getfield %₉ 1))
    11  TestMod.body
    12  (return %₁₁)
8   TestMod.#->##5
9   (new %₈)
10  TestMod.iter
11  (call top.Generator %₉ %₁₀)
12  (return %₁₁)

########################################
# return permitted in quoted syntax in generator
(:(return x) for _ in iter)
#---------------------
1   --- thunk
    1   (global TestMod.#->##6)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#->##6 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#->##6)
    8   (= TestMod.#->##6 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   TestMod.#->##6
3   (call core.svec %₂ core.Any)
4   (call core.svec)
5   SourceLocation::1:4
6   (call core.svec %₃ %₄ %₅)
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/_(!read)]
    1   (call JuliaLowering.interpolate_ast (inert (return x)))
    2   (return %₁)
8   TestMod.#->##6
9   (new %₈)
10  TestMod.iter
11  (call top.Generator %₉ %₁₀)
12  (return %₁₁)

########################################
# Error: `return` not permitted in generator body
((return x) + y for x in iter)
#---------------------
LoweringError:
((return x) + y for x in iter)
# └──────┘ ── `return` not allowed inside comprehension or generator

########################################
# Nested case with duplicate iteration variables
(x for x in 1:3 for x in 1:2)
#---------------------
1   --- thunk
    1   (global TestMod.#->##7)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#->##7 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#->##7)
    8   (= TestMod.#->##7 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   --- thunk
    1   (global TestMod.#->#->##0)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#->#->##0 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#->#->##0)
    8   (= TestMod.#->#->##0 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
3   TestMod.#->#->##0
4   (call core.svec %₃ core.Any)
5   (call core.svec)
6   SourceLocation::1:2
7   (call core.svec %₄ %₅ %₆)
8   --- method core.nothing %₇
    slots: [slot₁/#self#(!read) slot₂/x slot₃/x]
    1   slot₂/x
    2   (= slot₃/x %₁)
    3   slot₃/x
    4   (return %₃)
9   TestMod.#->##7
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:2
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/x(!read)]
    1   TestMod.#->#->##0
    2   (new %₁)
    3   TestMod.:
    4   (call %₃ 1 2)
    5   (call top.Generator %₂ %₄)
    6   (return %₅)
15  TestMod.#->##7
16  (new %₁₅)
17  TestMod.:
18  (call %₁₇ 1 3)
19  (call top.Generator %₁₆ %₁₈)
20  (call top.Flatten %₁₉)
21  (return %₂₀)

########################################
# Comprehension lowers to generator with collect
[x for x in xs]
#---------------------
1   TestMod.xs
2   (call top.Generator top.identity %₁)
3   (call top.collect %₂)
4   (return %₃)

########################################
# Simple typed comprehension lowers to for loop
T[(x,y) for x in xs, y in ys]
#---------------------
1   TestMod.xs
2   TestMod.ys
3   (call top.product %₁ %₂)
4   (call top.IteratorSize %₃)
5   (call core.isa %₄ top.SizeUnknown)
6   TestMod.T
7   (call top._array_for %₆ %₃ %₄)
8   (call top.LinearIndices %₇)
9   (= slot₁/idx (call top.first %₈))
10  (= slot₃/next (call top.iterate %₂))
11  slot₃/next
12  (call core.=== %₁₁ core.nothing)
13  (call top.not_int %₁₂)
14  (gotoifnot %₁₃ label₅₀)
15  slot₃/next
16  (= slot₄/y (call core.getfield %₁₅ 1))
17  (call core.getfield %₁₅ 2)
18  (= slot₂/next (call top.iterate %₁))
19  slot₂/next
20  (call core.=== %₁₉ core.nothing)
21  (call top.not_int %₂₀)
22  (gotoifnot %₂₁ label₄₄)
23  slot₄/y
24  (= slot₆/y %₂₃)
25  slot₂/next
26  (= slot₅/x (call core.getfield %₂₅ 1))
27  (call core.getfield %₂₅ 2)
28  slot₅/x
29  slot₆/y
30  (call core.tuple %₂₈ %₂₉)
31  (gotoifnot %₅ label₃₄)
32  (call top.push! %₇ %₃₀)
33  (goto label₃₆)
34  slot₁/idx
35  (call top.setindex! %₇ %₃₀ %₃₄)
36  slot₁/idx
37  (= slot₁/idx (call top.add_int %₃₆ 1))
38  (= slot₂/next (call top.iterate %₁ %₂₇))
39  slot₂/next
40  (call core.=== %₃₉ core.nothing)
41  (call top.not_int %₄₀)
42  (gotoifnot %₄₁ label₄₄)
43  (goto label₂₃)
44  (= slot₃/next (call top.iterate %₂ %₁₇))
45  slot₃/next
46  (call core.=== %₄₅ core.nothing)
47  (call top.not_int %₄₆)
48  (gotoifnot %₄₇ label₅₀)
49  (goto label₁₅)
50  (return %₇)

########################################
# FIXME - error in nested closure conversion: Triply nested generator
((x,y,z) for x in 1:3 for y in 4:5 for z in 6:7)
#---------------------
LoweringError:
((x,y,z) for x in 1:3 for y in 4:5 for z in 6:7)
#            ╙ ── Found unexpected binding of kind argument

Detailed provenance:
#₁₃/x
└─ x
   └─ x
      └─ @ :1



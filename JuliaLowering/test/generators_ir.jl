########################################
# Simple 1D generator
(x+1 for x in xs)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call core.svec)
4   (call JuliaLowering.eval_closure_type TestMod :##->###0 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###0
7   (new %₆)
8   (= slot₁/#-># %₇)
9   TestMod.##->###0
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:2
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.+
    2   (call %₁ slot₂/x 1)
    3   (return %₂)
15  latestworld
16  slot₁/#->#
17  TestMod.xs
18  (call top.Generator %₁₆ %₁₇)
19  (return %₁₈)

########################################
# Product iteration
(x+y for x in xs, y in ys)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call core.svec)
4   (call JuliaLowering.eval_closure_type TestMod :##->###1 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###1
7   (new %₆)
8   (= slot₁/#-># %₇)
9   TestMod.##->###1
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:2
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/#generator# slot₃/iterstate(single_assign) slot₄/x(single_assign) slot₅/y(single_assign)]
    1   (call top.indexed_iterate slot₂/#generator# 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/#generator# 2 %₄)
    6   (= slot₅/y (call core.getfield %₅ 1))
    7   TestMod.+
    8   slot₄/x
    9   slot₅/y
    10  (call %₇ %₈ %₉)
    11  (return %₁₀)
15  latestworld
16  slot₁/#->#
17  TestMod.xs
18  TestMod.ys
19  (call top.product %₁₇ %₁₈)
20  (call top.Generator %₁₆ %₁₉)
21  (return %₂₀)

########################################
# Use `identity` as the Generator function when possible eg in filters
((x,y) for (x,y) in iter if f(x))
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call core.svec)
4   (call JuliaLowering.eval_closure_type TestMod :##->###2 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###2
7   (new %₆)
8   (= slot₁/#-># %₇)
9   TestMod.##->###2
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:2
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/#generator# slot₃/iterstate(single_assign) slot₄/x(single_assign) slot₅/y(single_assign)]
    1   (call top.indexed_iterate slot₂/#generator# 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/#generator# 2 %₄)
    6   (= slot₅/y (call core.getfield %₅ 1))
    7   slot₄/x
    8   slot₅/y
    9   (call core.tuple %₇ %₈)
    10  (return %₉)
15  latestworld
16  slot₁/#->#
17  (call core.svec)
18  (call core.svec)
19  (call core.svec)
20  (call JuliaLowering.eval_closure_type TestMod :##->###3 %₁₇ %₁₈ %₁₉)
21  latestworld
22  TestMod.##->###3
23  (new %₂₂)
24  (= slot₂/#-># %₂₃)
25  TestMod.##->###3
26  (call core.svec %₂₅ core.Any)
27  (call core.svec)
28  SourceLocation::1:29
29  (call core.svec %₂₆ %₂₇ %₂₈)
30  --- method core.nothing %₂₉
    slots: [slot₁/#self#(!read) slot₂/#generator# slot₃/iterstate(single_assign) slot₄/x(single_assign) slot₅/y(!read,single_assign)]
    1   (call top.indexed_iterate slot₂/#generator# 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/#generator# 2 %₄)
    6   (= slot₅/y (call core.getfield %₅ 1))
    7   TestMod.f
    8   slot₄/x
    9   (call %₇ %₈)
    10  (return %₉)
31  latestworld
32  slot₂/#->#
33  TestMod.iter
34  (call top.Filter %₃₂ %₃₃)
35  (call top.Generator %₁₆ %₃₄)
36  (return %₃₅)

########################################
# Use of placeholders in iteration vars
(1 for _ in xs)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call core.svec)
4   (call JuliaLowering.eval_closure_type TestMod :##->###4 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###4
7   (new %₆)
8   (= slot₁/#-># %₇)
9   TestMod.##->###4
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:2
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/#unused#(!read)]
    1   (return 1)
15  latestworld
16  slot₁/#->#
17  TestMod.xs
18  (call top.Generator %₁₆ %₁₇)
19  (return %₁₈)

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
1   (call core.svec)
2   (call core.svec)
3   (call core.svec)
4   (call JuliaLowering.eval_closure_type TestMod :##->###5 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###5
7   (new %₆)
8   (= slot₁/#-># %₇)
9   TestMod.##->###5
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:2
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/#generator# slot₃/iterstate slot₄/x(!read,single_assign) slot₅/y(!read,single_assign)]
    1   (call top.indexed_iterate slot₂/#generator# 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/#generator# 2 %₄)
    6   (call core.getfield %₅ 1)
    7   (= slot₃/iterstate (call core.getfield %₅ 2))
    8   slot₃/iterstate
    9   (call top.indexed_iterate slot₂/#generator# 3 %₈)
    10  (= slot₅/y (call core.getfield %₉ 1))
    11  TestMod.body
    12  (return %₁₁)
15  latestworld
16  slot₁/#->#
17  TestMod.iter
18  (call top.Generator %₁₆ %₁₇)
19  (return %₁₈)

########################################
# return permitted in quoted syntax in generator
(:(return x) for _ in iter)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call core.svec)
4   (call JuliaLowering.eval_closure_type TestMod :##->###6 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###6
7   (new %₆)
8   (= slot₁/#-># %₇)
9   TestMod.##->###6
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:4
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/#unused#(!read)]
    1   (call JuliaLowering.interpolate_expr (inert (return x)))
    2   (return %₁)
15  latestworld
16  slot₁/#->#
17  TestMod.iter
18  (call top.Generator %₁₆ %₁₇)
19  (return %₁₈)

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
1   (call core.svec)
2   (call core.svec)
3   (call core.svec)
4   (call JuliaLowering.eval_closure_type TestMod :##->###7 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###7
7   (new %₆)
8   (= slot₁/#-># %₇)
9   (call core.svec)
10  (call core.svec)
11  (call core.svec)
12  (call JuliaLowering.eval_closure_type TestMod :##->###->###0 %₉ %₁₀ %₁₁)
13  latestworld
14  TestMod.##->###->###0
15  (call core.svec %₁₄ core.Any)
16  (call core.svec)
17  SourceLocation::1:2
18  (call core.svec %₁₅ %₁₆ %₁₇)
19  --- method core.nothing %₁₈
    slots: [slot₁/#self#(!read) slot₂/x slot₃/x(single_assign)]
    1   slot₂/x
    2   (= slot₃/x %₁)
    3   slot₃/x
    4   (return %₃)
20  latestworld
21  TestMod.##->###7
22  (call core.svec %₂₁ core.Any)
23  (call core.svec)
24  SourceLocation::1:2
25  (call core.svec %₂₂ %₂₃ %₂₄)
26  --- method core.nothing %₂₅
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/#->#(single_assign)]
    1   TestMod.##->###->###0
    2   (new %₁)
    3   (= slot₃/#-># %₂)
    4   slot₃/#->#
    5   TestMod.:
    6   (call %₅ 1 2)
    7   (call top.Generator %₄ %₆)
    8   (return %₇)
27  latestworld
28  slot₁/#->#
29  TestMod.:
30  (call %₂₉ 1 3)
31  (call top.Generator %₂₈ %₃₀)
32  (call top.Flatten %₃₁)
33  (return %₃₂)

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
14  (gotoifnot %₁₃ label₄₉)
15  slot₃/next
16  (= slot₄/y (call core.getfield %₁₅ 1))
17  (call core.getfield %₁₅ 2)
18  (= slot₂/next (call top.iterate %₁))
19  slot₂/next
20  (call core.=== %₁₉ core.nothing)
21  (call top.not_int %₂₀)
22  (gotoifnot %₂₁ label₄₃)
23  slot₄/y
24  (= slot₅/y %₂₃)
25  slot₂/next
26  (= slot₆/x (call core.getfield %₂₅ 1))
27  (call core.getfield %₂₅ 2)
28  slot₆/x
29  (call core.tuple %₂₈ slot₅/y)
30  (gotoifnot %₅ label₃₃)
31  (call top.push! %₇ %₂₉)
32  (goto label₃₅)
33  slot₁/idx
34  (call top.setindex! %₇ %₂₉ %₃₃)
35  slot₁/idx
36  (= slot₁/idx (call top.add_int %₃₅ 1))
37  (= slot₂/next (call top.iterate %₁ %₂₇))
38  slot₂/next
39  (call core.=== %₃₈ core.nothing)
40  (call top.not_int %₃₉)
41  (gotoifnot %₄₀ label₄₃)
42  (goto label₂₃)
43  (= slot₃/next (call top.iterate %₂ %₁₇))
44  slot₃/next
45  (call core.=== %₄₄ core.nothing)
46  (call top.not_int %₄₅)
47  (gotoifnot %₄₆ label₄₉)
48  (goto label₁₅)
49  (return %₇)

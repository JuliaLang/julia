########################################
# Simple 1D generator
(x+1 for x in xs)
#---------------------
1   (call core.tuple)
2   (call core.tuple)
3   (call core.tuple)
4   (call core.eval_closure_type TestMod :##->###0 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###0
7   (call core.svec %₆ core.Any)
8   (call core.svec)
9   SourceLocation::1:2
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.+
    2   (call %₁ slot₂/x 1)
    3   (return %₂)
12  latestworld
13  TestMod.##->###0
14  (new %₁₃)
15  (= slot₁/#-># %₁₄)
16  slot₁/#->#
17  TestMod.xs
18  (call top.Generator %₁₆ %₁₇)
19  (return %₁₈)

########################################
# Product iteration
(x+y for x in xs, y in ys)
#---------------------
1   (call core.tuple)
2   (call core.tuple)
3   (call core.tuple)
4   (call core.eval_closure_type TestMod :##->###1 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###1
7   (call core.svec %₆ core.Any)
8   (call core.svec)
9   SourceLocation::1:2
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read) slot₂/destructured slot₃/iterstate(single_assign) slot₄/x(single_assign) slot₅/y(single_assign)]
    1   (call top.indexed_iterate slot₂/destructured 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/destructured 2 %₄)
    6   (= slot₅/y (call core.getfield %₅ 1))
    7   TestMod.+
    8   slot₄/x
    9   slot₅/y
    10  (call %₇ %₈ %₉)
    11  (return %₁₀)
12  latestworld
13  TestMod.##->###1
14  (new %₁₃)
15  (= slot₁/#-># %₁₄)
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
1   (call core.tuple)
2   (call core.tuple)
3   (call core.tuple)
4   (call core.eval_closure_type TestMod :##->###2 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###2
7   (call core.svec %₆ core.Any)
8   (call core.svec)
9   SourceLocation::1:29
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read) slot₂/destructured slot₃/iterstate(single_assign) slot₄/x(single_assign) slot₅/y(!read,single_assign)]
    1   (call top.indexed_iterate slot₂/destructured 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/destructured 2 %₄)
    6   (= slot₅/y (call core.getfield %₅ 1))
    7   TestMod.f
    8   slot₄/x
    9   (call %₇ %₈)
    10  (return %₉)
12  latestworld
13  TestMod.##->###2
14  (new %₁₃)
15  (= slot₁/#-># %₁₄)
16  slot₁/#->#
17  TestMod.iter
18  (call top.Filter %₁₆ %₁₇)
19  (call top.Generator top.identity %₁₈)
20  (return %₁₉)

########################################
# Use of placeholders in iteration vars
(1 for _ in xs)
#---------------------
1   (call core.tuple)
2   (call core.tuple)
3   (call core.tuple)
4   (call core.eval_closure_type TestMod :##->###3 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###3
7   (call core.svec %₆ core.Any)
8   (call core.svec)
9   SourceLocation::1:2
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read) slot₂/#unused#(!read)]
    1   (return 1)
12  latestworld
13  TestMod.##->###3
14  (new %₁₃)
15  (= slot₁/#-># %₁₄)
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
1   (call core.tuple)
2   (call core.tuple)
3   (call core.tuple)
4   (call core.eval_closure_type TestMod :##->###4 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###4
7   (call core.svec %₆ core.Any)
8   (call core.svec)
9   SourceLocation::1:2
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read) slot₂/destructured slot₃/iterstate slot₄/x(!read,single_assign) slot₅/y(!read,single_assign)]
    1   (call top.indexed_iterate slot₂/destructured 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/destructured 2 %₄)
    6   (call core.getfield %₅ 1)
    7   (= slot₃/iterstate (call core.getfield %₅ 2))
    8   slot₃/iterstate
    9   (call top.indexed_iterate slot₂/destructured 3 %₈)
    10  (= slot₅/y (call core.getfield %₉ 1))
    11  TestMod.body
    12  (return %₁₁)
12  latestworld
13  TestMod.##->###4
14  (new %₁₃)
15  (= slot₁/#-># %₁₄)
16  slot₁/#->#
17  TestMod.iter
18  (call top.Generator %₁₆ %₁₇)
19  (return %₁₈)

########################################
# return permitted in quoted syntax in generator
(:(return x) for _ in iter)
#---------------------
1   (call core.tuple)
2   (call core.tuple)
3   (call core.tuple)
4   (call core.eval_closure_type TestMod :##->###5 %₁ %₂ %₃)
5   latestworld
6   TestMod.##->###5
7   (call core.svec %₆ core.Any)
8   (call core.svec)
9   SourceLocation::1:4
10  (call core.svec %₇ %₈ %₉)
11  --- method core.nothing %₁₀
    slots: [slot₁/#self#(!read) slot₂/#unused#(!read)]
    1   (call JuliaLowering.interpolate_ast SyntaxTree (inert_syntaxtree (return x)))
    2   (return %₁)
12  latestworld
13  TestMod.##->###5
14  (new %₁₃)
15  (= slot₁/#-># %₁₄)
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
1   (call core.tuple)
2   (call core.tuple)
3   (call core.tuple)
4   (call core.eval_closure_type TestMod :##->###6 %₁ %₂ %₃)
5   latestworld
6   (call core.tuple)
7   (call core.tuple)
8   (call core.tuple)
9   (call core.eval_closure_type TestMod :##->###->###0 %₆ %₇ %₈)
10  latestworld
11  TestMod.##->###->###0
12  (call core.svec %₁₁ core.Any)
13  (call core.svec)
14  SourceLocation::1:2
15  (call core.svec %₁₂ %₁₃ %₁₄)
16  --- method core.nothing %₁₅
    slots: [slot₁/#self#(!read) slot₂/x slot₃/x(single_assign)]
    1   slot₂/x
    2   (= slot₃/x %₁)
    3   slot₃/x
    4   (return %₃)
17  latestworld
18  TestMod.##->###6
19  (call core.svec %₁₈ core.Any)
20  (call core.svec)
21  SourceLocation::1:2
22  (call core.svec %₁₉ %₂₀ %₂₁)
23  --- method core.nothing %₂₂
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/#->#(single_assign)]
    1   TestMod.##->###->###0
    2   (new %₁)
    3   (= slot₃/#-># %₂)
    4   slot₃/#->#
    5   TestMod.:
    6   (call %₅ 1 2)
    7   (call top.Generator %₄ %₆)
    8   (return %₇)
24  latestworld
25  TestMod.##->###6
26  (new %₂₅)
27  (= slot₁/#-># %₂₆)
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

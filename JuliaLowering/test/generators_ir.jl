########################################
# Simple 1D generator
(x+1 for x in xs)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call core.svec)
4   (call JuliaLowering.eval_closure_type TestMod :#1#2 %₁ %₂ %₃)
5   latestworld
6   TestMod.#1#2
7   (new %₆)
8   TestMod.#1#2
9   (call core.svec %₈ core.Any)
10  (call core.svec)
11  SourceLocation::1:2
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.+
    2   (call %₁ slot₂/x 1)
    3   (return %₂)
14  latestworld
15  (= slot₁/#-># %₇)
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
4   (call JuliaLowering.eval_closure_type TestMod :#3#4 %₁ %₂ %₃)
5   latestworld
6   TestMod.#3#4
7   (new %₆)
8   TestMod.#3#4
9   (call core.svec %₈ core.Any)
10  (call core.svec)
11  SourceLocation::1:2
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
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
14  latestworld
15  (= slot₁/#-># %₇)
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
4   (call JuliaLowering.eval_closure_type TestMod :#5#6 %₁ %₂ %₃)
5   latestworld
6   TestMod.#5#6
7   (new %₆)
8   TestMod.#5#6
9   (call core.svec %₈ core.Any)
10  (call core.svec)
11  SourceLocation::1:2
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
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
14  latestworld
15  (= slot₁/#-># %₇)
16  slot₁/#->#
17  (call core.svec)
18  (call core.svec)
19  (call core.svec)
20  (call JuliaLowering.eval_closure_type TestMod :#7#8 %₁₇ %₁₈ %₁₉)
21  latestworld
22  TestMod.#7#8
23  (new %₂₂)
24  TestMod.#7#8
25  (call core.svec %₂₄ core.Any)
26  (call core.svec)
27  SourceLocation::1:29
28  (call core.svec %₂₅ %₂₆ %₂₇)
29  (call core.define_method TestMod core.nothing %₂₈
    --- code_info
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
30  latestworld
31  (= slot₂/#-># %₂₃)
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
4   (call JuliaLowering.eval_closure_type TestMod :#9#10 %₁ %₂ %₃)
5   latestworld
6   TestMod.#9#10
7   (new %₆)
8   TestMod.#9#10
9   (call core.svec %₈ core.Any)
10  (call core.svec)
11  SourceLocation::1:2
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/#unused#(!read)]
    1   (return 1)
14  latestworld
15  (= slot₁/#-># %₇)
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
4   (call JuliaLowering.eval_closure_type TestMod :#11#12 %₁ %₂ %₃)
5   latestworld
6   TestMod.#11#12
7   (new %₆)
8   TestMod.#11#12
9   (call core.svec %₈ core.Any)
10  (call core.svec)
11  SourceLocation::1:2
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
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
14  latestworld
15  (= slot₁/#-># %₇)
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
4   (call JuliaLowering.eval_closure_type TestMod :#13#14 %₁ %₂ %₃)
5   latestworld
6   TestMod.#13#14
7   (new %₆)
8   TestMod.#13#14
9   (call core.svec %₈ core.Any)
10  (call core.svec)
11  SourceLocation::1:4
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/#unused#(!read)]
    1   (call JuliaLowering.interpolate_expr (inert (return x)))
    2   (return %₁)
14  latestworld
15  (= slot₁/#-># %₇)
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
4   (call JuliaLowering.eval_closure_type TestMod :#15#16 %₁ %₂ %₃)
5   latestworld
6   TestMod.#15#16
7   (new %₆)
8   (call core.svec)
9   (call core.svec)
10  (call core.svec)
11  (call JuliaLowering.eval_closure_type TestMod :#17#18 %₈ %₉ %₁₀)
12  latestworld
13  TestMod.#17#18
14  (call core.svec %₁₃ core.Any)
15  (call core.svec)
16  SourceLocation::1:2
17  (call core.svec %₁₄ %₁₅ %₁₆)
18  (call core.define_method TestMod core.nothing %₁₇
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/x slot₃/x(single_assign)]
    1   slot₂/x
    2   (= slot₃/x %₁)
    3   slot₃/x
    4   (return %₃)
19  latestworld
20  TestMod.#15#16
21  (call core.svec %₂₀ core.Any)
22  (call core.svec)
23  SourceLocation::1:2
24  (call core.svec %₂₁ %₂₂ %₂₃)
25  (call core.define_method TestMod core.nothing %₂₄
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/#->#(single_assign)]
    1   TestMod.#17#18
    2   (new %₁)
    3   (= slot₃/#-># %₂)
    4   slot₃/#->#
    5   TestMod.:
    6   (call %₅ 1 2)
    7   (call top.Generator %₄ %₆)
    8   (return %₇)
26  latestworld
27  (= slot₁/#-># %₇)
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

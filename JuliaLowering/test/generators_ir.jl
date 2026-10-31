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
11  SourceLocation:1
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.+
    2   (call %₁ slot₂/x 1)
    3   (return %₂)
14  latestworld
15  (= slot₁/#-># %₇)
16  TestMod.xs
17  (call top.Generator slot₁/#-># %₁₆)
18  (return %₁₇)

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
11  SourceLocation:1
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/ slot₃/x(single_assign) slot₄/y(single_assign) slot₅/(single_assign)]
    1   (call top.indexed_iterate slot₂/ 1)
    2   (= slot₃/x (call core.getfield %₁ 1))
    3   (= slot₅/ (call core.getfield %₁ 2))
    4   (call top.indexed_iterate slot₂/ 2 slot₅/)
    5   (= slot₄/y (call core.getfield %₄ 1))
    6   TestMod.+
    7   (call %₆ slot₃/x slot₄/y)
    8   (return %₇)
14  latestworld
15  (= slot₁/#-># %₇)
16  TestMod.xs
17  TestMod.ys
18  (call top.product %₁₆ %₁₇)
19  (call top.Generator slot₁/#-># %₁₈)
20  (return %₁₉)

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
11  SourceLocation:1
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/ slot₃/x(single_assign) slot₄/y(single_assign) slot₅/(single_assign)]
    1   (call top.indexed_iterate slot₂/ 1)
    2   (= slot₃/x (call core.getfield %₁ 1))
    3   (= slot₅/ (call core.getfield %₁ 2))
    4   (call top.indexed_iterate slot₂/ 2 slot₅/)
    5   (= slot₄/y (call core.getfield %₄ 1))
    6   (call core.tuple slot₃/x slot₄/y)
    7   (return %₆)
14  latestworld
15  (= slot₁/#-># %₇)
16  (call core.svec)
17  (call core.svec)
18  (call core.svec)
19  (call JuliaLowering.eval_closure_type TestMod :#7#8 %₁₆ %₁₇ %₁₈)
20  latestworld
21  TestMod.#7#8
22  (new %₂₁)
23  TestMod.#7#8
24  (call core.svec %₂₃ core.Any)
25  (call core.svec)
26  SourceLocation:1
27  (call core.svec %₂₄ %₂₅ %₂₆)
28  (call core.define_method TestMod core.nothing %₂₇
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/ slot₃/x(single_assign) slot₄/y(!read,single_assign) slot₅/(single_assign)]
    1   (call top.indexed_iterate slot₂/ 1)
    2   (= slot₃/x (call core.getfield %₁ 1))
    3   (= slot₅/ (call core.getfield %₁ 2))
    4   (call top.indexed_iterate slot₂/ 2 slot₅/)
    5   (= slot₄/y (call core.getfield %₄ 1))
    6   TestMod.f
    7   (call %₆ slot₃/x)
    8   (return %₇)
29  latestworld
30  (= slot₂/#-># %₂₂)
31  TestMod.iter
32  (call top.Filter slot₂/#-># %₃₁)
33  (call top.Generator slot₁/#-># %₃₂)
34  (return %₃₃)

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
11  SourceLocation:1
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/#unused#(!read)]
    1   (return 1)
14  latestworld
15  (= slot₁/#-># %₇)
16  TestMod.xs
17  (call top.Generator slot₁/#-># %₁₆)
18  (return %₁₇)

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
11  SourceLocation:1
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/ slot₃/x(!read,single_assign) slot₄/y(!read,single_assign) slot₅/]
    1   (call top.indexed_iterate slot₂/ 1)
    2   (= slot₃/x (call core.getfield %₁ 1))
    3   (= slot₅/ (call core.getfield %₁ 2))
    4   slot₅/
    5   (call top.indexed_iterate slot₂/ 2 %₄)
    6   (call core.getfield %₅ 1)
    7   (= slot₅/ (call core.getfield %₅ 2))
    8   slot₅/
    9   (call top.indexed_iterate slot₂/ 3 %₈)
    10  (= slot₄/y (call core.getfield %₉ 1))
    11  TestMod.body
    12  (return %₁₁)
14  latestworld
15  (= slot₁/#-># %₇)
16  TestMod.iter
17  (call top.Generator slot₁/#-># %₁₆)
18  (return %₁₇)

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
11  SourceLocation:1
12  (call core.svec %₉ %₁₀ %₁₁)
13  (call core.define_method TestMod core.nothing %₁₂
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/#unused#(!read)]
    1   (copyast (inert (return x)))
    2   (return %₁)
14  latestworld
15  (= slot₁/#-># %₇)
16  TestMod.iter
17  (call top.Generator slot₁/#-># %₁₆)
18  (return %₁₇)

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
16  SourceLocation:1
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
23  SourceLocation:1
24  (call core.svec %₂₁ %₂₂ %₂₃)
25  (call core.define_method TestMod core.nothing %₂₄
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/x(!read) slot₃/#->#(single_assign)]
    1   TestMod.#17#18
    2   (new %₁)
    3   (= slot₃/#-># %₂)
    4   TestMod.:
    5   (call %₄ 1 2)
    6   (call top.Generator slot₃/#-># %₅)
    7   (return %₆)
26  latestworld
27  (= slot₁/#-># %₇)
28  TestMod.:
29  (call %₂₈ 1 3)
30  (call top.Generator slot₁/#-># %₂₉)
31  (call top.Flatten %₃₀)
32  (return %₃₁)

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
9   (= slot₁/ (call top.first %₈))
10  (= slot₂/ (call top.iterate %₂))
11  slot₂/
12  (call core.=== %₁₁ core.nothing)
13  (call top.not_int %₁₂)
14  (gotoifnot %₁₃ label₄₈)
15  slot₂/
16  (= slot₃/y (call core.getfield %₁₅ 1))
17  (call core.getfield %₁₅ 2)
18  (= slot₄/ (call top.iterate %₁))
19  slot₄/
20  (call core.=== %₁₉ core.nothing)
21  (call top.not_int %₂₀)
22  (gotoifnot %₂₁ label₄₂)
23  slot₃/y
24  (= slot₅/y %₂₃)
25  slot₄/
26  (= slot₆/x (call core.getfield %₂₅ 1))
27  (call core.getfield %₂₅ 2)
28  (call core.tuple slot₆/x slot₅/y)
29  (gotoifnot %₅ label₃₂)
30  (call top.push! %₇ %₂₈)
31  (goto label₃₄)
32  slot₁/
33  (call top.setindex! %₇ %₂₈ %₃₂)
34  slot₁/
35  (= slot₁/ (call top.add_int %₃₄ 1))
36  (= slot₄/ (call top.iterate %₁ %₂₇))
37  slot₄/
38  (call core.=== %₃₇ core.nothing)
39  (call top.not_int %₃₈)
40  (gotoifnot %₃₉ label₄₂)
41  (goto label₂₃)
42  (= slot₂/ (call top.iterate %₂ %₁₇))
43  slot₂/
44  (call core.=== %₄₃ core.nothing)
45  (call top.not_int %₄₄)
46  (gotoifnot %₄₅ label₄₈)
47  (goto label₁₅)
48  (return %₇)

########################################
# Simple 1D generator
(x+1 for x in xs)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##0 %₁ %₂)
4   latestworld
5   TestMod.#->##0
6   (call core.svec %₅ core.Any)
7   (call core.svec)
8   SourceLocation::1:2
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.+
    2   (call %₁ slot₂/x 1)
    3   (return %₂)
11  latestworld
12  TestMod.#->##0
13  (new %₁₂)
14  TestMod.xs
15  (call top.Generator %₁₃ %₁₄)
16  (return %₁₅)

########################################
# Product iteration
(x+y for x in xs, y in ys)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##1 %₁ %₂)
4   latestworld
5   TestMod.#->##1
6   (call core.svec %₅ core.Any)
7   (call core.svec)
8   SourceLocation::1:2
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
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
11  latestworld
12  TestMod.#->##1
13  (new %₁₂)
14  TestMod.xs
15  TestMod.ys
16  (call top.product %₁₄ %₁₅)
17  (call top.Generator %₁₃ %₁₆)
18  (return %₁₇)

########################################
# Use `identity` as the Generator function when possible eg in filters
((x,y) for (x,y) in iter if f(x))
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##2 %₁ %₂)
4   latestworld
5   TestMod.#->##2
6   (call core.svec %₅ core.Any)
7   (call core.svec)
8   SourceLocation::1:29
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
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
11  latestworld
12  TestMod.#->##2
13  (new %₁₂)
14  TestMod.iter
15  (call top.Filter %₁₃ %₁₄)
16  (call top.Generator top.identity %₁₅)
17  (return %₁₆)

########################################
# Use of placeholders in iteration vars
(1 for _ in xs)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##3 %₁ %₂)
4   latestworld
5   TestMod.#->##3
6   (call core.svec %₅ core.Any)
7   (call core.svec)
8   SourceLocation::1:2
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read) slot₂/_(!read)]
    1   (return 1)
11  latestworld
12  TestMod.#->##3
13  (new %₁₂)
14  TestMod.xs
15  (call top.Generator %₁₃ %₁₄)
16  (return %₁₅)

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
3   (call JuliaLowering.eval_closure_type TestMod :#->##5 %₁ %₂)
4   latestworld
5   TestMod.#->##5
6   (call core.svec %₅ core.Any)
7   (call core.svec)
8   SourceLocation::1:2
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
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
11  latestworld
12  TestMod.#->##5
13  (new %₁₂)
14  TestMod.iter
15  (call top.Generator %₁₃ %₁₄)
16  (return %₁₅)

########################################
# return permitted in quoted syntax in generator
(:(return x) for _ in iter)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##6 %₁ %₂)
4   latestworld
5   TestMod.#->##6
6   (call core.svec %₅ core.Any)
7   (call core.svec)
8   SourceLocation::1:4
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read) slot₂/_(!read)]
    1   (call JuliaLowering.interpolate_ast (inert (return x)))
    2   (return %₁)
11  latestworld
12  TestMod.#->##6
13  (new %₁₂)
14  TestMod.iter
15  (call top.Generator %₁₃ %₁₄)
16  (return %₁₅)

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
3   (call JuliaLowering.eval_closure_type TestMod :#->##7 %₁ %₂)
4   latestworld
5   (call core.svec)
6   (call core.svec)
7   (call JuliaLowering.eval_closure_type TestMod :#->#->##0 %₅ %₆)
8   latestworld
9   TestMod.#->#->##0
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::1:2
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- method core.nothing %₁₃
    slots: [slot₁/#self#(!read) slot₂/x slot₃/x]
    1   slot₂/x
    2   (= slot₃/x %₁)
    3   slot₃/x
    4   (return %₃)
15  latestworld
16  TestMod.#->##7
17  (call core.svec %₁₆ core.Any)
18  (call core.svec)
19  SourceLocation::1:2
20  (call core.svec %₁₇ %₁₈ %₁₉)
21  --- method core.nothing %₂₀
    slots: [slot₁/#self#(!read) slot₂/x(!read)]
    1   TestMod.#->#->##0
    2   (new %₁)
    3   TestMod.:
    4   (call %₃ 1 2)
    5   (call top.Generator %₂ %₄)
    6   (return %₅)
22  latestworld
23  TestMod.#->##7
24  (new %₂₃)
25  TestMod.:
26  (call %₂₅ 1 3)
27  (call top.Generator %₂₄ %₂₆)
28  (call top.Flatten %₂₇)
29  (return %₂₈)

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


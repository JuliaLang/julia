########################################
# Simple 1D generator
(x+1 for x in xs)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##0 %₁ %₂)
4   TestMod.#->##0
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:2
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.+
    2   (call %₁ slot₂/x 1)
    3   (return %₂)
10  TestMod.#->##0
11  (new %₁₀)
12  TestMod.xs
13  (call top.Generator %₁₁ %₁₂)
14  (return %₁₃)

########################################
# Product iteration
(x+y for x in xs, y in ys)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##1 %₁ %₂)
4   TestMod.#->##1
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:2
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
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
10  TestMod.#->##1
11  (new %₁₀)
12  TestMod.xs
13  TestMod.ys
14  (call top.product %₁₂ %₁₃)
15  (call top.Generator %₁₁ %₁₄)
16  (return %₁₅)

########################################
# Use `identity` as the Generator function when possible eg in filters
((x,y) for (x,y) in iter if f(x))
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##2 %₁ %₂)
4   TestMod.#->##2
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:29
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
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
10  TestMod.#->##2
11  (new %₁₀)
12  TestMod.iter
13  (call top.Filter %₁₁ %₁₂)
14  (call top.Generator top.identity %₁₃)
15  (return %₁₄)

########################################
# Use of placeholders in iteration vars
(1 for _ in xs)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##3 %₁ %₂)
4   TestMod.#->##3
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:2
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/_(!read)]
    1   (return 1)
10  TestMod.#->##3
11  (new %₁₀)
12  TestMod.xs
13  (call top.Generator %₁₁ %₁₂)
14  (return %₁₃)

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
4   TestMod.#->##5
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:2
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
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
10  TestMod.#->##5
11  (new %₁₀)
12  TestMod.iter
13  (call top.Generator %₁₁ %₁₂)
14  (return %₁₃)

########################################
# return permitted in quoted syntax in generator
(:(return x) for _ in iter)
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#->##6 %₁ %₂)
4   TestMod.#->##6
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:4
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/_(!read)]
    1   (call JuliaLowering.interpolate_ast (inert (return x)))
    2   (return %₁)
10  TestMod.#->##6
11  (new %₁₀)
12  TestMod.iter
13  (call top.Generator %₁₁ %₁₂)
14  (return %₁₃)

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
4   (call core.svec)
5   (call core.svec)
6   (call JuliaLowering.eval_closure_type TestMod :#->#->##0 %₄ %₅)
7   TestMod.#->#->##0
8   (call core.svec %₇ core.Any)
9   (call core.svec)
10  SourceLocation::1:2
11  (call core.svec %₈ %₉ %₁₀)
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read) slot₂/x slot₃/x]
    1   slot₂/x
    2   (= slot₃/x %₁)
    3   slot₃/x
    4   (return %₃)
13  TestMod.#->##7
14  (call core.svec %₁₃ core.Any)
15  (call core.svec)
16  SourceLocation::1:2
17  (call core.svec %₁₄ %₁₅ %₁₆)
18  --- method core.nothing %₁₇
    slots: [slot₁/#self#(!read) slot₂/x(!read)]
    1   TestMod.#->#->##0
    2   (new %₁)
    3   TestMod.:
    4   (call %₃ 1 2)
    5   (call top.Generator %₂ %₄)
    6   (return %₅)
19  TestMod.#->##7
20  (new %₁₉)
21  TestMod.:
22  (call %₂₁ 1 3)
23  (call top.Generator %₂₀ %₂₂)
24  (call top.Flatten %₂₃)
25  (return %₂₄)

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


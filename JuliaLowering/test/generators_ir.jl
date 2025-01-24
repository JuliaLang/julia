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
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.+
    2   (call %₁ slot₂/x 1)
    3   (return %₂)
7   TestMod.#->##0
8   (new %₇)
9   TestMod.xs
10  (call top.Generator %₈ %₉)
11  (return %₁₀)

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
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    slots: [slot₁/#self#(!read) slot₂/destructured_arg_1 slot₃/iterstate slot₄/x slot₅/y]
    1   (call top.indexed_iterate slot₂/destructured_arg_1 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/destructured_arg_1 2 %₄)
    6   (= slot₅/y (call core.getfield %₅ 1))
    7   TestMod.+
    8   slot₄/x
    9   slot₅/y
    10  (call %₇ %₈ %₉)
    11  (return %₁₀)
7   TestMod.#->##1
8   (new %₇)
9   TestMod.xs
10  TestMod.ys
11  (call top.product %₉ %₁₀)
12  (call top.Generator %₈ %₁₁)
13  (return %₁₂)

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
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    slots: [slot₁/#self#(!read) slot₂/destructured_arg_1 slot₃/iterstate slot₄/x slot₅/y(!read)]
    1   (call top.indexed_iterate slot₂/destructured_arg_1 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/destructured_arg_1 2 %₄)
    6   (= slot₅/y (call core.getfield %₅ 1))
    7   TestMod.f
    8   slot₄/x
    9   (call %₇ %₈)
    10  (return %₉)
7   TestMod.#->##2
8   (new %₇)
9   TestMod.iter
10  (call top.Filter %₈ %₉)
11  (call top.Generator top.identity %₁₀)
12  (return %₁₁)

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
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    slots: [slot₁/#self#(!read) slot₂/_(!read)]
    1   (return 1)
7   TestMod.#->##3
8   (new %₇)
9   TestMod.xs
10  (call top.Generator %₈ %₉)
11  (return %₁₀)

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
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    slots: [slot₁/#self#(!read) slot₂/destructured_arg_1 slot₃/iterstate slot₄/x(!read) slot₅/y(!read)]
    1   (call top.indexed_iterate slot₂/destructured_arg_1 1)
    2   (= slot₄/x (call core.getfield %₁ 1))
    3   (= slot₃/iterstate (call core.getfield %₁ 2))
    4   slot₃/iterstate
    5   (call top.indexed_iterate slot₂/destructured_arg_1 2 %₄)
    6   (call core.getfield %₅ 1)
    7   (= slot₃/iterstate (call core.getfield %₅ 2))
    8   slot₃/iterstate
    9   (call top.indexed_iterate slot₂/destructured_arg_1 3 %₈)
    10  (= slot₅/y (call core.getfield %₉ 1))
    11  TestMod.body
    12  (return %₁₁)
7   TestMod.#->##5
8   (new %₇)
9   TestMod.iter
10  (call top.Generator %₈ %₉)
11  (return %₁₀)

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
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method core.nothing %₅
    slots: [slot₁/#self#(!read) slot₂/_(!read)]
    1   (call JuliaLowering.interpolate_ast (inert (return x)))
    2   (return %₁)
7   TestMod.#->##6
8   (new %₇)
9   TestMod.iter
10  (call top.Generator %₈ %₉)
11  (return %₁₀)

########################################
# Error: `return` not permitted in generator body
((return x) + y for x in iter)
#---------------------
LoweringError:
((return x) + y for x in iter)
# └──────┘ ── `return` not allowed inside comprehension or generator

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


########################################
# Nested case with duplicate iteration variables
(x for x in 1:3 for x in 1:2)
#---------------------
1   --- thunk
    1   (global TestMod.#->##8)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#->##8 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#->##8)
    8   (= TestMod.#->##8 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   --- thunk
    1   (global TestMod.#->#->##1)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :#->#->##1 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#->#->##1)
    8   (= TestMod.#->#->##1 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
3   TestMod.#->#->##1
4   (call core.svec %₃ core.Any)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/x slot₃/x]
    1   slot₂/x
    2   (= slot₃/x %₁)
    3   slot₃/x
    4   (return %₃)
8   TestMod.#->##8
9   (call core.svec %₈ core.Any)
10  (call core.svec)
11  (call core.svec %₉ %₁₀ :($(QuoteNode(:(#= line 1 =#)))))
12  --- method core.nothing %₁₁
    slots: [slot₁/#self#(!read) slot₂/x(!read)]
    1   TestMod.#->#->##1
    2   (new %₁)
    3   TestMod.:
    4   (call %₃ 1 2)
    5   (call top.Generator %₂ %₄)
    6   (return %₅)
13  TestMod.#->##8
14  (new %₁₃)
15  TestMod.:
16  (call %₁₅ 1 3)
17  (call top.Generator %₁₄ %₁₆)
18  (call top.Flatten %₁₇)
19  (return %₁₈)

########################################
# Comprehension lowers to generator with collect
[x for x in xs]
#---------------------
1   TestMod.xs
2   (call top.Generator top.identity %₁)
3   (call top.collect %₂)
4   (return %₃)

########################################
# Typed comprehension lowers to generator with collect
T[x for x in xs]
#---------------------
1   TestMod.T
2   TestMod.xs
3   (call top.Generator top.identity %₂)
4   (call top.collect %₁ %₃)
5   (return %₄)


########################################
# Simple closure
let
    x = 1
    function f(y)
        x + y
    end
end
#---------------------
1   (= slot₂ (call core.Box))
2   (newvar slot₁)
3   --- thunk
    1   (global TestMod.#f##0)
    2   (call core.svec)
    3   (call core.svec :x)
    4   (call core.svec)
    5   (call core._structtype TestMod :#f##0 %₂ %₃ %₄ false 1)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#f##0)
    8   (= TestMod.#f##0 %₅)
    9   (call core.svec core.Box)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
4   TestMod.#f##0
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 3 =#)))))
8   --- method core.nothing %₇
    1   TestMod.+
    2   (call core.getfield slot₁/x :x)
    3   (call core.isdefined %₂ :contents)
    4   (gotoifnot %₃ label₆)
    5   (goto label₈)
    6   (newvar slot₃/x)
    7   slot₃/x
    8   (call core.getfield %₂ :contents)
    9   (call %₁ %₈ slot₂/y)
    10  (return %₉)
9   1
10  slot₂/x
11  (call core.setfield! %₁₀ :contents %₉)
12  TestMod.#f##0
13  slot₂/f
14  (= slot₁/f (new %₁₂ %₁₃))
15  slot₁/f
16  slot₁/f
17  (return %₁₆)

########################################
# Closure which sets the value of a captured variable
let
    x = 1
    function f(y)
        x = 2
    end
end
#---------------------
1   (= slot₂ (call core.Box))
2   (newvar slot₁)
3   --- thunk
    1   (global TestMod.#f##1)
    2   (call core.svec)
    3   (call core.svec :x)
    4   (call core.svec)
    5   (call core._structtype TestMod :#f##1 %₂ %₃ %₄ false 1)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#f##1)
    8   (= TestMod.#f##1 %₅)
    9   (call core.svec core.Box)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
4   TestMod.#f##1
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 3 =#)))))
8   --- method core.nothing %₇
    1   2
    2   (call core.getfield slot₁/x :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
9   1
10  slot₂/x
11  (call core.setfield! %₁₀ :contents %₉)
12  TestMod.#f##1
13  slot₂/f
14  (= slot₁/f (new %₁₂ %₁₃))
15  slot₁/f
16  slot₁/f
17  (return %₁₆)

########################################
# Function where arguments are captured into a closure
function f(x)
    function g()
        x = 10
    end
    g()
    x
end
#---------------------
1   (method TestMod.f)
2   --- thunk
    1   (global TestMod.#f#g##0)
    2   (call core.svec)
    3   (call core.svec :x)
    4   (call core.svec)
    5   (call core._structtype TestMod :#f#g##0 %₂ %₃ %₄ false 1)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.#f#g##0)
    8   (= TestMod.#f#g##0 %₅)
    9   (call core.svec core.Box)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
3   TestMod.#f#g##0
4   (call core.svec %₃)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 2 =#)))))
7   --- method core.nothing %₆
    1   10
    2   (call core.getfield slot₁/x :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
8   TestMod.f
9   (call core.Typeof %₈)
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  (call core.svec %₁₀ %₁₁ :($(QuoteNode(:(#= line 1 =#)))))
13  --- method core.nothing %₁₂
    1   (= slot₂/x (call core.Box slot₂/x))
    2   slot₂/x
    3   (call core.isdefined %₂ :contents)
    4   (gotoifnot %₃ label₆)
    5   (goto label₈)
    6   (newvar slot₅/x)
    7   slot₅/x
    8   (call core.getfield %₂ :contents)
    9   (call core.Box %₈)
    10  (call core.setfield! slot₂/x :contents %₉)
    11  (newvar slot₃)
    12  TestMod.#f#g##0
    13  slot₂/g
    14  (call core.isdefined %₁₃ :contents)
    15  (gotoifnot %₁₄ label₁₇)
    16  (goto label₁₉)
    17  (newvar slot₆/x)
    18  slot₆/x
    19  (call core.getfield %₁₃ :contents)
    20  (= slot₃/g (new %₁₂ %₁₉))
    21  slot₃/g
    22  slot₃/g
    23  slot₃/g
    24  (call %₂₃)
    25  slot₂/x
    26  (call core.isdefined %₂₅ :contents)
    27  (gotoifnot %₂₆ label₂₉)
    28  (goto label₃₁)
    29  (newvar slot₇/x)
    30  slot₇/x
    31  (call core.getfield %₂₅ :contents)
    32  (call core.isdefined %₃₁ :contents)
    33  (gotoifnot %₃₂ label₃₅)
    34  (goto label₃₇)
    35  (newvar slot₄/x)
    36  slot₄/x
    37  (call core.getfield %₃₁ :contents)
    38  (return %₃₇)
14  (return %₁₂)

########################################
# Anonymous function syntax with ->
x -> x*x
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
3   (new %₂)
4   TestMod.#->##0
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 1 =#)))))
8   --- method core.nothing %₇
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
9   (return %₃)

########################################
# Anonymous function syntax with `function`
function (x)
    x*x
end
#---------------------
1   --- thunk
    1   (global TestMod.##anon###0)
    2   (call core.svec)
    3   (call core.svec)
    4   (call core.svec)
    5   (call core._structtype TestMod :##anon###0 %₂ %₃ %₄ false 0)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.##anon###0)
    8   (= TestMod.##anon###0 %₅)
    9   (call core.svec)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
2   TestMod.##anon###0
3   (new %₂)
4   TestMod.##anon###0
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 1 =#)))))
8   --- method core.nothing %₇
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
9   (return %₃)


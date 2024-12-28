########################################
# Simple closure
# (FIXME: #self# should have `read` flag set)
let
    x = 1
    function f(y)
        x + y
    end
end
#---------------------
1   (= slot₂/x (call core.Box))
2   --- thunk
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
3   TestMod.#f##0
4   (call core.svec %₃ core.Any)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 3 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/y slot₃/x(!read)]
    1   TestMod.+
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.isdefined %₂ :contents)
    4   (gotoifnot %₃ label₆)
    5   (goto label₈)
    6   (newvar slot₃/x)
    7   slot₃/x
    8   (call core.getfield %₂ :contents)
    9   (call %₁ %₈ slot₂/y)
    10  (return %₉)
8   1
9   slot₂/x
10  (call core.setfield! %₉ :contents %₈)
11  TestMod.#f##0
12  slot₂/x
13  (= slot₁/f (new %₁₁ %₁₂))
14  slot₁/f
15  (return %₁₄)

########################################
# Closure which sets the value of a captured variable
let
    x = 1
    function f(y)
        x = 2
    end
end
#---------------------
1   (= slot₂/x (call core.Box))
2   --- thunk
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
3   TestMod.#f##1
4   (call core.svec %₃ core.Any)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 3 =#)))))
7   --- method core.nothing %₆
    slots: [slot₁/#self#(!read) slot₂/y(!read)]
    1   2
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
8   1
9   slot₂/x
10  (call core.setfield! %₉ :contents %₈)
11  TestMod.#f##1
12  slot₂/x
13  (= slot₁/f (new %₁₁ %₁₂))
14  slot₁/f
15  (return %₁₄)

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
    slots: [slot₁/#self#(!read)]
    1   10
    2   (call core.getfield slot₁/#self# :x)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
8   TestMod.f
9   (call core.Typeof %₈)
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  (call core.svec %₁₀ %₁₁ :($(QuoteNode(:(#= line 1 =#)))))
13  --- method core.nothing %₁₂
    slots: [slot₁/#self#(!read) slot₂/x slot₃/g(called) slot₄/x(!read)]
    1   (= slot₂/x (call core.Box slot₂/x))
    2   TestMod.#f#g##0
    3   (= slot₃/g (new %₂ slot₂/x))
    4   slot₃/g
    5   slot₃/g
    6   (call %₅)
    7   slot₂/x
    8   (call core.isdefined %₇ :contents)
    9   (gotoifnot %₈ label₁₁)
    10  (goto label₁₃)
    11  (newvar slot₄/x)
    12  slot₄/x
    13  (call core.getfield %₇ :contents)
    14  (return %₁₃)
14  TestMod.f
15  (return %₁₄)

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
    slots: [slot₁/#self#(!read) slot₂/x]
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
    slots: [slot₁/#self#(!read) slot₂/x]
    1   TestMod.*
    2   (call %₁ slot₂/x slot₂/x)
    3   (return %₂)
9   (return %₃)


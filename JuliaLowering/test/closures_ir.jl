########################################
# Simple closure
let
    x = 1
    function f(y)
        x + y
    end
end
#---------------------
1   (newvar slot₁)
2   (= slot₂ (call core.Box))
3   --- thunk
    1   (global TestMod.##closure#277)
    2   (call core.svec)
    3   (call core.svec :x)
    4   (call core.svec)
    5   (call core._structtype TestMod :##closure#277 %₂ %₃ %₄ false 1)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.##closure#277)
    8   (= TestMod.##closure#277 %₅)
    9   (call core.svec core.Box)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
4   TestMod.##closure#277
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
12  TestMod.##closure#277
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
1   (newvar slot₁)
2   (= slot₂ (call core.Box))
3   --- thunk
    1   (global TestMod.##closure#278)
    2   (call core.svec)
    3   (call core.svec :x)
    4   (call core.svec)
    5   (call core._structtype TestMod :##closure#278 %₂ %₃ %₄ false 1)
    6   (call core._setsuper! %₅ core.Function)
    7   (const TestMod.##closure#278)
    8   (= TestMod.##closure#278 %₅)
    9   (call core.svec core.Box)
    10  (call core._typebody! %₅ %₉)
    11  (return core.nothing)
4   TestMod.##closure#278
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
12  TestMod.##closure#278
13  slot₂/f
14  (= slot₁/f (new %₁₂ %₁₃))
15  slot₁/f
16  slot₁/f
17  (return %₁₆)

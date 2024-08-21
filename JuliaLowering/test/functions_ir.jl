########################################
# Return types
function f(x)::Int
    if x
        42.0
    end
    0xff
end
#----------
1   (method :f)
2   core.svec
3   core.svec
4   core.Typeof
5   TestMod.f
6   (call %₄ %₅)
7   core.Any
8   (call %₃ %₆ %₇)
9   core.svec
10  (call %₉)
11  (call %₂ %₈ %₁₀ :($(QuoteNode(:(#= line 1 =#)))))
12  --- method :f %₁₁
    1   TestMod.Int
    2   slot₂/x
    3   (gotoifnot %₂ label₄)
    4   (= slot₃/tmp 0xff)
    5   core.isa
    6   slot₃/tmp
    7   (call %₅ %₆ %₁)
    8   (gotoifnot %₇ label₁₀)
    9   (goto label₁₅)
    10  core.typeassert
    11  top.convert
    12  slot₃/tmp
    13  (call %₁₁ %₁ %₁₂)
    14  (= slot₃/tmp (call %₁₀ %₁₃ %₁))
    15  slot₃/tmp
    16  (return %₁₅)
13  (return %₁)

using JuliaLowering
using JuliaLowering: kind, @chk, @ast, @K_str

function var"@islocal"(__context__::JuliaLowering.MacroContext, ex)
    @chk kind(ex) == K"Identifier"
    @ast __context__ ex [K"extension"
        "islocal"::K"Symbol"
        ex
    ]
end

#*******************************************************************************
########################################
# @islocal
let x = 1
    @islocal(a), @islocal(x)
end
#---------------------
1   1
2   (= slot₁/x %₁)
3   (call core.tuple false true)
4   (return %₃)

########################################
# @islocal
let y = 2
    function f(x)
        @islocal(a), @islocal(x), @islocal(y)
    end
end
#---------------------
1   2
2   (= slot₁/y %₁)
3   (method :f)
4   TestMod.f
5   (call core.Typeof %₄)
6   (call core.svec %₅ core.Any)
7   (call core.svec)
8   (call core.svec %₆ %₇ :($(QuoteNode(:(#= line 2 =#)))))
9   --- method :f %₈
    1   (call core.tuple false true true)
    2   (return %₁)
10  (return %₃)

########################################
# @islocal
begin
    global x
    @islocal(x)
end
#---------------------
1   (global TestMod.x)
2   (return false)


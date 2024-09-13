using JuliaLowering
using JuliaLowering: kind, @chk, @ast, @K_str

function var"@islocal"(__context__::JuliaLowering.MacroContext, ex)
    @chk kind(ex) == K"Identifier"
    @ast __context__ ex [K"extension"
        "islocal"::K"Symbol"
        ex
    ]
end

function var"@locals"(__context__::JuliaLowering.MacroContext)
    @ast __context__ __context__.macroname [K"extension" "locals"::K"Symbol"]
end

#*******************************************************************************
########################################
# @islocal with locals and undefined vars
let x = 1
    @islocal(a), @islocal(x)
end
#---------------------
1   1
2   (= slot₁/x %₁)
3   (call core.tuple false true)
4   (return %₃)

########################################
# @islocal with function arguments
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
# @islocal with global
begin
    global x
    @islocal(x)
end
#---------------------
1   (global TestMod.x)
2   (return false)

########################################
# @locals with local and global
begin
    global x
    local y
    @locals
end
#---------------------
1   (global TestMod.x)
2   (call core.apply_type top.Dict core.Symbol core.Any)
3   (call %₂)
4   (isdefined slot₁/y)
5   (isdefined slot₁/y)
6   (gotoifnot %₅ label₉)
7   slot₁/y
8   (call top.setindex! %₃ %₇ :y)
9   (return %₃)

########################################
# @locals with function args (TODO: static parameters)
function f(z)
    @locals
end
#---------------------
1   (method :f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃ core.Any)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method :f %₆
    1   (call core.apply_type top.Dict core.Symbol core.Any)
    2   (call %₁)
    3   (isdefined slot₂/z)
    4   (isdefined slot₂/z)
    5   (gotoifnot %₄ label₇)
    6   (call top.setindex! %₂ slot₂/z :z)
    7   (return %₂)
8   (return %₁)


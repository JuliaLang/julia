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
4   (call core.Typeof %₃)
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   (call core.svec %₅ %₆ :($(QuoteNode(:(#= line 2 =#)))))
8   --- method :f %₇
    1   (call core.tuple false true true)
    2   (return %₁)
9   (return %₃)

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
5   (gotoifnot %₄ label₈)
6   slot₁/y
7   (call top.setindex! %₃ %₆ :y)
8   (return %₃)

########################################
# @locals with function args (TODO: static parameters)
function f(z)
    @locals
end
#---------------------
1   (method :f)
2   (call core.Typeof %₁)
3   (call core.svec %₂ core.Any)
4   (call core.svec)
5   (call core.svec %₃ %₄ :($(QuoteNode(:(#= line 1 =#)))))
6   --- method :f %₅
    1   (call core.apply_type top.Dict core.Symbol core.Any)
    2   (call %₁)
    3   (isdefined slot₂/z)
    4   (gotoifnot %₃ label₆)
    5   (call top.setindex! %₂ slot₂/z :z)
    6   (return %₂)
7   (return %₁)


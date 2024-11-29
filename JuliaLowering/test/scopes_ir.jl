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
8   --- method core.nothing %₇
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
6   --- method core.nothing %₅
    1   (call core.apply_type top.Dict core.Symbol core.Any)
    2   (call %₁)
    3   (isdefined slot₂/z)
    4   (gotoifnot %₃ label₆)
    5   (call top.setindex! %₂ slot₂/z :z)
    6   (return %₂)
7   (return %₁)

########################################
# Error: Duplicate function argument names
function f(x, x)
end
#---------------------
LoweringError:
function f(x, x)
#             ╙ ── function argument name not unique
end

########################################
# Error: Duplicate function argument with destructured arg
function f(x, (x,))
end
#---------------------
LoweringError:
function f(x, (x,))
#              ╙ ── function argument name not unique
end

########################################
# Error: Static parameter name not unique
function f() where T where T
end
#---------------------
LoweringError:
function f() where T where T
#                  ╙ ── function static parameter name not unique
end

########################################
# Error: static parameter colliding with argument names
function f(x::x) where x
end
#---------------------
LoweringError:
function f(x::x) where x
#                      ╙ ── static parameter name not distinct from function argument
end

########################################
# Error: duplicate destructure args
function f((x,), (x,))
end
#---------------------
LoweringError:
function f((x,), (x,))
#                 ╙ ── function argument name not unique
end

########################################
# Error: Conflicting local and global decls
let
    local x
    global x
end
#---------------------
LoweringError:
let
    local x
    global x
#   └──────┘ ── Variable `x` declared both local and global
end

########################################
# Error: Conflicting argument and local
function f(x)
    local x
end
#---------------------
LoweringError:
function f(x)
    local x
#   └─────┘ ── local variable name `x` conflicts with an argument
end

########################################
# Error: Conflicting argument and global
function f(x)
    global x
end
#---------------------
LoweringError:
function f(x)
    global x
#   └──────┘ ── global variable name `x` conflicts with an argument
end

########################################
# Error: Conflicting destructured argument and global
# TODO: The error could probably be a bit better here
function f((x,))
    global x
end
#---------------------
LoweringError:
function f((x,))
    global x
#   └──────┘ ── Variable `x` declared both local and global
end

########################################
# Error: Conflicting static parameter and local
function f() where T
    local T
end
#---------------------
LoweringError:
function f() where T
    local T
#   └─────┘ ── local variable name `T` conflicts with a static parameter
end

########################################
# Error: Conflicting static parameter and global
function f() where T
    global T
end
#---------------------
LoweringError:
function f() where T
    global T
#   └──────┘ ── global variable name `T` conflicts with a static parameter
end

########################################
# Error: Conflicting static parameter and local in nested scope
function f() where T
    let
        local T
    end
end
#---------------------
LoweringError:
function f() where T
    let
        local T
#       └─────┘ ── local variable name `T` conflicts with a static parameter
    end
end

########################################
# Error: Conflicting static parameter and global in nested scope
function f() where T
    let
        global T
    end
end
#---------------------
LoweringError:
function f() where T
    let
        global T
#       └──────┘ ── global variable name `T` conflicts with a static parameter
    end
end

########################################
# Error: Conflicting static parameter and implicit local
function f() where T
    let
        T = rhs
    end
end
#---------------------
LoweringError:
function f() where T
    let
        T = rhs
#       ╙ ── local variable name `T` conflicts with a static parameter
    end
end


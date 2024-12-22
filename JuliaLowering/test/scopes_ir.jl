using JuliaLowering: @islocal
using Base: @locals

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
begin
    local y = 2
    function f(x)
        @islocal(a), @islocal(x), @islocal(y)
    end
end
#---------------------
1   (= slot₁ (call core.Box))
2   2
3   slot₁/y
4   (call core.setfield! %₃ :contents %₂)
5   (method TestMod.f)
6   TestMod.f
7   (call core.Typeof %₆)
8   (call core.svec %₇ core.Any)
9   (call core.svec)
10  (call core.svec %₈ %₉ :($(QuoteNode(:(#= line 3 =#)))))
11  --- method core.nothing %₁₀
    1   (call core.tuple false true true)
    2   (return %₁)
12  TestMod.f
13  (return %₁₂)

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
1   (newvar slot₁)
2   (global TestMod.x)
3   (call core.apply_type top.Dict core.Symbol core.Any)
4   (call %₃)
5   (isdefined slot₁/y)
6   (gotoifnot %₅ label₉)
7   slot₁/y
8   (call top.setindex! %₄ %₇ :y)
9   (return %₄)

########################################
# @locals with function args (TODO: static parameters)
function f(z)
    @locals
end
#---------------------
1   (method TestMod.f)
2   TestMod.f
3   (call core.Typeof %₂)
4   (call core.svec %₃ core.Any)
5   (call core.svec)
6   (call core.svec %₄ %₅ :($(QuoteNode(:(#= line 1 =#)))))
7   --- method core.nothing %₆
    1   (call core.apply_type top.Dict core.Symbol core.Any)
    2   (call %₁)
    3   (isdefined slot₂/z)
    4   (gotoifnot %₃ label₆)
    5   (call top.setindex! %₂ slot₂/z :z)
    6   (return %₂)
8   TestMod.f
9   (return %₈)

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

########################################
# @isdefined with defined variables
let x = 1
    @isdefined x
    @isdefined y
end
#---------------------
1   1
2   (= slot₁/x %₁)
3   (isdefined TestMod.y)
4   (return %₃)


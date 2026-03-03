using .JuliaLowering: @islocal
using Base: @locals

#*******************************************************************************
########################################
# let syntax with decl in binding list
let x::T = rhs
    local T = 1
    T # <- This is a different `T` from the T in `x::T`
end
#---------------------
1   TestMod.rhs
2   TestMod.T
3   (newvar slot₂/T)
4   (= slot₃/tmp %₁)
5   (call core.isa slot₃/tmp %₂)
6   (gotoifnot %₅ label₈)
7   (goto label₁₀)
8   (call top.convert %₂ slot₃/tmp)
9   (= slot₃/tmp (call core.typeassert %₈ %₂))
10  slot₃/tmp
11  (= slot₁/x %₁₀)
12  (= slot₂/T 1)
13  slot₂/T
14  (return %₁₃)

########################################
# let syntax with tuple on lhs
let (x,y) = rhs
end
#---------------------
1   TestMod.rhs
2   (call top.indexed_iterate %₁ 1)
3   (= slot₂/x (call core.getfield %₂ 1))
4   (= slot₁/iterstate (call core.getfield %₂ 2))
5   slot₁/iterstate
6   (call top.indexed_iterate %₁ 2 %₅)
7   (= slot₃/y (call core.getfield %₆ 1))
8   (return core.nothing)

########################################
# let syntax with named tuple on lhs creates locals for the unpacked vars
let (; x,y) = rhs
end
#---------------------
1   TestMod.rhs
2   (= slot₁/x (call top.getproperty %₁ :x))
3   (= slot₂/y (call top.getproperty %₁ :y))
4   (return core.nothing)

########################################
# Let syntax with the same name creates nested bindings
let x = f(x), x = g(x)
end
#---------------------
1   TestMod.f
2   TestMod.x
3   (call %₁ %₂)
4   (= slot₁/x %₃)
5   TestMod.g
6   (call %₅ slot₁/x)
7   (= slot₂/x %₆)
8   (return core.nothing)

########################################
# let syntax with a function definition in the binding list creates a closure
let f() = body
end
#---------------------
1   (call core.svec)
2   (call core.svec)
3   (call JuliaLowering.eval_closure_type TestMod :#f##0 %₁ %₂)
4   latestworld
5   TestMod.#f##0
6   (new %₅)
7   (= slot₁/f %₆)
8   TestMod.#f##0
9   (call core.svec %₈)
10  (call core.svec)
11  SourceLocation::1:5
12  (call core.svec %₉ %₁₀ %₁₁)
13  --- method core.nothing %₁₂
    slots: [slot₁/#self#(!read)]
    1   TestMod.body
    2   (return %₁)
14  latestworld
15  (return core.nothing)

########################################
# Error: Invalid `let` var with K"::"
let f[]::T = rhs
end
#---------------------
LoweringError:
let f[]::T = rhs
#   └─┘ ── Invalid assignment location in let syntax
end

########################################
# Error: Invalid `let` var
let f[] = rhs
end
#---------------------
LoweringError:
let f[] = rhs
#   └─┘ ── Invalid assignment location in let syntax
end

########################################
# Error: Invalid function def in `let`
let (obj::Callable)() = rhs
end
#---------------------
LoweringError:
let (obj::Callable)() = rhs
#   └───────────────┘ ── Function signature does not define a local function name
end

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
# (y is single-assigned before capture, so no Box needed)
begin
    local y = 2
    function f(x)
        @islocal(a), @islocal(x), @islocal(y)
    end
end
#---------------------
1   (= slot₁/y 2)
2   (method TestMod.f)
3   latestworld
4   TestMod.f
5   (call core.Typeof %₄)
6   (call core.svec %₅ core.Any)
7   (call core.svec)
8   SourceLocation::3:14
9   (call core.svec %₆ %₇ %₈)
10  --- method core.nothing %₉
    slots: [slot₁/#self#(!read) slot₂/x(!read)]
    1   (call core.tuple false true true)
    2   (return %₁)
11  latestworld
12  TestMod.f
13  (return %₁₂)

########################################
# @islocal with global
begin
    global x
    @islocal(x)
end
#---------------------
1   (call core.declare_global TestMod :x false)
2   latestworld
3   (return false)

########################################
# @locals with local and global
begin
    global x
    local y
    @locals
end
#---------------------
1   (newvar slot₁/y)
2   (call core.declare_global TestMod :x false)
3   latestworld
4   (call core.apply_type top.Dict core.Symbol core.Any)
5   (call %₄)
6   (isdefined slot₁/y)
7   (gotoifnot %₆ label₁₀)
8   slot₁/y
9   (call top.setindex! %₅ %₈ :y)
10  (return %₅)

########################################
# @locals with function args (TODO: static parameters)
function f(z)
    @locals
end
#---------------------
1   (method TestMod.f)
2   latestworld
3   TestMod.f
4   (call core.Typeof %₃)
5   (call core.svec %₄ core.Any)
6   (call core.svec)
7   SourceLocation::1:10
8   (call core.svec %₅ %₆ %₇)
9   --- method core.nothing %₈
    slots: [slot₁/#self#(!read) slot₂/z]
    1   (call core.apply_type top.Dict core.Symbol core.Any)
    2   (call %₁)
    3   (gotoifnot true label₅)
    4   (call top.setindex! %₂ slot₂/z :z)
    5   (return %₂)
10  latestworld
11  TestMod.f
12  (return %₁₁)

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
#              ╙ ── destructured argument name `x` conflicts with an existing argument from the same scope
end

########################################
# Error: Static parameter name not unique
function f(::T) where T where T
end
#---------------------
LoweringError:
function f(::T) where T where T
#                     ╙ ── function static parameter name not unique
end

########################################
# Error: static parameter colliding with argument names
function f(x::x) where x
end
#---------------------
LoweringError:
function f(x::x) where x
#                      ╙ ── static parameter name `x` conflicts with an existing argument from the same scope
end

########################################
# Error: duplicate destructure args
function f((x,), (x,))
end
#---------------------
LoweringError:
function f((x,), (x,))
#                 ╙ ── destructured argument name `x` conflicts with an existing local variable from the same scope
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
#          ╙ ── global variable name `x` conflicts with an existing local variable from the same scope
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
#         ╙ ── local variable name `x` conflicts with an existing argument from the same scope
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
#          ╙ ── global variable name `x` conflicts with an existing argument from the same scope
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
#          ╙ ── global variable name `x` conflicts with an existing local variable from the same scope
end

########################################
# Error: Conflicting static parameter and local
function f(::T) where T
    local T
end
#---------------------
LoweringError:
function f(::T) where T
    local T
#         ╙ ── local variable name `T` conflicts with an existing static parameter from the same scope
end

########################################
# Error: Conflicting static parameter and global
function f(::T) where T
    global T
end
#---------------------
LoweringError:
function f(::T) where T
    global T
#          ╙ ── global variable name `T` conflicts with an existing static parameter from the same scope
end

########################################
# Error: Conflicting static parameter and implicit local
function f(::T) where T
    let
        T = rhs
    end
end
#---------------------
LoweringError:
function f(::T) where T
    let
        T = rhs
#       ╙ ── cannot overwrite a static parameter
    end
end

########################################
# Error: Attempt to add methods to a function argument
function f(g)
    function g()
    end
end
#---------------------
LoweringError:
function f(g)
    function g()
#            ╙ ── Cannot add method to a function argument
    end
end

########################################
# Error: Global method definition inside function scope
function f()
    global global_method
    function global_method()
    end
end
#---------------------
LoweringError:
function f()
    global global_method
    function global_method()
#            └───────────┘ ── Global method definition needs to be placed at the top level, or use `eval()`
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
3   (call core.isdefinedglobal TestMod :y false)
4   (return %₃)

########################################
# Global function defined inside let (let over lambda)
let x = 1
    global f(y) = x = y
    global g() = x
end
#---------------------
1   1
2   (= slot₁/x (call core.Box))
3   (call core.setfield! slot₁/x :contents %₁)
4   (call core.declare_global TestMod :f false)
5   latestworld
6   (method TestMod.f)
7   latestworld
8   TestMod.f
9   (call core.Typeof %₈)
10  (call core.svec %₉ core.Any)
11  (call core.svec)
12  SourceLocation::2:12
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- code_info
    slots: [slot₁/#self#(!read) slot₂/y]
    1   slot₂/y
    2   (captured_local 1)
    3   (call core.setfield! %₂ :contents %₁)
    4   (return %₁)
15  (call core.svec slot₁/x)
16  (call JuliaLowering.replace_captured_locals! %₁₄ %₁₅)
17  --- method core.nothing %₁₃ %₁₆
18  latestworld
19  (call core.declare_global TestMod :g false)
20  latestworld
21  (method TestMod.g)
22  latestworld
23  TestMod.g
24  (call core.Typeof %₂₃)
25  (call core.svec %₂₄)
26  (call core.svec)
27  SourceLocation::3:12
28  (call core.svec %₂₅ %₂₆ %₂₇)
29  --- code_info
    slots: [slot₁/#self#(!read) slot₂/x(!read,maybe_undef)]
    1   (captured_local 1)
    2   (call core.isdefined %₁ :contents)
    3   (gotoifnot %₂ label₅)
    4   (goto label₇)
    5   (newvar slot₂/x)
    6   slot₂/x
    7   (call core.getfield %₁ :contents)
    8   (return %₇)
30  (call core.svec slot₁/x)
31  (call JuliaLowering.replace_captured_locals! %₂₉ %₃₀)
32  --- method core.nothing %₂₈ %₃₁
33  latestworld
34  TestMod.g
35  (return %₃₄)

########################################
# Modify assignment operator on closure variable
let x = 1
    global f() = x += 1
end
#---------------------
1   1
2   (= slot₁/x (call core.Box))
3   (call core.setfield! slot₁/x :contents %₁)
4   (call core.declare_global TestMod :f false)
5   latestworld
6   (method TestMod.f)
7   latestworld
8   TestMod.f
9   (call core.Typeof %₈)
10  (call core.svec %₉)
11  (call core.svec)
12  SourceLocation::2:12
13  (call core.svec %₁₀ %₁₁ %₁₂)
14  --- code_info
    slots: [slot₁/#self#(!read) slot₂/x(!read,maybe_undef)]
    1   TestMod.+
    2   (captured_local 1)
    3   (call core.isdefined %₂ :contents)
    4   (gotoifnot %₃ label₆)
    5   (goto label₈)
    6   (newvar slot₂/x)
    7   slot₂/x
    8   (call core.getfield %₂ :contents)
    9   (call %₁ %₈ 1)
    10  (captured_local 1)
    11  (call core.setfield! %₁₀ :contents %₉)
    12  (return %₉)
15  (call core.svec slot₁/x)
16  (call JuliaLowering.replace_captured_locals! %₁₄ %₁₅)
17  --- method core.nothing %₁₃ %₁₆
18  latestworld
19  TestMod.f
20  (return %₁₉)

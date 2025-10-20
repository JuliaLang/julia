module JuxtuposeTest
    macro emit_juxtupose()
        :(10x)
    end
end

#*******************************************************************************
########################################
# Getproperty syntax
x.a
#---------------------
1   TestMod.x
2   (call top.getproperty %₁ :a)
3   (return %₂)

########################################
# Getproperty syntax with a string on right hand side
x."b"
#---------------------
1   TestMod.x
2   (call top.getproperty %₁ "b")
3   (return %₂)

########################################
# Standalone dot syntax
.*
#---------------------
1   TestMod.*
2   (call top.BroadcastFunction %₁)
3   (return %₂)

########################################
# Error: Wrong number of children in `.`
@ast_ [K"." "x"::K"Identifier" "a"::K"Identifier" 3::K"Integer"]
#---------------------
LoweringError:
#= line 1 =# - `.` form requires either one or two children

########################################
# Error: Placeholder value used
_ + 1
#---------------------
LoweringError:
_ + 1
╙ ── all-underscore identifiers are write-only and their values cannot be used in expressions

########################################
# Named tuple
(a=1, b=2)
#---------------------
1   (call core.tuple :a :b)
2   (call core.apply_type core.NamedTuple %₁)
3   (call core.tuple 1 2)
4   (call %₂ %₃)
5   (return %₄)

########################################
# Named tuple with parameters
(; a=1, b=2)
#---------------------
1   (call core.tuple :a :b)
2   (call core.apply_type core.NamedTuple %₁)
3   (call core.tuple 1 2)
4   (call %₂ %₃)
5   (return %₄)

########################################
# Empty named tuple
(;)
#---------------------
1   (call core.NamedTuple)
2   (return %₁)

########################################
# Named tuple with implicit field names
(;x, a.b.c, y._)
#---------------------
1   (call core.tuple :x :c :_)
2   (call core.apply_type core.NamedTuple %₁)
3   TestMod.x
4   TestMod.a
5   (call top.getproperty %₄ :b)
6   (call top.getproperty %₅ :c)
7   TestMod.y
8   (call top.getproperty %₇ :_)
9   (call core.tuple %₃ %₆ %₈)
10  (call %₂ %₉)
11  (return %₁₀)

########################################
# Named tuple with splats
(; a=1, b=2, bs..., c=3, ds...)
#---------------------
1   (call core.tuple :a :b)
2   (call core.apply_type core.NamedTuple %₁)
3   (call core.tuple 1 2)
4   (call %₂ %₃)
5   TestMod.bs
6   (call top.merge %₄ %₅)
7   (call core.tuple :c)
8   (call core.apply_type core.NamedTuple %₇)
9   (call core.tuple 3)
10  (call %₈ %₉)
11  (call top.merge %₆ %₁₀)
12  TestMod.ds
13  (call top.merge %₁₁ %₁₂)
14  (return %₁₃)

########################################
# Named tuple with only splats
(; as..., bs...)
#---------------------
1   (call core.NamedTuple)
2   TestMod.as
3   (call top.merge %₁ %₂)
4   TestMod.bs
5   (call top.merge %₃ %₄)
6   (return %₅)

########################################
# Named tuple with dynamic names
(; a=1, b=2, c=>d)
#---------------------
1   (call core.tuple :a :b)
2   (call core.apply_type core.NamedTuple %₁)
3   (call core.tuple 1 2)
4   (call %₂ %₃)
5   TestMod.c
6   (call core.tuple %₅)
7   (call core.apply_type core.NamedTuple %₆)
8   TestMod.d
9   (call core.tuple %₈)
10  (call %₇ %₉)
11  (call top.merge %₄ %₁₀)
12  (return %₁₁)

########################################
# Error: Named tuple with repeated fields
(; a=1, bs..., c=3, a=2)
#---------------------
LoweringError:
(; a=1, bs..., c=3, a=2)
#                   ╙ ── Repeated named tuple field name

########################################
# Error: Named tuple frankentuple
(a=1; b=2, c=3)
#---------------------
LoweringError:
(a=1; b=2, c=3)
#   └────────┘ ── unexpected semicolon in tuple - use `,` to separate tuple elements

########################################
# Error: Named tuple field dots in rhs
(; a=xs...)
#---------------------
LoweringError:
(; a=xs...)
#    └───┘ ── `...` cannot be used in a value for a named tuple field

########################################
# Error: Named tuple field invalid lhs
(; a[]=1)
#---------------------
LoweringError:
(; a[]=1)
#  └─┘ ── invalid named tuple field name

########################################
# Error: Named tuple element with weird dot syntax
(; a."b")
#---------------------
LoweringError:
(; a."b")
#  └───┘ ── invalid named tuple element

########################################
# Error: Named tuple element without valid name
(; a=1, f())
#---------------------
LoweringError:
(; a=1, f())
#       └─┘ ── Invalid named tuple element

########################################
# Error: Modules not allowed inside blocks
begin
    module C
    end
end
#---------------------
LoweringError:
begin
#   ┌───────
    module C
    end
#─────┘ ── `module` is only allowed at top level
end

########################################
# Error: Modules not allowed in local scope
function f()
    module C
    end
end
#---------------------
LoweringError:
function f()
#   ┌───────
    module C
    end
#─────┘ ── `module` is only allowed at top level
end

########################################
# Basic type assert
x::T
#---------------------
1   TestMod.x
2   TestMod.T
3   (call core.typeassert %₁ %₂)
4   (return %₃)

########################################
# Error: Invalid :: syntax outside function arg list
::T
#---------------------
LoweringError:
::T
└─┘ ── `::` must be written `value::type` outside function argument lists

########################################
# Error: braces vector syntax
{x, y}
#---------------------
LoweringError:
{x, y}
└────┘ ── { } syntax is reserved for future use

########################################
# Error: braces matrix syntax
{x y; y z}
#---------------------
LoweringError:
{x y; y z}
└────────┘ ── { } syntax is reserved for future use

########################################
# Error: Test AST which has no source form and thus must have been constructed
# programmatically (eg, a malformed if)
@ast_ [K"if"]
#---------------------
LoweringError:
#= line 1 =# - expected `numchildren(ex) >= 2`

########################################
# Error: @atomic in wrong position
let
    @atomic x
end
#---------------------
LoweringError:
let
    @atomic x
#   └───────┘ ── unimplemented or unsupported atomic declaration
end

########################################
# GC.@preserve support
GC.@preserve a b begin
    f(a,b)
end
#---------------------
1   TestMod.a
2   TestMod.b
3   (= slot₂/s (gc_preserve_begin %₁ %₂))
4   TestMod.f
5   TestMod.a
6   TestMod.b
7   (= slot₁/r (call %₄ %₅ %₆))
8   (gc_preserve_end slot₂/s)
9   slot₁/r
10  (return %₉)

########################################
# Error: GC.@preserve bad args
GC.@preserve a b g() begin
    body
end
#---------------------
MacroExpansionError while expanding GC.@preserve in module Main.TestMod:
GC.@preserve a b g() begin
#                └─┘ ── Preserved variable must be a symbol
    body
end

########################################
# Juxtaposition
20x
#---------------------
1   TestMod.*
2   TestMod.x
3   (call %₁ 20 %₂)
4   (return %₃)

########################################
# Juxtaposition - check the juxtapose multiply is resolved to `JuxtuposeTest.*` when
# emitted by the macro in the JuxtuposeTest module.
#
# This is consistent with Julia's existing system but it's not entirely clear
# this is good - perhaps we should resolve to Base.* instead? Resolving to the
# module-local version makes it exactly equivalent to `*`. But one might argue
# this is confusing because the symbol `*` appears nowhere in the user's source
# code.
JuxtuposeTest.@emit_juxtupose
#---------------------
1   TestMod.JuxtuposeTest.*
2   TestMod.JuxtuposeTest.x
3   (call %₁ 10 %₂)
4   (return %₃)

########################################
# @cfunction expansion with global generic function as function argument
@cfunction(callable, Int, (Int, Float64))
#---------------------
1   (cfunction Ptr{Nothing} (static_eval TestMod.callable) (static_eval TestMod.Int) (static_eval (call core.svec TestMod.Int TestMod.Float64)) :ccall)
2   (return %₁)

########################################
# @cfunction expansion with closed-over callable argument
@cfunction($close_over, Int, (Int, Float64))
#---------------------
1   TestMod.close_over
2   (cfunction Base.CFunction %₁ (static_eval TestMod.Int) (static_eval (call core.svec TestMod.Int TestMod.Float64)) :ccall)
3   (return %₂)

########################################
# Error: Bad arg types to @cfunction
@cfunction(f, Int, NotATuple)
#---------------------
MacroExpansionError while expanding @cfunction in module Main.TestMod:
@cfunction(f, Int, NotATuple)
#                  └───────┘ ── @cfunction argument types must be a literal tuple

########################################
# Error: Locals used in @cfunction return type
let T=Float64
    @cfunction(f, T, (Float64,))
end
#---------------------
LoweringError:
let T=Float64
    @cfunction(f, T, (Float64,))
#                 ╙ ── cfunction return type cannot reference local variable
end

########################################
# Error: Locals used in @cfunction arg type
let T=Float64
    @cfunction(f, Float64, (Float64,T))
end
#---------------------
LoweringError:
let T=Float64
    @cfunction(f, Float64, (Float64,T))
#                                   ╙ ── cfunction argument type cannot reference local variable
end

########################################
# Basic @ccall lowering
@ccall foo(x::X, y::Y)::R
#---------------------
1   JuliaLowering.Base
2   (call top.getproperty %₁ :cconvert)
3   TestMod.X
4   TestMod.x
5   (= slot₁/arg1 (call %₂ %₃ %₄))
6   JuliaLowering.Base
7   (call top.getproperty %₆ :cconvert)
8   TestMod.Y
9   TestMod.y
10  (= slot₂/arg2 (call %₇ %₈ %₉))
11  JuliaLowering.Base
12  (call top.getproperty %₁₁ :unsafe_convert)
13  TestMod.X
14  slot₁/arg1
15  (call %₁₂ %₁₃ %₁₄)
16  JuliaLowering.Base
17  (call top.getproperty %₁₆ :unsafe_convert)
18  TestMod.Y
19  slot₂/arg2
20  (call %₁₇ %₁₈ %₁₉)
21  slot₁/arg1
22  slot₂/arg2
23  (foreigncall :foo (static_eval TestMod.R) (static_eval (call core.svec TestMod.X TestMod.Y)) 0 :($(QuoteNode((:ccall, 0x0000, false)))) %₁₅ %₂₀ %₂₁ %₂₂)
24  (return %₂₃)

########################################
# @ccall lowering with varargs and gc_safe
@ccall foo(x::X; y::Y)::R gc_safe=true
#---------------------
1   JuliaLowering.Base
2   (call top.getproperty %₁ :cconvert)
3   TestMod.X
4   TestMod.x
5   (= slot₁/arg1 (call %₂ %₃ %₄))
6   JuliaLowering.Base
7   (call top.getproperty %₆ :cconvert)
8   TestMod.Y
9   TestMod.y
10  (= slot₂/arg2 (call %₇ %₈ %₉))
11  JuliaLowering.Base
12  (call top.getproperty %₁₁ :unsafe_convert)
13  TestMod.X
14  slot₁/arg1
15  (call %₁₂ %₁₃ %₁₄)
16  JuliaLowering.Base
17  (call top.getproperty %₁₆ :unsafe_convert)
18  TestMod.Y
19  slot₂/arg2
20  (call %₁₇ %₁₈ %₁₉)
21  slot₁/arg1
22  slot₂/arg2
23  (foreigncall :foo (static_eval TestMod.R) (static_eval (call core.svec TestMod.X TestMod.Y)) 1 :($(QuoteNode((:ccall, 0x0000, true)))) %₁₅ %₂₀ %₂₁ %₂₂)
24  (return %₂₃)

########################################
# Error: No return annotation on @ccall
@ccall strlen("foo"::Cstring)
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall strlen("foo"::Cstring)
#                            └ ── Expected a return type annotation `::SomeType`

########################################
# Error: No argument type on @ccall
@ccall foo("blah"::Cstring, "bad")::Int
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall foo("blah"::Cstring, "bad")::Int
#                           └───┘ ── argument needs a type annotation

########################################
# Error: @ccall varags without one fixed argument
@ccall foo(; x::Int)::Int
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall foo(; x::Int)::Int
#          └──────┘ ── C ABI prohibits varargs without one required argument

########################################
# Error: Multiple varargs blocks
@ccall foo(; x::Int; y::Float64)::Int
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall foo(; x::Int; y::Float64)::Int
#                  └──────────┘ ── Multiple parameter blocks not allowed

########################################
# Error: Bad @ccall option
@ccall foo(x::Int)::Int bad_opt
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall foo(x::Int)::Int bad_opt
#                       └─────┘ ── Bad option to ccall

########################################
# Error: Unknown @ccall option name
@ccall foo(x::Int)::Int bad_opt=true
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall foo(x::Int)::Int bad_opt=true
#                       └─────┘ ── Unknown option name for ccall

########################################
# Error: Unknown option type
@ccall foo(x::Int)::Int gc_safe="hi"
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall foo(x::Int)::Int gc_safe="hi"
#                               └──┘ ── gc_safe must be true or false

########################################
# Error: unary & syntax
&x
#---------------------
LoweringError:
&x
└┘ ── invalid syntax

########################################
# Error: $ outside quote/string
$x
#---------------------
LoweringError:
$x
└┘ ── `$` expression outside string or quote block

########################################
# Error: splat outside call
x...
#---------------------
LoweringError:
x...
└──┘ ── `...` expression outside call

########################################
# `include` should increment world age
include("hi.jl")
#---------------------
1   TestMod.include
2   (call %₁ "hi.jl")
3   latestworld
4   (return %₂)

########################################
# Const function assignment syntax (legacy)
const f(x::Int)::Int = x+1
#---------------------
1   TestMod.f
2   TestMod.x
3   TestMod.Int
4   (call core.typeassert %₂ %₃)
5   (call %₁ %₄)
6   TestMod.Int
7   (call core.typeassert %₅ %₆)
8   (return %₇)

########################################
# Error: Destructuring assignment method definitions (broken, legacy)
f(x)::Int, g() = [1.0, 2.0]
#---------------------
LoweringError:
f(x)::Int, g() = [1.0, 2.0]
└──┘ ── invalid assignment location

########################################
# Error: Destructuring assignment typedef, variable, and function (broken, legacy)
T{U}, (x::Float64, g()) = [Bool, (1, 2)]
#---------------------
LoweringError:
T{U}, (x::Float64, g()) = [Bool, (1, 2)]
#                  └─┘ ── invalid assignment location

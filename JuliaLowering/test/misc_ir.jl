module JuxtuposeTest
    using ..JuliaLowering
    macro emit_juxtupose()
        JuliaLowering.@legacy_quote_to_syntax :(10x)
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
2   (call top.getproperty %₁ (inert "b"))
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
@ast_ [:. "x"::identifier "a"::identifier 3::value]
#---------------------
invalid syntax: unknown form `.` or number of arguments 3
@ :1
└───Expression: (. x a 3)

########################################
# Error: Placeholder value used
_ + 1
#---------------------
all-underscore identifiers are write-only and their values cannot be used in expressions
@ :1
_ + 1
╙

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
Repeated named tuple field name
@ :1
(; a=1, bs..., c=3, a=2)
#                   ╙

########################################
# Error: Named tuple frankentuple
(a=1; b=2, c=3)
#---------------------
cannot mix tuple `(a,b,c)` and named tuple `(;a,b,c)` syntax
@ :1
(a=1; b=2, c=3)
#   └────────┘

########################################
# Error: Named tuple field dots in rhs
(; a=xs...)
#---------------------
unexpected `...`
splatting can only be done into a `call`, `tuple`, `curly`, or array-like expression
@ :1
(; a=xs...)
#    └───┘

########################################
# Error: Named tuple field invalid lhs
(; a[]=1)
#---------------------
expected identifier
@ :1
(; a[]=1)
#  └─┘

########################################
# Error: Named tuple element with weird dot syntax
(; a."b")
#---------------------
expected identifier
@ :1
(; a."b")
#    └─┘

########################################
# Error: Named tuple element without valid name
(; a=1, f())
#---------------------
expected identifier, `=`, or `...` after semicolon
@ :1
(; a=1, f())
#       └─┘

########################################
# Error: Modules not allowed inside blocks
begin
    module C
    end
end
#---------------------
`module` is only allowed at top level
@ :2
begin
#   ┌───────
    module C
    end
#─────┘
end

########################################
# Error: Modules not allowed in local scope
function f()
    module C
    end
end
#---------------------
this syntax is only allowed at top level
@ :2
function f()
#   ┌───────
    module C
    end
#─────┘
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
`::` must be written `value::type` outside function argument lists
@ :1
::T
└─┘

########################################
# Error: braces vector syntax
{x, y}
#---------------------
`{ }` outside of `where` is reserved for future use
@ :1
{x, y}
└────┘

########################################
# Error: braces matrix syntax
{x y; y z}
#---------------------
`{ }` outside of `where` is reserved for future use
@ :1
{x y; y z}
└────────┘

########################################
# Error: Test AST which has no source form and thus must have been constructed
# programmatically (eg, a malformed if)
@ast_ [:if]
#---------------------
expected (if cond body) or (if cond body else)
@ :1
└───Expression: (if)

########################################
# Error: @atomic in wrong position
let
    @atomic x
end
#---------------------
unimplemented or unsupported `atomic` declaration
@ :2
└───Expression: (atomic x)

########################################
# GC.@preserve support
GC.@preserve a b begin
    f(a,b)
end
#---------------------
1   TestMod.a
2   TestMod.b
3   (gc_preserve_begin %₁ %₂)
4   TestMod.f
5   TestMod.a
6   TestMod.b
7   (call %₄ %₅ %₆)
8   (gc_preserve_end %₃)
9   (return %₇)

########################################
# Error: GC.@preserve bad args
GC.@preserve a b g() begin
    body
end
#---------------------
MacroExpansionError while expanding GC.@preserve in module Main.TestMod:
┌─────────────────────────
GC.@preserve a b g() begin
    body
end
#─┘ ── Error expanding macro
Caused by: Preserved variable must be a symbol
@ :1
GC.@preserve a b g() begin
#                └─┘
    body
end

########################################
# @eval without module
@eval $f(x, y)
#---------------------
1   TestMod.f
2   (call core.tuple %₁)
3   (call JuliaLowering.interpolate_syntax (syntaxinert (call (syntaxunquote f) x y)) %₂)
4   (call Base.fill_context %₃ SyntaxContext(#=omitted=#))
5   (= slot₁/eval_result (call Base.JuliaLowering.at_eval_hook TestMod %₄))
6   latestworld
7   slot₁/eval_result
8   (return %₇)

########################################
# @eval with module
@eval mod $f(x, y)
#---------------------
1   TestMod.mod
2   TestMod.f
3   (call core.tuple %₂)
4   (call JuliaLowering.interpolate_syntax (syntaxinert (call (syntaxunquote f) x y)) %₃)
5   (call Base.fill_context %₄ SyntaxContext(#=omitted=#))
6   (= slot₁/eval_result (call Base.JuliaLowering.at_eval_hook %₁ %₅))
7   latestworld
8   slot₁/eval_result
9   (return %₈)

########################################
# Juxtaposition
20x
#---------------------
1   TestMod.*
2   TestMod.x
3   (call %₁ 20 %₂)
4   (return %₃)

########################################
# Juxtaposition - resolve to macro's mod's `JuxtuposeTest.*`
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
└───────────────────────────┘ ── Error expanding macro
Caused by: @cfunction argument types must be a literal tuple
@ :1
@cfunction(f, Int, NotATuple)
#                  └───────┘

########################################
# Error: Locals used in @cfunction return type
let T=Float64
    @cfunction(f, T, (Float64,))
end
#---------------------
cfunction return type cannot reference local variable
@ :2
let T=Float64
    @cfunction(f, T, (Float64,))
#                 ╙
end

########################################
# Error: Locals used in @cfunction arg type
let T=Float64
    @cfunction(f, Float64, (Float64,T))
end
#---------------------
cfunction argument type cannot reference local variable
@ :2
let T=Float64
    @cfunction(f, Float64, (Float64,T))
#                                   ╙
end

########################################
# Basic @ccall lowering
@ccall foo(x::X, y::Y)::R
#---------------------
1   TestMod.X
2   TestMod.Y
3   TestMod.x
4   (call top.cconvert %₁ %₃)
5   TestMod.y
6   (call top.cconvert %₂ %₅)
7   (call top.unsafe_convert %₁ %₄)
8   (call top.unsafe_convert %₂ %₆)
9   (foreigncall (foreignsymbol (tuple (inert foo))) (static_eval TestMod.R) (static_eval (call core.svec TestMod.X TestMod.Y)) 0 (inert (:ccall, 0x0000, false)) %₇ %₈ %₄ %₆)
10  (return %₉)

########################################
# @ccall lowering with gc_safe
@ccall gc_safe=true foo(x::X; y::Y)::R
#---------------------
1   TestMod.X
2   TestMod.Y
3   TestMod.x
4   (call top.cconvert %₁ %₃)
5   TestMod.y
6   (call top.cconvert %₂ %₅)
7   (call top.unsafe_convert %₁ %₄)
8   (call top.unsafe_convert %₂ %₆)
9   (foreigncall (foreignsymbol (tuple (inert foo))) (static_eval TestMod.R) (static_eval (call core.svec TestMod.X TestMod.Y)) 1 (inert (:ccall, 0x0000, true)) %₇ %₈ %₄ %₆)
10  (return %₉)

########################################
# non-macro ccall with vararg in signature, but none provided
ccall(:fcntl, Cint, (RawFD, Cint, Cint...), s, F_GETFL)
#---------------------
1   TestMod.RawFD
2   TestMod.Cint
3   TestMod.s
4   (call top.cconvert %₁ %₃)
5   TestMod.F_GETFL
6   (call top.cconvert %₂ %₅)
7   (call top.unsafe_convert %₁ %₄)
8   (call top.unsafe_convert %₂ %₆)
9   (foreigncall :fcntl (static_eval TestMod.Cint) (static_eval (call core.svec TestMod.RawFD TestMod.Cint)) 2 :ccall %₇ %₈ %₄ %₆)
10  (return %₉)

########################################
# Error: No return annotation on @ccall
@ccall strlen("foo"::Cstring)
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall strlen("foo"::Cstring)
└───────────────────────────┘ ── Error expanding macro
Caused by: expected a return type annotation `::SomeType`
@ :1
@ccall strlen("foo"::Cstring)
#      └────────────────────┘

########################################
# Error: No argument type on @ccall
@ccall foo("blah"::Cstring, "bad")::Int
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall foo("blah"::Cstring, "bad")::Int
└─────────────────────────────────────┘ ── Error expanding macro
Caused by: argument needs a type annotation
@ :1
@ccall foo("blah"::Cstring, "bad")::Int
#                           └───┘

########################################
# Error: @ccall varargs without one fixed argument
@ccall foo(; x::Int)::Int
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall foo(; x::Int)::Int
└───────────────────────┘ ── Error expanding macro
Caused by: C ABI prohibits varargs without one required argument
@ :1
@ccall foo(; x::Int)::Int
#          └──────┘

########################################
# Error: Multiple varargs blocks
@ccall foo(; x::Int; y::Float64)::Int
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall foo(; x::Int; y::Float64)::Int
└───────────────────────────────────┘ ── Error expanding macro
Caused by: C ABI prohibits varargs without one required argument
@ :1
@ccall foo(; x::Int; y::Float64)::Int
#          └──────┘

########################################
# Error: Bad @ccall first arg
@ccall $(:(foo))(1::Cint)::Cint
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall $(:(foo))(1::Cint)::Cint
└─────────────────────────────┘ ── Error expanding macro
Caused by: interpolated value should be a variable or expression, not a literal name or tuple
@ :1
@ccall $(:(foo))(1::Cint)::Cint
#      └───────┘

########################################
# Error: Bad @ccall option
@ccall bad_opt foo(x::Int)::Int
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall bad_opt foo(x::Int)::Int
└─────────────────────────────┘ ── Error expanding macro
Caused by: bad option to ccall
@ :1
@ccall bad_opt foo(x::Int)::Int
#      └─────┘

########################################
# Error: Unknown @ccall option name
@ccall bad_opt=true foo(x::Int)::Int
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall bad_opt=true foo(x::Int)::Int
└──────────────────────────────────┘ ── Error expanding macro
Caused by: unknown option name for ccall
@ :1
@ccall bad_opt=true foo(x::Int)::Int
#      └─────┘

########################################
# Error: Unknown option type
@ccall gc_safe="hi" foo(x::Int)::Int
#---------------------
MacroExpansionError while expanding @ccall in module Main.TestMod:
@ccall gc_safe="hi" foo(x::Int)::Int
└──────────────────────────────────┘ ── Error expanding macro
Caused by: gc_safe must be true or false
@ :1
@ccall gc_safe="hi" foo(x::Int)::Int
#              └──┘

########################################
# Error: unary & syntax
&x
#---------------------
invalid syntax: unknown form `&` or number of arguments 1
@ :1
&x
└┘

########################################
# Error: $ outside quote/string
$x
#---------------------
`$` expression outside string or quote
@ :1
$x
└┘

########################################
# Error: splat outside call
x...
#---------------------
unexpected `...`
splatting can only be done into a `call`, `tuple`, `curly`, or array-like expression
@ :1
x...
└──┘

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
1   (call core.define_method TestMod :f)
2   latestworld
3   TestMod.f
4   (call core.TypeEqOf %₃)
5   TestMod.Int
6   (call core.svec %₄ %₅)
7   (call core.svec)
8   SourceLocation:1
9   (call core.svec %₆ %₇ %₈)
10  (call core.define_method TestMod TestMod.f %₉
    --- code_info
    slots: [slot₁/#self#(!read) slot₂/x slot₃/tmp(!read)]
    1   TestMod.Int
    2   TestMod.+
    3   (= slot₃/tmp (call %₂ slot₂/x 1))
    4   (call core.isa slot₃/tmp %₁)
    5   (gotoifnot %₄ label₇)
    6   (goto label₉)
    7   (call top.convert %₁ slot₃/tmp)
    8   (= slot₃/tmp (call core.typeassert %₇ %₁))
    9   slot₃/tmp
    10  (return %₉)
11  latestworld
12  TestMod.f
13  (return %₁₂)

########################################
# Error: Destructuring assignment method definitions (broken, legacy)
f(x)::Int, g() = [1.0, 2.0]
#---------------------
invalid assignment location
@ :1
f(x)::Int, g() = [1.0, 2.0]
└──┘

########################################
# Error: Destructuring assignment typedef, variable, and function (broken, legacy)
T{U}, (x::Float64, g()) = [Bool, (1, 2)]
#---------------------
invalid assignment location
@ :1
T{U}, (x::Float64, g()) = [Bool, (1, 2)]
#                  └─┘

########################################
# aliasscope form: should be passed through unless implementation changes
Base.Experimental.@aliasscope 1
#---------------------
1   (aliasscope)
2   (= slot₁/aliasscope_result 1)
3   (popaliasscope)
4   slot₁/aliasscope_result
5   (return %₄)

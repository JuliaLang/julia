# This file is a part of Julia. License is MIT: https://julialang.org/license

# test meta-expressions that annotate blocks of code

const inlining_on = Base.JLOptions().can_inline != 0

function f(x)
    y = x+5
    z = y*y
    q = z/y
    m = q-3
end

@inline function f_inlined(x)
    y = x+5
    z = y*y
    q = z/y
    m = q-3
end

g(x) = f(2x)
g_inlined(x) = f_inlined(2x)

@test g(3) == g_inlined(3)
@test f(3) == f_inlined(3)

f() = backtrace()
@inline g_inlined() = f()
@noinline g_noinlined() = f()
h_inlined() = g_inlined()
h_noinlined() = g_noinlined()

function foundfunc(bt, funcname)
    for b in bt
        for lkup in StackTraces.lookup(b)
            if lkup.func == funcname
                return true
            end
        end
    end
    false
end
@test foundfunc(h_inlined(), :g_inlined)
@test foundfunc(h_noinlined(), :g_noinlined)

using Base: pushmeta!, popmeta!

macro attach(val, ex)
    esc(_attach(val, ex))
end

_attach(val, ex) = pushmeta!(ex, :test, val)

@attach 42 function dummy()
    false
end

asts = code_lowered(dummy, Tuple{})
@test length(asts) == 1
ast = asts[1]

body = Expr(:block)
body.args = ast.code

@test popmeta!(body, :test) == (true, [42])
@test popmeta!(body, :nonexistent) == (false, [])

# Simple popmeta!() tests
ex1 = quote
    $(Expr(:meta, :foo))
    x*x+1
end
@test popmeta!(ex1, :foo)[1]
@test !popmeta!(ex1, :foo)[1]
@test !popmeta!(ex1, :bar)[1]
@test !(popmeta!(:(x*x+1), :foo)[1])

# Find and pop meta information from general ast locations
multi_meta = quote
    $(Expr(:meta, :foo1))
    y = x
    $(Expr(:meta, :foo2, :foo3))
    begin
        $(Expr(:meta, :foo4, Expr(:foo5, 1, 2)))
    end
    x*x+1
end
@test popmeta!(deepcopy(multi_meta), :foo1) == (true, [])
@test popmeta!(deepcopy(multi_meta), :foo2) == (true, [])
@test popmeta!(deepcopy(multi_meta), :foo3) == (true, [])
@test popmeta!(deepcopy(multi_meta), :foo4) == (true, [])
@test popmeta!(deepcopy(multi_meta), :foo5) == (true, [1,2])
@test popmeta!(deepcopy(multi_meta), :bar)  == (false, [])

# Test that popmeta!() removes meta blocks entirely when they become empty.
for m in [:foo1, :foo2, :foo3, :foo4, :foo5]
    @test popmeta!(multi_meta, m)[1]
end
@test Base.findmeta(multi_meta.args)[1] == 0

# Test that pushmeta! can push across other macros,
# in the case multiple pushmeta!-based macros are combined

@attach 40 @attach 41 @attach 42 dummy_multi() = return nothing

asts = code_lowered(dummy_multi, Tuple{})
@test length(asts) == 1
ast = asts[1]

body = Expr(:block)
body.args = ast.code

@test popmeta!(body, :test) == (true, [40])
@test popmeta!(body, :test) == (true, [41])
@test popmeta!(body, :test) == (true, [42])
@test popmeta!(body, :nonexistent) == (false, [])

# tests to fully cover functions in base/meta.jl
using Base.Meta

@test isexpr(:(1+1),Set([:call]))
@test isexpr(:(1+1),Vector([:call]))
@test isexpr(:(1+1),(:call,))
@test isexpr(1,:call)==false
@test isexpr(:(1+1),:call,3)
ioB = IOBuffer()
show_sexpr(ioB,:(1+1))

show_sexpr(ioB,QuoteNode(1),1)

# test base/expr.jl
baremodule B
    eval = 0
    x = 1
    module M; x = 2; end
    import Base
    @Base.eval x = 3
    @Base.eval M x = 4
end
@test B.x == 3
@test B.M.x == 4

# specialization annotations

function _nospec_some_args(@nospecialize(x), y, @nospecialize z::Int)
end
@test first(methods(_nospec_some_args)).nospecialize == 5
@test first(methods(_nospec_some_args)).sig == Tuple{typeof(_nospec_some_args),Any,Any,Int}
function _nospec_some_args2(x, y, z)
    @nospecialize x y
    return 0
end
@test first(methods(_nospec_some_args2)).nospecialize == 3
function _nospec_with_default(@nospecialize x = 1)
    2x
end
@test collect(methods(_nospec_with_default))[2].nospecialize == 1
@test _nospec_with_default() == 2
@test _nospec_with_default(10) == 20


let oldout = stdout
    ex = Meta.@lower @dump x + y
    local rdout, wrout, out
    try
        rdout, wrout = redirect_stdout()
        out = @async read(rdout, String)

        @test eval(ex) === nothing

        redirect_stdout(oldout)
        close(wrout)

        @test fetch(out) == """
            Expr
              head: Symbol call
              args: Array{Any}((3,))
                1: Symbol +
                2: Symbol x
                3: Symbol y
            """
    finally
        redirect_stdout(oldout)
    end
end

macro is_dollar_expr(ex)
    return Meta.isexpr(ex, :$)
end

module TestExpandModule
macro is_in_def_module()
    return __module__ === @__MODULE__
end
end

let a = 1
    @test @is_dollar_expr $a
    @test !TestExpandModule.@is_in_def_module
    @test @eval TestExpandModule @is_in_def_module

    @test Meta.lower(@__MODULE__, :($a)) === 1
    @test !Meta.lower(@__MODULE__, :(@is_dollar_expr $a))
    @test Meta.@lower @is_dollar_expr $a
    @test Meta.@lower @__MODULE__() @is_dollar_expr $a
    @test !Meta.@lower TestExpandModule.@is_in_def_module
    @test Meta.@lower TestExpandModule @is_in_def_module

    @test macroexpand(@__MODULE__, :($a)) === 1
    @test !macroexpand(@__MODULE__, :(@is_dollar_expr $a))
    @test @macroexpand @is_dollar_expr $a
end

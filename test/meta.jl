# This file is a part of Julia. License is MIT: http://julialang.org/license

# test meta-expressions that annotate blocks of code

module MetaTest

using Base.Test

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
        lkup = StackTraces.lookup(b)
        if lkup.func == funcname
            return true
        end
    end
    false
end
@test !foundfunc(h_inlined(), :g_inlined)
@test foundfunc(h_noinlined(), :g_noinlined)

using Base.pushmeta!, Base.popmeta!

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
body.args = Base.uncompressed_ast(ast)

@test popmeta!(body, :test) == (true, [42])
@test popmeta!(body, :nonexistent) == (false, [])

metaex = Expr(:meta, :foo)
ex1 = quote
    $metaex
    x*x+1
end
metaex = Expr(:meta, :foo)
ex2 = quote
    y = x
    $metaex
    y*y+1
end
metaex = Expr(:block, Expr(:meta, :foo))
ex3 = quote
    y = x
    $metaex
    x*x+1
end

@test popmeta!(ex1, :foo)[1]
@test popmeta!(ex2, :foo)[1]
@test popmeta!(ex3, :foo)[1]
@test !(popmeta!(:(x*x+1), :foo)[1])

end


# tests to fully cover functions in base/meta.jl
module MetaJLtest

using Base.Test
using Base.Meta

@test isexpr(:(1+1),Set([:call]))
@test isexpr(:(1+1),Vector([:call]))
@test isexpr(1,:call)==false
@test isexpr(:(1+1),:call,3)
ioB = IOBuffer();
show_sexpr(ioB,:(1+1))

show_sexpr(ioB,QuoteNode(1),1)

end

# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "meta" begin
module MetaTest

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

end


# tests to fully cover functions in base/meta.jl
module MetaJLtest

using Base.Meta

@test isexpr(:(1+1),Set([:call]))
@test isexpr(:(1+1),Vector([:call]))
@test isexpr(1,:call)==false
@test isexpr(:(1+1),:call,3)
ioB = IOBuffer();
show_sexpr(ioB,:(1+1))

show_sexpr(ioB,QuoteNode(1),1)

end
end

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
        lkup = Profile.lookup(b)
        if lkup.func == funcname
            return true
        end
    end
    false
end
@test !foundfunc(h_inlined(), "g_inlined")
@test foundfunc(h_noinlined(), "g_noinlined")

using Base.pushmeta!, Base.popmeta!

macro attach(val, ex)
    esc(_attach(val, ex))
end

_attach(val, ex) = pushmeta!(ex, :test, val)

@attach 42 function dummy()
    false
end

asts = code_lowered(dummy, ())
@assert length(asts) == 1
ast = asts[1]

body = Expr(:block)
body.args = ast.args[3].args

@test popmeta!(body, :test) == (true, [42])
@test popmeta!(body, :nonexistent) == (false, [])

end

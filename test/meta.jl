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

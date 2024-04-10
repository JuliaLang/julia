# Just some hacking

using JuliaSyntax
using JuliaLowering

using JuliaLowering: SyntaxGraph, SyntaxTree, ensure_attributes!, newnode!, setchildren!, haschildren, children, child, setattr!, sourceref, makenode

function wrapscope(ex, scope_type)
    makenode(ex, ex, K"block", ex; scope_type=scope_type)
end

function softscope_test(ex)
    wrapscope(wrapscope(ex, :neutral), :soft)
end

#-------------------------------------------------------------------------------

# src = """
# let
#     local x, (y = 2), (w::T = ww), q::S
# end
# """

# src = """
# function foo(x::f(T), y::w(let ; S end))
#     "a \$("b \$("c")")"
# end
# """

src = """
let
    y = 1
    x = 2
    let x = 3
        y = x + 1
    end
    (x, y)
end
"""

src = """
begin
    function f(x)
        y = x + 1
        "hello world", x, y
    end

    f(1)
end
"""

# src = """
#     x = 1
# """

# src = """
#     x + y
# """

ex = parsestmt(SyntaxTree, src, filename="foo.jl")
# t = softscope_test(t)
@info "Input code" ex

in_mod = Main
ctx, ex_desugar = JuliaLowering.expand_forms(ex)
@info "Desugared" ex_desugar

ctx2, ex_scoped = JuliaLowering.resolve_scopes!(ctx, in_mod, ex_desugar)
@info "Resolved scopes" ex_scoped

ctx3, ex_compiled = JuliaLowering.linearize_ir(ctx2, ex_scoped)
@info "Linear IR" ex_compiled

ex_expr = JuliaLowering.to_expr(in_mod, ctx2.var_info, ex_compiled)
@info "CodeInfo" ex_expr
x = 100
y = 200
@info "Eval" Base.eval(in_mod, ex_expr)

# Syntax tree ideas: Want following to work?
# This can be fully inferrable!
#
# t2[3].bindings[1].lhs.string
# t2[3].body[1].signature


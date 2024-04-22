# Just some hacking

using JuliaSyntax
using JuliaLowering

using JuliaLowering: SyntaxGraph, SyntaxTree, ensure_attributes!, newnode!, setchildren!, haschildren, children, child, setattr!, sourceref, makenode

function wrapscope(ex, scope_type)
    makenode(ex, ex, K"scope_block", ex; scope_type=scope_type)
end

function softscope_test(ex)
    wrapscope(wrapscope(ex, :neutral), :soft)
end

#-------------------------------------------------------------------------------
# Demos of the prototype

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
    y = 0
    x = 1
    let x = x + 1
        y = x
    end
    (x, y)
end
"""

# src = """
# begin
#     function f(x)
#         y = x + 1
#         "hello world", x, y
#     end
#
#     f(1)
# end
# """

# src = """
#     x = 1
# """

# src = """
#     x + y
# """

src = """
module A
    function f(x)::Int
        x + 1
    end

    b = f(2)
end
"""

src = """
function f()
end
"""

src = """
# import A.B: C.c as d, E.e as f
# import JuliaLowering
using JuliaLowering
"""

src = """
module A
    z = 1 + 1
end
"""

ex = parsestmt(SyntaxTree, src, filename="foo.jl")
# t = softscope_test(t)
@info "Input code" ex

in_mod = Main
ctx, ex_desugar = JuliaLowering.expand_forms(in_mod, ex)
@info "Desugared" ex_desugar

ctx2, ex_scoped = JuliaLowering.resolve_scopes!(ctx, ex_desugar)
@info "Resolved scopes" ex_scoped

ctx3, ex_compiled = JuliaLowering.linearize_ir(ctx2, ex_scoped)
@info "Linear IR" ex_compiled

ex_expr = JuliaLowering.to_lowered_expr(in_mod, ctx2.var_info, ex_compiled)
@info "CodeInfo" ex_expr

x = 100
y = 200
eval_result = Base.eval(in_mod, ex_expr)
@info "Eval" eval_result

# Syntax tree ideas: Want following to work?
# This can be fully inferrable!
#
# t2[3].bindings[1].lhs.string
# t2[3].body[1].signature


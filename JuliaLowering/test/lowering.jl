# Just some hacking

using JuliaSyntax
using JuliaLowering

using JuliaLowering: SyntaxGraph, SyntaxTree, ensure_attributes!, newnode!, setchildren!, haschildren, children, child, setattr!, sourceref

src = """
let
    y = 1
    x = 2
    let x = sin(x)
        y = x
    end
    (x, y)
end
"""

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

# src = """
# let
#     function f() Int end
#     function foo(y::f(a))
#         y
#     end
# end
# """


# src = """
#     x + y
# """

t = parsestmt(SyntaxNode, src, filename="foo.jl")

ctx = JuliaLowering.DesugaringContext()

t2 = SyntaxTree(ctx.graph, t)

t3 = JuliaLowering.expand_forms(ctx, t2)

ctx2 = JuliaLowering.ScopeResolutionContext(ctx)

t4 = JuliaLowering.resolve_scopes!(ctx2, t3)

@info "Resolved scopes" t4

code = JuliaLowering.compile_toplevel(ctx2, Main, t4)

@info "Code" code


# flisp parts to do
# let
# desugar/let => 76
# desugar/func => ~100 (partial)
# desugar/call => 70
# handle-scopes => 195
# handle-scopes/scope-block => 99
# handle-scopes/locals => 16
# linear-ir => 250 (partial, approximate)
# linear-ir/func => 22


# Syntax tree ideas: Want following to work?
# This can be fully inferrable!
#
# t2[3].bindings[1].lhs.string
# t2[3].body[1].signature


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

t = parsestmt(SyntaxNode, src, filename="foo.jl")

ctx = JuliaLowering.DesugaringContext()
t2 = SyntaxTree(ctx.graph, t)
# t2 = softscope_test(t2)
@info "Input code" t2

t3 = JuliaLowering.expand_forms(ctx, t2)
@info "Desugared" t3

in_mod = Main # Module(:Foo)
ctx2 = JuliaLowering.ScopeResolutionContext(ctx, in_mod)
t4 = JuliaLowering.resolve_scopes!(ctx2, t3)
@info "Resolved scopes" t4

t5 = JuliaLowering.compile_lambda(ctx2, t4)

@info "Linear IR" t5

t6 = JuliaLowering.to_expr(in_mod, ctx2.var_info, t5)

@info "CodeInfo" t6

x = 100
y = 200
@info "Eval" Base.eval(in_mod, t6)

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


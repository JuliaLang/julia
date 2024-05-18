# Just some hacking

using JuliaSyntax
using JuliaLowering

using JuliaLowering: SyntaxGraph, SyntaxTree, ensure_attributes!, newnode!, setchildren!, haschildren, children, child, setattr!, sourceref, makenode

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

src = """
begin
    function f(x)
        nothing
    end

    f(1)
end
"""

# src = """
#     x + y
# """

# src = """
# module A
#     function f(x)::Int
#         x + 1
#     end
#
#     b = f(2)
# end
# """

# src = """
# function f()
# end
# """
#
# src = """
# # import A.B: C.c as d, E.e as f
# # import JuliaLowering
# using JuliaLowering
# """
#
# src = """
# module A
#     z = 1 + 1
# end
# """
#
# src = """
# begin
#     x = 10
#     y = :(g(z))
#     quote
#         f(\$(x+1), \$y)
#     end
# end
# """

JuliaLowering.include_string(Main, """
module M
    using JuliaLowering: JuliaLowering, @ast, @chk
    using JuliaSyntax

    # Introspection
    macro __MODULE__()
        __context__.mod
    end

    macro __FILE__()
        JuliaLowering.filename(__context__.macroname)
    end

    macro __LINE__()
        JuliaLowering.source_location(__context__.macroname)[1]
    end

    someglobal = "global in module M"

    # Macro with local variables
    macro foo(ex)
        :(begin
            x = "`x` from @foo"
            (x, someglobal, \$ex)
        end)
    end

    a_global = nothing

    macro set_a_global(val)
        :(begin
            global a_global = \$val
        end)
    end
end
""")

Base.eval(M, quote
    # Recursive macro call
    function var"@recursive"(__context__, N)
        @chk kind(N) == K"Integer"
        Nval = N.value::Int
        if Nval < 1
            return N
        end
        # quote
        #     x = $N
        #     (@recursive $(Nval-1), x)
        # end
        @ast __context__ (@HERE) [K"block"
            [K"="(@HERE)
                "x"::K"Identifier"(@HERE)
                N
            ]
            [K"tuple"(@HERE)
                "x"::K"Identifier"(@HERE)
                [K"macrocall"(@HERE)
                    "@recursive"::K"Identifier"
                    (Nval-1)::K"Integer"
                ]
            ]
        ]
    end
end)

function wrapscope(ex, scope_type)
    makenode(ex, ex, K"scope_block", ex; scope_type=scope_type)
end

function softscope_test(ex)
    g = JuliaLowering.ensure_attributes(ex.graph, scope_type=Symbol)
    wrapscope(wrapscope(JuliaLowering.reparent(g, ex), :neutral), :soft)
end

src = """
begin
    x = 42
    M.@foo x
end
"""

src = """
begin
    M.@set_a_global 42
    M.a_global
end
"""

# src = """
# M.@recursive 3
# """

# src = """
# macro mmm(a; b=2)
# end
# macro A.b(ex)
# end
# """

# TODO:
# "hygiene bending" / (being unhygenic, or bending hygiene to the context of a
# macro argument on purpose)
# * bend to macro name to get to parent layer?
# * already needed in `#self#` argument

ex = parsestmt(SyntaxTree, src, filename="foo.jl")
#ex = softscope_test(ex)
@info "Input code" ex

in_mod = Main
ctx1, ex_macroexpand = JuliaLowering.expand_forms_1(in_mod, ex)
@info "Macro expanded" ex_macroexpand

ctx2, ex_desugar = JuliaLowering.expand_forms_2(ctx1, ex_macroexpand)
@info "Desugared" ex_desugar

ctx3, ex_scoped = JuliaLowering.resolve_scopes!(ctx2, ex_desugar)
@info "Resolved scopes" ex_scoped

ctx4, ex_compiled = JuliaLowering.linearize_ir(ctx3, ex_scoped)
@info "Linear IR" ex_compiled

ex_expr = JuliaLowering.to_lowered_expr(in_mod, ctx4.var_info, ex_compiled)
@info "CodeInfo" ex_expr

x = 1
eval_result = Base.eval(in_mod, ex_expr)
@info "Eval" eval_result


# Just some hacking

using JuliaSyntax
using JuliaLowering

using JuliaLowering: SyntaxGraph, SyntaxTree, ensure_attributes!, ensure_attributes, newnode!, setchildren!, haschildren, children, child, setattr!, sourceref, makenode, sourcetext

using JuliaSyntaxFormatter
using JuliaSyntaxFormatter: FormatContext

# Extract variable kind for highlighting purposes
function var_kind(e)
    id = get(e, :var_id, nothing)
    if isnothing(id)
        return nothing
    end
    info = get(ctx3.var_info, id, nothing)
    if isnothing(info)
        return nothing
    end
    return info.kind
end

function formatsrc(ex; color_by=nothing, kws...)
    format_token_style = if isnothing(color_by)
        e->nothing
    elseif color_by isa Symbol
        e->get(e, color_by, nothing)
    else
        color_by
    end
    Text(JuliaSyntaxFormatter.format(ex; format_token_style, kws...))
end

function annotate_scopes(mod, ex)
    ex = ensure_attributes(ex, var_id=Int)
    ctx1, ex_macroexpand = JuliaLowering.expand_forms_1(mod, ex)
    ctx2, ex_desugar = JuliaLowering.expand_forms_2(ctx1, ex_macroexpand)
    ctx3, ex_scoped = JuliaLowering.resolve_scopes!(ctx2, ex_desugar)
    ex
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

JuliaLowering.include(Main, "demo_include.jl")

Base.eval(M, quote
    function var"@inert"(__context__::JuliaLowering.MacroContext, ex)
        @chk kind(ex) == K"quote"
        @ast __context__ ex [K"inert" ex]
    end

    # Recursive macro call
    function var"@recursive"(__context__::JuliaLowering.MacroContext, N)
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

JuliaLowering.include_string(M, """
xx = "xx in M"
macro test_inert_quote()
    println(xx)
    @inert quote
        (\$xx, xx)
    end
end
""")

function wrapscope(ex, scope_type)
    makenode(ex, ex, K"scope_block", ex; scope_type=scope_type)
end

function softscope_test(ex)
    g = ensure_attributes(ex.graph, scope_type=Symbol)
    wrapscope(wrapscope(JuliaLowering.reparent(g, ex), :neutral), :soft)
end

# src = """
# M.@test_inert_quote()
# """

# src = """
# macro mmm(a; b=2)
# end
# macro A.b(ex)
# end
# """

src = """
M.@set_global_in_parent "bent hygiene!"
"""

# src = """
# begin
# M.@__LINE__
# end
# """

# src = """@foo z"""

src = """
begin
    x = 42
    M.@foo x
end
"""

# src = """
# M.@recursive 3
# """

# src = """
# begin
#     M.@set_a_global 1000
#     M.a_global
# end
# """

# src = """
# M.@set_global_in_parent "bent hygiene!"
# """

src = """
begin
   x = 10
   y = 20
   let x = y + x
       z = "some string \$x \$y"

       function f(y)
           a = M.@foo z
           "\$z \$y \$a \$x"
       end
       print(x)
   end
   print(x)
end
"""

# src = """
# begin
#     x = -1
#     M.@baz x
# end
# """

ex = parsestmt(SyntaxTree, src, filename="foo.jl")
ex = ensure_attributes(ex, var_id=Int)
#ex = softscope_test(ex)
@info "Input code" ex

in_mod = Main
ctx1, ex_macroexpand = JuliaLowering.expand_forms_1(in_mod, ex)
@info "Macro expanded" formatsrc(ex_macroexpand, color_by=:scope_layer)

ctx2, ex_desugar = JuliaLowering.expand_forms_2(ctx1, ex_macroexpand)
@info "Desugared" formatsrc(ex_desugar, color_by=:scope_layer)

ctx3, ex_scoped = JuliaLowering.resolve_scopes!(ctx2, ex_desugar)
@info "Resolved scopes" formatsrc(ex_scoped, color_by=:var_id)

ctx4, ex_compiled = JuliaLowering.linearize_ir(ctx3, ex_scoped)
@info "Linear IR" formatsrc(ex_compiled, color_by=:var_id)

# ex_expr = JuliaLowering.to_lowered_expr(in_mod, ctx4.var_info, ex_compiled)
# @info "CodeInfo" ex_expr
#
# eval_result = Base.eval(in_mod, ex_expr)
# @info "Eval" eval_result
#

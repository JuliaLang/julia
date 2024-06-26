# Just some hacking

using JuliaSyntax
using JuliaLowering

using JuliaLowering: SyntaxGraph, SyntaxTree, ensure_attributes!, ensure_attributes, newnode!, setchildren!, haschildren, children, child, setattr!, sourceref, makenode, sourcetext, showprov

using JuliaSyntaxFormatter

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

function formatsrc(ex; kws...)
    Text(JuliaSyntaxFormatter.formatsrc(ex; kws...))
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

src = raw"""
begin
    x = 10
    y = :(g(z))
    quote
        f($(x+1), $y)
    end
end
"""

JuliaLowering.include(Main, "demo_include.jl")

Base.eval(M, quote
    function var"@inert"(__context__::JuliaLowering.MacroContext, ex)
        @chk kind(ex) == JuliaSyntax.K"quote"
        @ast __context__ ex [JuliaSyntax.K"inert" ex]
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
    g = ensure_attributes(ex._graph, scope_type=Symbol)
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

# src = """
# M.@set_global_in_parent "bent hygiene!"
# """

# src = """
# begin
# M.@__LINE__
# end
# """

# src = """@foo z"""

src = """
M.@recursive 3
"""

# src = """
# begin
#     M.@set_a_global 1000
#     M.a_global
# end
# """

# src = """
# M.@set_global_in_parent "bent hygiene!"
# """

# src = """
# begin
#    x = 10
#    y = 20
#    let x = y + x
#        z = "some string \$x \$y"
#
#        function f(y)
#            a = M.@foo z
#            "\$z \$y \$a \$x"
#        end
#        print(x)
#    end
#    print(x)
# end
# """

# src = """
# begin
#     x = -1
#     M.@baz x
# end
# """

# src = """
#     _ = -1
# """

# src = """
# M.@make_module
# """

# src = """
# M.@nested_return_a_value
# """

# src = """
# function f(y)
#     x = 42 + y
#     M.@foo error(x)
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

#src = """M.@outer"""

src = """
begin
    local a, b, c
    if a
        b
    else
        c
    end
end
"""

src = """
begin
    local i = 0
    while i < 10
        i = i + 1
        if isodd(i)
            continue
        end
        println(i)
    end
end
"""

ex = parsestmt(SyntaxTree, src, filename="foo.jl")
ex = ensure_attributes(ex, var_id=Int)
#ex = softscope_test(ex)
@info "Input code" formatsrc(ex)

in_mod = Main
ctx1, ex_macroexpand = JuliaLowering.expand_forms_1(in_mod, ex)
@info "Macro expanded" ex_macroexpand formatsrc(ex_macroexpand, color_by=:scope_layer)
#@info "Macro expanded" formatsrc(ex_macroexpand, color_by=e->JuliaLowering.flattened_provenance(e)[1:end-1])

ctx2, ex_desugar = JuliaLowering.expand_forms_2(ctx1, ex_macroexpand)
@info "Desugared" ex_desugar formatsrc(ex_desugar, color_by=:scope_layer)

ctx3, ex_scoped = JuliaLowering.resolve_scopes!(ctx2, ex_desugar)
@info "Resolved scopes" ex_scoped formatsrc(ex_scoped, color_by=:var_id)

ctx4, ex_compiled = JuliaLowering.linearize_ir(ctx3, ex_scoped)
@info "Linear IR" ex_compiled formatsrc(ex_compiled, color_by=:var_id)

ex_expr = JuliaLowering.to_lowered_expr(in_mod, ctx4.var_info, ex_compiled)
@info "CodeInfo" ex_expr

eval_result = Base.eval(in_mod, ex_expr)
@info "Eval" eval_result


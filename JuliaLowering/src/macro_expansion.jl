# Lowering pass 1: Macro expansion, simple normalizations and quote expansion

"""
A `ScopeLayer` is a mechanism for automatic hygienic macros; every identifier
is assigned to a particular layer and can only match against bindings which are
themselves part of that layer.

Normal code contains a single scope layer, whereas each macro expansion
generates a new layer.
"""
struct ScopeLayer
    id::LayerId
    mod::Module
    is_macro_expansion::Bool
end

struct MacroExpansionContext{GraphType} <: AbstractLoweringContext
    graph::GraphType
    next_var_id::Ref{VarId}
    scope_layers::Vector{ScopeLayer}
    current_layer::ScopeLayer
end

function MacroExpansionContext(ctx, mod::Module)
    graph = ensure_attributes(syntax_graph(ctx),
                              var_id=VarId,
                              scope_layer=LayerId)
    layers = Vector{ScopeLayer}()
    MacroExpansionContext(graph, Ref{VarId}(1), layers, new_scope_layer(layers, mod, false))
end

function new_scope_layer(layers, mod::Module, is_macro_expansion)
    layer = ScopeLayer(length(layers)+1, mod, is_macro_expansion)
    push!(layers, layer)
    return layer
end

#--------------------------------------------------
function _contains_active_interp(ex, depth)
    k = kind(ex)
    if k == K"$" && depth == 0
        return true
    end
    inner_depth = k == K"quote" ? depth + 1 :
                  k == K"$"     ? depth - 1 :
                  depth
    return any(_contains_active_interp(c, inner_depth) for c in children(ex))
end

function expand_interpolation(ctx, interp_ctx_var, ex)
    @ast ctx ex [K"call"
        interpolate_value::K"Value"
        interp_ctx_var
        [K"inert" ex]
        expand_forms_1(ctx, ex)
    ]
end

# TODO: Rewrite this recursive expansion to happen partially at
# runtime rather than entirely in lowering? That is, we'd expand to
# 
# interpolate_expression(ex, val1, val2, ...)
#
# where `ex` is an inert version of the quoted block and `val1, val2, ...` are
# the expressions within `$` escaping.
#
# Advantages:
# * Much more compact lowered AST
# * Clearer lowered AST - `ex` only appears once, rather than many times
# * Smaller runtime API surface area
#
# Disadvantages:
# * Recursive traversal and processing of quote depth appears both here and in
#   the runtime. But can unify the traversal code?
#
# Beware tricky expansion gotchas like getting the meaning of the following correct
#
# x = 42
# macro m()
#     simplequote
#         println(x)
#         quote
#             $x, x
#         end
#     end
# end
#
function expand_quote_content(ctx, interp_ctx_var, ex, depth)
    if !_contains_active_interp(ex, depth)
        return @ast ctx ex [K"call"
            copy_ast::K"Value"
            interp_ctx_var
            ex::K"Value"
        ]
    end

    # We have an interpolation deeper in the tree somewhere - expand to an
    # expression 
    inner_depth = kind(ex) == K"quote" ? depth + 1 :
                  kind(ex) == K"$"     ? depth - 1 :
                  depth
    expanded_children = SyntaxList(ctx)
    for e in children(ex)
        if kind(e) == K"$" && inner_depth == 0
            for x in children(e)
                push!(expanded_children, expand_interpolation(ctx, interp_ctx_var, x))
            end
        else
            push!(expanded_children, expand_quote_content(ctx, interp_ctx_var, e, inner_depth))
        end
    end

    return @ast ctx ex [K"call"
        interpolate_node::K"Value"
        interp_ctx_var
        [K"inert" ex]
        expanded_children...
    ]
end

function expand_quote(ctx, ex)
    interp_ctx_var = ssavar(ctx, ex)
    expanded = if kind(ex) == K"$"
        @chk numchildren(ex) == 1
        e1 = ex[1]
        if kind(e1) == K"..."
            throw(LoweringError(e1, "`...` expression outside of call"))
        end
        expand_interpolation(ctx, interp_ctx_var, e1)
    else
        expand_quote_content(ctx, interp_ctx_var, ex, 0)
    end
    @ast ctx ex [K"block"
        [K"="
           interp_ctx_var
           [K"call"
               InterpolationContext::K"Value"
           ]
        ]
        expanded
    ]
end

#--------------------------------------------------
struct MacroContext <: AbstractLoweringContext
    graph::SyntaxGraph
    macroname::SyntaxTree
    scope_layer::ScopeLayer
end

function adopt_scope(ex, ctx::MacroContext)
    adopt_scope(ex, ctx.scope_layer.id)
end

struct MacroExpansionError
    context::Union{Nothing,MacroContext}
    ex::SyntaxTree
    msg::String
end

function MacroExpansionError(ex::SyntaxTree, msg::AbstractString)
    MacroExpansionError(nothing, ex, msg)
end

function Base.showerror(io::IO, exc::MacroExpansionError)
    print(io, "MacroExpansionError")
    ctx = exc.context
    if !isnothing(ctx)
        print(io, " while expanding ", ctx.macroname,
              " in module ", ctx.scope_layer.mod)
    end
    print(io, ":\n")
    src = sourceref(exc.ex)
    highlight(io, src.file, first_byte(src):last_byte(src), note=exc.msg)
end

function set_scope_layer!(ex, id, force)
    k = kind(ex)
    if (k == K"Identifier" || k == K"MacroName" || (is_operator(k) && !haschildren(ex))) &&
            (force || !hasattr(ex, :scope_layer))
        setattr!(ex; scope_layer=id)
    end
end

function set_scope_layer_recursive!(ex, id, force)
    k = kind(ex)
    if k == K"module" || k == K"toplevel"
        return
    end
    if haschildren(ex)
        for c in children(ex)
            set_scope_layer_recursive!(c, id, force)
        end
    else
        set_scope_layer!(ex, id, force)
    end
    ex
end

function eval_macro_name(ctx, ex)
    # `ex1` might contain a nontrivial mix of scope layers so we can't just
    # `eval()` it, as it's already been partially lowered by this point.
    # Instead, we repeat the latter parts of `lower()` here.
    ex1 = expand_forms_1(ctx, ex)
    ctx2, ex2 = expand_forms_2(ctx, ex1)
    ctx3, ex3 = resolve_scopes!(ctx2, ex2)
    ctx4, ex4 = linearize_ir(ctx3, ex3)
    mod = ctx.current_layer.mod
    expr_form = to_lowered_expr(mod, ex4.var_info, ex4)
    eval(mod, expr_form)
end

function expand_macro(ctx, ex)
    @assert kind(ex) == K"macrocall"

    macname = ex[1]
    macfunc = eval_macro_name(ctx, macname)
    # Macro call arguments may be either
    # * Unprocessed by the macro expansion pass
    # * Previously processed, but spliced into a further macro call emitted by
    #   a macro expansion.
    # In either case, we need to set any unset scope layers before passing the
    # arguments to the macro call.
    macro_args = [set_scope_layer_recursive!(e, ctx.current_layer.id, false)
                  for e in children(ex)[2:end]]
    mctx = MacroContext(ctx.graph, macname, ctx.current_layer)
    expanded = try
        # TODO: Allow invoking old-style macros for compat
        invokelatest(macfunc, mctx, macro_args...)
    catch exc
        if exc isa MacroExpansionError
            # Add context to the error.
            # TODO: Using rethrow() is kinda ugh. Is there a way to avoid it?
            rethrow(MacroExpansionError(mctx, ex.ex, exc.msg))
        else
            throw(MacroExpansionError(mctx, ex, "Error expanding macro"))
        end
    end

    if expanded isa SyntaxTree
        if syntax_graph(expanded) !== syntax_graph(ctx)
            # If the macro has produced syntax outside the macro context, copy it over.
            # TODO: Do we expect this always to happen?  What is the API for access
            # to the macro expansion context?
            expanded = copy_ast(ctx, expanded)
        end
        new_layer = new_scope_layer(ctx.scope_layers, parentmodule(macfunc), true)
        ctx2 = MacroExpansionContext(ctx.graph, ctx.next_var_id, ctx.scope_layers, new_layer)
        expand_forms_1(ctx2, expanded)
    else
        @ast ctx ex expanded::K"Value"
    end
end

"""
Lowering pass 1

This pass contains some simple expansion to make the rest of desugaring easier
to write and expands user defined macros. Macros see the surface syntax, so
need to be dealt with before other lowering.

* Does identifier normalization
* Strips semantically irrelevant "container" nodes like parentheses
* Expands macros
* Processes quoted syntax turning `K"quote"` into `K"inert"` (eg, expanding
  interpolations)
"""
function expand_forms_1(ctx::MacroExpansionContext, ex::SyntaxTree)
    set_scope_layer!(ex, ctx.current_layer.id, false)
    k = kind(ex)
    if k == K"Identifier"
        # TODO: Insert is_placeholder() transformation here.
        ex
    elseif k == K"var" || k == K"char" || k == K"parens"
        # Strip "container" nodes
        @chk numchildren(ex) == 1
        expand_forms_1(ctx, ex[1])
    elseif k == K"MacroName"
        @ast ctx ex ex=>K"Identifier"
    elseif is_operator(k) && !haschildren(ex) # TODO: do in JuliaSyntax?
        @ast ctx ex ex=>K"Identifier"
    elseif k == K"quote"
        @chk numchildren(ex) == 1
        expand_quote(ctx, ex[1])
    elseif k == K"module" || k == K"toplevel" || k == K"inert"
        ex
    elseif k == K"macrocall"
        expand_macro(ctx, ex)
    elseif !haschildren(ex)
        ex
    else
        mapchildren(e->expand_forms_1(ctx,e), ctx, ex)
    end
end

function expand_forms_1(ctx::MacroExpansionContext, exs::Union{Tuple,AbstractVector})
    res = SyntaxList(ctx)
    for e in exs
        push!(res, expand_forms_1(ctx, e))
    end
    res
end

function expand_forms_1(mod::Module, ex::SyntaxTree)
    ctx = MacroExpansionContext(ex, mod)
    ctx, expand_forms_1(ctx, reparent(ctx, ex))
end

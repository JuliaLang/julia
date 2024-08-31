# Runtime support functionality.
#
# Lowering generates code which uses these functions and types but it doesn't
# call them directly.
#
# These should probably move to `Core` at some point?

struct InterpolationContext{Graph} <: AbstractLoweringContext
    graph::Graph
    values::Tuple
    current_index::Ref{Int}
end

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

# Produce interpolated node for `$x` syntax
function _interpolated_value(ctx, srcref, ex)
    if ex isa SyntaxTree
        if !is_compatible_graph(ctx, ex)
            ex = copy_ast(ctx, ex)
        end
        append_sourceref(ctx, ex, srcref)
    else
        makeleaf(ctx, srcref, K"Value", ex)
    end
end

function _interpolate_ast(ctx::InterpolationContext, ex, depth)
    if ctx.current_index[] > length(ctx.values) || !_contains_active_interp(ex, depth)
        return ex
    end

    # We have an interpolation deeper in the tree somewhere - expand to an
    # expression 
    inner_depth = kind(ex) == K"quote" ? depth + 1 :
                  kind(ex) == K"$"     ? depth - 1 :
                  depth
    expanded_children = SyntaxList(ctx)
    for e in children(ex)
        if kind(e) == K"$" && inner_depth == 0
            vals = ctx.values[ctx.current_index[]]::Tuple
            ctx.current_index[] += 1
            for (i,v) in enumerate(vals)
                srcref = numchildren(e) == 1 ? e : e[i]
                push!(expanded_children, _interpolated_value(ctx, srcref, v))
            end
        else
            push!(expanded_children, _interpolate_ast(ctx, e, inner_depth))
        end
    end

    makenode(ctx, ex, head(ex), expanded_children)
end

function interpolate_ast(ex, values...)
    # Construct graph for interpolation context. We inherit this from the macro
    # context where possible by detecting it using __macro_ctx__. This feels
    # hacky though.
    #
    # Perhaps we should use a ScopedValue for this instead or get it from
    # the macro __context__? Nothing feels great here.
    graph = nothing
    for vals in values
        for v in vals
            if v isa SyntaxTree && hasattr(syntax_graph(v), :__macro_ctx__)
                graph = syntax_graph(v)
                break
            end
        end
    end
    if isnothing(graph)
        graph = ensure_attributes(SyntaxGraph(), kind=Kind, syntax_flags=UInt16, source=SourceAttrType,
                                  value=Any, name_val=String, scope_layer=LayerId)
    end
    ctx = InterpolationContext(graph, values, Ref(1))
    # We must copy the AST into our context to use it as the source reference
    # of generated expressions.
    ex1 = copy_ast(ctx, ex)
    if kind(ex1) == K"$"
        @assert length(values) == 1
        vs = values[1]
        if length(vs) > 1
            # :($($(xs...))) where xs is more than length 1
            throw(LoweringError(ex1, "More than one value in bare `\$` expression"))
        end
        _interpolated_value(ctx, ex1, only(vs))
    else
        _interpolate_ast(ctx, ex1, 0)
    end
end

# Construct new bare module including only the "default names"
#
#     using Core
#     const modname = modval
#     public modname
#
# And run statments in the toplevel expression `body`
function eval_module(parentmod, modname, body)
    # Here we just use `eval()` with an Expr.
    # If we wanted to avoid this we'd need to reproduce a lot of machinery from
    # jl_eval_module_expr()
    #
    # 1. Register / deparent toplevel modules
    # 2. Set binding in parent module
    # 3. Deal with replacing modules
    #    * Warn if replacing
    #    * Root old module being replaced
    # 4. Run __init__
    #    * Also run __init__ for any children after parent is defined
    # mod = @ccall jl_new_module(Symbol(modname)::Symbol, parentmod::Module)::Any
    # ...
    name = Symbol(modname)
    eval(parentmod, :(
        baremodule $name
            $eval($name, $body)
        end
    ))
end

# Evaluate content of `import` or `using` statement
function module_import(into_mod::Module, is_using::Bool,
                       from_mod::Union{Nothing,Core.SimpleVector}, paths::Core.SimpleVector)
    # For now, this function converts our lowered representation back to Expr
    # and calls eval() to avoid replicating all of the fiddly logic in
    # jl_toplevel_eval_flex.
    # TODO: ccall Julia runtime functions directly?
    #   * jl_module_using jl_module_use_as
    #   * import_module jl_module_import_as
    path_args = []
    i = 1
    while i < length(paths)
        nsyms = paths[i]::Int
        n = i + nsyms
        path = Expr(:., [Symbol(paths[i+j]::String) for j = 1:nsyms]...)
        as_name = paths[i+nsyms+1]
        push!(path_args, isnothing(as_name) ? path :
                         Expr(:as, path, Symbol(as_name)))
        i += nsyms + 2
    end
    ex = if isnothing(from_mod)
        Expr(is_using ? :using : :import,
             path_args...)
    else
        from_path = Expr(:., [Symbol(s::String) for s in from_mod]...)
        Expr(is_using ? :using : :import,
             Expr(:(:), from_path, path_args...))
    end
    eval(into_mod, ex)
    nothing
end

# Return the current exception. In JuliaLowering we use this rather than the
# special form `K"the_exception"` to reduces the number of special forms.
Base.@assume_effects :removable :nothrow function current_exception()
    @ccall jl_current_exception(current_task()::Any)::Any
end

function bind_docs!(f::Function, docstr, method_metadata)
    mod = parentmodule(f)
    bind = Base.Docs.Binding(mod, nameof(f))
    full_sig = method_metadata[1]
    arg_sig = Tuple{full_sig[2:end]...}
    linenum = method_metadata[3]
    metadata = Dict{Symbol, Any}(
        :linenumber => linenum.line,
        :module => mod,
    )
    if !isnothing(linenum.file)
        push!(metadata, :path => string(linenum.file))
    end
    Docs.doc!(mod, bind, Base.Docs.docstr(docstr, metadata), arg_sig)
end


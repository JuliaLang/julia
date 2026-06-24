# Lowering pass 1: Macro expansion, simple normalizations and quote expansion

# One per pass
struct MacroExpansionContext{Attrs} <: AbstractLoweringContext
    graph::SyntaxGraph{Attrs}
    syntax_context::SyntaxContext
    bindings::Bindings
    world::UInt
    recursive::Bool
end

#--------------------------------------------------
# Expansion of quoted expressions
function collect_unquoted!(ctx, unquoted, ex, depth)
    if kind(ex) == K"$" && depth == 0
        # children(ex) is usually length 1, but for double interpolation it may
        # be longer and the children may contain K"..." expressions. Wrapping
        # in a tuple groups the arguments together correctly in those cases.
        push!(unquoted, @ast ctx ex [K"tuple" children(ex)...])
    else
        inner_depth = kind(ex) == K"quote" ? depth + 1 :
                      kind(ex) == K"$"     ? depth - 1 :
                      depth
        for e in children(ex)
            collect_unquoted!(ctx, unquoted, e, inner_depth)
        end
    end
    return unquoted
end

# TODO: Implementing interpolations with a macro could give us better provenance
function expand_quote(ctx, st)
    unquoted = SyntaxList(ctx)
    collect_unquoted!(ctx, unquoted, st, 0)
    # not just optimizations; expected e.g. in `(. mod (quote field))`
    if is_expr_value(st)
        @jl_assert isempty(unquoted) st
        st
    elseif kind(st) === K"$"
        numchildren(st) != 1 && throw(LoweringError(
            st, raw"More than one value in bare `$` expression"))
        kind(st[1]) === K"..." && throw(LoweringError(
            st, raw"unexpected `...` in bare `$` expression"))
        @ast ctx st st[1]
    elseif kind(st) === K"Identifier" && !hasattr(st, :mod)
        @jl_assert isempty(unquoted) st
        @ast ctx st [K"inert" st]
    else
        @ast ctx st [K"call" interpolate_expr::K"Value" [K"inert" st] unquoted...]
    end
end

function collect_syntaxunquote!(ctx, unquoted, st, depth)
    if kind(st) === K"syntaxunquote" && depth == 0
        numchildren(st) !== 1 && throw(LoweringError(st, "malformed syntaxunquote"))
        push!(unquoted, @ast ctx st[1] [K"tuple" st[1]])
    else
        inner_depth = kind(st) == K"syntaxquote" ? depth + 1 :
                      kind(st) == K"syntaxunquote" ? depth - 1 : depth
        for c in children(st)
            collect_syntaxunquote!(ctx, unquoted, c, inner_depth)
        end
    end
    unquoted
end

# we do not optimize (syntaxquote (syntaxunquote x)) -> x, since x may not be a
# SyntaxTree
function expand_syntaxquote(ctx, st)
    if kind(st) === K"syntaxunquote"
        numchildren(st) != 1 && throw(LoweringError(
            st, raw"More than one value in bare `syntaxunquote` expression"))
        kind(st[1]) === K"..." && throw(LoweringError(
            st, raw"unexpected `...` in bare `syntaxunquote` expression"))
    end
    unquoted = collect_syntaxunquote!(ctx, SyntaxList(ctx), st, 0)
    @ast ctx st [K"call" interpolate_syntax::K"Value"
                 [K"syntaxinert" st] unquoted...]
end

# Passed to the user as an implicit macro argument
struct MacroContext <: AbstractLoweringContext
    graph::SyntaxGraph
    macrocall::Union{SyntaxTree,LineNumberNode,SourceRef}
end

struct MacroExpansionError <: Exception
    context::Union{Nothing,MacroContext}
    ex::SyntaxTree
    msg::String
    "The source position relative to the node - may be `:begin` or `:end` or `:all`"
    position::Symbol
    "Error that occurred inside the macro function call (`nothing` if no inner exception)"
    err
    MacroExpansionError(
        context::Union{Nothing,MacroContext}, ex::SyntaxTree, msg::AbstractString, position::Symbol,
        @nospecialize err = nothing
    ) = new(context, ex, msg, position, err)
end

function MacroExpansionError(ex::SyntaxTree, msg::AbstractString; position=:all)
    MacroExpansionError(nothing, ex, msg, position)
end

function Base.showerror(io::IO, exc::MacroExpansionError)
    print(io, "MacroExpansionError")
    ctx = exc.context
    if !isnothing(ctx)
        # Use `Expr` formatting to pretty print the macro name for now -
        # there's quite a lot of special cases. We could alternatively consider
        # calling sourcetext() though that won't work well if it's a
        # synthetically-generated macro name path.
        macname_str = string(Expr(
            :macrocall, est_to_expr(ctx.macrocall[1]), nothing))
        print(io, " while expanding ", macname_str,
              " in module ", syntax_module(ctx.macrocall))
    end
    print(io, ":\n")
    # TODO: Display niceties:
    # * Show the full provenance tree somehow, in addition to the primary
    #   source location we're showing here?
    # * What if the expression doesn't arise from a source file?
    # * How to deal with highlighting trivia? Could provide a token kind or
    #   child position within the raw tree? How to abstract this??
    src = sourceref(exc.ex)
    if src isa LineNumberNode
        highlight(io, src, note=exc.msg)
    else
        fb = first_byte(src)
        lb = last_byte(src)
        pos = exc.position
        byterange = pos == :all     ? (fb:lb)   :
            pos == :begin   ? (fb:fb-1) :
            pos == :end     ? (lb+1:lb) :
            error("Unknown position $pos")
        highlight(io, src.file[], byterange, note=exc.msg)
    end
    if !isnothing(exc.err)
        print(io, "\nCaused by:\n")
        showerror(io, exc.err)
    end
end

function _eval_dot(world::UInt, mod, ex::SyntaxTree)
    if kind(ex) === K"."
        mod = _eval_dot(world, mod, ex[1])
        ex = ex[2]
    end
    if kind(ex) === K"inert"
        ex = ex[1]
    end
    kind(ex) in KSet"Identifier Symbol" && mod isa Module ?
        Base.invoke_in_world(world, getproperty, mod, Symbol(ex.name_val)) :
        nothing
end

# If macroexpand(ex[1]) is an identifier or dot-expression, we can simply grab
# it from the correct module in ctx.world.  Otherwise, we need to eval arbitrary
# code (which, TODO: does not use the correct world age, and it isn't clear the
# language is meant to support this).
function eval_macro_name(ctx, mctx::MacroContext, st0::SyntaxTree)
    sc = st0.context::SyntaxContext
    mod = syntax_module(sc)
    st = expand_forms_1(ctx, st0)
    try
        if kind(st) === K"Value"
            st.value
        elseif kind(st) === K"Identifier"
            Base.invoke_in_world(ctx.world, getproperty,
                                 syntax_module(st), Symbol(st.name_val))
        elseif kind(st) === K"." &&
                # TODO: correct mod?
                (ed = _eval_dot(ctx.world, mod, st); !isnothing(ed))
            ed
        else
            # `ex` might contain a nontrivial mix of scopes so we can't just
            # `eval()` it, as it's already been partially lowered by this point.
            # Instead, we repeat the latter parts of `lower()` here.
             ctx2, st2 = expand_forms_2(ctx, st)
             ctx3, st3 = resolve_scopes(ctx2, st2)
             ctx4, st4 = convert_closures(ctx3, st3)
            _ctx5, st5 = linearize_ir(ctx4, st4)
            expr_form  = to_lowered_expr(st5)
            ccall(:jl_toplevel_eval, Any, (Any, Any), mod, expr_form)
        end
    catch err
        throw(MacroExpansionError(mctx, st, "Macro not found", :all, err))
    end
end

function _macrocall_expr_location(st::SyntaxTree)
    @jl_assert kind(st) === K"macrocall" st
    if kind(st[2]) === K"Value"
        loc = st[2].value
        if loc isa MacroSource
            loc
        elseif loc isa LineNumberNode
            # Some macros, e.g. @cmd, don't play nicely with file == nothing
            isnothing(loc.file) ? LineNumberNode(loc.line, :none) : loc
        else
            LineNumberNode(0, :none)
        end
    elseif kind(st[2]) === K"VERSION"
        loc = source_location(LineNumberNode, st)
        isdefined(Core, :MacroSource) ? Core.MacroSource(loc, st[2].value) : loc
    else
        LineNumberNode(0, :none)
    end
end

function expand_macro(ctx::MacroExpansionContext, st::SyntaxTree)
    @jl_assert kind(st) === K"macrocall" st
    numchildren(st) >= 2 || throw(LoweringError(
        st, "`macrocall` requires a macro name and source location"))
    sc_in = st.context::SyntaxContext
    macname = st[1]
    mctx = MacroContext(ctx.graph, st)
    macfunc = eval_macro_name(ctx, mctx, macname)
    raw_args = st[3:end]

    # TODO: hasmethod always returns false for our `typemax(UInt)` meaning
    # "latest world," which we shouldn't be using.
    has_new_macro = ctx.world === typemax(UInt) ?
        hasmethod(macfunc, Tuple{typeof(mctx), typeof.(raw_args)...}) :
        hasmethod(macfunc, Tuple{typeof(mctx), typeof.(raw_args)...}; world=ctx.world)

    if has_new_macro
        macro_args = [mctx, raw_args...]
        expanded = try
            Base.invoke_in_world(ctx.world, macfunc, macro_args...)
        catch exc
            newexc = exc isa MacroExpansionError ?
                MacroExpansionError(mctx, exc.ex, exc.msg, exc.position, exc.err) :
                MacroExpansionError(mctx, st, "Error expanding macro", :all, exc)
            rethrow(newexc)
        end
        st_out = if expanded isa SyntaxTree
            expanded._graph !== ctx.graph ? copy_ast(ctx, expanded) : expanded
        else
            expanded isa Expr && throw(LoweringError(
                st, "implicit expr->syntaxtree: may later be allowed, but is probably a mistake today"))
            expr_to_est(st._graph, expanded, st._id)
        end
    else
        macro_loc = _macrocall_expr_location(st)
        macro_args = Any[macro_loc, ctx.syntax_context.layer.mod]
        for arg in raw_args
            @jl_assert kind(arg) !== K"VERSION" arg # handled in EST conversion
            push!(macro_args, est_to_expr(arg))
        end
        st_out = try
            Base.invoke_in_world(ctx.world, macfunc, macro_args...)
        catch exc
            if exc isa MethodError && exc.f === macfunc && !isempty(
                methods_in_world(macfunc, Tuple{typeof(mctx), Vararg{Any}}, ctx.world, st))
                # If the macro has at least some methods implemented in the
                # new style, assume the user meant to call one of those
                # rather than any old-style macro methods which might exist
                exc = MethodError(macfunc, (mctx, raw_args...,), ctx.world)
            end
            rethrow(MacroExpansionError(mctx, st, "Error expanding macro", :all, exc))
        end
        macro_lnn = macro_loc isa MacroSource ? macro_loc.lno : macro_loc
        st_out = expr_to_est(st._graph, st_out, macro_lnn)
    end
    # Module scope for the returned AST is the module where this particular
    # method was defined (may be different from `parentmodule(macfunc)`)
    mod_for_ast = lookup_method_instance(macfunc, macro_args, ctx.world).def.module
    sc2 = SyntaxContext(
        ScopeLayer(mod_for_ast, sc_in.layer), st,
        (has_new_macro ? JL_NEW_SYNTAX_VERSION : JL_OLD_SYNTAX_VERSION), false)
    !ctx.recursive ? st_out :
        expand_forms_1(ctx, apply_expansion_layer(ctx, st_out, sc2, sc_in.layer))
end

# If `unknown_layer(seen, macro_lstack, tree_sc.layer)`, we should not
# update the tree's layer, since it was passed as an argument to a macro, or
# purposefully escaped.  If not, we should update it, since `tree_sc.layer` is
# missing or unknown to us, so we treat it as brand-new syntax.
function unknown_layer(slcache::Dict{ScopeLayer, Bool},
                       lstack::Union{ScopeLayer, Nothing},
                       sl::Union{ScopeLayer, Nothing})
    isnothing(lstack) && return true
    isnothing(sl) && return false
    out = get(slcache, sl, nothing)
    !isnothing(out) && return out
    while lstack !== sl && lstack.escaped !== nothing
        lstack = lstack.escaped
    end
    slcache[sl] = sl !== lstack
end

# When a macro expands, we add a layer to all new syntax in the expansion.  This
# is similar to racket's flip-scope operation.  Set lstack=nothing to set the
# layer unconditionally.
function apply_expansion_layer(ctx, st, sc_in, lstack)
    out = apply_expansion_layer(
        ctx, st, sc_in, lstack,
        isnothing(lstack) ? nothing : Dict{ScopeLayer, Bool}(), true)
    # @info "apply_expansion_layer" st out
    out
end

function apply_expansion_layer(ctx, st::SyntaxTree, sc_in, lstack, slcache, absorb_esc)
    sc0 = get(st, :context, nothing)::Union{Nothing, SyntaxContext}
    sc = (isnothing(sc0) || isnothing(lstack) ||
        unknown_layer(slcache, lstack, sc0.layer)) ? sc_in : sc0
    k = kind(st)
    out = if is_leaf(st)
        setattr(st, :context, sc)
    elseif k === K"escape" && absorb_esc
        if numchildren(st) !== 1
            throw(LoweringError(st, "`escape` requires one argument"))
        elseif is_base_layer(sc)
            throw(LoweringError(st, "`escape` node in outer context"))
        elseif !is_flisp_compat(sc)
            throw(LoweringError(st, "new macros should not use `escape`"))
        end
        sc2 = escape_layer(sc)
        apply_expansion_layer(ctx, st[1], sc2, sc2.layer.escaped)
    elseif k === K"hygienic-scope" && absorb_esc
        if !(2 <= numchildren(st) <= 3)
            throw(LoweringError(st, "`hygienic-scope` requires 2-3 children"))
        elseif kind(st[2]) !== K"Value" || !(st[2].value isa Module)
            throw(LoweringError(st, "`hygienic-scope` arg 2: expected Module"))
        elseif !is_flisp_compat(sc)
            throw(LoweringError(st, "new macros should not use `hygienic-scope`"))
        end
        new_sl = ScopeLayer(st[2].value::Module, sc.layer)
        sc2 = SyntaxContext(new_sl, sc.unexpanded, sc.version, sc.internal)
        apply_expansion_layer(ctx, st[1], sc2, lstack, slcache, true)
    elseif k === K"module"
        # Modules get a strange lack of special treatment in macroexpand.scm:
        # escapes are collapsed and hygienic references are resolved eagerly
        # (TODO: only happens with the name right now).
        mod_body = st[end]
        out = mapchildren(c->(c === mod_body ? c :
            apply_expansion_layer(ctx, c, sc_in, lstack, slcache, true)),
                          st._graph, st)
        setattr!(out, :context, sc)
    else
        ae2 = absorb_esc && !(k in KSet"macrocall toplevel inert syntaxinert")
        out = mapchildren(
            c->apply_expansion_layer(
                ctx, c, sc_in, lstack, slcache, ae2), st._graph, st)
        setattr!(out, :context, sc)
    end
    out
end

"""
Expands macros and quote/interpolation forms
"""
function expand_forms_1(ctx::MacroExpansionContext, st::SyntaxTree)
    k = kind(st)
    if is_leaf(st)
        st
    elseif k === K"macrocall"
        expand_macro(ctx, st)
    elseif (k === K"do" && numchildren(st) == 2 && kind(st[1]) === K"macrocall" &&
        kind(st[2]) === K"->")
        mac_ex = @ast ctx st [
            K"macrocall"
            st[1][1] # mac name
            st[1][2] # loc
            st[2]    # do-lambda
            children(st[1])[3:end]...
        ]
        expand_macro(ctx, mac_ex)
    elseif k in KSet"inert syntaxinert toplevel module"
        st
    elseif k === K"quote"
        if numchildren(st) !== 1
            throw(LoweringError(st, "`quote` requires one argument"))
        end
        sc = st.context::SyntaxContext
        expand_forms_1(ctx, apply_expansion_layer(
            ctx, expand_quote(ctx, st[1]), sc, sc.layer))
    elseif k === K"syntaxquote"
        if numchildren(st) !== 1
            throw(LoweringError(st, "`syntaxquote` requires one argument"))
        end
        sc = st.context::SyntaxContext
        expand_forms_1(ctx, apply_expansion_layer(
            ctx, expand_syntaxquote(ctx, st[1]), sc, sc.layer))
    else
        mapchildren(c->expand_forms_1(ctx, c), ctx, st)
    end
end

function ensure_macro_attributes!(graph)
    g2 = ensure_attributes!(
        graph;
        var_id=IdTag,
        meta=CompileHints)
    DEBUG ? ensure_attributes!(g2; jl_source=LineNumberNode) : g2
end

@fzone "JL: macroexpand" function expand_forms_1(
    mod::Module, st::SyntaxTree, expr_compat_mode::Bool, world::UInt, recursive::Bool)

    graph = ensure_macro_attributes!(copy_attrs(syntax_graph(st)))
    st = reparent(graph, st)
    # TODO: Change `expr_compat_mode` to `version` further up the API
    sc = hasattr(st, :context) ? st.context::SyntaxContext :
        SyntaxContext(mod, (expr_compat_mode ?
            JL_OLD_SYNTAX_VERSION : JL_NEW_SYNTAX_VERSION))
    ctx = MacroExpansionContext(graph, sc, Bindings(), world, recursive)
    # @warn "mx in" st
    st_out = expand_forms_1(ctx, apply_expansion_layer(ctx, st, sc, nothing))
    # @warn "mx out" st_out
    return ctx, st_out
end

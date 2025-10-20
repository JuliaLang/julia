# Lowering Pass 2 - syntax desugaring

struct DesugaringContext{GraphType} <: AbstractLoweringContext
    graph::GraphType
    bindings::Bindings
    scope_layers::Vector{ScopeLayer}
    mod::Module
    expr_compat_mode::Bool
end

function DesugaringContext(ctx, expr_compat_mode::Bool)
    graph = ensure_attributes(syntax_graph(ctx),
                              kind=Kind, syntax_flags=UInt16,
                              source=SourceAttrType,
                              value=Any, name_val=String,
                              scope_type=Symbol, # :hard or :soft
                              var_id=IdTag,
                              is_toplevel_thunk=Bool,
                              toplevel_pure=Bool)
    DesugaringContext(graph,
                      ctx.bindings,
                      ctx.scope_layers,
                      current_layer(ctx).mod,
                      expr_compat_mode)
end

#-------------------------------------------------------------------------------

# Return true when `x` and `y` are "the same identifier", but also works with
# bindings (and hence ssa vars). See also `is_identifier_like()`
function is_same_identifier_like(ex::SyntaxTree, y::SyntaxTree)
    return (kind(ex) == K"Identifier" && kind(y) == K"Identifier" && NameKey(ex) == NameKey(y)) ||
           (kind(ex) == K"BindingId"  && kind(y) == K"BindingId"  && ex.var_id   == y.var_id)
end

function is_same_identifier_like(ex::SyntaxTree, name::AbstractString)
    return kind(ex) == K"Identifier" && ex.name_val == name
end

function contains_identifier(ex::SyntaxTree, idents::AbstractVector{<:SyntaxTree})
    contains_unquoted(ex) do e
        any(is_same_identifier_like(e, id) for id in idents)
    end
end

function contains_identifier(ex::SyntaxTree, idents...)
    contains_unquoted(ex) do e
        any(is_same_identifier_like(e, id) for id in idents)
    end
end

function contains_ssa_binding(ctx, ex)
    contains_unquoted(ex) do e
        kind(e) == K"BindingId" && lookup_binding(ctx, e).is_ssa
    end
end

# Return true if `f(e)` is true for any unquoted child of `ex`, recursively.
function contains_unquoted(f::Function, ex::SyntaxTree)
    if f(ex)
        return true
    elseif !is_leaf(ex) && !(kind(ex) in KSet"quote inert meta")
        return any(contains_unquoted(f, e) for e in children(ex))
    else
        return false
    end
end

# Identify some expressions that are safe to repeat
#
# TODO: Can we use this in more places?
function is_effect_free(ex)
    k = kind(ex)
    # TODO: metas
    is_literal(k) || is_identifier_like(ex) || k == K"Symbol" ||
        k == K"inert" || k == K"top" || k == K"core" || k == K"Value"
    # flisp also includes `a.b` with simple `a`, but this seems like a bug
    # because this calls the user-defined getproperty?
end

function check_no_parameters(ex::SyntaxTree, msg)
    i = find_parameters_ind(children(ex))
    if i > 0
        throw(LoweringError(ex[i], msg))
    end
end

function check_no_assignment(exs, msg="misplaced assignment statement in `[ ... ]`")
    i = findfirst(kind(e) == K"=" for e in exs)
    if !isnothing(i)
        throw(LoweringError(exs[i], msg))
    end
end

#-------------------------------------------------------------------------------
# Destructuring

# Convert things like `(x,y,z) = (a,b,c)` to assignments, eliminating the
# tuple. Includes support for slurping/splatting. This function assumes that
# `_tuple_sides_match` returns true, so the following have already been
# checked:
#   * There's max one `...` on the left hand side
#   * There's max one `...` on the right hand side, in the last place, or
#     matched with an lhs... in the last place. (required so that
#     pairwise-matching terms from the right is valid)
#   * Neither side has any key=val terms or parameter blocks
#
# Tuple elimination must act /as if/ the right hand side tuple was first
# constructed followed by destructuring. In particular, any side effects due to
# evaluating the individual terms in the right hand side tuple must happen in
# order.
function tuple_to_assignments(ctx, ex, is_const)
    lhs = ex[1]
    rhs = ex[2]
    wrap(asgn) = is_const ? (@ast ctx ex [K"const" asgn]) : asgn

    # Tuple elimination aims to turn assignments between tuples into lists of assignments.
    #
    # However, there's a complex interplay of side effects due to the
    # individual assignments and these can be surprisingly complicated to
    # model. For example `(x[i], y) = (f(), g)` can contain the following
    # surprises:
    # * `tmp = f()` calls `f` which might throw, or modify the bindings for
    #   `x` or `y`.
    # * `x[i] = tmp` is lowered to `setindex!` which might throw or modify the
    #   bindings for `x` or `y`.
    # * `g` might throw an `UndefVarError`
    #
    # Thus for correctness we introduce temporaries for all right hand sides
    # with observable side effects and ensure they're evaluated in order.
    n_lhs = numchildren(lhs)
    n_rhs = numchildren(rhs)
    stmts = SyntaxList(ctx)
    rhs_tmps = SyntaxList(ctx)
    for i in 1:n_rhs
        rh = rhs[i]
        r = if kind(rh) == K"..."
            rh[1]
        else
            rh
        end
        k = kind(r)
        if is_literal(k) || k == K"Symbol" || k == K"inert" || k == K"top" ||
                k == K"core" || k == K"Value"
            # Effect-free and nothrow right hand sides do not need a temporary
            # (we require nothrow because the order of rhs terms is observable
            #  due to sequencing, thus identifiers are not allowed)
        else
            # Example rhs which need a temporary
            # * `f()` - arbitrary side effects to any binding
            # * `z`   - might throw UndefVarError
            tmp = emit_assign_tmp(stmts, ctx, r)
            rh = kind(rh) == K"..." ? @ast(ctx, rh, [K"..." tmp]) : tmp
        end
        push!(rhs_tmps, rh)
    end

    il = 0
    ir = 0
    while il < n_lhs
        il += 1
        ir += 1
        lh = lhs[il]
        if kind(lh) == K"..."
            # Exactly one lhs `...` occurs in the middle somewhere, with a
            # general rhs which has at least as many non-`...` terms or one
            # `...` term at the end.
            # Examples:
            #   (x, ys..., z) = (a, b, c, d)
            #   (x, ys..., z) = (a, bs...)
            #   (xs..., y)    = (a, bs...)
            #   (xs...) = (a, b, c)
            # in this case we can pairwise-match arguments from the end
            # backward and emit a general tuple assignment for the middle.
            jl = n_lhs
            jr = n_rhs
            while jl > il && jr > ir
                if kind(lhs[jl]) == K"..." || kind(rhs_tmps[jr]) == K"..."
                    break
                end
                jl -= 1
                jr -= 1
            end
            middle = emit_assign_tmp(stmts, ctx,
                @ast(ctx, rhs, [K"tuple" rhs_tmps[ir:jr]...]),
                "rhs_tmp"
            )
            if il == jl
                # (x, ys...) = (a,b,c)
                # (x, ys...) = (a,bs...)
                # (ys...)    = ()
                push!(stmts, wrap(@ast ctx ex [K"=" lh[1] middle]))
            else
                # (x, ys..., z) = (a, b, c, d)
                # (x, ys..., z) = (a, bs...)
                # (xs..., y)    = (a, bs...)
                push!(stmts, wrap(@ast ctx ex [K"=" [K"tuple" lhs[il:jl]...] middle]))
            end
            # Continue with the remainder of the list of non-splat terms
            il = jl
            ir = jr
        else
            rh = rhs_tmps[ir]
            if kind(rh) == K"..."
                push!(stmts, wrap(@ast ctx ex [K"=" [K"tuple" lhs[il:end]...] rh[1]]))
                break
            else
                push!(stmts, wrap(@ast ctx ex [K"=" lh rh]))
            end
        end
    end

    @ast ctx ex [K"block"
        stmts...
        [K"removable" [K"tuple" rhs_tmps...]]
    ]
end

# Create an assignment `$lhs = $rhs` where `lhs` must be "simple". If `rhs` is
# a block, sink the assignment into the last statement of the block to keep
# more expressions at top level. `rhs` should already be expanded.
#
# flisp: sink-assignment
function sink_assignment(ctx, srcref, lhs, rhs)
    @assert is_identifier_like(lhs)
    if kind(rhs) == K"block"
        @ast ctx srcref [K"block"
            rhs[1:end-1]...
            [K"=" lhs rhs[end]]
        ]
    else
        @ast ctx srcref [K"=" lhs rhs]
    end
end

function _tuple_sides_match(lhs, rhs)
    N = max(length(lhs), length(rhs))
    for i = 1:N+1
        if i > length(lhs)
            # (x, y)        = (a, b)      # match
            # (x,)          = (a, b)      # no match
            return i > length(rhs)
        elseif kind(lhs[i]) == K"..."
            # (x, ys..., z) = (a, b)      # match
            # (x, ys...)    = (a,)        # match
            return true
        elseif i > length(rhs)
            # (x, y)        = (a,)        # no match
            # (x, y, zs...) = (a,)        # no match
            return false
        elseif kind(rhs[i]) == K"..."
            # (x, y)        = (as...,)    # match
            # (x, y, z)     = (a, bs...)  # match
            # (x, y)        = (as..., b)  # no match
            return i == length(rhs)
        end
    end
end

# Lower `(lhss...) = rhs` in contexts where `rhs` must be a tuple at runtime
# by assuming that `getfield(rhs, i)` works and is efficient.
function lower_tuple_assignment(ctx, assignment_srcref, lhss, rhs)
    stmts = SyntaxList(ctx)
    tmp = emit_assign_tmp(stmts, ctx, rhs, "rhs_tmp")
    for (i, lh) in enumerate(lhss)
        push!(stmts, @ast ctx assignment_srcref [K"="
            lh
            [K"call" "getfield"::K"core" tmp i::K"Integer"]
        ])
    end
    makenode(ctx, assignment_srcref, K"block", stmts)
end

# Implement destructuring with `lhs` a tuple expression (possibly with
# slurping) and `rhs` a general expression.
#
# Destructuring in this context is done via the iteration interface, though
# calls `Base.indexed_iterate()` to allow for a fast path in cases where the
# right hand side is directly indexable.
function _destructure(ctx, assignment_srcref, stmts, lhs, rhs, is_const)
    n_lhs = numchildren(lhs)
    if n_lhs > 0
        iterstate = new_local_binding(ctx, rhs, "iterstate")
    end

    end_stmts = SyntaxList(ctx)
    wrap(asgn) = is_const ? (@ast ctx assignment_srcref [K"const" asgn]) : asgn

    i = 0
    for lh in children(lhs)
        i += 1
        if kind(lh) == K"..."
            lh1 = if is_identifier_like(lh[1]) && !is_const
                lh[1]
            else
                lhs_tmp = ssavar(ctx, lh[1], "lhs_tmp")
                push!(end_stmts, expand_forms_2(ctx, wrap(@ast ctx lh[1] [K"=" lh[1] lhs_tmp])))
                lhs_tmp
            end
            if i == n_lhs
                # Slurping as last lhs, eg, for `zs` in
                #   (x, y, zs...) = rhs
                if kind(lh1) != K"Placeholder"
                    push!(stmts, expand_forms_2(ctx,
                        @ast ctx assignment_srcref [K"="
                            lh1
                            [K"call"
                                "rest"::K"top"
                                rhs
                                if i > 1
                                    iterstate
                                end
                            ]
                        ]
                    ))
                end
            else
                # Slurping before last lhs. Eg, for `xs` in
                #   (xs..., y, z) = rhs
                # For this we call
                #   (xs, tail) = Base.split_rest(...)
                # then continue iteration with `tail` as new rhs.
                tail = ssavar(ctx, lh, "tail")
                push!(stmts,
                    expand_forms_2(ctx,
                        lower_tuple_assignment(ctx,
                            assignment_srcref,
                            (lh1, tail),
                            @ast ctx assignment_srcref [K"call"
                                "split_rest"::K"top"
                                rhs
                                (n_lhs - i)::K"Integer"
                                if i > 1
                                    iterstate
                                end
                            ]
                        )
                    )
                )
                rhs = tail
                n_lhs = n_lhs - i
                i = 0
            end
        else
            # Normal case, eg, for `y` in
            #   (x, y, z) = rhs
            lh1 = if is_identifier_like(lh) && !is_const
                lh
            # elseif is_eventually_call(lh) (TODO??)
            else
                lhs_tmp = ssavar(ctx, lh, "lhs_tmp")
                push!(end_stmts, expand_forms_2(ctx, wrap(@ast ctx lh [K"=" lh lhs_tmp])))
                lhs_tmp
            end
            push!(stmts,
                expand_forms_2(ctx,
                    lower_tuple_assignment(ctx,
                        assignment_srcref,
                        i == n_lhs ? (lh1,) : (lh1, iterstate),
                        @ast ctx assignment_srcref [K"call"
                            "indexed_iterate"::K"top"
                            rhs
                            i::K"Integer"
                            if i > 1
                                iterstate
                            end
                        ]
                    )
                )
            )
        end
    end
    # Actual assignments must happen after the whole iterator is desctructured
    # (https://github.com/JuliaLang/julia/issues/40574)
    append!(stmts, end_stmts)
    stmts
end

# Expands cases of property destructuring
function expand_property_destruct(ctx, ex, is_const)
    @assert numchildren(ex) == 2
    lhs = ex[1]
    @assert kind(lhs) == K"tuple"
    if numchildren(lhs) != 1
        throw(LoweringError(lhs, "Property destructuring must use a single `;` before the property names, eg `(; a, b) = rhs`"))
    end
    params = lhs[1]
    @assert kind(params) == K"parameters"
    rhs = ex[2]
    stmts = SyntaxList(ctx)
    rhs1 = emit_assign_tmp(stmts, ctx, expand_forms_2(ctx, rhs))
    for prop in children(params)
        propname = kind(prop) == K"Identifier"                           ? prop    :
                   kind(prop) == K"::" && kind(prop[1]) == K"Identifier" ? prop[1] :
                   throw(LoweringError(prop, "invalid assignment location"))
        push!(stmts, expand_forms_2(ctx, @ast ctx rhs1 [K"="
            prop
            [K"call"
                "getproperty"::K"top"
                rhs1
                propname=>K"Symbol"
            ]
        ]))
    end
    push!(stmts, @ast ctx rhs1 [K"removable" rhs1])
    makenode(ctx, ex, K"block", stmts)
end

# Expands all cases of general tuple destructuring, eg
#   (x,y) = (a,b)
function expand_tuple_destruct(ctx, ex, is_const)
    lhs = ex[1]
    @assert kind(lhs) == K"tuple"
    rhs = ex[2]

    num_slurp = 0
    for lh in children(lhs)
        num_slurp += (kind(lh) == K"...")
        if num_slurp > 1
            throw(LoweringError(lh, "multiple `...` in destructuring assignment are ambiguous"))
        end
    end

    if kind(rhs) == K"tuple"
        num_splat = sum(kind(rh) == K"..." for rh in children(rhs); init=0)
        if num_splat == 0 && (numchildren(lhs) - num_slurp) > numchildren(rhs)
            throw(LoweringError(ex, "More variables on left hand side than right hand in tuple assignment"))
        end

        if !any_assignment(children(rhs)) && !has_parameters(rhs) &&
                _tuple_sides_match(children(lhs), children(rhs))
            return expand_forms_2(ctx, tuple_to_assignments(ctx, ex, is_const))
        end
    end

    stmts = SyntaxList(ctx)
    rhs1 = if is_ssa(ctx, rhs) ||
            (is_identifier_like(rhs) &&
             !any(is_same_identifier_like(kind(l) == K"..." ? l[1] : l, rhs)
                  for l in children(lhs)))
        rhs
    else
        emit_assign_tmp(stmts, ctx, expand_forms_2(ctx, rhs))
    end
    _destructure(ctx, ex, stmts, lhs, rhs1, is_const)
    push!(stmts, @ast ctx rhs1 [K"removable" rhs1])
    makenode(ctx, ex, K"block", stmts)
end

#-------------------------------------------------------------------------------
# Expand comparison chains

function expand_scalar_compare_chain(ctx, srcref, terms, i)
    comparisons = nothing
    while i + 2 <= length(terms)
        lhs = terms[i]
        op = terms[i+1]
        rhs = terms[i+2]
        if kind(op) == K"."
            break
        end
        comp = @ast ctx op [K"call"
            op
            lhs
            rhs
        ]
        if isnothing(comparisons)
            comparisons = comp
        else
            comparisons = @ast ctx srcref [K"&&"
                comparisons
                comp
            ]
        end
        i += 2
    end
    (comparisons, i)
end

# Expanding comparison chains: (comparison a op b op c ...)
#
# We use && to combine pairs of adjacent scalar comparisons and .& to combine
# vector-vector and vector-scalar comparisons. Combining scalar comparisons are
# treated as having higher precedence than vector comparisons, thus:
#
# a < b < c   ==>  (a < b) && (b < c)
# a .< b .< c   ==>  (a .< b) .& (b .< c)
# a < b < c .< d .< e   ==>  (a < b && b < c) .& (c .< d) .& (d .< e)
# a .< b .< c < d < e   ==>  (a .< b) .& (b .< c) .& (c < d && d < e)
function expand_compare_chain(ctx, ex)
    @assert kind(ex) == K"comparison"
    terms = children(ex)
    @chk numchildren(ex) >= 3
    @chk isodd(numchildren(ex))
    i = 1
    comparisons = nothing
    # Combine any number of dotted comparisons
    while i + 2 <= length(terms)
        if kind(terms[i+1]) != K"."
            (comp, i) = expand_scalar_compare_chain(ctx, ex, terms, i)
        else
            lhs = terms[i]
            op = terms[i+1]
            rhs = terms[i+2]
            i += 2
            comp = @ast ctx op [K"dotcall"
                op[1]
                lhs
                rhs
            ]
        end
        if isnothing(comparisons)
            comparisons = comp
        else
            comparisons = @ast ctx ex [K"dotcall"
                "&"::K"top"
                # ^^ NB: Flisp bug. Flisp lowering essentially does
                #     adopt_scope("&"::K"Identifier", ctx.mod)
                # here which seems wrong if the comparison chain arose from
                # a macro in a different module. One fix would be to use
                #     adopt_scope("&"::K"Identifier", ex)
                # to get the module of the comparison expression for the
                # `&` operator. But a simpler option is probably to always
                # use `Base.&` so we do that.
                comparisons
                comp
            ]
        end
    end
    comparisons
end

#-------------------------------------------------------------------------------
# Expansion of array indexing
function _arg_to_temp(ctx, stmts, ex, eq_is_kw=false)
    k = kind(ex)
    if is_effect_free(ex)
        ex
    elseif k == K"..."
        @ast ctx ex [k _arg_to_temp(ctx, stmts, ex[1])]
    elseif k == K"=" && eq_is_kw
        @ast ctx ex [K"=" ex[1] _arg_to_temp(ex[2])]
    else
        emit_assign_tmp(stmts, ctx, ex)
    end
end

# Make the *arguments* of an expression safe for multiple evaluation, for
# example
#
#   a[f(x)] => (temp=f(x); a[temp])
#
# Any assignments are added to `stmts` and a result expression returned which
# may be used in further desugaring.
function remove_argument_side_effects(ctx, stmts, ex)
    if is_literal(ex) || is_identifier_like(ex)
        ex
    else
        k = kind(ex)
        if k == K"let"
            emit_assign_tmp(stmts, ctx, ex)
        else
            args = SyntaxList(ctx)
            eq_is_kw = ((k == K"call" || k == K"dotcall") && is_prefix_call(ex)) || k == K"ref"
            for (i,e) in enumerate(children(ex))
                push!(args, _arg_to_temp(ctx, stmts, e, eq_is_kw && i > 1))
            end
            # TODO: Copy attributes?
            @ast ctx ex [k args...]
        end
    end
end

# Replace any `begin` or `end` symbols with an expression indexing the array
# `arr` in the `n`th index. `splats` are a list of the splatted arguments that
# precede index `n` `is_last` is true when this is this
# last index
function replace_beginend(ctx, ex, arr, n, splats, is_last)
    k = kind(ex)
    if k == K"Identifier" && ex.name_val in ("begin", "end")
        indexfunc = @ast ctx ex (ex.name_val == "begin" ? "firstindex" : "lastindex")::K"top"
        if length(splats) == 0
            if is_last && n == 1
                @ast ctx ex [K"call" indexfunc arr]
            else
                @ast ctx ex [K"call" indexfunc arr n::K"Integer"]
            end
        else
            splat_lengths = SyntaxList(ctx)
            for splat in splats
                push!(splat_lengths, @ast ctx ex [K"call" "length"::K"top" splat])
            end
            @ast ctx ex [K"call"
                indexfunc
                arr
                [K"call"
                    "+"::K"top"
                    (n - length(splats))::K"Integer"
                    splat_lengths...
                ]
            ]
        end
    elseif is_leaf(ex) || is_quoted(ex)
        ex
    elseif k == K"ref"
        # inside ref, only replace within the first argument
        @ast ctx ex [k
            replace_beginend(ctx, ex[1], arr, n, splats, is_last)
            ex[2:end]...
        ]
    # elseif k == K"kw" - keyword args - what does this mean here?
    #   # note from flisp
    #   # TODO: this probably should not be allowed since keyword args aren't
    #   # positional, but in this context we have just used their positions anyway
    else
        mapchildren(e->replace_beginend(ctx, e, arr, n, splats, is_last), ctx, ex)
    end
end

# Go through indices and replace the `begin` or `end` symbol
# `arr` - array being indexed
# `idxs` - list of indices
# returns the expanded indices. Any statements that need to execute first are
# added to ctx.stmts.
function process_indices(sctx::StatementListCtx, arr, idxs)
    has_splats = any(kind(i) == K"..." for i in idxs)
    idxs_out = SyntaxList(sctx)
    splats = SyntaxList(sctx)
    for (n, idx0) in enumerate(idxs)
        is_splat = kind(idx0) == K"..."
        val = replace_beginend(sctx, is_splat ? idx0[1] : idx0,
                               arr, n, splats, n == length(idxs))
        # TODO: kwarg?
        idx = !has_splats || is_simple_atom(sctx, val) ? val : emit_assign_tmp(sctx, val)
        if is_splat
            push!(splats, idx)
        end
        push!(idxs_out, is_splat ? @ast(sctx, idx0, [K"..." idx]) : idx)
    end
    return idxs_out
end

# Expand things like `f()[i,end]`, add to `sctx.stmts` (temporaries for
# computing indices) and return
# * `arr` -  The array (may be a temporary ssa value)
# * `idxs` - List of indices
function expand_ref_components(sctx::StatementListCtx, ex)
    check_no_parameters(ex, "unexpected semicolon in array expression")
    @assert kind(ex) == K"ref"
    @chk numchildren(ex) >= 1
    arr = ex[1]
    idxs = ex[2:end]
    if any(contains_identifier(e, "begin", "end") for e in idxs)
        arr = emit_assign_tmp(sctx, arr)
    end
    new_idxs = process_indices(sctx, arr, idxs)
    return (arr, new_idxs)
end

function expand_setindex(ctx, ex)
    @assert kind(ex) == K"=" && numchildren(ex) == 2
    lhs = ex[1]
    sctx = with_stmts(ctx)
    (arr, idxs) = expand_ref_components(sctx, lhs)
    rhs = emit_assign_tmp(sctx, ex[2])
    @ast ctx ex [K"block"
        sctx.stmts...
        expand_forms_2(ctx, [K"call"
            "setindex!"::K"top"
            arr
            rhs
            idxs...
        ])
        [K"removable" rhs]
    ]
end

#-------------------------------------------------------------------------------
# Expansion of broadcast notation `f.(x .+ y)`

function expand_dotcall(ctx, ex)
    k = kind(ex)
    if k == K"dotcall"
        @chk numchildren(ex) >= 1
        farg = ex[1]
        args = SyntaxList(ctx)
        append!(args, ex[2:end])
        kws = remove_kw_args!(ctx, args)
        @ast ctx ex [K"call"
            (isnothing(kws) ? "broadcasted" : "broadcasted_kwsyntax")::K"top"
            farg    # todo: What about (z=f).(x,y) ?
            (expand_dotcall(ctx, arg) for arg in args)...
            if !isnothing(kws)
                [K"parameters"
                    kws...
                ]
            end
        ]
    elseif k == K"comparison"
        expand_dotcall(ctx, expand_compare_chain(ctx, ex))
    elseif k == K".&&" || k == K".||"
        @ast ctx ex [K"call"
            "broadcasted"::K"top"
            (k == K".&&" ? "andand" : "oror")::K"top"
            (expand_dotcall(ctx, arg) for arg in children(ex))...
        ]
    else
        ex
    end
end

function expand_fuse_broadcast(ctx, ex)
    if kind(ex) == K".=" || kind(ex) == K".op="
        @chk numchildren(ex) == 2
        lhs = ex[1]
        kl = kind(lhs)
        rhs = expand_dotcall(ctx, ex[2])
        @ast ctx ex [K"call"
            "materialize!"::K"top"
            if kl == K"ref"
                sctx = with_stmts(ctx)
                (arr, idxs) = expand_ref_components(sctx, lhs)
                [K"block"
                    sctx.stmts...
                    [K"call"
                        "dotview"::K"top"
                        arr
                        idxs...
                    ]
                ]
            elseif kl == K"." && numchildren(lhs) == 2
                [K"call"
                    "dotgetproperty"::K"top"
                    children(lhs)...
                ]
            else
                lhs
            end
            if !(kind(rhs) == K"call" && kind(rhs[1]) == K"top" && rhs[1].name_val == "broadcasted")
                # Ensure the rhs of .= is always wrapped in a call to `broadcasted()`
                [K"call"(rhs)
                    "broadcasted"::K"top"
                    "identity"::K"top"
                    rhs
                ]
            else
                rhs
            end
        ]
    else
        @ast ctx ex [K"call"
            "materialize"::K"top"
            expand_dotcall(ctx, ex)
        ]
    end
end

#-------------------------------------------------------------------------------
# Expansion of generators and comprehensions

# Return any subexpression which is a 'return` statement, not including any
# inside quoted sections or method bodies.
function find_return(ex::SyntaxTree)
    if kind(ex) == K"return"
        return ex
    elseif !is_leaf(ex) && !(kind(ex) in KSet"quote inert meta function ->")
        for e in children(ex)
            r = find_return(e)
            if !isnothing(r)
                return r
            end
        end
    else
        return nothing
    end
end

function check_no_return(ex)
    r = find_return(ex)
    if !isnothing(r)
        throw(LoweringError(r, "`return` not allowed inside comprehension or generator"))
    end
end

# Return true for nested tuples of the same identifiers
function similar_tuples_or_identifiers(a, b)
    if kind(a) == K"tuple" && kind(b) == K"tuple"
        return numchildren(a) == numchildren(b) &&
            all( ((x,y),)->similar_tuples_or_identifiers(x,y),
                zip(children(a), children(b)))
    else
        is_same_identifier_like(a,b)
    end
end

# Return the anonymous function taking an iterated value, for use with the
# first argument to `Base.Generator`
function func_for_generator(ctx, body, iter_value_destructuring)
    if similar_tuples_or_identifiers(iter_value_destructuring, body)
        # Use Base.identity for generators which are filters such as
        # `(x for x in xs if f(x))`. This avoids creating a new type.
        @ast ctx body "identity"::K"top"
    else
        @ast ctx body [K"->"
            [K"tuple"
                iter_value_destructuring
            ]
            [K"block"
                body
            ]
        ]
    end
end

function expand_generator(ctx, ex)
    @chk numchildren(ex) >= 2
    body = ex[1]
    check_no_return(body)
    if numchildren(ex) > 2
        # Uniquify outer vars by NameKey
        outervars_by_key = Dict{NameKey,typeof(ex)}()
        for iterspecs in ex[2:end-1]
            for iterspec in children(iterspecs)
                foreach_lhs_name(iterspec[1]) do var
                    @assert kind(var) == K"Identifier" # Todo: K"BindingId"?
                    outervars_by_key[NameKey(var)] = var
                end
            end
        end
        outervar_assignments = SyntaxList(ctx)
        for (k,v) in sort(collect(pairs(outervars_by_key)), by=first)
            push!(outervar_assignments, @ast ctx v [K"=" v v])
        end
        body = @ast ctx ex [K"let"
            [K"block"
                outervar_assignments...
            ]
            [K"block"
                body
            ]
        ]
    end
    for iterspecs_ind in numchildren(ex):-1:2
        iterspecs = ex[iterspecs_ind]
        filter_test = nothing
        if kind(iterspecs) == K"filter"
            filter_test = iterspecs[2]
            iterspecs = iterspecs[1]
        end
        if kind(iterspecs) != K"iteration"
            throw(LoweringError("""Expected `K"iteration"` iteration specification in generator"""))
        end
        iter_ranges = SyntaxList(ctx)
        iter_lhss = SyntaxList(ctx)
        for iterspec in children(iterspecs)
            @chk kind(iterspec) == K"in"
            @chk numchildren(iterspec) == 2
            push!(iter_lhss, iterspec[1])
            push!(iter_ranges, iterspec[2])
        end
        iter_value_destructuring = if numchildren(iterspecs) == 1
            iterspecs[1][1]
        else
            iter_lhss = SyntaxList(ctx)
            for iterspec in children(iterspecs)
                push!(iter_lhss, iterspec[1])
            end
            @ast ctx iterspecs [K"tuple" iter_lhss...]
        end
        iter = if length(iter_ranges) > 1
            @ast ctx iterspecs [K"call"
                "product"::K"top"
                iter_ranges...
            ]
        else
            iter_ranges[1]
        end
        if !isnothing(filter_test)
            iter = @ast ctx ex [K"call"
                "Filter"::K"top"
                func_for_generator(ctx, filter_test, iter_value_destructuring)
                iter
            ]
        end
        body = @ast ctx ex [K"call"
            "Generator"::K"top"
            func_for_generator(ctx, body, iter_value_destructuring)
            iter
        ]
        if iterspecs_ind < numchildren(ex)
            body = @ast ctx ex [K"call"
                "Flatten"::K"top"
                body
            ]
        end
    end
    body
end

function expand_comprehension_to_loops(ctx, ex)
    @assert kind(ex) == K"typed_comprehension"
    element_type = ex[1]
    gen = ex[2]
    @assert kind(gen) == K"generator"
    body = gen[1]
    check_no_return(body)
    # TODO: check_no_break_continue
    iterspecs = gen[2]
    @assert kind(iterspecs) == K"iteration"
    new_iterspecs = SyntaxList(ctx)
    iters = SyntaxList(ctx)
    iter_defs = SyntaxList(ctx)
    for iterspec in children(iterspecs)
        iter = emit_assign_tmp(iter_defs, ctx, iterspec[2], "iter")
        push!(iters, iter)
        push!(new_iterspecs, @ast ctx iterspec [K"in" iterspec[1] iter])
    end
    # Lower to nested for loops
    idx = new_local_binding(ctx, iterspecs, "idx")
    @ast ctx ex [K"block"
        iter_defs...
        full_iter := if length(iters) == 1
            iters[1]
        else
            [K"call"
                "product"::K"top"
                iters...
            ]
        end
        iter_size := [K"call" "IteratorSize"::K"top" full_iter]
        size_unknown := [K"call" "isa"::K"core" iter_size "SizeUnknown"::K"top"]
        result    := [K"call" "_array_for"::K"top" element_type full_iter iter_size]
        [K"=" idx [K"call" "first"::K"top" [K"call" "LinearIndices"::K"top" result]]]
        [K"for" [K"iteration" Iterators.reverse(new_iterspecs)...]
            [K"block"
                val := body
                # TODO: inbounds setindex
                [K"if" size_unknown
                    [K"call" "push!"::K"top" result val]
                    [K"call" "setindex!"::K"top" result val idx]
                ]
                #[K"call" "println"::K"top" [K"call" "typeof"::K"core" idx]]
                [K"=" idx [K"call" "add_int"::K"top" idx 1::K"Integer"]]
            ]
        ]
        result
    ]
end

# Mimics native lowerer's tuple-wrap function (julia-syntax.scm:2723-2736)
# Unwraps only ONE layer of `...` and wraps sequences of non-splat args in tuples.
# Example: `[a, b, xs..., c]` -> `[tuple(a, b), xs, tuple(c)]`
function _wrap_unsplatted_args(ctx, call_ex, args)
    result = SyntaxList(ctx)
    non_splat_run = SyntaxList(ctx)
    for arg in args
        if kind(arg) == K"..."
            # Flush any accumulated non-splat args
            if !isempty(non_splat_run)
                push!(result, @ast ctx call_ex [K"call" "tuple"::K"core" non_splat_run...])
                non_splat_run = SyntaxList(ctx)
            end
            # Unwrap only ONE layer of `...` (corresponds to (cadr x) in native lowerer)
            push!(result, arg[1])
        else
            # Accumulate non-splat args
            push!(non_splat_run, arg)
        end
    end
    # Flush any remaining non-splat args
    if !isempty(non_splat_run)
        push!(result, @ast ctx call_ex [K"call" "tuple"::K"core" non_splat_run...])
    end
    result
end

function expand_splat(ctx, ex, topfunc, args)
    # Matches native lowerer's algorithm
    # https://github.com/JuliaLang/julia/blob/f362f47338de099cdeeb1b2d81b3ec1948443274/src/julia-syntax.scm#L2761-2762:
    # 1. Unwrap one layer of `...` from each argument (via _wrap_unsplatted_args)
    # 2. Create `_apply_iterate(iterate, f, wrapped_args...)` WITHOUT expanding args yet
    # 3. Recursively expand the entire call - if any wrapped_arg still contains `...`,
    #    the recursive expansion will handle it, naturally building nested structure
    #
    # Example: tuple((xs...)...) recursion:
    #   Pass 1: unwrap outer `...` -> _apply_iterate(iterate, tuple, (xs...))
    #   Pass 2: expand sees (xs...) in call context, unwraps again
    #           -> _apply_iterate(iterate, _apply_iterate, tuple(iterate, tuple), xs)

    wrapped_args = _wrap_unsplatted_args(ctx, ex, args)

    # Construct the unevaluated _apply_iterate call
    result = @ast ctx ex [K"call"
        "_apply_iterate"::K"core"
        "iterate"::K"top"
        topfunc
        wrapped_args...
    ]

    # Recursively expand the entire call (matching native's expand-forms)
    return expand_forms_2(ctx, result)
end

function expand_array(ctx, ex, topfunc)
    args = children(ex)
    check_no_assignment(args)
    topfunc = @ast ctx ex topfunc::K"top"
    if any(kind(arg) == K"..." for arg in args)
        expand_splat(ctx, ex, topfunc, args)
    else
        @ast ctx ex [K"call"
            topfunc
            expand_forms_2(ctx, args)...
        ]
    end
end

#-------------------------------------------------------------------------------
# Expansion of array concatenation notation `[a b ; c d]` etc

function expand_vcat(ctx, ex)
    check_no_parameters(ex, "unexpected semicolon in array expression")
    check_no_assignment(children(ex))
    had_row = false
    had_row_splat = false
    is_typed = kind(ex) == K"typed_vcat"
    eltype   = is_typed ? ex[1]     : nothing
    elements = is_typed ? ex[2:end] : ex[1:end]
    for e in elements
        k = kind(e)
        if k == K"row"
            had_row = true
            had_row_splat = had_row_splat || any(kind(e1) == K"..." for e1 in children(e))
        end
    end
    if had_row_splat
        # In case there is splatting inside `hvcat`, collect each row as a
        # separate tuple and pass those to `hvcat_rows` instead (ref #38844)
        rows = SyntaxList(ctx)
        for e in elements
            if kind(e) == K"row"
                push!(rows, @ast ctx e [K"tuple" children(e)...])
            else
                push!(rows, @ast ctx e [K"tuple" e])
            end
        end
        fname = is_typed ? "typed_hvcat_rows" : "hvcat_rows"
        @ast ctx ex [K"call"
            fname::K"top"
            eltype
            rows...
        ]
    else
        row_sizes = SyntaxList(ctx)
        flat_elems = SyntaxList(ctx)
        for e in elements
            if kind(e) == K"row"
                rowsize = numchildren(e)
                append!(flat_elems, children(e))
            else
                rowsize = 1
                push!(flat_elems, e)
            end
            push!(row_sizes, @ast ctx e rowsize::K"Integer")
        end
        if had_row
            fname = is_typed ? "typed_hvcat" : "hvcat"
            @ast ctx ex [K"call"
                fname::K"top"
                eltype
                [K"tuple" row_sizes...]
                flat_elems...
            ]
        else
            fname = is_typed ? "typed_vcat" : "vcat"
            @ast ctx ex [K"call"
                fname::K"top"
                eltype
                flat_elems...
            ]
        end
    end
end

function ncat_contains_row(ex)
    k = kind(ex)
    if k == K"row"
        return true
    elseif k == K"nrow"
        return any(ncat_contains_row(e) for e in children(ex))
    else
        return false
    end
end

# flip first and second dimension for row major layouts
function nrow_flipdim(row_major, d)
    return !row_major ? d :
           d == 1     ? 2 :
           d == 2     ? 1 : d
end

function flatten_ncat_rows!(flat_elems, nrow_spans, row_major, parent_layout_dim, ex)
    # Note that most of the checks for valid nesting here are also checked in
    # the parser - they can only fail when nrcat is constructed
    # programmatically (eg, by a macro).
    k = kind(ex)
    if k == K"row"
        layout_dim = 1
        @chk parent_layout_dim != 1 (ex,"Badly nested rows in `ncat`")
    elseif k == K"nrow"
        dim = numeric_flags(ex)
        @chk dim > 0                (ex,"Unsupported dimension $dim in ncat")
        @chk !row_major || dim != 2 (ex,"2D `nrow` cannot be mixed with `row` in `ncat`")
        layout_dim = nrow_flipdim(row_major, dim)
    elseif kind(ex) == K"..."
        throw(LoweringError(ex, "Splatting ... in an `ncat` with multiple dimensions is not supported"))
    else
        push!(flat_elems, ex)
        for ld in parent_layout_dim-1:-1:1
            push!(nrow_spans, (ld, 1))
        end
        return
    end
    row_start = length(flat_elems)
    @chk parent_layout_dim > layout_dim (ex, "Badly nested rows in `ncat`")
    for e in children(ex)
        if layout_dim == 1
            @chk kind(e) ∉ KSet"nrow row" (e,"Badly nested rows in `ncat`")
        end
        flatten_ncat_rows!(flat_elems, nrow_spans, row_major, layout_dim, e)
    end
    n_elems_in_row = length(flat_elems) - row_start
    for ld in parent_layout_dim-1:-1:layout_dim
        push!(nrow_spans, (ld, n_elems_in_row))
    end
end

# ncat comes in various layouts which we need to lower to special cases
# - one dimensional along some dimension
# - balanced column first or row first
# - ragged column first or row first
function expand_ncat(ctx, ex)
    is_typed = kind(ex) == K"typed_ncat"
    outer_dim = numeric_flags(ex)
    @chk outer_dim > 0 (ex,"Unsupported dimension in ncat")
    eltype      = is_typed ? ex[1]     : nothing
    elements    = is_typed ? ex[2:end] : ex[1:end]
    hvncat_name = is_typed ? "typed_hvncat" : "hvncat"
    if !any(kind(e) in KSet"row nrow" for e in elements)
        # One-dimensional ncat along some dimension
        #   [a ;;; b ;;; c]
        return @ast ctx ex [K"call"
            hvncat_name::K"top"
            eltype
            outer_dim::K"Integer"
            elements...
        ]
    end
    # N-dimensional case. May be
    # * column first or row first:
    #   [a;b ;;; c;d]
    #   [a b ;;; c d]
    # * balanced or ragged:
    #   [a ; b ;;; c ; d]
    #   [a ; b ;;; c]
    row_major = any(ncat_contains_row, elements)
    @chk !row_major || outer_dim != 2 (ex,"2D `nrow` cannot be mixed with `row` in `ncat`")
    flat_elems = SyntaxList(ctx)
    # `ncat` syntax nests lower dimensional `nrow` inside higher dimensional
    # ones (with the exception of K"row" when `row_major` is true). Each nrow
    # spans a number of elements and we first extract that.
    nrow_spans = Vector{Tuple{Int,Int}}()
    for e in elements
        flatten_ncat_rows!(flat_elems, nrow_spans, row_major,
                           nrow_flipdim(row_major, outer_dim), e)
    end
    push!(nrow_spans, (outer_dim, length(flat_elems)))
    # Construct the shape specification by postprocessing the flat list of
    # spans.
    sort!(nrow_spans, by=first) # depends on a stable sort
    is_balanced = true
    i = 1
    dim_lengths = zeros(outer_dim)
    prev_dimspan = 1
    while i <= length(nrow_spans)
        layout_dim, dimspan = nrow_spans[i]
        while i <= length(nrow_spans) && nrow_spans[i][1] == layout_dim
            if dimspan != nrow_spans[i][2]
                is_balanced = false
                break
            end
            i += 1
        end
        is_balanced || break
        @assert dimspan % prev_dimspan == 0
        dim_lengths[layout_dim] = dimspan ÷ prev_dimspan
        prev_dimspan = dimspan
    end
    shape_spec = SyntaxList(ctx)
    if is_balanced
        if row_major
            dim_lengths[1], dim_lengths[2] = dim_lengths[2], dim_lengths[1]
        end
        # For balanced concatenations, the shape is specified by the length
        # along each dimension.
        for dl in dim_lengths
            push!(shape_spec, @ast ctx ex dl::K"Integer")
        end
    else
        # For unbalanced/ragged concatenations, the shape is specified by the
        # number of elements in each N-dimensional slice of the array, from layout
        # dimension 1 to N. See the documentation for `hvncat` for details.
        i = 1
        while i <= length(nrow_spans)
            groups_for_dim = Int[]
            layout_dim = nrow_spans[i][1]
            while i <= length(nrow_spans) && nrow_spans[i][1] == layout_dim
                push!(groups_for_dim, nrow_spans[i][2])
                i += 1
            end
            push!(shape_spec,
                @ast ctx ex [K"tuple"
                    [i::K"Integer" for i in groups_for_dim]...
                ]
            )
        end
    end
    @ast ctx ex [K"call"
        hvncat_name::K"top"
        eltype
        [K"tuple" shape_spec...]
        row_major::K"Bool"
        flat_elems...
    ]
end

#-------------------------------------------------------------------------------
# Expand assignments

# Expand UnionAll definitions, eg `X{T} = Y{T,T}`
function expand_unionall_def(ctx, srcref, lhs, rhs, is_const=true)
    if numchildren(lhs) <= 1
        throw(LoweringError(lhs, "empty type parameter list in type alias"))
    end
    name = lhs[1]
    expand_forms_2(
        ctx,
        @ast ctx srcref [K"block"
            rr := [K"where" rhs lhs[2:end]...]
            [is_const ? K"constdecl" : K"assign_or_constdecl_if_global" name rr]
            [K"removable" rr]
        ]
    )
end

# Expand general assignment syntax, including
#   * UnionAll definitions
#   * Chained assignments
#   * Setting of structure fields
#   * Assignments to array elements
#   * Destructuring
#   * Typed variable declarations
function expand_assignment(ctx, ex, is_const=false)
    @chk numchildren(ex) == 2
    lhs = ex[1]
    rhs = ex[2]
    kl = kind(lhs)
    if kind(ex) == K"function"
        # `const f() = ...` - The `const` here is inoperative, but the syntax
        # happened to work in earlier versions, so simply strip `const`.
        expand_forms_2(ctx, ex[1])
    elseif kl == K"curly"
        expand_unionall_def(ctx, ex, lhs, rhs, is_const)
    elseif kind(rhs) == K"="
        # Expand chains of assignments
        # a = b = c  ==>  b=c; a=c
        stmts = SyntaxList(ctx)
        push!(stmts, lhs)
        while kind(rhs) == K"="
            push!(stmts, rhs[1])
            rhs = rhs[2]
        end
        if is_identifier_like(rhs)
            tmp_rhs = nothing
            rr = rhs
        else
            tmp_rhs = ssavar(ctx, rhs, "rhs")
            rr = tmp_rhs
        end
        # In const a = b = c, only a is const
        stmts[1] = @ast ctx ex [(is_const ? K"constdecl" : K"=") stmts[1] rr]
        for i in 2:length(stmts)
            stmts[i] = @ast ctx ex [K"=" stmts[i] rr]
        end
        if !isnothing(tmp_rhs)
            pushfirst!(stmts, @ast ctx ex [K"=" tmp_rhs rhs])
        end
        expand_forms_2(ctx,
            @ast ctx ex [K"block"
                stmts...
                [K"removable" rr]
            ]
        )
    elseif is_identifier_like(lhs)
        if is_const
            @ast ctx ex [K"block"
                rr := expand_forms_2(ctx, rhs)
                [K"constdecl" lhs rr]
                [K"removable" rr]
            ]
        else
            sink_assignment(ctx, ex, lhs, expand_forms_2(ctx, rhs))
        end
    elseif kl == K"."
        # a.b = rhs  ==>  setproperty!(a, :b, rhs)
        @chk !is_const (ex, "cannot declare `.` form const")
        @chk numchildren(lhs) == 2
        a = lhs[1]
        b = lhs[2]
        stmts = SyntaxList(ctx)
        # TODO: Do we need these first two temporaries?
        if !is_identifier_like(a)
            a = emit_assign_tmp(stmts, ctx, expand_forms_2(ctx, a), "a_tmp")
        end
        if kind(b) != K"Symbol"
            b = emit_assign_tmp(stmts, ctx, expand_forms_2(ctx, b), "b_tmp")
        end
        if !is_identifier_like(rhs) && !is_literal(rhs)
            rhs = emit_assign_tmp(stmts, ctx, expand_forms_2(ctx, rhs), "rhs_tmp")
        end
        @ast ctx ex [K"block"
            stmts...
            [K"call" "setproperty!"::K"top" a b rhs]
            [K"removable" rhs]
        ]
    elseif kl == K"tuple"
        if has_parameters(lhs)
            expand_property_destruct(ctx, ex, is_const)
        else
            expand_tuple_destruct(ctx, ex, is_const)
        end
    elseif kl == K"ref"
        # a[i1, i2] = rhs
        @chk !is_const (ex, "cannot declare ref form const")
        expand_forms_2(ctx, expand_setindex(ctx, ex))
    elseif kl == K"::" && numchildren(lhs) == 2
        x = lhs[1]
        T = lhs[2]
        res = if is_const
            expand_forms_2(ctx, @ast ctx ex [K"const"
                [K"="
                     lhs[1]
                     convert_for_type_decl(ctx, ex, rhs, T, true)
                 ]])
        elseif is_identifier_like(x)
            # Identifier in lhs[1] is a variable type declaration, eg
            # x::T = rhs
            @ast ctx ex [K"block"
                [K"decl" lhs[1] lhs[2]]
                is_const ? [K"const" [K"=" lhs[1] rhs]] : [K"=" lhs[1] rhs]
            ]
        else
            # Otherwise just a type assertion, eg
            # a[i]::T = rhs  ==>  (a[i]::T; a[i] = rhs)
            # a[f(x)]::T = rhs  ==>  (tmp = f(x); a[tmp]::T; a[tmp] = rhs)
            stmts = SyntaxList(ctx)
            l1 = remove_argument_side_effects(ctx, stmts, lhs[1])
            # TODO: What about (f(z),y)::T = rhs? That's broken syntax and
            # needs to be detected somewhere but won't be detected here. Maybe
            # it shows that remove_argument_side_effects() is not the ideal
            # solution here?
            # TODO: handle underscore?
            @ast ctx ex [K"block"
                stmts...
                [K"::" l1 lhs[2]]
                [K"=" l1 rhs]
            ]
        end
        expand_forms_2(ctx, res)
    elseif kl == K"dotcall"
        throw(LoweringError(lhs, "invalid dot call syntax on left hand side of assignment"))
    elseif kl == K"typed_hcat"
        throw(LoweringError(lhs, "invalid spacing in left side of indexed assignment"))
    elseif kl == K"typed_vcat" || kl == K"typed_ncat"
        throw(LoweringError(lhs, "unexpected `;` in left side of indexed assignment"))
    elseif kl == K"vect" || kl == K"hcat" || kl == K"vcat" || kl == K"ncat"
        throw(LoweringError(lhs, "use `(a, b) = ...` to assign multiple values"))
    else
        throw(LoweringError(lhs, "invalid assignment location"))
    end
end

function expand_update_operator(ctx, ex)
    k = kind(ex)
    dotted = k == K".op="

    @chk numchildren(ex) == 3
    lhs = ex[1]
    op = ex[2]
    rhs = ex[3]

    stmts = SyntaxList(ctx)

    declT = nothing
    if kind(lhs) == K"::"
        # eg `a[i]::T += 1`
        declT = lhs[2]
        decl_lhs = lhs
        lhs = lhs[1]
    end

    if kind(lhs) == K"ref"
        # eg `a[end] = rhs`
        sctx = with_stmts(ctx, stmts)
        (arr, idxs) = expand_ref_components(sctx, lhs)
        lhs = @ast ctx lhs [K"ref" arr idxs...]
    end

    lhs = remove_argument_side_effects(ctx, stmts, lhs)

    if dotted
        if !(kind(lhs) == K"ref" || (kind(lhs) == K"." && numchildren(lhs) == 2))
            # `f() .+= rhs`
            lhs = emit_assign_tmp(stmts, ctx, lhs)
        end
    else
        if kind(lhs) == K"tuple" && contains_ssa_binding(ctx, lhs)
            # If remove_argument_side_effects needed to replace an expression
            # with an ssavalue, then it can't be updated by assignment
            # (JuliaLang/julia#30062)
            throw(LoweringError(lhs, "invalid multiple assignment location"))
        end
    end

    @ast ctx ex [K"block"
        stmts...
        [(dotted ? K".=" : K"=")
            lhs
            [(dotted ? K"dotcall" : K"call")
                op
                if isnothing(declT)
                    lhs
                else
                    [K"::"(decl_lhs) lhs declT]
                end
                rhs
            ]
        ]
    ]
end

#-------------------------------------------------------------------------------
# Expand logical conditional statements

# Flatten nested && or || nodes and expand their children
function expand_cond_children(ctx, ex, cond_kind=kind(ex), flat_children=SyntaxList(ctx))
    for e in children(ex)
        if kind(e) == cond_kind
            expand_cond_children(ctx, e, cond_kind, flat_children)
        else
            push!(flat_children, expand_forms_2(ctx, e))
        end
    end
    flat_children
end

# Expand condition in, eg, `if` or `while`
function expand_condition(ctx, ex)
    isblock = kind(ex) == K"block"
    test = isblock ? ex[end] : ex
    k = kind(test)
    if k == K"&&" || k == K"||"
        # `||` and `&&` get special lowering so that they compile directly to
        # jumps rather than first computing a bool and then jumping.
        cs = expand_cond_children(ctx, test)
        @assert length(cs) > 1
        test = makenode(ctx, test, k, cs)
    else
        test = expand_forms_2(ctx, test)
    end
    if isblock
        # Special handling so that the rules for `&&` and `||` can be applied
        # to the last statement of a block
        @ast ctx ex [K"block" map(e->expand_forms_2(ctx,e), ex[1:end-1])... test]
    else
        test
    end
end

#-------------------------------------------------------------------------------
# Expand let blocks

function expand_let(ctx, ex)
    @chk numchildren(ex) == 2
    bindings = ex[1]
    @chk kind(bindings) == K"block"
    blk = ex[2]
    scope_type = get(ex, :scope_type, :hard)
    if numchildren(bindings) == 0
        return @ast ctx ex [K"scope_block"(scope_type=scope_type) blk]
    end
    for binding in Iterators.reverse(children(bindings))
        kb = kind(binding)
        if is_sym_decl(kb)
            blk = @ast ctx ex [K"scope_block"(scope_type=scope_type)
                [K"local" binding]
                blk
            ]
        elseif kb == K"=" && numchildren(binding) == 2
            lhs = binding[1]
            rhs = binding[2]
            kl = kind(lhs)
            if kl == K"Identifier" || kl == K"BindingId"
                blk = @ast ctx binding [K"block"
                    tmp := rhs
                    [K"scope_block"(ex, scope_type=scope_type)
                        [K"local"(lhs) lhs]
                        [K"always_defined" lhs]
                        [K"="(binding) lhs tmp]
                        blk
                    ]
                ]
            elseif kl == K"::"
                var = lhs[1]
                if !(kind(var) in KSet"Identifier BindingId")
                    throw(LoweringError(var, "Invalid assignment location in let syntax"))
                end
                blk = @ast ctx binding [K"block"
                    tmp := rhs
                    type := lhs[2]
                    [K"scope_block"(ex, scope_type=scope_type)
                        [K"local"(lhs) [K"::" var type]]
                        [K"always_defined" var]
                        [K"="(binding) var tmp]
                        blk
                    ]
                ]
            elseif kind(lhs) == K"tuple"
                lhs_locals = SyntaxList(ctx)
                foreach_lhs_name(lhs) do var
                    push!(lhs_locals, @ast ctx var [K"local" var])
                    push!(lhs_locals, @ast ctx var [K"always_defined" var])
                end
                blk = @ast ctx binding [K"block"
                    tmp := rhs
                    [K"scope_block"(ex, scope_type=scope_type)
                        lhs_locals...
                        [K"="(binding) lhs tmp]
                        blk
                    ]
                ]
            else
                throw(LoweringError(lhs, "Invalid assignment location in let syntax"))
            end
        elseif kind(binding) == K"function"
            sig = binding[1]
            func_name = assigned_function_name(sig)
            if isnothing(func_name)
                # Some valid function syntaxes define methods on existing types and
                # don't really make sense with let:
                #    let A.f() = 1 ... end
                #    let (obj::Callable)() = 1 ... end
                throw(LoweringError(sig, "Function signature does not define a local function name"))
            end
            blk = @ast ctx binding [K"block"
                [K"scope_block"(ex, scope_type=scope_type)
                    [K"local"(func_name) func_name]
                    [K"always_defined" func_name]
                    binding
                    [K"scope_block"(ex, scope_type=scope_type)
                        # The inside of the block is isolated from the closure,
                        # which itself can only capture values from the outside.
                        blk
                    ]
                ]
            ]
        else
            throw(LoweringError(binding, "Invalid binding in let"))
            continue
        end
    end
    return blk
end

#-------------------------------------------------------------------------------
# Expand named tuples

function _named_tuple_expr(ctx, srcref, names, values)
    if isempty(names)
        @ast ctx srcref [K"call" "NamedTuple"::K"core"]
    else
        @ast ctx srcref [K"call"
            [K"curly" "NamedTuple"::K"core" [K"tuple" names...]]
            # NOTE: don't use `tuple` head, so an assignment expression as a value
            # doesn't turn this into another named tuple.
            [K"call" "tuple"::K"core" values...]
        ]
    end
end

function _merge_named_tuple(ctx, srcref, old, new)
    if isnothing(old)
        new
    else
        @ast ctx srcref [K"call" "merge"::K"top" old new]
    end
end

function expand_named_tuple(ctx, ex, kws;
                            field_name="named tuple field",
                            element_name="named tuple element")
    name_strs = Set{String}()
    names = SyntaxList(ctx)
    values = SyntaxList(ctx)
    current_nt = nothing
    for (i,kw) in enumerate(kws)
        k = kind(kw)
        appended_nt = nothing
        name = nothing
        if kind(k) == K"Identifier"
            # x  ==>  x = x
            name = to_symbol(ctx, kw)
            value = kw
        elseif k == K"="
            # x = a
            if kind(kw[1]) != K"Identifier" && kind(kw[1]) != K"Placeholder"
                throw(LoweringError(kw[1], "invalid $field_name name"))
            end
            if kind(kw[2]) == K"..."
                throw(LoweringError(kw[2], "`...` cannot be used in a value for a $field_name"))
            end
            name = to_symbol(ctx, kw[1])
            value = kw[2]
        elseif k == K"."
            # a.x ==> x=a.x
            if kind(kw[2]) != K"Symbol"
                throw(LoweringError(kw, "invalid $element_name"))
            end
            name = to_symbol(ctx, kw[2])
            value = kw
        elseif k == K"call" && is_infix_op_call(kw) && numchildren(kw) == 3 &&
                is_same_identifier_like(kw[1], "=>")
            # a=>b   ==>  $a=b
            appended_nt = _named_tuple_expr(ctx, kw, (kw[2],), (kw[3],))
            nothing, nothing
        elseif k == K"..."
            # args...  ==> splat pairs
            appended_nt = kw[1]
            if isnothing(current_nt) && isempty(names)
                # Must call merge to create NT from an initial splat
                current_nt = _named_tuple_expr(ctx, ex, (), ())
            end
            nothing, nothing
        else
            throw(LoweringError(kw, "Invalid $element_name"))
        end
        if !isnothing(name)
            if kind(name) == K"Symbol"
                name_str = name.name_val
                if name_str in name_strs
                    throw(LoweringError(name, "Repeated $field_name name"))
                end
                push!(name_strs, name_str)
            end
            push!(names, name)
            push!(values, value)
        end
        if !isnothing(appended_nt)
            if !isempty(names)
                current_nt = _merge_named_tuple(ctx, ex, current_nt,
                                                _named_tuple_expr(ctx, ex, names, values))
                empty!(names)
                empty!(values)
            end
            current_nt = _merge_named_tuple(ctx, ex, current_nt, appended_nt)
        end
    end
    if !isempty(names) || isnothing(current_nt)
        current_nt = _merge_named_tuple(ctx, ex, current_nt,
                                        _named_tuple_expr(ctx, ex, names, values))
    end
    @assert !isnothing(current_nt)
    current_nt
end

#-------------------------------------------------------------------------------
# Call expansion

function expand_kw_call(ctx, srcref, farg, args, kws)
    @ast ctx srcref [K"block"
        func := farg
        kw_container := expand_named_tuple(ctx, srcref, kws;
                                           field_name="keyword argument",
                                           element_name="keyword argument")
        if all(kind(kw) == K"..." for kw in kws)
            # In this case need to check kws nonempty at runtime
            [K"if"
                [K"call" "isempty"::K"top" kw_container]
                [K"call" func args...]
                [K"call" "kwcall"::K"core" kw_container func args...]
            ]
        else
            [K"call" "kwcall"::K"core" kw_container func args...]
        end
    ]
end

# Expand the (sym,lib) argument to ccall/cglobal
function expand_C_library_symbol(ctx, ex)
    expanded = expand_forms_2(ctx, ex)
    if kind(ex) == K"tuple"
        expanded = @ast ctx ex [K"static_eval"(meta=name_hint("function name and library expression"))
            expanded
        ]
    end
    return expanded
end

function expand_ccall(ctx, ex)
    @assert kind(ex) == K"call" && is_core_ref(ex[1], "ccall")
    if numchildren(ex) < 4
        throw(LoweringError(ex, "too few arguments to ccall"))
    end
    cfunc_name = ex[2]
    # Detect calling convention if present.
    known_conventions = ("cdecl", "stdcall", "fastcall", "thiscall", "llvmcall")
    cconv = if any(is_same_identifier_like(ex[3], id) for id in known_conventions)
        ex[3]
    end
    if isnothing(cconv)
        rt_idx = 3
    else
        rt_idx = 4
        if numchildren(ex) < 5
            throw(LoweringError(ex, "too few arguments to ccall with calling convention specified"))
        end
    end
    return_type = ex[rt_idx]
    arg_type_tuple = ex[rt_idx+1]
    args = ex[rt_idx+2:end]
    if kind(arg_type_tuple) != K"tuple"
        msg = "ccall argument types must be a tuple; try `(T,)`"
        if kind(return_type) == K"tuple"
            throw(LoweringError(return_type, msg*" and check if you specified a correct return type"))
        else
            throw(LoweringError(arg_type_tuple, msg))
        end
    end
    arg_types = children(arg_type_tuple)
    vararg_type = nothing
    if length(arg_types) >= 1
        va = arg_types[end]
        if kind(va) == K"..."
            @chk numchildren(va) == 1
            # Ok: vararg function
            vararg_type = va
        end
    end
    # todo: use multi-range errors here
    if length(args) < length(arg_types)
        throw(LoweringError(ex, "Too few arguments in ccall compared to argument types"))
    elseif length(args) > length(arg_types) && isnothing(vararg_type)
        throw(LoweringError(ex, "More arguments than types in ccall"))
    end
    if isnothing(vararg_type)
        num_required_args = 0
    else
        num_required_args = length(arg_types) - 1
        if num_required_args < 1
            throw(LoweringError(vararg_type, "C ABI prohibits vararg without one required argument"))
        end
    end
    sctx = with_stmts(ctx)
    expanded_types = SyntaxList(ctx)
    for (i, argt) in enumerate(arg_types)
        if kind(argt) == K"..."
            if i == length(arg_types)
                argt = argt[1]
            else
                throw(LoweringError(argt, "only the trailing ccall argument type should have `...`"))
            end
        end
        if is_same_identifier_like(argt, "Any")
            # Special rule: Any becomes core.Any regardless of the module
            # scope, and don't need GC roots.
            argt = @ast ctx argt "Any"::K"core"
        end
        push!(expanded_types, expand_forms_2(ctx, argt))
    end
    #
    # An improvement might be wrap the use of types in cconvert in a special
    # K"global_scope" expression which modifies the scope resolution. This
    # would at least make the rules self consistent if not pretty.
    #
    # One small improvement we make here is to emit temporaries for all the
    # types used during expansion so at least we don't have their side effects
    # more than once.
    types_for_conv = SyntaxList(ctx)
    for argt in expanded_types
        push!(types_for_conv, emit_assign_tmp(sctx, argt))
    end
    gc_roots = SyntaxList(ctx)
    unsafe_args  = SyntaxList(ctx)
    for (i,arg) in enumerate(args)
        if i > length(expanded_types)
            raw_argt = expanded_types[end]
            push!(expanded_types, raw_argt)
            argt = types_for_conv[end]
        else
            raw_argt = expanded_types[i]
            argt = types_for_conv[i]
        end
        exarg = expand_forms_2(ctx, arg)
        if is_core_Any(raw_argt)
            push!(unsafe_args, exarg)
        else
            cconverted_arg = emit_assign_tmp(sctx,
                @ast ctx argt [K"call"
                    "cconvert"::K"top"
                    argt
                    exarg
                ]
            )
            push!(gc_roots, cconverted_arg)
            push!(unsafe_args,
                @ast ctx argt [K"call"
                    "unsafe_convert"::K"top"
                    argt
                    cconverted_arg
                ]
            )
        end
    end
    @ast ctx ex [K"block"
        sctx.stmts...
        [K"foreigncall"
            expand_C_library_symbol(ctx, cfunc_name)
            [K"static_eval"(meta=name_hint("ccall return type"))
                expand_forms_2(ctx, return_type)
            ]
            [K"static_eval"(meta=name_hint("ccall argument type"))
                [K"call"
                    "svec"::K"core"
                    expanded_types...
                ]
            ]
            num_required_args::K"Integer"
            if isnothing(cconv)
                "ccall"::K"Symbol"
            else
                cconv=>K"Symbol"
            end
            unsafe_args...
            gc_roots... # GC roots
        ]
    ]
end

function remove_kw_args!(ctx, args::SyntaxList)
    kws = nothing
    j = 0
    num_parameter_blocks = 0
    for i in 1:length(args)
        arg = args[i]
        k = kind(arg)
        if k == K"="
            if isnothing(kws)
                kws = SyntaxList(ctx)
            end
            push!(kws, arg)
        elseif k == K"parameters"
            num_parameter_blocks += 1
            if num_parameter_blocks > 1
                throw(LoweringError(arg, "Cannot have more than one group of keyword arguments separated with `;`"))
            end
            if numchildren(arg) == 0
                continue # ignore empty parameters (issue #18845)
            end
            if isnothing(kws)
                kws = SyntaxList(ctx)
            end
            append!(kws, children(arg))
        else
            j += 1
            if j < i
                args[j] = args[i]
            end
        end
    end
    resize!(args, j)
    return kws
end

function expand_call(ctx, ex)
    farg = ex[1]
    if is_core_ref(farg, "ccall")
        return expand_ccall(ctx, ex)
    elseif is_core_ref(farg, "cglobal")
        @chk numchildren(ex) in 2:3  (ex, "cglobal must have one or two arguments")
        return @ast ctx ex [K"call"
            ex[1]
            expand_C_library_symbol(ctx, ex[2])
            if numchildren(ex) == 3
                expand_forms_2(ctx, ex[3])
            end
        ]
    end
    args = copy(ex[2:end])
    kws = remove_kw_args!(ctx, args)
    if !isnothing(kws)
        return expand_forms_2(ctx, expand_kw_call(ctx, ex, farg, args, kws))
    end
    if any(kind(arg) == K"..." for arg in args)
        # Splatting, eg, `f(a, xs..., b)`
        expand_splat(ctx, ex, expand_forms_2(ctx, farg), args)
    elseif kind(farg) == K"Identifier" && farg.name_val == "include"
        # world age special case
        r = ssavar(ctx, ex)
        @ast ctx ex [K"block"
            [K"=" r [K"call"
                expand_forms_2(ctx, farg)
                expand_forms_2(ctx, args)...
            ]]
            (::K"latestworld_if_toplevel")
            r
        ]
    else
        @ast ctx ex [K"call"
            expand_forms_2(ctx, farg)
            expand_forms_2(ctx, args)...
        ]
    end
end

#-------------------------------------------------------------------------------

function expand_dot(ctx, ex)
    @chk numchildren(ex) in (1,2)  (ex, "`.` form requires either one or two children")

    if numchildren(ex) == 1
        # eg, `f = .+`
        # Upstream TODO: Remove the (. +) representation and replace with use
        # of DOTOP_FLAG? This way, `K"."` will be exclusively used for
        # getproperty.
        @ast ctx ex [K"call"
            "BroadcastFunction"::K"top"
            ex[1]
        ]
    elseif numchildren(ex) == 2
        # eg, `x.a` syntax
        rhs = ex[2]
        # Required to support the possibly dubious syntax `a."b"`. See
        # https://github.com/JuliaLang/julia/issues/26873
        # Syntax edition TODO: reconsider this; possibly restrict to only K"String"?
        if !(kind(rhs) == K"string" || is_leaf(rhs))
            throw(LoweringError(rhs, "Unrecognized field access syntax"))
        end
        @ast ctx ex [K"call"
            "getproperty"::K"top"
            ex[1]
            rhs
        ]
    end
end

#-------------------------------------------------------------------------------
# Expand for loops

function expand_for(ctx, ex)
    iterspecs = ex[1]

    @chk kind(iterspecs) == K"iteration"

    # Loop variables not declared `outer` are reassigned for each iteration of
    # the innermost loop in case the user assigns them to something else.
    # (Maybe we should filter these to remove vars not assigned in the loop?
    # But that would ideally happen after the variable analysis pass, not
    # during desugaring.)
    copied_vars = SyntaxList(ctx)
    for iterspec in iterspecs[1:end-1]
        @chk kind(iterspec) == K"in"
        lhs = iterspec[1]
        if kind(lhs) != K"outer"
            foreach_lhs_name(lhs) do var
                push!(copied_vars, @ast ctx var [K"=" var var])
            end
        end
    end

    loop = ex[2]
    for i in numchildren(iterspecs):-1:1
        iterspec = iterspecs[i]
        lhs = iterspec[1]

        outer = kind(lhs) == K"outer"
        lhs_local_defs = SyntaxList(ctx)
        lhs_outer_defs = SyntaxList(ctx)
        if outer
            lhs = lhs[1]
        end
        foreach_lhs_name(lhs) do var
            if outer
                push!(lhs_outer_defs, @ast ctx var var)
            else
                push!(lhs_local_defs, @ast ctx var [K"local" var])
            end
        end

        iter_ex = iterspec[2]
        next = new_local_binding(ctx, iterspec, "next")
        state = ssavar(ctx, iterspec, "state")
        collection = ssavar(ctx, iter_ex, "collection")

        # Assign iteration vars and next state
        body = @ast ctx iterspec [K"block"
            lhs_local_defs...
            lower_tuple_assignment(ctx, iterspec, (lhs, state), next)
            loop
        ]

        body = if i == numchildren(iterspecs)
            # Innermost loop gets the continue label and copied vars
            @ast ctx ex [K"break_block"
                "loop_cont"::K"symbolic_label"
                [K"let"(scope_type=:neutral)
                     [K"block"
                         copied_vars...
                     ]
                     body
                ]
            ]
        else
            # Outer loops get a scope block to contain the iteration vars
            @ast ctx ex [K"scope_block"(scope_type=:neutral)
                body
            ]
        end

        loop = @ast ctx ex [K"block"
            if outer
                [K"assert"
                    "require_existing_locals"::K"Symbol"
                    lhs_outer_defs...
                ]
            end
            [K"="(iter_ex) collection iter_ex]
            # First call to iterate is unrolled
            #   next = top.iterate(collection)
            [K"="(iterspec) next [K"call" "iterate"::K"top" collection]]
            [K"if"(iterspec) # if next !== nothing
                [K"call"(iterspec)
                    "not_int"::K"top"
                    [K"call" "==="::K"core" next "nothing"::K"core"]
                ]
                [K"_do_while"(ex)
                    [K"block"
                        body
                        # Advance iterator
                        [K"="(iterspec) next [K"call" "iterate"::K"top" collection state]]
                    ]
                    [K"call"(iterspec)
                        "not_int"::K"top"
                        [K"call" "==="::K"core" next "nothing"::K"core"]
                    ]
                ]
            ]
        ]
    end

    @ast ctx ex [K"break_block" "loop_exit"::K"symbolic_label"
        loop
    ]
end

#-------------------------------------------------------------------------------
# Expand try/catch/finally

function match_try(ex)
    @chk numchildren(ex) > 1 "Invalid `try` form"
    try_ = ex[1]
    catch_ = nothing
    finally_ = nothing
    else_ = nothing
    for e in ex[2:end]
        k = kind(e)
        if k == K"catch" && isnothing(catch_)
            @chk numchildren(e) == 2 "Invalid `catch` form"
            catch_ = e
        elseif k == K"else" && isnothing(else_)
            @chk numchildren(e) == 1
            else_ = e[1]
        elseif k == K"finally" && isnothing(finally_)
            @chk numchildren(e) == 1
            finally_ = e[1]
        else
            throw(LoweringError(ex, "Invalid clause in `try` form"))
        end
    end
    (try_, catch_, else_, finally_)
end

function expand_try(ctx, ex)
    (try_, catch_, else_, finally_) = match_try(ex)

    if !isnothing(finally_)
        # TODO: check unmatched symbolic gotos in try.
    end

    try_body = @ast ctx try_ [K"scope_block"(scope_type=:neutral) try_]

    if isnothing(catch_)
        try_block = try_body
    else
        exc_var = catch_[1]
        catch_block = catch_[2]
        if !is_identifier_like(exc_var)
            throw(LoweringError(exc_var, "Expected an identifier as exception variable"))
        end
        try_block = @ast ctx ex [K"trycatchelse"
            try_body
            [K"scope_block"(catch_, scope_type=:neutral)
                if kind(exc_var) != K"Placeholder"
                    [K"block"
                        [K"="(exc_var) exc_var [K"call" current_exception::K"Value"]]
                        catch_block
                    ]
                else
                    catch_block
                end
            ]
            if !isnothing(else_)
                [K"scope_block"(else_, scope_type=:neutral) else_]
            end
        ]
    end

    if isnothing(finally_)
        try_block
    else
        @ast ctx ex [K"tryfinally"
            try_block
            [K"scope_block"(finally_, scope_type=:neutral) finally_]
        ]
    end
end

#-------------------------------------------------------------------------------
# Expand local/global/const declarations

# Create local/global declarations, and possibly type declarations for each name
# on an assignment LHS.  Works recursively with complex left hand side
# assignments containing tuple destructuring.  Eg, given
#   (x::T, (y::U, z))
#   strip out stmts = (local x) (decl x T) (local x) (decl y U) (local z)
#   and return (x, (y, z))
function make_lhs_decls(ctx, stmts, declkind, declmeta, ex, type_decls=true)
    k = kind(ex)
    if k == K"Identifier" || k == K"Value" && ex.value isa GlobalRef
        # TODO: consider removing support for Expr(:global, GlobalRef(...)) and
        # other Exprs that cannot be produced by the parser (tested by
        # test/precompile.jl #50538).
        if !isnothing(declmeta)
            push!(stmts, makenode(ctx, ex, declkind, ex; meta=declmeta))
        else
            push!(stmts, makenode(ctx, ex, declkind, ex))
        end
    elseif k == K"Placeholder"
        nothing
    elseif (k === K"::" && numchildren(ex) === 2) || k in KSet"call curly where"
        if type_decls
            @chk numchildren(ex) == 2
            name = ex[1]
            @chk kind(name) == K"Identifier"
            push!(stmts, makenode(ctx, ex, K"decl", name, ex[2]))
        end
        make_lhs_decls(ctx, stmts, declkind, declmeta, ex[1], type_decls)
    elseif k == K"tuple" || k == K"parameters"
        for e in children(ex)
            make_lhs_decls(ctx, stmts, declkind, declmeta, e, type_decls)
        end
    else
        throw(LoweringError(ex, "invalid kind $k in $declkind declaration"))
    end
end

# Separate decls and assignments (which require re-expansion)
# local x, (y=2), z ==> local x; local z; y = 2
function expand_decls(ctx, ex)
    declkind = kind(ex)
    @assert declkind in KSet"local global"
    declmeta = get(ex, :meta, nothing)
    bindings = children(ex)
    stmts = SyntaxList(ctx)
    for binding in bindings
        if is_prec_assignment(kind(binding))
            @chk numchildren(binding) == 2
            # expand_assignment will create the type decls
            make_lhs_decls(ctx, stmts, declkind, declmeta, binding[1], false)
            push!(stmts, expand_assignment(ctx, binding))
        elseif is_sym_decl(binding) || kind(binding) == K"Value"
            make_lhs_decls(ctx, stmts, declkind, declmeta, binding, true)
        elseif kind(binding) == K"function"
            make_lhs_decls(ctx, stmts, declkind, declmeta, binding[1], false)
            push!(stmts, expand_forms_2(ctx, binding))
        else
            throw(LoweringError(ex, "invalid syntax in variable declaration"))
        end
    end
    makenode(ctx, ex, K"block", stmts)
end

# Iterate over the variable names assigned to from a "fancy assignment left hand
# side" such as nested tuple destructuring, curlies, and calls.
function foreach_lhs_name(f::Function, ex)
    k = kind(ex)
    if k == K"Placeholder"
        # Ignored
    elseif is_identifier_like(ex)
        f(ex)
    elseif (k === K"::" && numchildren(ex) === 2) || k in KSet"call curly where"
        foreach_lhs_name(f, ex[1])
    elseif k in KSet"tuple parameters"
        for c in children(ex)
            foreach_lhs_name(f, c)
        end
    end
    return nothing
end

function expand_const_decl(ctx, ex)
    k = kind(ex[1])
    if k == K"global"
        asgn = ex[1][1]
        @chk (kind(asgn) == K"=" || kind(asgn) == K"function") (ex, "expected assignment after `const`")
        globals = SyntaxList(ctx)
        foreach_lhs_name(asgn[1]) do x
            push!(globals, @ast ctx ex [K"global" x])
        end
        @ast ctx ex [K"block"
            globals...
            expand_assignment(ctx, asgn, true)
        ]
    elseif k == K"=" || k == K"function"
        expand_assignment(ctx, ex[1], true)
    elseif k == K"local"
        throw(LoweringError(ex, "unsupported `const local` declaration"))
    elseif k == K"Identifier" || k == K"Value"
        # Expr(:const, v) where v is a Symbol or a GlobalRef is an unfortunate
        # remnant from the days when const-ness was a flag that could be set on
        # any global.  It creates a binding with kind PARTITION_KIND_UNDEF_CONST.
        # TODO: deprecate and delete this "feature"
        @chk numchildren(ex) == 1
        @ast ctx ex [K"constdecl" ex[1]]
    else
        throw(LoweringError(ex, "expected assignment after `const`"))
    end
end

#-------------------------------------------------------------------------------
# Expansion of function definitions

function expand_function_arg(ctx, body_stmts, arg, is_last_arg, is_kw)
    ex = arg

    if kind(ex) == K"="
        default = ex[2]
        ex = ex[1]
    else
        default = nothing
    end

    if kind(ex) == K"..."
        if !is_last_arg
            typmsg = is_kw ? "keyword" : "positional"
            throw(LoweringError(arg, "`...` may only be used for the last $typmsg argument"))
        end
        @chk numchildren(ex) == 1
        slurp_ex = ex
        ex = ex[1]
    else
        slurp_ex = nothing
    end

    if kind(ex) == K"::"
        @chk numchildren(ex) in (1,2)
        if numchildren(ex) == 1
            type = ex[1]
            ex = @ast ctx ex "_"::K"Placeholder"
        else
            type = ex[2]
            ex = ex[1]
        end
        if is_kw && !isnothing(slurp_ex)
            throw(LoweringError(slurp_ex, "keyword argument with `...` may not be given a type"))
        end
    else
        type = @ast ctx ex "Any"::K"core"
    end
    if !isnothing(slurp_ex)
        type = @ast ctx slurp_ex [K"curly" "Vararg"::K"core" type]
    end

    k = kind(ex)
    if k == K"tuple" && !is_kw
        # Argument destructuring
        is_nospecialize = getmeta(arg, :nospecialize, false)
        name = new_local_binding(ctx, ex, "destructured_arg";
                                 kind=:argument, is_nospecialize=is_nospecialize)
        push!(body_stmts, @ast ctx ex [
            K"local"(meta=CompileHints(:is_destructured_arg, true))
            [K"=" ex name]
        ])
    elseif k == K"Identifier" || k == K"Placeholder"
        name = ex
    else
        throw(LoweringError(ex, is_kw ? "Invalid keyword name" : "Invalid function argument"))
    end

    return (name, type, default, !isnothing(slurp_ex))
end

# Expand `where` clause(s) of a function into (typevar_names, typevar_stmts) where
# - `typevar_names` are the names of the type's type parameters
# - `typevar_stmts` are a list of statements to define a `TypeVar` for each parameter
#   name in `typevar_names`, with exactly one per `typevar_name`. Some of these
#   may already have been emitted.
# - `new_typevar_stmts` is the list of statements which needs to to be emitted
#   prior to uses of `typevar_names`.
function _split_wheres!(ctx, typevar_names, typevar_stmts, new_typevar_stmts, ex)
    if kind(ex) == K"where" && numchildren(ex) == 2
        vars_kind = kind(ex[2])
        if vars_kind == K"_typevars"
            append!(typevar_names, children(ex[2][1]))
            append!(typevar_stmts, children(ex[2][2]))
        else
            params = vars_kind == K"braces" ? ex[2][1:end] : ex[2:2]
            n_existing = length(new_typevar_stmts)
            expand_typevars!(ctx, typevar_names, new_typevar_stmts, params)
            append!(typevar_stmts, view(new_typevar_stmts, n_existing+1:length(new_typevar_stmts)))
        end
        _split_wheres!(ctx, typevar_names, typevar_stmts, new_typevar_stmts, ex[1])
    else
        ex
    end
end

function method_def_expr(ctx, srcref, callex_srcref, method_table,
                         typevar_names, arg_names, arg_types, body, ret_var=nothing)
    @ast ctx srcref [K"block"
        # metadata contains svec(types, sparms, location)
        method_metadata := [K"call"(callex_srcref)
            "svec"              ::K"core"
            [K"call"
                "svec"          ::K"core"
                arg_types...
            ]
            [K"call"
                "svec"          ::K"core"
                typevar_names...
            ]
            ::K"SourceLocation"(callex_srcref)
        ]
        [K"method"
            isnothing(method_table) ? "nothing"::K"core" : method_table
            method_metadata
            [K"lambda"(body, is_toplevel_thunk=false, toplevel_pure=false)
                [K"block" arg_names...]
                [K"block" typevar_names...]
                body
                ret_var  # might be `nothing` and hence removed
            ]
        ]
        [K"removable" method_metadata]
    ]
end

# Select static parameters which are used in function arguments `arg_types`, or
# transitively used.
#
# The transitive usage check probably doesn't guarantee that the types are
# inferable during dispatch as they may only be part of the bounds of another
# type. Thus we might get false positives here but we shouldn't get false
# negatives.
function select_used_typevars(arg_types, typevar_names, typevar_stmts)
    n_typevars = length(typevar_names)
    @assert n_typevars == length(typevar_stmts)
    # Filter typevar names down to those which are directly used in the arg list
    typevar_used = Bool[any(contains_identifier(argtype, tn) for argtype in arg_types)
                        for tn in typevar_names]
    # _Or_ used transitively via other typevars. The following code
    # computes this by incrementally coloring the graph of dependencies
    # between type vars.
    found_used = true
    while found_used
        found_used = false
        for (i,tn) in enumerate(typevar_names)
            if typevar_used[i]
                continue
            end
            for j = i+1:n_typevars
                if typevar_used[j] && contains_identifier(typevar_stmts[j], tn)
                    found_used = true
                    typevar_used[i] = true
                    break
                end
            end
        end
    end
    typevar_used
end

function check_all_typevars_used(arg_types, typevar_names, typevar_stmts)
    selected = select_used_typevars(arg_types, typevar_names, typevar_stmts)
    unused_typevar = findfirst(s->!s, selected)
    if !isnothing(unused_typevar)
        # Type variables which may be statically determined to be unused in
        # any function argument and therefore can't be inferred during
        # dispatch.
        throw(LoweringError(typevar_names[unused_typevar],
                            "Method definition declares type variable but does not use it in the type of any function parameter"))
    end
end

# Return `typevar_names` which are used directly or indirectly in `arg_types`.
function trim_used_typevars(ctx, arg_types, typevar_names, typevar_stmts)
    typevar_used = select_used_typevars(arg_types, typevar_names, typevar_stmts)
    trimmed_typevar_names = SyntaxList(ctx)
    for (used,tn) in zip(typevar_used, typevar_names)
        if used
            push!(trimmed_typevar_names, tn)
        end
    end
    return trimmed_typevar_names
end

function is_if_generated(ex)
    kind(ex) == K"if" && kind(ex[1]) == K"generated"
end

# Return true if a function body contains a code generator from `@generated` in
# the form `[K"if" [K"generated"] ...]`
function is_generated(ex)
    if is_if_generated(ex)
        return true
    elseif is_quoted(ex) || kind(ex) == K"function"
        return false
    else
        return any(is_generated, children(ex))
    end
end

function split_generated(ctx, ex, gen_part)
    if is_leaf(ex)
        ex
    elseif is_if_generated(ex)
        gen_part ? @ast(ctx, ex, [K"$" ex[2]]) : ex[3]
    else
        mapchildren(e->split_generated(ctx, e, gen_part), ctx, ex)
    end
end

# Split @generated function body into two parts:
# * The code generator
# * The non-generated function body
function expand_function_generator(ctx, srcref, callex_srcref, func_name, func_name_str, body, arg_names, typevar_names)
    gen_body = if is_if_generated(body)
        body[2] # Simple case - don't need interpolation when the whole body is generated
    else
        expand_quote(ctx, @ast ctx body [K"block" split_generated(ctx, body, true)])
    end
    gen_name_str = reserve_module_binding_i(ctx.mod,
                        "#$(isnothing(func_name_str) ? "_" : func_name_str)@generator#")
    gen_name = new_global_binding(ctx, body, gen_name_str, ctx.mod)

    # Set up the arguments for the code generator
    gen_arg_names = SyntaxList(ctx)
    gen_arg_types = SyntaxList(ctx)
    # Self arg
    push!(gen_arg_names, new_local_binding(ctx, callex_srcref, "#self#"; kind=:argument))
    push!(gen_arg_types, @ast ctx callex_srcref [K"function_type" gen_name])
    # Macro expansion context arg
    if kind(func_name) != K"Identifier"
        TODO(func_name, "Which scope do we adopt for @generated generator `__context__` in this case?")
    end
    push!(gen_arg_names, adopt_scope(@ast(ctx, callex_srcref, "__context__"::K"Identifier"), func_name))
    push!(gen_arg_types, @ast(ctx, callex_srcref, MacroContext::K"Value"))
    # Trailing arguments to the generator are provided by the Julia runtime. They are:
    # static_parameters... parent_function arg_types...
    first_trailing_arg = length(gen_arg_names) + 1
    append!(gen_arg_names, typevar_names)
    append!(gen_arg_names, arg_names)
    # Apply nospecialize to all arguments to prevent so much codegen and add
    # Core.Any type for them
    for i in first_trailing_arg:length(gen_arg_names)
        gen_arg_names[i] = setmeta(gen_arg_names[i]; nospecialize=true)
        push!(gen_arg_types, @ast ctx gen_arg_names[i] "Any"::K"core")
    end
    # Code generator definition
    gen_func_method_defs = @ast ctx srcref [K"block"
        [K"function_decl" gen_name]
        [K"scope_block"(scope_type=:hard)
            [K"method_defs"
                gen_name
                [K"block"
                    method_def_expr(ctx, srcref, callex_srcref, nothing, SyntaxList(ctx),
                                    gen_arg_names, gen_arg_types, gen_body, nothing)
                ]
            ]
        ]
    ]

    # Extract non-generated body
    nongen_body = @ast ctx body [K"block"
        # The Julia runtime associates the code generator with the
        # non-generated method by adding this meta to the body. This feels like
        # a hack though since the generator ultimately gets attached to the
        # method rather than the CodeInfo which we're putting it inside.
        [K"meta"
            "generated"::K"Symbol"
            # The following is code to be evaluated at top level and will wrap
            # whatever code comes from the user's generator into an appropriate
            # K"lambda" (+ K"with_static_parameters") suitable for lowering
            # into a CodeInfo.
            #
            # todo: As isolated top-level code, we don't actually want to apply
            # the normal scope rules of the surrounding function ... it should
            # technically have scope resolved at top level.
            [K"new"
                GeneratedFunctionStub::K"Value" # Use stub type from JuliaLowering
                gen_name
                # Truncate provenance to just the source file range, as this
                # will live permanently in the IR and we probably don't want
                # the full provenance tree and intermediate expressions
                # (TODO: More truncation. We certainly don't want to store the
                #  source file either.)
                sourceref(srcref)::K"Value"
                [K"call"
                    "svec"::K"core"
                    "#self#"::K"Symbol"
                    (n.name_val::K"Symbol"(n) for n in arg_names[2:end])...
                ]
                [K"call"
                    "svec"::K"core"
                    (n.name_val::K"Symbol"(n) for n in typevar_names)...
                ]
            ]
        ]
        split_generated(ctx, body, false)
    ]

    return gen_func_method_defs, nongen_body
end

# Generate a method for every number of allowed optional arguments
# For example for `f(x, y=1, z=2)` we generate two additional methods
# f(x) = f(x, 1, 2)
# f(x, y) = f(x, y, 2)
function optional_positional_defs!(ctx, method_stmts, srcref, callex,
                                   method_table, typevar_names, typevar_stmts,
                                   arg_names, arg_types, first_default,
                                   arg_defaults)
    # Replace placeholder arguments with variables - we need to pass them to
    # the inner method for dispatch even when unused in the inner method body
    def_arg_names = map(arg_names) do arg
        kind(arg) == K"Placeholder" ?
            new_local_binding(ctx, arg, arg.name_val; kind=:argument) :
            arg
    end
    for def_idx = 1:length(arg_defaults)
        first_omitted = first_default + def_idx - 1
        trimmed_arg_names = def_arg_names[1:first_omitted-1]
        # Call the full method directly if no arguments are reused in
        # subsequent defaults. Otherwise conservatively call the function with
        # only one additional default argument supplied and let the chain of
        # function calls eventually lead to the full method.
        any_args_in_trailing_defaults =
            any(arg_defaults[def_idx+1:end]) do defaultval
                contains_identifier(defaultval, def_arg_names[first_omitted:end])
            end
        last_used_default = any_args_in_trailing_defaults ?
            def_idx : lastindex(arg_defaults)
        body = @ast ctx callex [K"block"
            [K"call"
                trimmed_arg_names...
                arg_defaults[def_idx:last_used_default]...
            ]
        ]
        trimmed_arg_types = arg_types[1:first_omitted-1]
        trimmed_typevar_names = trim_used_typevars(ctx, trimmed_arg_types,
                                                   typevar_names, typevar_stmts)
        # TODO: Ensure we preserve @nospecialize metadata in args
        push!(method_stmts,
              method_def_expr(ctx, srcref, callex, method_table,
                              trimmed_typevar_names, trimmed_arg_names, trimmed_arg_types,
                              body))
    end
end

function scope_nest(ctx, names, values, body)
    for (name, value) in Iterators.reverse(zip(names, values))
        body = @ast ctx name [K"let" [K"block" [K"=" name value]]
            body
        ]
    end
    body
end

# Generate body function and `Core.kwcall` overloads for functions taking keywords.
function keyword_function_defs(ctx, srcref, callex_srcref, name_str, typevar_names,
                               typevar_stmts, new_typevar_stmts, arg_names,
                               arg_types, has_slurp, first_default, arg_defaults,
                               keywords, body, ret_var)
    mangled_name = let n = isnothing(name_str) ? "_" : name_str
        reserve_module_binding_i(ctx.mod, string(startswith(n, '#') ? "" : "#", n, "#"))
    end
    # TODO: Is the layer correct here? Which module should be the parent module
    # of this body function?
    layer = new_scope_layer(ctx)
    body_func_name = adopt_scope(@ast(ctx, callex_srcref, mangled_name::K"Identifier"), layer)

    kwcall_arg_names = SyntaxList(ctx)
    kwcall_arg_types = SyntaxList(ctx)

    push!(kwcall_arg_names, new_local_binding(ctx, callex_srcref, "#self#"; kind=:argument))
    push!(kwcall_arg_types,
        @ast ctx callex_srcref [K"call"
            "typeof"::K"core"
            "kwcall"::K"core"
        ]
    )
    kws_arg = new_local_binding(ctx, keywords, "kws"; kind=:argument)
    push!(kwcall_arg_names, kws_arg)
    push!(kwcall_arg_types, @ast ctx keywords "NamedTuple"::K"core")

    body_arg_names = SyntaxList(ctx)
    body_arg_types = SyntaxList(ctx)
    push!(body_arg_names, new_local_binding(ctx, body_func_name, "#self#"; kind=:argument))
    push!(body_arg_types, @ast ctx body_func_name [K"function_type" body_func_name])

    non_positional_typevars = typevar_names[map(!,
        select_used_typevars(arg_types, typevar_names, typevar_stmts))]

    kw_values = SyntaxList(ctx)
    kw_defaults = SyntaxList(ctx)
    kw_names = SyntaxList(ctx)
    kw_name_syms = SyntaxList(ctx)
    has_kw_slurp = false
    kwtmp = new_local_binding(ctx, keywords, "kwtmp")
    for (i,arg) in enumerate(children(keywords))
        (aname, atype, default, is_slurp) =
            expand_function_arg(ctx, nothing, arg, i == numchildren(keywords), true)
        push!(kw_names, aname)
        name_sym = @ast ctx aname aname=>K"Symbol"
        push!(body_arg_names, aname)

        if is_slurp
            if !isnothing(default)
                throw(LoweringError(arg, "keyword argument with `...` cannot have a default value"))
            end
            has_kw_slurp = true
            push!(body_arg_types, @ast ctx arg [K"call" "pairs"::K"top" "NamedTuple"::K"core"])
            push!(kw_defaults, @ast ctx arg [K"call" "pairs"::K"top" [K"call" "NamedTuple"::K"core"]])
            continue
        else
            push!(body_arg_types, atype)
        end

        if isnothing(default)
            default = @ast ctx arg [K"call"
                "throw"::K"core"
                [K"call"
                    "UndefKeywordError"::K"core"
                    name_sym
                ]
            ]
        end
        push!(kw_defaults, default)

        # Extract the keyword argument value and check the type
        push!(kw_values, @ast ctx arg [K"block"
            [K"if"
                [K"call" "isdefined"::K"core" kws_arg name_sym]
                [K"block"
                    kwval := [K"call" "getfield"::K"core" kws_arg name_sym]
                    if is_core_Any(atype) || contains_identifier(atype, non_positional_typevars)
                        # <- Do nothing in this branch because `atype` includes
                        # something from the typevars and those static
                        # parameters don't have values yet. Instead, the type
                        # will be picked up when the body method is called and
                        # result in a MethodError during dispatch rather than
                        # the `TypeError` below.
                        #
                        # In principle we could probably construct the
                        # appropriate UnionAll here in some simple cases but
                        # the fully general case probably requires simulating
                        # the runtime's dispatch machinery.
                    else
                        [K"if" [K"call" "isa"::K"core" kwval atype]
                            "nothing"::K"core"
                            [K"call"
                                "throw"::K"core"
                                [K"new" "TypeError"::K"core"
                                    "keyword argument"::K"Symbol"
                                    name_sym
                                    atype
                                    kwval
                                ]
                            ]
                        ]
                    end
                    # Compiler performance hack: we reuse the kwtmp slot in all
                    # keyword if blocks rather than using the if block in value
                    # position. This cuts down on the number of slots required
                    # https://github.com/JuliaLang/julia/pull/44333
                    [K"=" kwtmp kwval]
                ]
                [K"=" kwtmp default]
            ]
            kwtmp
        ])

        push!(kw_name_syms, name_sym)
    end
    append!(body_arg_names, arg_names)
    append!(body_arg_types, arg_types)

    first_default += length(kwcall_arg_names)
    append!(kwcall_arg_names, arg_names)
    append!(kwcall_arg_types, arg_types)

    kwcall_mtable = @ast(ctx, srcref, "nothing"::K"core")

    kwcall_method_defs = SyntaxList(ctx)
    if !isempty(arg_defaults)
        # Construct kwcall overloads which forward default positional args on
        # to the main kwcall overload.
        optional_positional_defs!(ctx, kwcall_method_defs, srcref, callex_srcref,
                                  kwcall_mtable, typevar_names, typevar_stmts,
                                  kwcall_arg_names, kwcall_arg_types, first_default, arg_defaults)
    end

    positional_forwarding_args = if has_slurp
        a = copy(arg_names)
        a[end] = @ast ctx a[end] [K"..." a[end]]
        a
    else
        arg_names
    end

    #--------------------------------------------------
    # Construct the "main kwcall overload" which unpacks keywords and checks
    # their consistency before dispatching to the user's code in the body
    # method.
    defaults_depend_on_kw_names = any(val->contains_identifier(val, kw_names), kw_defaults)
    defaults_have_assign = any(val->contains_unquoted(e->kind(e) == K"=", val), kw_defaults)
    use_ssa_kw_temps = !defaults_depend_on_kw_names && !defaults_have_assign

    if use_ssa_kw_temps
        kw_val_stmts = SyntaxList(ctx)
        for n in kw_names
            # If not using slots for the keyword argument values, still declare
            # them for reflection purposes.
            push!(kw_val_stmts, @ast ctx n [K"local" n])
        end
        kw_val_vars = SyntaxList(ctx)
        for val in kw_values
            v = emit_assign_tmp(kw_val_stmts, ctx, val, "kwval")
            push!(kw_val_vars, v)
        end
    else
        kw_val_vars = kw_names
    end

    kwcall_body_tail = @ast ctx keywords [K"block"
        if has_kw_slurp
            # Slurp remaining keywords into last arg
            remaining_kws := [K"call"
                "pairs"::K"top"
                if isempty(kw_name_syms)
                    kws_arg
                else
                    [K"call"
                        "structdiff"::K"top"
                        kws_arg
                        [K"curly"
                            "NamedTuple"::K"core"
                            [K"tuple" kw_name_syms...]
                        ]
                    ]
                end
            ]
        else
            # Check that there's no unexpected keywords
            [K"if"
                [K"call"
                    "isempty"::K"top"
                    [K"call"
                        "diff_names"::K"top"
                        [K"call" "keys"::K"top" kws_arg]
                        [K"tuple" kw_name_syms...]
                    ]
                ]
                "nothing"::K"core"
                [K"call"
                    "kwerr"::K"top"
                    kws_arg
                    positional_forwarding_args...
                ]
            ]
        end
        [K"call"
            body_func_name
            kw_val_vars...
            if has_kw_slurp
                remaining_kws
            end
            positional_forwarding_args...
        ]
    ]
    kwcall_body = if use_ssa_kw_temps
        @ast ctx keywords [K"block"
            kw_val_stmts...
            kwcall_body_tail
        ]
    else
        scope_nest(ctx, kw_names, kw_values, kwcall_body_tail)
    end
    main_kwcall_typevars = trim_used_typevars(ctx, kwcall_arg_types, typevar_names, typevar_stmts)
    push!(kwcall_method_defs,
          method_def_expr(ctx, srcref, callex_srcref, kwcall_mtable,
                          main_kwcall_typevars, kwcall_arg_names, kwcall_arg_types, kwcall_body))

    # Check kws of body method
    check_all_typevars_used(body_arg_types, typevar_names, typevar_stmts)

    kw_func_method_defs = @ast ctx srcref [K"block"
        [K"function_decl" body_func_name]
        [K"scope_block"(scope_type=:hard)
            [K"method_defs"
                body_func_name
                [K"block"
                    new_typevar_stmts...
                    method_def_expr(ctx, srcref, callex_srcref, "nothing"::K"core",
                                    typevar_names, body_arg_names, body_arg_types,
                                    [K"block"
                                        [K"meta" "nkw"::K"Symbol" numchildren(keywords)::K"Integer"]
                                        body
                                    ],
                                    ret_var)
                ]
            ]
        ]
        [K"scope_block"(scope_type=:hard)
            [K"method_defs"
                "nothing"::K"core"
                [K"block"
                    new_typevar_stmts...
                    kwcall_method_defs...
                ]
            ]
        ]
    ]

    #--------------------------------------------------
    # Body for call with no keywords
    body_for_positional_args_only = if defaults_depend_on_kw_names
        scope_nest(ctx, kw_names, kw_defaults,
            @ast ctx srcref [K"call" body_func_name
                kw_names...
                positional_forwarding_args...
            ]
        )
    else
        @ast ctx srcref [K"call" body_func_name
            kw_defaults...
            positional_forwarding_args...
        ]
    end

    kw_func_method_defs, body_for_positional_args_only
end

# Check valid identifier/function names
function is_invalid_func_name(ex)
    k = kind(ex)
    if k == K"Identifier"
        name = ex.name_val
    elseif k == K"." && numchildren(ex) == 2 && kind(ex[2]) == K"Symbol"
        # `function A.f(x,y) ...`
        name = ex[2].name_val
    else
        return true
    end
    return is_ccall_or_cglobal(name)
end

function expand_function_def(ctx, ex, docs, rewrite_call=identity, rewrite_body=identity; doc_only=false)
    @chk numchildren(ex) in (1,2)
    name = ex[1]
    if numchildren(ex) == 1 && is_identifier_like(name)
        # Function declaration with no methods
        if is_invalid_func_name(name)
            throw(LoweringError(name, "Invalid function name"))
        end
        return @ast ctx ex [K"block"
            [K"function_decl" name]
            name
        ]
    end

    typevar_names = SyntaxList(ctx)
    typevar_stmts = SyntaxList(ctx)
    new_typevar_stmts = SyntaxList(ctx)
    if kind(name) == K"where"
        # `where` vars end up in two places
        # 1. Argument types - the `T` in `x::T` becomes a `TypeVar` parameter in
        #    the method sig, eg, `function f(x::T) where T ...`.  These define the
        #    static parameters of the method.
        # 2. In the method body - either explicitly or implicitly via the method
        #    return type or default arguments - where `T` turns up as the *name* of
        #    a special slot of kind ":static_parameter"
        name = _split_wheres!(ctx, typevar_names, typevar_stmts, new_typevar_stmts, name)
    end

    return_type = nothing
    if kind(name) == K"::"
        @chk numchildren(name) == 2
        return_type = name[2]
        name = name[1]
    end

    callex = if kind(name) == K"call"
        name
    elseif kind(name) == K"tuple"
        # Anonymous function syntax `function (x,y) ... end`
        @ast ctx name [K"call"
            "#anon#"::K"Placeholder"
            children(name)...
        ]
    elseif kind(name) == K"dotcall"
        throw(LoweringError(name, "Cannot define function using `.` broadcast syntax"))
    else
        throw(LoweringError(name, "Bad function definition"))
    end

    # Fixup for `new` constructor sigs if necessary
    callex = rewrite_call(callex)

    # Construct method argument lists of names and types.
    #
    # First, match the "self" argument: In the method signature, each function
    # gets a self argument name+type. For normal generic functions, this is a
    # singleton and subtype of `Function`. But objects of any type can be made
    # callable when the self argument is explicitly given using `::` syntax in
    # the function name.
    name = callex[1]
    bare_func_name = nothing
    name_str = nothing
    doc_obj = nothing
    self_name = nothing
    if kind(name) == K"::"
        # Self argument is specified by user
        if numchildren(name) == 1
            # function (::T)() ...
            self_type = name[1]
        else
            # function (f::T)() ...
            @chk numchildren(name) == 2
            self_name = name[1]
            self_type = name[2]
        end
        doc_obj = self_type
    else
        if kind(name) == K"Placeholder"
            # Anonymous function. In this case we may use an ssavar for the
            # closure's value.
            name_str = name.name_val
            name = ssavar(ctx, name, name.name_val)
            bare_func_name = name
        elseif is_invalid_func_name(name)
            throw(LoweringError(name, "Invalid function name"))
        elseif is_identifier_like(name)
            # Add methods to a global `Function` object, or local closure
            # type function f() ...
            name_str = name.name_val
            bare_func_name = name
        else
            # Add methods to an existing Function
            # function A.B.f() ...
            if kind(name) == K"." && kind(name[2]) == K"Symbol"
                name_str = name[2].name_val
            end
        end
        doc_obj = name # todo: can closures be documented?
        self_type = @ast ctx name [K"function_type" name]
    end
    # Add self argument
    if isnothing(self_name)
        # TODO: #self# should be symbolic rather than a binding for the cases
        # where it's reused in `optional_positional_defs!` because it's
        # probably unsafe to reuse bindings for multiple different methods in
        # the presence of closure captures or other global binding properties.
        #
        # This is reminiscent of the need to renumber SSA vars in certain cases
        # in the flisp implementation.
        self_name = new_local_binding(ctx, name, "#self#"; kind=:argument)
    end

    # Expand remaining argument names and types
    arg_names = SyntaxList(ctx)
    arg_types = SyntaxList(ctx)
    push!(arg_names, self_name)
    push!(arg_types, self_type)
    args = callex[2:end]
    keywords = nothing
    if !isempty(args) && kind(args[end]) == K"parameters"
        keywords = args[end]
        args = args[1:end-1]
        if numchildren(keywords) == 0
            keywords = nothing
        end
    end
    body_stmts = SyntaxList(ctx)
    has_slurp = false
    first_default = 0 # index into arg_names/arg_types
    arg_defaults = SyntaxList(ctx)
    for (i,arg) in enumerate(args)
        (aname, atype, default, is_slurp) = expand_function_arg(ctx, body_stmts, arg,
                                                                i == length(args), false)
        has_slurp |= is_slurp
        push!(arg_names, aname)

        # TODO: Ideally, ensure side effects of evaluating arg_types only
        # happen once - we should create an ssavar if there's any following
        # defaults. (flisp lowering doesn't ensure this either). Beware if
        # fixing this that optional_positional_defs! depends on filtering the
        # *symbolic* representation of arg_types.
        push!(arg_types, atype)

        if isnothing(default)
            if !isempty(arg_defaults) && !is_slurp
                # TODO: Referring to multiple pieces of syntax in one error message is necessary.
                # TODO: Poison ASTs with error nodes and continue rather than immediately throwing.
                #
                # We should make something like the following kind of thing work!
                # arg_defaults[1] = @ast_error ctx arg_defaults[1] """
                #     Positional arguments with defaults must occur at the end.
                #
                #     We found a [non-optional position argument]($arg) *after*
                #     one with a [default value]($(first(arg_defaults)))
                # """
                #
                throw(LoweringError(args[first_default-1], "optional positional arguments must occur at end"))
            end
        else
            if isempty(arg_defaults)
                first_default = i + 1 # Offset for self argument
            end
            push!(arg_defaults, default)
        end
    end

    if doc_only
        # The (doc str (call ...)) form requires method signature lowering, but
        # does not execute or define any method, so we can't use function_type.
        # This is a bit of a messy case in the docsystem which we'll hopefully
        # be able to delete at some point.
        sig_stmts = SyntaxList(ctx)
        @assert first_default != 1 && length(arg_types) >= 1
        last_required = first_default === 0 ? length(arg_types) : first_default - 1
        for i in last_required:length(arg_types)
            push!(sig_stmts, @ast(ctx, ex, [K"curly" "Tuple"::K"core" arg_types[2:i]...]))
        end
        sig_type = @ast ctx ex [K"where"
            [K"curly" "Union"::K"core" sig_stmts...]
            [K"_typevars" [K"block" typevar_names...] [K"block"]]
        ]
        out = @ast ctx docs [K"block"
            typevar_stmts...
            [K"call"
                bind_static_docs!::K"Value"
                (kind(name) == K"." ? name[1] : ctx.mod::K"Value")
                name_str::K"Symbol"
                docs[1]
                ::K"SourceLocation"(ex)
                sig_type
            ]
        ]
        return expand_forms_2(ctx, out)
    end

    if !isnothing(return_type)
        ret_var = ssavar(ctx, return_type, "return_type")
        push!(body_stmts, @ast ctx return_type [K"=" ret_var return_type])
    else
        ret_var = nothing
    end

    body = rewrite_body(ex[2])
    if !isempty(body_stmts)
        body = @ast ctx body [
            K"block"
            body_stmts...
            body
        ]
    end

    gen_func_method_defs = nothing
    if is_generated(body)
        gen_func_method_defs, body =
            expand_function_generator(ctx, ex, callex, name, name_str, body, arg_names, typevar_names)

    end

    if isnothing(keywords)
        kw_func_method_defs = nothing
        # NB: The following check seems good as it statically catches any useless
        # static parameters which can't be bound during method invocation.
        # However it wasn't previously an error so we might need to reduce it
        # to a warning?
        check_all_typevars_used(arg_types, typevar_names, typevar_stmts)
        main_typevar_names = typevar_names
    else
        # Rewrite `body` here so that the positional-only versions dispatch there.
        kw_func_method_defs, body =
            keyword_function_defs(ctx, ex, callex, name_str, typevar_names, typevar_stmts,
                                  new_typevar_stmts, arg_names, arg_types, has_slurp,
                                  first_default, arg_defaults, keywords, body, ret_var)
        # The main function (but without keywords) needs its typevars trimmed,
        # as some of them may be for the keywords only.
        main_typevar_names = trim_used_typevars(ctx, arg_types, typevar_names, typevar_stmts)
        # ret_var is used only in the body method
        ret_var = nothing
    end

    method_table_val = nothing # TODO: method overlays
    method_table = isnothing(method_table_val)            ?
                   @ast(ctx, callex, "nothing"::K"core")  :
                   ssavar(ctx, ex, "method_table")
    method_stmts = SyntaxList(ctx)

    if !isempty(arg_defaults)
        optional_positional_defs!(ctx, method_stmts, ex, callex,
                                  method_table, typevar_names, typevar_stmts,
                                  arg_names, arg_types, first_default, arg_defaults)
    end

    # The method with all non-default arguments
    push!(method_stmts,
          method_def_expr(ctx, ex, callex, method_table, main_typevar_names, arg_names,
                          arg_types, body, ret_var))

    if !isnothing(docs)
        method_stmts[end] = @ast ctx docs [K"block"
            method_metadata := method_stmts[end]
            [K"call"
                bind_docs!::K"Value"
                doc_obj
                docs[1]
                method_metadata
            ]
        ]
    end

    @ast ctx ex [K"block"
        if !isnothing(bare_func_name)
            # Need the main function type created here before running any code
            # in kw_func_method_defs
            [K"function_decl"(bare_func_name) bare_func_name]
        end
        gen_func_method_defs
        kw_func_method_defs
        [K"scope_block"(scope_type=:hard)
            [K"method_defs"
                isnothing(bare_func_name) ? "nothing"::K"core" : bare_func_name
                [K"block"
                    new_typevar_stmts...
                    if !isnothing(method_table_val)
                        [K"=" method_table method_table_val]
                    end
                    method_stmts...
                ]
            ]
        ]
        [K"removable"
            isnothing(bare_func_name) ? "nothing"::K"core" : bare_func_name
        ]
    ]
end

#-------------------------------------------------------------------------------
# Anon function syntax
function expand_arrow_arglist(ctx, arglist, arrowname)
    k = kind(arglist)
    if k == K"where"
        @ast ctx arglist [K"where"
            expand_arrow_arglist(ctx, arglist[1], arrowname)
            arglist[2]
        ]
    else
        # The arglist can sometimes be parsed as a block, or something else, and
        # fixing this is extremely awkward when nested inside `where`. See
        # https://github.com/JuliaLang/JuliaSyntax.jl/pull/522
        if k == K"block"
            @chk numchildren(arglist) == 2
            arglist = @ast ctx arglist [K"tuple"
                arglist[1]
                [K"parameters" arglist[2]]
            ]
        elseif k != K"tuple"
            arglist = @ast ctx arglist [K"tuple"
                arglist[1]
            ]
        end
        @ast ctx arglist [K"call"
            arrowname::K"Placeholder"
            children(arglist)...
        ]
    end
end

function expand_arrow(ctx, ex)
    @chk numchildren(ex) == 2
    expand_forms_2(ctx,
        @ast ctx ex [K"function"
            expand_arrow_arglist(ctx, ex[1], string(kind(ex)))
            ex[2]
        ]
    )
end

function expand_opaque_closure(ctx, ex)
    arg_types_spec = ex[1]
    return_lower_bound = ex[2]
    return_upper_bound = ex[3]
    allow_partial = ex[4]
    func_expr = ex[5]
    @chk kind(func_expr) == K"->"
    @chk numchildren(func_expr) == 2
    args = func_expr[1]
    @chk kind(args) == K"tuple"
    check_no_parameters(ex, args)

    arg_names = SyntaxList(ctx)
    arg_types = SyntaxList(ctx)
    push!(arg_names, new_local_binding(ctx, args, "#self#"; kind=:argument))
    body_stmts = SyntaxList(ctx)
    is_va = false
    for (i, arg) in enumerate(children(args))
        (aname, atype, default, is_slurp) = expand_function_arg(ctx, body_stmts, arg,
                                                                i == numchildren(args), false)
        is_va |= is_slurp
        push!(arg_names, aname)
        push!(arg_types, atype)
        if !isnothing(default)
            throw(LoweringError(default, "Default positional arguments cannot be used in an opaque closure"))
        end
    end

    nargs = length(arg_names) - 1 # ignoring #self#

    @ast ctx ex [K"_opaque_closure"
        ssavar(ctx, ex, "opaque_closure_id") # only a placeholder. Must be :local
        if is_core_nothing(arg_types_spec)
            [K"curly"
                "Tuple"::K"core"
                arg_types...
            ]
        else
            arg_types_spec
        end
        is_core_nothing(return_lower_bound) ? [K"curly" "Union"::K"core"] : return_lower_bound
        is_core_nothing(return_upper_bound) ? "Any"::K"core" : return_upper_bound
        allow_partial
        nargs::K"Integer"
        is_va::K"Bool"
        ::K"SourceLocation"(func_expr)
        [K"lambda"(func_expr, is_toplevel_thunk=false, toplevel_pure=false)
            [K"block" arg_names...]
            [K"block"]
            [K"block"
                body_stmts...
                func_expr[2]
            ]
        ]
    ]
end

#-------------------------------------------------------------------------------
# Expand macro definitions

function _make_macro_name(ctx, ex)
    k = kind(ex)
    if k == K"Identifier" || k == K"Symbol"
        name = mapleaf(ctx, ex, k)
        name.name_val = "@$(ex.name_val)"
        name
    elseif is_valid_modref(ex)
        @chk numchildren(ex) == 2
        @ast ctx ex [K"." ex[1] _make_macro_name(ctx, ex[2])]
    else
        throw(LoweringError(ex, "invalid macro name"))
    end
end

# flisp: expand-macro-def
function expand_macro_def(ctx, ex)
    @chk numchildren(ex) >= 1 (ex,"invalid macro definition")
    if numchildren(ex) == 1
        name = ex[1]
        # macro with zero methods
        # `macro m end`
        return @ast ctx ex [K"function" _make_macro_name(ctx, name)]
    end
    # TODO: Making this manual pattern matching robust is such a pain!!!
    sig = ex[1]
    @chk (kind(sig) == K"call" && numchildren(sig) >= 1) (sig, "invalid macro signature")
    name = sig[1]
    args = remove_empty_parameters(children(sig))
    @chk kind(args[end]) != K"parameters" (args[end], "macros cannot accept keyword arguments")
    scope_ref = kind(name) == K"." ? name[1] : name
    if ctx.expr_compat_mode
        @ast ctx ex [K"function"
            [K"call"(sig)
                _make_macro_name(ctx, name)
                [K"::"
                    # TODO: should we be adopting the scope of the K"macro" expression itself?
                    adopt_scope(@ast(ctx, sig, "__source__"::K"Identifier"), scope_ref)
                    LineNumberNode::K"Value"
                ]
                [K"::"
                    adopt_scope(@ast(ctx, sig, "__module__"::K"Identifier"), scope_ref)
                    Module::K"Value"
                ]
                map(e->_apply_nospecialize(ctx, e), args[2:end])...
            ]
            ex[2]
        ]
    else
        @ast ctx ex [K"function"
            [K"call"(sig)
                _make_macro_name(ctx, name)
                [K"::"
                    adopt_scope(@ast(ctx, sig, "__context__"::K"Identifier"), scope_ref)
                    MacroContext::K"Value"
                ]
                # flisp: We don't mark these @nospecialize because all arguments to
                # new macros will be of type SyntaxTree
                args[2:end]...
            ]
            ex[2]
        ]
    end
end

#-------------------------------------------------------------------------------
# Expand type definitions

# Match `x<:T<:y` etc, returning `(name, lower_bound, upper_bound)`
# A bound is `nothing` if not specified
function analyze_typevar(ctx, ex)
    k = kind(ex)
    if k == K"Identifier"
        (ex, nothing, nothing)
    elseif k == K"comparison" && numchildren(ex) == 5
        kind(ex[3]) == K"Identifier" || throw(LoweringError(ex[3], "expected type name"))
        if !((kind(ex[2]) == K"Identifier" && ex[2].name_val == "<:") &&
             (kind(ex[4]) == K"Identifier" && ex[4].name_val == "<:"))
            throw(LoweringError(ex, "invalid type bounds"))
        end
        # a <: b <: c
        (ex[3], ex[1], ex[5])
    elseif k == K"<:" && numchildren(ex) == 2
        kind(ex[1]) == K"Identifier" || throw(LoweringError(ex[1], "expected type name"))
        (ex[1], nothing, ex[2])
    elseif k == K">:" && numchildren(ex) == 2
        kind(ex[2]) == K"Identifier" || throw(LoweringError(ex[2], "expected type name"))
        (ex[1], ex[2], nothing)
    else
        throw(LoweringError(ex, "expected type name or type bounds"))
    end
end

function bounds_to_TypeVar(ctx, srcref, bounds)
    name, lb, ub = bounds
    # Generate call to one of
    # TypeVar(name)
    # TypeVar(name, ub)
    # TypeVar(name, lb, ub)
    @ast ctx srcref [K"call"
        "TypeVar"::K"core"
        name=>K"Symbol"
        lb
        if isnothing(ub) && !isnothing(lb)
            "Any"::K"core"
        else
            ub
        end
    ]
end

# Analyze type signatures such as `A{C} <: B where C`
#
# Return (name, typevar_names, typevar_stmts, supertype) where
# - `name` is the name of the type
# - `supertype` is the super type of the type
function analyze_type_sig(ctx, ex)
    k = kind(ex)
    if k == K"Identifier"
        name = ex
        type_params = ()
        supertype = @ast ctx ex "Any"::K"core"
    elseif k == K"curly" && numchildren(ex) >= 1 && kind(ex[1]) == K"Identifier"
        # name{type_params}
        name = ex[1]
        type_params = ex[2:end]
        supertype = @ast ctx ex "Any"::K"core"
    elseif k == K"<:" && numchildren(ex) == 2
        if kind(ex[1]) == K"Identifier"
            name = ex[1]
            type_params = ()
            supertype = ex[2]
        elseif kind(ex[1]) == K"curly" && numchildren(ex[1]) >= 1 && kind(ex[1][1]) == K"Identifier"
            name = ex[1][1]
            type_params = ex[1][2:end]
            supertype = ex[2]
        end
    end
    @isdefined(name) || throw(LoweringError(ex, "invalid type signature"))

    return (name, type_params, supertype)
end

# Expand type_params into (typevar_names, typevar_stmts) where
# - `typevar_names` are the names of the type's type parameters
# - `typevar_stmts` are a list of statements to define a `TypeVar` for each parameter
#   name in `typevar_names`, to be emitted prior to uses of `typevar_names`.
#   There is exactly one statement from each typevar.
function expand_typevars!(ctx, typevar_names, typevar_stmts, type_params)
    for param in type_params
        bounds = analyze_typevar(ctx, param)
        n = bounds[1]
        push!(typevar_names, n)
        push!(typevar_stmts, @ast ctx param [K"block"
            [K"local" n]
            [K"=" n bounds_to_TypeVar(ctx, param, bounds)]
        ])
    end
    return nothing
end

function expand_typevars(ctx, type_params)
    typevar_names = SyntaxList(ctx)
    typevar_stmts = SyntaxList(ctx)
    expand_typevars!(ctx, typevar_names, typevar_stmts, type_params)
    return (typevar_names, typevar_stmts)
end

function expand_abstract_or_primitive_type(ctx, ex)
    is_abstract = kind(ex) == K"abstract"
    if is_abstract
        @chk numchildren(ex) == 1
    else
        @assert kind(ex) == K"primitive"
        @chk numchildren(ex) == 2
        nbits = ex[2]
    end
    name, type_params, supertype = analyze_type_sig(ctx, ex[1])
    typevar_names, typevar_stmts = expand_typevars(ctx, type_params)
    newtype_var = ssavar(ctx, ex, "new_type")
    @ast ctx ex [K"block"
        [K"scope_block"(scope_type=:hard)
            [K"block"
                [K"local" name]
                [K"always_defined" name]
                typevar_stmts...
                [K"="
                    newtype_var
                    [K"call"
                        (is_abstract ? "_abstracttype" : "_primitivetype")::K"core"
                        ctx.mod::K"Value"
                        name=>K"Symbol"
                        [K"call" "svec"::K"core" typevar_names...]
                        if !is_abstract
                            nbits
                        end
                    ]
                ]
                [K"=" name newtype_var]
                [K"call" "_setsuper!"::K"core" newtype_var supertype]
                [K"call" "_typebody!"::K"core" false::K"Bool" name]
            ]
        ]
        [K"assert" "toplevel_only"::K"Symbol" [K"inert" ex] ]
        [K"global" name]
        [K"if"
            [K"&&"
                [K"call"
                   "isdefinedglobal"::K"core"
                   ctx.mod::K"Value"
                   name=>K"Symbol"
                   false::K"Bool"]
                [K"call" "_equiv_typedef"::K"core" name newtype_var]
            ]
            nothing_(ctx, ex)
            [K"constdecl" name newtype_var]
        ]
        nothing_(ctx, ex)
    ]
end

function _match_struct_field(x0)
    type=nothing
    docs=nothing
    atomic=false
    _const=false
    x = x0
    while true
        k = kind(x)
        if k == K"Identifier"
            return (name=x, type=type, atomic=atomic, _const=_const, docs=docs)
        elseif k == K"::" && numchildren(x) == 2
            isnothing(type) || throw(LoweringError(x0, "multiple types in struct field"))
            type = x[2]
            x = x[1]
        elseif k == K"atomic"
            atomic = true
            x = x[1]
        elseif k == K"const"
            _const = true
            x = x[1]
        elseif k == K"doc"
            docs = x[1]
            x = x[2]
        else
            return nothing
        end
    end
end

function _collect_struct_fields(ctx, field_names, field_types, field_attrs, field_docs, inner_defs, exs)
    for e in exs
        if kind(e) == K"block"
            _collect_struct_fields(ctx, field_names, field_types, field_attrs, field_docs,
                                   inner_defs, children(e))
        elseif kind(e) == K"="
            throw(LoweringError(e, "assignment syntax in structure fields is reserved"))
        else
            m = _match_struct_field(e)
            if !isnothing(m)
                # Struct field
                push!(field_names, m.name)
                n = length(field_names)
                push!(field_types, isnothing(m.type) ? @ast(ctx, e, "Any"::K"core") : m.type)
                if m.atomic
                    push!(field_attrs, @ast ctx e n::K"Integer")
                    push!(field_attrs, @ast ctx e "atomic"::K"Symbol")
                end
                if m._const
                    push!(field_attrs, @ast ctx e n::K"Integer")
                    push!(field_attrs, @ast ctx e "const"::K"Symbol")
                end
                if !isnothing(m.docs)
                    push!(field_docs, @ast ctx e n::K"Integer")
                    push!(field_docs, @ast ctx e m.docs)
                end
            else
                # Inner constructors and inner functions
                # TODO: Disallow arbitrary expressions inside `struct`?
                push!(inner_defs, e)
            end
        end
    end
end

# generate call to `convert()` for `(call new ...)` expressions
function _new_call_convert_arg(ctx, full_struct_type, field_type, field_index, val)
    if is_core_Any(field_type)
        return val
    end
    # kt = kind(field_type)
    # TODO: Allow kt == K"Identifier" && kt in static_params to avoid fieldtype call?
    @ast ctx field_type [K"block"
        tmp_type := [K"call"
            "fieldtype"::K"core"
            full_struct_type
            field_index::K"Integer"
        ]
        convert_for_type_decl(ctx, field_type, val, tmp_type, false)
    ]
end

function default_inner_constructors(ctx, srcref, global_struct_name,
                                    typevar_names, typevar_stmts, field_names, field_types)
    # TODO: Consider using srcref = @HERE ?
    exact_ctor = if isempty(typevar_names)
        # Definition with exact types for all arguments
        field_decls = SyntaxList(ctx)
        @ast ctx srcref [K"function"
            [K"call"
                [K"::" [K"curly" "Type"::K"core" global_struct_name]]
                [[K"::" n t] for (n,t) in zip(field_names, field_types)]...
            ]
            [K"new"
                global_struct_name
                field_names...
            ]
        ]
    end
    maybe_non_Any_field_types = filter(!is_core_Any, field_types)
    converting_ctor = if !isempty(typevar_names) || !isempty(maybe_non_Any_field_types)
        # Definition which takes `Any` for all arguments and uses
        # `Base.convert()` to convert those to the exact field type. Only
        # defined if at least one field type is not Any.
        ctor_self = new_local_binding(ctx, srcref, "#ctor-self#"; kind=:argument)
        @ast ctx srcref [K"function"
            [K"call"
                 [K"::"
                     ctor_self
                     if isempty(typevar_names)
                         [K"curly" "Type"::K"core" global_struct_name]
                     else
                         [K"where"
                             [K"curly"
                                 "Type"::K"core"
                                 [K"curly"
                                     global_struct_name
                                     typevar_names...
                                 ]
                             ]
                             [K"_typevars" [K"block" typevar_names...] [K"block" typevar_stmts...]]
                         ]
                     end
                ]
                field_names...
            ]
            [K"block"
                [K"new"
                    ctor_self
                    [_new_call_convert_arg(ctx, ctor_self, type, i, name)
                     for (i, (name,type)) in enumerate(zip(field_names, field_types))]...
                ]
            ]
        ]
    end
    if isnothing(exact_ctor)
        converting_ctor
    else
        if isnothing(converting_ctor)
            exact_ctor
        else
            @ast ctx srcref [K"block"
                [K"if"
                    # Only define converting_ctor if at least one field type is not Any.
                    mapfoldl(t     -> [K"call" "==="::K"core" "Any"::K"core" t],
                             (t,u) -> [K"&&" u t],
                             maybe_non_Any_field_types)
                    [K"block"]
                    converting_ctor
                ]
                exact_ctor
            ]
        end
    end
end

# Generate outer constructor for structs with type parameters. Eg, for
#     struct X{U,V}
#         x::U
#         y::V
#     end
#
# We basically generate
#     function (::Type{X})(x::U, y::V) where {U,V}
#         new(X{U,V}, x, y)
#     end
#
function default_outer_constructor(ctx, srcref, global_struct_name,
                                   typevar_names, typevar_stmts, field_names, field_types)
    @ast ctx srcref [K"function"
        [K"where"
            [K"call"
                # We use `::Type{$global_struct_name}` here rather than just
                # `struct_name` because global_struct_name is a binding to a
                # type - we know we're not creating a new `Function` and
                # there's no reason to emit the 1-arg `Expr(:method, name)` in
                # the next phase of expansion.
                [K"::" [K"curly" "Type"::K"core" global_struct_name]]
                [[K"::" n t] for (n,t) in zip(field_names, field_types)]...
            ]
            [K"_typevars" [K"block" typevar_names...] [K"block" typevar_stmts...]]
        ]
        [K"new" [K"curly" global_struct_name typevar_names...] field_names...]
    ]
end

function _is_new_call(ex)
    kind(ex) == K"call" &&
        ((kind(ex[1]) == K"Identifier" && ex[1].name_val == "new") ||
         (kind(ex[1]) == K"curly" && kind(ex[1][1]) == K"Identifier" && ex[1][1].name_val == "new"))
end

# Rewrite inner constructor signatures for struct `X` from `X(...)`
# to `(ctor_self::Type{X})(...)`
function _rewrite_ctor_sig(ctx, callex, struct_name, global_struct_name, struct_typevars, ctor_self)
    @assert kind(callex) == K"call"
    name = callex[1]
    if is_same_identifier_like(struct_name, name)
        # X(x,y)  ==>  (#ctor-self#::Type{X})(x,y)
        ctor_self[] = new_local_binding(ctx, callex, "#ctor-self#"; kind=:argument)
        @ast ctx callex [K"call"
            [K"::"
                ctor_self[]
                [K"curly" "Type"::K"core" global_struct_name]
            ]
            callex[2:end]...
        ]
    elseif kind(name) == K"curly" && is_same_identifier_like(struct_name, name[1])
        # X{T}(x,y)  ==>  (#ctor-self#::Type{X{T}})(x,y)
        self = new_local_binding(ctx, callex, "#ctor-self#"; kind=:argument)
        if numchildren(name) - 1 == length(struct_typevars)
            # Self fully parameterized - can be used as the full type to
            # rewrite new() calls in constructor body.
            ctor_self[] = self
        end
        @ast ctx callex [K"call"
            [K"::"
                self
                [K"curly"
                    "Type"::K"core"
                    [K"curly"
                        global_struct_name
                        name[2:end]...
                    ]
                ]
            ]
            callex[2:end]...
        ]
    else
        callex
    end
end

# Rewrite calls to `new` in bodies of inner constructors and inner functions
# into `new` or `splatnew` expressions.  For example:
#
#     struct X{T,S}
#         X() = new()
#         X() = new{A,B}()
#         X{T,S}() where {T,S} = new()
#         X{A,B}() = new()
#         X{A}() = new()
#         (t::Type{X})() = new{A,B}()
#         f() = new()
#         f() = new{A,B}()
#         f() = new{Ts...}()
#     end
#
# Map to the following
#
#     X() = ERROR
#     (#ctor-self#::Type{X})() = (new X{A,B})
#     (Type{X{T,S}}() where {T,S} = (new #ctor-self#)
#     X{A,B}() = (new #ctor-self#)
#     X{A}() = ERROR
#     (t::Type{X})() = (new X{A,B})
#     f() = ERROR
#     f() = (new X{A,B})
#     f() = (new X{Ts...})
#
# TODO: Arguably the following "could also work", but any symbolic match of
# this case would be heuristic and rely on assuming Type == Core.Type. So
# runtime checks would really be required and flisp lowering doesn't catch
# this case either.
#
#     (t::Type{X{A,B}})() = new()
function _rewrite_ctor_new_calls(ctx, ex, struct_name, global_struct_name, ctor_self,
                                 struct_typevars, field_types)
    if is_leaf(ex)
        return ex
    elseif !_is_new_call(ex)
        return mapchildren(
            e->_rewrite_ctor_new_calls(ctx, e, struct_name, global_struct_name,
                                       ctor_self, struct_typevars, field_types),
            ctx, ex
        )
    end
    # Rewrite a call to new()
    kw_arg_i = findfirst(e->(k = kind(e); k == K"=" || k == K"parameters"), children(ex))
    if !isnothing(kw_arg_i)
        throw(LoweringError(ex[kw_arg_i], "`new` does not accept keyword arguments"))
    end
    full_struct_type = if kind(ex[1]) == K"curly"
        # new{A,B}(...)
        new_type_params = ex[1][2:end]
        n_type_splat = sum(kind(t) == K"..." for t in new_type_params; init=0)
        n_type_nonsplat = length(new_type_params) - n_type_splat
        if n_type_splat == 0 && n_type_nonsplat < length(struct_typevars)
            throw(LoweringError(ex[1], "too few type parameters specified in `new{...}`"))
        elseif n_type_nonsplat > length(struct_typevars)
            throw(LoweringError(ex[1], "too many type parameters specified in `new{...}`"))
        end
        @ast ctx ex[1] [K"curly" global_struct_name new_type_params...]
    elseif !isnothing(ctor_self)
        # new(...) in constructors
        ctor_self
    else
        # new(...) inside non-constructor inner functions
        if isempty(struct_typevars)
            global_struct_name
        else
            throw(LoweringError(ex[1], "too few type parameters specified in `new`"))
        end
    end
    new_args = ex[2:end]
    n_splat = sum(kind(t) == K"..." for t in new_args; init=0)
    n_nonsplat = length(new_args) - n_splat
    n_fields = length(field_types)
    function throw_n_fields_error(desc)
        @ast ctx ex [K"call"
            "throw"::K"core"
            [K"call"
                "ArgumentError"::K"top"
                "too $desc arguments in `new` (expected $n_fields)"::K"String"
            ]
        ]
    end
    if n_nonsplat > n_fields
        return throw_n_fields_error("many")
    else
        # "Too few" args are allowed in partially initialized structs
    end
    if n_splat == 0
        @ast ctx ex [K"block"
            struct_type := full_struct_type
            [K"new"
                struct_type
                [_new_call_convert_arg(ctx, struct_type, type, i, name)
                 for (i, (name,type)) in enumerate(zip(ex[2:end], field_types))]...
            ]
        ]
    else
        fields_all_Any = all(is_core_Any, field_types)
        if fields_all_Any
            @ast ctx ex [K"block"
                struct_type := full_struct_type
                [K"splatnew"
                    struct_type
                    # Note: `jl_new_structt` ensures length of this tuple is
                    # exactly the number of fields.
                    [K"call" "tuple"::K"core" ex[2:end]...]
                ]
            ]
        else
            # `new` with splatted args which are symbolically not `Core.Any`
            # (might be `Any` at runtime but we can't know that here.)
            @ast ctx ex [K"block"
                args := [K"call" "tuple"::K"core" ex[2:end]...]
                n_args := [K"call" "nfields"::K"core" args]
                [K"if"
                    [K"call" "ult_int"::K"top" n_args n_fields::K"Integer"]
                    throw_n_fields_error("few")
                ]
                [K"if"
                    [K"call" "ult_int"::K"top" n_fields::K"Integer" n_args]
                    throw_n_fields_error("many")
                ]
                struct_type := full_struct_type
                [K"new"
                    struct_type
                    [_new_call_convert_arg(ctx, struct_type, type, i,
                         [K"call" "getfield"::K"core" args i::K"Integer"])
                     for (i, type) in enumerate(field_types)]...
                ]
            ]
        end
    end
end

# Rewrite calls to `new( ... )` to `new` expressions on the appropriate
# type, determined by the containing type and constructor definitions.
#
# This is mainly for constructors, but also needs to work for inner functions
# which may call new() but are not constructors.
function rewrite_new_calls(ctx, ex, struct_name, global_struct_name,
                           typevar_names, field_names, field_types)
    if kind(ex) == K"doc"
        docs = ex[1]
        ex = ex[2]
    else
        docs = nothing
    end
    if kind(ex) != K"function"
        return ex
    end
    if !(numchildren(ex) == 2 && is_eventually_call(ex[1]))
        throw(LoweringError(ex, "Expected constructor or named inner function"))
    end

    ctor_self = Ref{Union{Nothing,SyntaxTree}}(nothing)
    expand_function_def(ctx, ex, docs,
        callex->_rewrite_ctor_sig(ctx, callex, struct_name,
                                  global_struct_name, typevar_names, ctor_self),
        body->_rewrite_ctor_new_calls(ctx, body, struct_name, global_struct_name,
                                      ctor_self[], typevar_names, field_types)
    )
end

function _constructor_min_initialized(ex::SyntaxTree)
    if _is_new_call(ex)
        if any(kind(e) == K"..." for e in ex[2:end])
            # Lowering ensures new with splats always inits all fields
            # or in the case of splatnew this is enforced by the runtime.
            typemax(Int)
        else
            numchildren(ex) - 1
        end
    elseif !is_leaf(ex)
        minimum((_constructor_min_initialized(e) for e in children(ex)), init=typemax(Int))
    else
        typemax(Int)
    end
end

# Let S be a struct we're defining in module M.  Below is a hack to allow its
# field types to refer to S as M.S.  See #56497.
function insert_struct_shim(ctx, fieldtypes, name)
    function replace_type(ex)
        if kind(ex) == K"." &&
            numchildren(ex) == 2 &&
            kind(ex[2]) == K"Symbol" &&
            ex[2].name_val == name.name_val
            @ast ctx ex [K"call" "struct_name_shim"::K"core" ex[1] ex[2] ctx.mod::K"Value" name]
        elseif numchildren(ex) > 0
            mapchildren(replace_type, ctx, ex)
        else
            ex
        end
    end
    map(replace_type, fieldtypes)
end

function expand_struct_def(ctx, ex, docs)
    @chk numchildren(ex) == 2
    type_sig = ex[1]
    type_body = ex[2]
    if kind(type_body) != K"block"
        throw(LoweringError(type_body, "expected block for `struct` fields"))
    end
    struct_name, type_params, supertype = analyze_type_sig(ctx, type_sig)
    typevar_names, typevar_stmts = expand_typevars(ctx, type_params)
    field_names = SyntaxList(ctx)
    field_types = SyntaxList(ctx)
    field_attrs = SyntaxList(ctx)
    field_docs = SyntaxList(ctx)
    inner_defs = SyntaxList(ctx)
    _collect_struct_fields(ctx, field_names, field_types, field_attrs, field_docs,
                           inner_defs, children(type_body))
    is_mutable = has_flags(ex, JuliaSyntax.MUTABLE_FLAG)
    min_initialized = minimum((_constructor_min_initialized(e) for e in inner_defs),
                              init=length(field_names))
    newtype_var = ssavar(ctx, ex, "struct_type")
    hasprev = ssavar(ctx, ex, "hasprev")
    prev = ssavar(ctx, ex, "prev")
    newdef = ssavar(ctx, ex, "newdef")
    layer = new_scope_layer(ctx, struct_name)
    global_struct_name = adopt_scope(struct_name, layer)
    if !isempty(typevar_names)
        # Generate expression like `prev_struct.body.body.parameters`
        prev_typevars = global_struct_name
        for _ in 1:length(typevar_names)
            prev_typevars = @ast ctx type_sig [K"." prev_typevars "body"::K"Symbol"]
        end
        prev_typevars = @ast ctx type_sig [K"." prev_typevars "parameters"::K"Symbol"]
    end

    # New local variable names for constructor args to avoid clashing with any
    # type names
    if isempty(inner_defs)
        field_names_2 = adopt_scope(field_names, layer)
    end

    need_outer_constructor = false
    if isempty(inner_defs) && !isempty(typevar_names)
        # To generate an outer constructor each struct type parameter must be
        # able to be inferred from the list of fields passed as constructor
        # arguments.
        #
        # More precisely, it must occur in a field type, or in the bounds of a
        # subsequent type parameter. For example the following won't work
        #     struct X{T}
        #         a::Int
        #     end
        #     X(a::Int) where T = #... construct X{T} ??
        #
        # But the following does
        #     struct X{T}
        #         a::T
        #     end
        #     X(a::T) where {T} = # construct X{typeof(a)}(a)
        need_outer_constructor = true
        for i in 1:length(typevar_names)
            typevar_name = typevar_names[i]
            typevar_in_fields = any(contains_identifier(ft, typevar_name) for ft in field_types)
            if !typevar_in_fields
                typevar_in_bounds = any(type_params[i+1:end]) do param
                    # Check the bounds of subsequent type params
                    (_,lb,ub) = analyze_typevar(ctx, param)
                    # todo: flisp lowering tests `lb` here so we also do. But
                    # in practice this doesn't seem to constrain `typevar_name`
                    # and the generated constructor doesn't work?
                    (!isnothing(ub) && contains_identifier(ub, typevar_name)) ||
                    (!isnothing(lb) && contains_identifier(lb, typevar_name))
                end
                if !typevar_in_bounds
                    need_outer_constructor = false
                    break
                end
            end
        end
    end

    # The following lowering covers several subtle issues in the ordering of
    # typevars when "redefining" structs.
    # See https://github.com/JuliaLang/julia/pull/36121
    @ast ctx ex [K"block"
        [K"assert" "toplevel_only"::K"Symbol" [K"inert" ex] ]
        [K"scope_block"(scope_type=:hard)
            # Needed for later constdecl to work, though plain global form may be removed soon.
            [K"global" global_struct_name]
            [K"block"
                [K"local" struct_name]
                [K"always_defined" struct_name]
                typevar_stmts...
                [K"="
                    newtype_var
                    [K"call"
                        "_structtype"::K"core"
                        ctx.mod::K"Value"
                        struct_name=>K"Symbol"
                        [K"call"(type_sig) "svec"::K"core" typevar_names...]
                        [K"call"(type_body) "svec"::K"core" [n=>K"Symbol" for n in field_names]...]
                        [K"call"(type_body) "svec"::K"core" field_attrs...]
                        is_mutable::K"Bool"
                        min_initialized::K"Integer"
                    ]
                ]
                [K"=" struct_name newtype_var]
                [K"call"(supertype) "_setsuper!"::K"core" newtype_var supertype]
                [K"=" hasprev
                      [K"&&" [K"call" "isdefinedglobal"::K"core"
                              ctx.mod::K"Value"
                              struct_name=>K"Symbol"
                              false::K"Bool"]
                             [K"call" "_equiv_typedef"::K"core" global_struct_name newtype_var]
                       ]]
                [K"=" prev [K"if" hasprev global_struct_name false::K"Bool"]]
                [K"if" hasprev
                   [K"block"
                    # if this is compatible with an old definition, use the old parameters, but the
                    # new object. This will fail to capture recursive cases, but the call to typebody!
                    # below is permitted to choose either type definition to put into the binding table
                    if !isempty(typevar_names)
                        # And resassign the typevar_names - these may be
                        # referenced in the definition of the field
                        # types below
                        [K"=" [K"tuple" typevar_names...] prev_typevars]
                    end
                    ]
                ]
                [K"=" newdef
                   [K"call"(type_body)
                      "_typebody!"::K"core"
                      prev
                      newtype_var
                      [K"call" "svec"::K"core" insert_struct_shim(ctx, field_types, struct_name)...]
                   ]]
                [K"constdecl"
                    global_struct_name
                    newdef
                 ]
                # Default constructors
                if isempty(inner_defs)
                    default_inner_constructors(ctx, ex, global_struct_name,
                                               typevar_names, typevar_stmts, field_names_2, field_types)
                else
                    map!(inner_defs, inner_defs) do def
                        rewrite_new_calls(ctx, def, struct_name, global_struct_name,
                                          typevar_names, field_names, field_types)
                    end
                    [K"block" inner_defs...]
                end
                if need_outer_constructor
                    default_outer_constructor(ctx, ex, global_struct_name,
                                              typevar_names, typevar_stmts, field_names_2, field_types)
                end
            ]
        ]

        # Documentation
        if !isnothing(docs) || !isempty(field_docs)
            [K"call"(isnothing(docs) ? ex : docs)
                bind_docs!::K"Value"
                struct_name
                isnothing(docs) ? nothing_(ctx, ex) : docs[1]
                ::K"SourceLocation"(ex)
                [K"="
                    "field_docs"::K"Identifier"
                    [K"call" "svec"::K"core" field_docs...]
                ]
            ]
        end
        nothing_(ctx, ex)
    ]
end

#-------------------------------------------------------------------------------
# Expand `where` syntax

function expand_where(ctx, srcref, lhs, rhs)
    bounds = analyze_typevar(ctx, rhs)
    v = bounds[1]
    @ast ctx srcref [K"let"
        [K"block" [K"=" v bounds_to_TypeVar(ctx, srcref, bounds)]]
        [K"call" "UnionAll"::K"core" v lhs]
    ]
end

function expand_wheres(ctx, ex)
    body = ex[1]
    rhs = ex[2]
    if kind(rhs) == K"braces"
        # S{X,Y} where {X,Y}
        for r in reverse(children(rhs))
            body = expand_where(ctx, ex, body, r)
        end
    elseif kind(rhs) == K"_typevars"
        # Eg, `S{X,Y} where {X, Y}` but with X and Y
        # already allocated `TypeVar`s
        for r in reverse(children(rhs[1]))
            body = @ast ctx ex [K"call" "UnionAll"::K"core" r body]
        end
    else
        # S{X} where X
        body = expand_where(ctx, ex, body, rhs)
    end
    body
end

# Match implicit where parameters for `Foo{<:Bar}` ==> `Foo{T} where T<:Bar`
function expand_curly(ctx, ex)
    @assert kind(ex) == K"curly"
    check_no_parameters(ex, "unexpected semicolon in type parameter list")
    check_no_assignment(children(ex), "misplace assignment in type parameter list")

    typevar_stmts = SyntaxList(ctx)
    type_args = SyntaxList(ctx)
    implicit_typevars = SyntaxList(ctx)

    i = 1
    for e in children(ex)
        k = kind(e)
        if (k == K"<:" || k == K">:") && numchildren(e) == 1
            # `X{<:A}` and `X{>:A}`
            name = @ast ctx e "#T$i"::K"Placeholder"
            i += 1
            typevar = k == K"<:" ?
                bounds_to_TypeVar(ctx, e, (name, nothing, e[1])) :
                bounds_to_TypeVar(ctx, e, (name, e[1], nothing))
            arg = emit_assign_tmp(typevar_stmts, ctx, typevar)
            push!(implicit_typevars, arg)
        else
            arg = e
        end
        push!(type_args, arg)
    end

    type = @ast ctx ex [K"call" "apply_type"::K"core" type_args...]
    if !isempty(implicit_typevars)
        type = @ast ctx ex [K"block"
            typevar_stmts...
            [K"where" type [K"_typevars" [K"block" implicit_typevars...] [K"block" typevar_stmts...]]]
        ]
    end

    return type
end

#-------------------------------------------------------------------------------
# Expand import / using / export

function expand_importpath(path)
    @chk kind(path) == K"importpath"
    path_spec = Expr(:.)
    prev_was_dot = true
    for component in children(path)
        k = kind(component)
        if k == K"quote"
            # Permit quoted path components as in
            # import A.(:b).:c
            component = component[1]
        end
        @chk kind(component) in (K"Identifier", K".")
        name = component.name_val
        is_dot = kind(component) == K"."
        if is_dot && !prev_was_dot
            throw(LoweringError(component, "invalid import path: `.` in identifier path"))
        end
        prev_was_dot = is_dot
        push!(path_spec.args, Symbol(name))
    end
    return path_spec
end

function expand_import_or_using(ctx, ex)
    if kind(ex[1]) == K":"
        # import M: x.y as z, w
        # (import (: (importpath M) (as (importpath x y) z) (importpath w)))
        # =>
        # (call module_import
        #  false
        #  (call core.svec "M")
        #  (call core.svec  2 "x" "y" "z"  1 "w" "w"))
        @chk numchildren(ex[1]) >= 2
        from = ex[1][1]
        from_path = @ast ctx from QuoteNode(expand_importpath(from))::K"Value"
        paths = ex[1][2:end]
    else
        # import A.B
        # (using (importpath A B))
        # (call eval_import true nothing (call core.svec 1 "w"))
        @chk numchildren(ex) >= 1
        from_path = nothing
        paths = children(ex)
    end
    # Here we represent the paths as quoted `Expr` data structures
    path_specs = SyntaxList(ctx)
    for spec in paths
        as_name = nothing
        if kind(spec) == K"as"
            @chk numchildren(spec) == 2
            @chk kind(spec[2]) == K"Identifier"
            as_name = Symbol(spec[2].name_val)
            path = QuoteNode(Expr(:as, expand_importpath(spec[1]), as_name))
        else
            path = QuoteNode(expand_importpath(spec))
        end
        push!(path_specs, @ast ctx spec path::K"Value")
    end
    is_using = kind(ex) == K"using"
    stmts = SyntaxList(ctx)
    if isnothing(from_path)
        for spec in path_specs
            if is_using
                push!(stmts,
                    @ast ctx spec [K"call"
                        eval_using   ::K"Value"
                        ctx.mod      ::K"Value"
                        spec
                    ]
                )
            else
                push!(stmts,
                    @ast ctx spec [K"call"
                        eval_import   ::K"Value"
                        (!is_using)   ::K"Bool"
                        ctx.mod       ::K"Value"
                        "nothing"     ::K"top"
                        spec
                    ]
                )
            end
            # latestworld required between imports so that previous symbols
            # become visible
            push!(stmts, @ast ctx spec (::K"latestworld"))
        end
    else
        push!(stmts, @ast ctx ex [K"call"
            eval_import   ::K"Value"
            (!is_using)   ::K"Bool"
            ctx.mod       ::K"Value"
            from_path
            path_specs...
        ])
        push!(stmts, @ast ctx ex (::K"latestworld"))
    end
    @ast ctx ex [K"block"
        [K"assert" "toplevel_only"::K"Symbol" [K"inert" ex]]
        stmts...
        [K"removable" "nothing"::K"core"]
    ]
end

# Expand `public` or `export`
function expand_public(ctx, ex)
    identifiers = String[]
    for e in children(ex)
        @chk kind(e) == K"Identifier" (ex, "Expected identifier")
        push!(identifiers, e.name_val)
    end
    (e.name_val::K"String" for e in children(ex))
    @ast ctx ex [K"call"
        eval_public::K"Value"
        ctx.mod::K"Value"
        (kind(ex) == K"export")::K"Bool"
        identifiers::K"Value"
    ]
end

#-------------------------------------------------------------------------------
# Expand docstring-annotated expressions

function expand_doc(ctx, ex, docex, mod=ctx.mod)
    if kind(ex) in (K"Identifier", K".")
        expand_forms_2(ctx, @ast ctx docex [K"call"
            bind_static_docs!::K"Value"
            (kind(ex) === K"." ? ex[1] : ctx.mod::K"Value")
            (kind(ex) === K"." ? ex[2] : ex).name_val::K"Symbol"
            docex[1]
            ::K"SourceLocation"(ex)
            Union{}::K"Value"
        ])
    elseif is_eventually_call(ex)
        expand_function_def(ctx, @ast(ctx, ex, [K"function" ex [K"block"]]),
                            docex; doc_only=true)
    else
        expand_forms_2(ctx, ex, docex)
    end
end

#-------------------------------------------------------------------------------
# Desugaring's "big switch": expansion of some simple forms; dispatch to other
# expansion functions for the rest.

"""
Lowering pass 2 - desugaring

This pass simplifies expressions by expanding complicated syntax sugar into a
small set of core syntactic forms. For example, field access syntax `a.b` is
expanded to a function call `getproperty(a, :b)`.
"""
function expand_forms_2(ctx::DesugaringContext, ex::SyntaxTree, docs=nothing)
    k = kind(ex)
    if k == K"atomic"
        throw(LoweringError(ex, "unimplemented or unsupported atomic declaration"))
    elseif k == K"call"
        expand_call(ctx, ex)
    elseif k == K"dotcall" || k == K".&&" || k == K".||" || k == K".="
        expand_forms_2(ctx, expand_fuse_broadcast(ctx, ex))
    elseif k == K"."
        expand_forms_2(ctx, expand_dot(ctx, ex))
    elseif k == K"?"
        @chk numchildren(ex) == 3
        expand_forms_2(ctx, @ast ctx ex [K"if" children(ex)...])
    elseif k == K"&&" || k == K"||"
        @chk numchildren(ex) > 1
        cs = expand_cond_children(ctx, ex)
        # Attributing correct provenance for `cs[1:end-1]` is tricky in cases
        # like `a && (b && c)` because the expression constructed here arises
        # from the source fragment `a && (b` which doesn't follow the tree
        # structure. For now we attribute to the parent node.
        cond = length(cs) == 2 ?
            cs[1] :
            makenode(ctx, ex, k, cs[1:end-1])
        # This transformation assumes the type assertion `cond::Bool` will be
        # added by a later compiler pass (currently done in codegen)
        if k == K"&&"
            @ast ctx ex [K"if" cond cs[end] false::K"Bool"]
        else
            @ast ctx ex [K"if" cond true::K"Bool" cs[end]]
        end
    elseif k == K"::"
        @chk numchildren(ex) == 2 "`::` must be written `value::type` outside function argument lists"
        @ast ctx ex [K"call"
            "typeassert"::K"core"
            expand_forms_2(ctx, ex[1])
            expand_forms_2(ctx, ex[2])
        ]
    elseif k == K"<:" || k == K">:" || k == K"-->"
        expand_forms_2(ctx, @ast ctx ex [K"call"
            adopt_scope(string(k)::K"Identifier", ex)
            children(ex)...
        ])
    elseif k == K"op=" || k == K".op="
        expand_forms_2(ctx, expand_update_operator(ctx, ex))
    elseif k == K"="
        expand_assignment(ctx, ex)
    elseif k == K"break"
        numchildren(ex) > 0 ? ex :
            @ast ctx ex [K"break" "loop_exit"::K"symbolic_label"]
    elseif k == K"continue"
        @ast ctx ex [K"break" "loop_cont"::K"symbolic_label"]
    elseif k == K"comparison"
        expand_forms_2(ctx, expand_compare_chain(ctx, ex))
    elseif k == K"doc"
        @chk numchildren(ex) == 2
        expand_doc(ctx, ex[2], ex)
    elseif k == K"for"
        expand_forms_2(ctx, expand_for(ctx, ex))
    elseif k == K"comprehension"
        @chk numchildren(ex) == 1
        @chk kind(ex[1]) == K"generator"
        @ast ctx ex [K"call"
            "collect"::K"top"
            expand_forms_2(ctx, ex[1])
        ]
    elseif k == K"typed_comprehension"
        @chk numchildren(ex) == 2
        @chk kind(ex[2]) == K"generator"
        if numchildren(ex[2]) == 2 && kind(ex[2][2]) == K"iteration"
            # Hack to lower simple typed comprehensions to loops very early,
            # greatly reducing the number of functions and load on the compiler
            expand_forms_2(ctx, expand_comprehension_to_loops(ctx, ex))
        else
            @ast ctx ex [K"call"
                "collect"::K"top"
                expand_forms_2(ctx, ex[1])
                expand_forms_2(ctx, ex[2])
            ]
        end
    elseif k == K"generator"
        expand_forms_2(ctx, expand_generator(ctx, ex))
    elseif k == K"->" || k == K"do"
        expand_forms_2(ctx, expand_arrow(ctx, ex))
    elseif k == K"function"
        expand_forms_2(ctx, expand_function_def(ctx, ex, docs))
    elseif k == K"macro"
        @ast ctx ex [K"block"
            [K"assert"
                "global_toplevel_only"::K"Symbol"
                [K"inert" ex]
            ]
            expand_forms_2(ctx, expand_macro_def(ctx, ex))
        ]
    elseif k == K"if" || k == K"elseif"
        @chk numchildren(ex) >= 2
        @ast ctx ex [k
            expand_condition(ctx, ex[1])
            expand_forms_2(ctx, ex[2:end])...
        ]
    elseif k == K"let"
        expand_forms_2(ctx, expand_let(ctx, ex))
    elseif k == K"const"
        expand_const_decl(ctx, ex)
    elseif k == K"local" || k == K"global"
        if k == K"global" && kind(ex[1]) == K"const"
            # Normalize `global const` to `const global`
            expand_const_decl(ctx, @ast ctx ex [K"const" [K"global" ex[1][1]]])
        else
            expand_decls(ctx, ex)
        end
    elseif k == K"where"
        expand_forms_2(ctx, expand_wheres(ctx, ex))
    elseif k == K"braces" || k == K"bracescat"
        throw(LoweringError(ex, "{ } syntax is reserved for future use"))
    elseif k == K"string"
        if numchildren(ex) == 1 && kind(ex[1]) == K"String"
            ex[1]
        else
            @ast ctx ex [K"call"
                "string"::K"top"
                expand_forms_2(ctx, children(ex))...
            ]
        end
    elseif k == K"try"
        expand_forms_2(ctx, expand_try(ctx, ex))
    elseif k == K"tuple"
        if has_parameters(ex)
            if numchildren(ex) > 1
                throw(LoweringError(ex[end], "unexpected semicolon in tuple - use `,` to separate tuple elements"))
            end
            expand_forms_2(ctx, expand_named_tuple(ctx, ex, children(ex[1])))
        elseif any_assignment(children(ex))
            expand_forms_2(ctx, expand_named_tuple(ctx, ex, children(ex)))
        else
            expand_forms_2(ctx, @ast ctx ex [K"call"
                "tuple"::K"core"
                children(ex)...
            ])
        end
    elseif k == K"$"
        throw(LoweringError(ex, "`\$` expression outside string or quote block"))
    elseif k == K"module"
        throw(LoweringError(ex, "`module` is only allowed at top level"))
    elseif k == K"import" || k == K"using"
        expand_import_or_using(ctx, ex)
    elseif k == K"export" || k == K"public"
        expand_public(ctx, ex)
    elseif k == K"abstract" || k == K"primitive"
        expand_forms_2(ctx, expand_abstract_or_primitive_type(ctx, ex))
    elseif k == K"struct"
        expand_forms_2(ctx, expand_struct_def(ctx, ex, docs))
    elseif k == K"ref"
        sctx = with_stmts(ctx)
        (arr, idxs) = expand_ref_components(sctx, ex)
        expand_forms_2(ctx,
            @ast ctx ex [K"block"
                sctx.stmts...
                [K"call"
                    "getindex"::K"top"
                    arr
                    idxs...
                ]
            ]
        )
    elseif k == K"curly"
        expand_forms_2(ctx, expand_curly(ctx, ex))
    elseif k == K"toplevel"
        # The toplevel form can't be lowered here - it needs to just be quoted
        # and passed through to a call to eval.
        ex2 = @ast ctx ex [K"block"
            [K"assert" "toplevel_only"::K"Symbol" [K"inert" ex]]
            [K"call"
                eval                  ::K"Value"
                ctx.mod               ::K"Value"
                [K"inert" ex]
                [K"parameters"
                    [K"="
                        "expr_compat_mode"::K"Identifier"
                        ctx.expr_compat_mode::K"Bool"
                    ]
                ]
            ]
        ]
        expand_forms_2(ctx, ex2)
    elseif k == K"vect"
        check_no_parameters(ex, "unexpected semicolon in array expression")
        expand_array(ctx, ex, "vect")
    elseif k == K"hcat"
        expand_array(ctx, ex, "hcat")
    elseif k == K"typed_hcat"
        expand_array(ctx, ex, "typed_hcat")
    elseif k == K"opaque_closure"
        expand_forms_2(ctx, expand_opaque_closure(ctx, ex))
    elseif k == K"vcat" || k == K"typed_vcat"
        expand_forms_2(ctx, expand_vcat(ctx, ex))
    elseif k == K"ncat" || k == K"typed_ncat"
        expand_forms_2(ctx, expand_ncat(ctx, ex))
    elseif k == K"while"
        @chk numchildren(ex) == 2
        @ast ctx ex [K"break_block" "loop_exit"::K"symbolic_label"
            [K"_while"
                expand_condition(ctx, ex[1])
                [K"break_block" "loop_cont"::K"symbolic_label"
                    [K"scope_block"(scope_type=:neutral)
                         expand_forms_2(ctx, ex[2])
                    ]
                ]
            ]
        ]
    elseif k == K"inert"
        ex
    elseif k == K"gc_preserve"
        s = ssavar(ctx, ex)
        r = ssavar(ctx, ex)
        @ast ctx ex [K"block"
            s := [K"gc_preserve_begin" children(ex)[2:end]...]
            r := expand_forms_2(ctx, children(ex)[1])
            [K"gc_preserve_end" s]
            r
        ]
    elseif k == K"&"
        throw(LoweringError(ex, "invalid syntax"))
    elseif k == K"$"
        throw(LoweringError(ex, "`\$` expression outside string or quote"))
    elseif k == K"..."
        throw(LoweringError(ex, "`...` expression outside call"))
    elseif is_leaf(ex)
        ex
    elseif k == K"return"
        if numchildren(ex) == 0
            @ast ctx ex [K"return" "nothing"::K"core"]
        elseif numchildren(ex) == 1
            mapchildren(e->expand_forms_2(ctx,e), ctx, ex)
        else
            throw(LoweringError(ex, "More than one argument to return"))
        end
    else
        mapchildren(e->expand_forms_2(ctx,e), ctx, ex)
    end
end

function expand_forms_2(ctx::DesugaringContext, exs::Union{Tuple,AbstractVector})
    res = SyntaxList(ctx)
    for e in exs
        push!(res, expand_forms_2(ctx, e))
    end
    res
end

function expand_forms_2(ctx::StatementListCtx, args...)
    expand_forms_2(ctx.ctx, args...)
end

@fzone "JL: desugar" function expand_forms_2(ctx::MacroExpansionContext, ex::SyntaxTree)
    ctx1 = DesugaringContext(ctx, ctx.expr_compat_mode)
    ex1 = expand_forms_2(ctx1, reparent(ctx1, ex))
    ctx1, ex1
end

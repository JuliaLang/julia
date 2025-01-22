# Lowering Pass 2 - syntax desugaring 

struct DesugaringContext{GraphType} <: AbstractLoweringContext
    graph::GraphType
    bindings::Bindings
    scope_layers::Vector{ScopeLayer}
    mod::Module
end

function DesugaringContext(ctx)
    graph = ensure_attributes(syntax_graph(ctx),
                              kind=Kind, syntax_flags=UInt16,
                              source=SourceAttrType,
                              value=Any, name_val=String,
                              scope_type=Symbol, # :hard or :soft
                              var_id=IdTag,
                              is_toplevel_thunk=Bool)
    DesugaringContext(graph, ctx.bindings, ctx.scope_layers, ctx.current_layer.mod)
end

#-------------------------------------------------------------------------------

function is_identifier_like(ex)
    k = kind(ex)
    k == K"Identifier" || k == K"BindingId" || k == K"Placeholder"
end

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
        k == K"inert" || k == K"top" || k == K"core"
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
# tuple. Includes support for slurping/splatting.
#
# If lhss and rhss are the list of terms on each side, this function assumes
# the following have been checked:
#   * There's only one `...` on the left hand side
#   * Neither side has any key=val terms
#   * _tuple_sides_match returns true
function tuple_to_assignments(ctx, ex)
    lhs = ex[1]
    rhs = ex[2]
    stmts = SyntaxList(ctx)
    end_stmts = SyntaxList(ctx)
    elements = SyntaxList(ctx)
    assigned = SyntaxList(ctx)

    il = 0
    ir = 0
    while il < numchildren(lhs)
        il += 1
        ir += 1
        lh = lhs[il]
        if kind(lh) == K"..."
            TODO(lhs, "... in tuple lhs")
            n_lhs = numchildren(lhs)
            n_rhs = numchildren(rhs)
            if il == n_lhs
                # Simple case: exactly one `...` at end of lhs. Examples:
                #   (x, ys...) = (a,b,c)
                #   (ys...)    = ()
                rhs_tmp = emit_assign_tmp(stmts, ctx,
                    @ast(ctx, rhs, [K"tuple" rhs[ir:end]...]),
                    "rhs_tmp"
                )
                push!(stmts, @ast ctx ex [K"=" lh[1] rhs_tmp])
                push!(elements, @ast ctx rhs_tmp [K"..." rhs_tmp])
                break
            else
                # Exactly one lhs `...` occurs in the middle somewhere, with a
                # general rhs which has one `...` term or at least as many
                # non-`...` terms.
                # Examples:
                #   (x, ys..., z) = (a, b, c, d)
                #   (x, ys..., z) = (a, bs...)
                #   (xs..., y)    = (a, bs...)
                # in this case we pairwise-match arguments from the end
                # backward, with rhs splats falling back to the general case.
                jl = n_lhs + 1
                jr = n_rhs + 1
                while jl > il && jr > ir
                    if kind(lhs[jl-1]) == K"..." || kind(rhs[jr-1]) == K"..."
                        break
                    end
                    jl -= 1
                    jr -= 1
                end
                rhs[jr]
            end
            continue
        end
        rh = rhs[ir] # In other cases `rhs[ir]` must exist
        if kind(rh) == K"..."
            @assert ir == numchildren(rhs) # _tuple_sides_match ensures this
            rh_tmp = emit_assign_tmp(stmts, ctx, rh[1])
            push!(end_stmts, @ast ctx ex [K"=" [K"tuple" lhs[il:end]...] rh_tmp])
            push!(elements, @ast ctx rh [K"..." rh_tmp])
            break
        else
            if is_identifier_like(lh) && is_effect_free(rh) &&
                    !any(contains_identifier(rhs[j], lh) for j in ir+1:lastindex(rhs))
                    !any(contains_identifier(a, rh) for a in assigned)
                # Overwrite `lh` directly if that won't cause conflicts with
                # other symbols
                push!(stmts, @ast ctx ex [K"=" lh rh])
                push!(assigned, lh)
                push!(elements, rh)
            else
                # In other cases we need a temporary and we'll overwrite `lh` at the end.
                tmp = ssavar(ctx, rh)
                push!(stmts,     @ast ctx ex [K"=" tmp rh])
                # `push!(assigned, lh)` is not required when we assign `lh` later.
                push!(end_stmts, @ast ctx ex [K"=" lh  tmp])
                push!(elements, tmp)
            end
        end
    end

    @ast ctx ex [K"block"
        stmts...
        end_stmts...
        [K"unnecessary" [K"tuple" elements...]]
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
function _destructure(ctx, assignment_srcref, stmts, lhs, rhs)
    n_lhs = numchildren(lhs)
    if n_lhs > 0
        iterstate = new_local_binding(ctx, rhs, "iterstate")
    end

    end_stmts = SyntaxList(ctx)

    i = 0
    for lh in children(lhs)
        i += 1
        if kind(lh) == K"..."
            lh1 = if is_identifier_like(lh[1])
                lh[1]
            else
                lhs_tmp = ssavar(ctx, lh[1], "lhs_tmp")
                push!(end_stmts, expand_forms_2(ctx, @ast ctx lh[1] [K"=" lh[1] lhs_tmp]))
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
            lh1 = if is_identifier_like(lh)
                lh
            # elseif is_eventually_call(lh) (TODO??)
            else
                lhs_tmp = ssavar(ctx, lh, "lhs_tmp")
                push!(end_stmts, expand_forms_2(ctx, @ast ctx lh [K"=" lh lhs_tmp]))
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
function expand_property_destruct(ctx, ex)
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
    rhs1 = if is_ssa(ctx, rhs) || (is_identifier_like(rhs) &&
                                   !any(is_same_identifier_like(l, rhs) for l in children(params)))
        rhs
    else
        emit_assign_tmp(stmts, ctx, expand_forms_2(ctx, rhs))
    end
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
    push!(stmts, @ast ctx rhs1 [K"unnecessary" rhs1])
    makenode(ctx, ex, K"block", stmts)
end

# Expands all cases of general tuple destructuring, eg
#   (x,y) = (a,b)
function expand_tuple_destruct(ctx, ex)
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
        num_splat = sum(kind(rh) == K"..." for rh in children(rhs))
        if num_splat == 0 && (numchildren(lhs) - num_slurp) > numchildren(rhs)
            throw(LoweringError(ex, "More variables on left hand side than right hand in tuple assignment"))
        end

        if !any_assignment(children(rhs)) && !has_parameters(rhs) &&
                _tuple_sides_match(children(lhs), children(rhs))
            return expand_forms_2(ctx, tuple_to_assignments(ctx, ex))
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
    _destructure(ctx, ex, stmts, lhs, rhs1)
    push!(stmts, @ast ctx rhs1 [K"unnecessary" rhs1])
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
        [K"unnecessary" rhs]
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
    elseif (k == K"&&" || k == K"||") && is_dotted(ex)
        @ast ctx ex [K"call"
            "broadcasted"::K"top"
            (k == K"&&" ? "andand" : "oror")::K"top"
            (expand_dotcall(ctx, arg) for arg in children(ex))...
        ]
    else
        ex
    end
end

function expand_fuse_broadcast(ctx, ex)
    if kind(ex) == K"="
        @assert is_dotted(ex)
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
# - ragged colum first or row first
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
        # number of elements in each ND slice of the array, from layout
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
function expand_unionall_def(ctx, srcref, lhs, rhs)
    if numchildren(lhs) <= 1
        throw(LoweringError(lhs, "empty type parameter list in type alias"))
    end
    name = lhs[1]
    @ast ctx srcref [K"block"
        [K"const_if_global" name]
        unionall_type := expand_forms_2(ctx, [K"where" rhs lhs[2:end]...])
        expand_forms_2(ctx, [K"=" name unionall_type])
    ]
end

# Expand general assignment syntax, including
#   * UnionAll definitions
#   * Chained assignments
#   * Setting of structure fields
#   * Assignments to array elements
#   * Destructuring
#   * Typed variable declarations
function expand_assignment(ctx, ex)
    @chk numchildren(ex) == 2
    lhs = ex[1]
    rhs = ex[2]
    kl = kind(lhs)
    if kl == K"curly"
        expand_unionall_def(ctx, ex, lhs, rhs)
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
        for i in 1:length(stmts)
            stmts[i] = @ast ctx ex [K"=" stmts[i] rr]
        end
        if !isnothing(tmp_rhs)
            pushfirst!(stmts, @ast ctx ex [K"=" tmp_rhs rhs])
        end
        expand_forms_2(ctx,
            @ast ctx ex [K"block"
                stmts...
                [K"unnecessary" rr]
            ]
        )
    elseif is_identifier_like(lhs)
        sink_assignment(ctx, ex, lhs, expand_forms_2(ctx, rhs))
    elseif kl == K"."
        # a.b = rhs  ==>  setproperty!(a, :b, rhs)
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
            [K"unnecessary" rhs]
        ]
    elseif kl == K"tuple"
        if has_parameters(lhs)
            expand_property_destruct(ctx, ex)
        else
            expand_tuple_destruct(ctx, ex)
        end
    elseif kl == K"ref"
        # a[i1, i2] = rhs
        expand_forms_2(ctx, expand_setindex(ctx, ex))
    elseif kl == K"::" && numchildren(lhs) == 2
        x = lhs[1]
        T = lhs[2]
        res = if is_identifier_like(x)
            # Identifer in lhs[1] is a variable type declaration, eg
            # x::T = rhs
            @ast ctx ex [K"block"
                [K"decl" lhs[1] lhs[2]]
                [K"=" lhs[1] rhs]
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
    dotted = is_dotted(ex)

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
        [K"="(syntax_flags=(dotted ? JuliaSyntax.DOTOP_FLAG : nothing))
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
        @ast ctx ex [K"block" ex[1:end-1]... test]
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
            if is_sym_decl(lhs)
                blk = @ast ctx binding [K"block"
                    tmp := rhs
                    [K"scope_block"(ex, scope_type=scope_type)
                        # TODO: Use single child for scope_block?
                        [K"local_def"(lhs) lhs] # TODO: Use K"local" with attr?
                        [K"="(rhs)
                            decl_var(lhs)
                            tmp
                        ]
                        blk
                    ]
                ]
            else
                TODO("Functions and multiple assignment")
            end
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

function expand_ccall(ctx, ex)
    @assert kind(ex) == K"call" && is_same_identifier_like(ex[1], "ccall")
    if numchildren(ex) < 4
        throw(LoweringError(ex, "too few arguments to ccall"))
    end
    cfunc_name = ex[2]
    # Detect calling convention if present.
    #
    # Note `@ccall` also emits `Expr(:cconv, convention, nreq)`, but this is a
    # somewhat undocumented performance workaround. Instead we should just make
    # sure @ccall can emit foreigncall directly and efficiently.
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
        if kind(raw_argt) == K"core" && raw_argt.name_val == "Any"
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
            expand_forms_2(ctx, cfunc_name)
            expand_forms_2(ctx, return_type)
            [K"call"
                "svec"::K"core"
                expanded_types...
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

# Wrap unsplatted arguments in `tuple`:
# `[a, b, xs..., c]` -> `[(a, b), xs, (c,)]`
function _wrap_unsplatted_args(ctx, call_ex, args)
    wrapped = SyntaxList(ctx)
    i = 1
    while i <= length(args)
        if kind(args[i]) == K"..."
            splatarg = args[i]
            @chk numchildren(splatarg) == 1
            push!(wrapped, splatarg[1])
        else
            i1 = i
            # Find range of non-splatted args
            while i < length(args) && kind(args[i+1]) != K"..."
                i += 1
            end
            push!(wrapped, @ast ctx call_ex [K"call" "tuple"::K"core" args[i1:i]...])
        end
        i += 1
    end
    wrapped
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
    if is_same_identifier_like(farg, "ccall")
        return expand_ccall(ctx, ex)
    end
    args = copy(ex[2:end])
    kws = remove_kw_args!(ctx, args)
    if !isnothing(kws)
        return expand_forms_2(ctx, expand_kw_call(ctx, ex, farg, args, kws))
    end
    if any(kind(arg) == K"..." for arg in args)
        # Splatting, eg, `f(a, xs..., b)`
        @ast ctx ex [K"call" 
            "_apply_iterate"::K"core"
            "iterate"::K"top"
            expand_forms_2(ctx, farg)
            expand_forms_2(ctx, _wrap_unsplatted_args(ctx, ex, args))...
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

function foreach_lhs_var(f::Function, ex)
    k = kind(ex)
    if k == K"Identifier"
        f(ex)
    elseif k == K"Placeholder"
        # Ignored
    else
        TODO(ex, "LHS vars")
    end
end

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
            foreach_lhs_var(lhs) do var
                @chk kind(var) == K"Identifier"
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
        foreach_lhs_var(lhs) do var
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

# Strip variable type declarations from within a `local` or `global`, returning
# the stripped expression. Works recursively with complex left hand side
# assignments containing tuple destructuring. Eg, given
#   (x::T, (y::U, z))
#   strip out stmts = (local x) (decl x T) (local x) (decl y U) (local z)
#   and return (x, (y, z))
function strip_decls!(ctx, stmts, declkind, declkind2, declmeta, ex)
    k = kind(ex)
    if k == K"Identifier"
        if !isnothing(declmeta)
            push!(stmts, makenode(ctx, ex, declkind, ex; meta=declmeta))
        else
            push!(stmts, makenode(ctx, ex, declkind, ex))
        end
        if !isnothing(declkind2)
            push!(stmts, makenode(ctx, ex, declkind2, ex))
        end
        ex
    elseif k == K"Placeholder"
        ex
    elseif k == K"::"
        @chk numchildren(ex) == 2
        name = ex[1]
        @chk kind(name) == K"Identifier"
        push!(stmts, makenode(ctx, ex, K"decl", name, ex[2]))
        strip_decls!(ctx, stmts, declkind, declkind2, declmeta, ex[1])
    elseif k == K"tuple" || k == K"parameters"
        cs = SyntaxList(ctx)
        for e in children(ex)
            push!(cs, strip_decls!(ctx, stmts, declkind, declkind2, declmeta, e))
        end
        makenode(ctx, ex, k, cs)
    end
end

# local x, (y=2), z ==> local x; local y; y = 2; local z
# const x = 1       ==> const x; x = 1
# global x::T = 1   ==> (block (global x) (decl x T) (x = 1))
function expand_decls(ctx, ex)
    declkind = kind(ex)
    declmeta = get(ex, :meta, nothing)
    if numchildren(ex) == 1 && kind(ex[1]) ∈ KSet"const global local"
        declkind2 = kind(ex[1])
        bindings = children(ex[1])
    else
        declkind2 = nothing
        bindings = children(ex)
    end
    stmts = SyntaxList(ctx)
    for binding in bindings
        kb = kind(binding)
        if is_prec_assignment(kb)
            @chk numchildren(binding) == 2
            lhs = strip_decls!(ctx, stmts, declkind, declkind2, declmeta, binding[1])
            push!(stmts, @ast ctx binding [kb lhs binding[2]])
        elseif is_sym_decl(binding)
            if declkind == K"const" || declkind2 == K"const"
                throw(LoweringError(ex, "expected assignment after `const`"))
            end
            strip_decls!(ctx, stmts, declkind, declkind2, declmeta, binding)
        else
            throw(LoweringError(ex, "invalid syntax in variable declaration"))
        end
    end
    makenode(ctx, ex, K"block", stmts)
end

#-------------------------------------------------------------------------------
# Expansion of function definitions

function match_function_arg(full_ex)
    name = nothing
    type = nothing
    default = nothing
    is_slurp = false
    ex = full_ex
    while true
        k = kind(ex)
        if k == K"Identifier" || k == K"Placeholder" || k == K"tuple"
            name = ex
            break
        elseif k == K"::"
            @chk numchildren(ex) in (1,2)
            if numchildren(ex) == 1
                type = ex[1]
            else
                name = ex[1]
                type = ex[2]
            end
            break
        elseif k == K"..."
            @chk !is_slurp (full_ex,"nested `...` in function argument")
            @chk numchildren(ex) == 1
            is_slurp = true
            ex = ex[1]
        elseif k == K"="
            if !isnothing(default)
                throw(full_ex, "multiple defaults provided with `=` in function argument")
            end
            default = ex[2]
            ex = ex[1]
        else
            throw(LoweringError(ex, "Invalid function argument"))
        end
    end
    return (name=name,
            type=type,
            default=default,
            is_slurp=is_slurp)
end

# Expand `where` clause(s) of a function into (typevar_names, typevar_stmts) where
# - `typevar_names` are the names of the type's type parameters
# - `typevar_stmts` are a list of statements to define a `TypeVar` for each parameter
#   name in `typevar_names`, to be emitted prior to uses of `typevar_names`.
#   There is exactly one statement from each typevar.
function _split_wheres!(ctx, typevar_names, typevar_stmts, ex)
    if kind(ex) == K"where" && numchildren(ex) == 2
        vars_kind = kind(ex[2])
        if vars_kind == K"_typevars"
            append!(typevar_names, children(ex[2]))
        else
            params = vars_kind == K"braces" ? ex[2][1:end] : ex[2:2]
            expand_typevars!(ctx, typevar_names, typevar_stmts, params)
        end
        _split_wheres!(ctx, typevar_names, typevar_stmts, ex[1])
    else
        ex
    end
end

function method_def_expr(ctx, srcref, callex, method_table,
                         docs, typevar_names, arg_names, arg_types, ret_var, body)
    @ast ctx srcref [K"block"
        # metadata contains svec(types, sparms, location)
        method_metadata := [K"call"(callex)
            "svec"              ::K"core"
            [K"call"
                "svec"          ::K"core"
                arg_types...
            ]
            [K"call"
                "svec"          ::K"core"
                typevar_names...
            ]
            QuoteNode(source_location(LineNumberNode, callex))::K"Value"
        ]
        [K"method"
            method_table
            method_metadata
            [K"lambda"(body, is_toplevel_thunk=false)
                [K"block" arg_names...]
                [K"block" typevar_names...]
                body
                ret_var  # might be `nothing` and hence removed
            ]
        ]
        [K"unnecessary" method_metadata]
    ]
end

function trim_used_typevars(ctx, arg_types, typevar_names, typevar_stmts)
    n_typevars = length(typevar_names)
    @assert n_typevars == length(typevar_stmts)
    # Filter typevar names down to those which are directly used in the arg list
    typevar_used = [contains_identifier(tn, arg_types) for tn in typevar_names]
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
    trimmed_typevar_names = SyntaxList(ctx)
    for (used,tn) in zip(typevar_used, typevar_names)
        if used
            push!(trimmed_typevar_names, tn)
        end
    end
    return trimmed_typevar_names
end

# Generate a method for every number of allowed optional arguments
# For example for `f(x, y=1, z=2)` we generate two additional methods
# f(x) = f(x, 1, 2)
# f(x, y) = f(x, y, 2)
function optional_positional_defs!(ctx, method_stmts, srcref, callex,
                                   method_table, typevar_names, typevar_stmts,
                                   arg_names, arg_types, first_default,
                                   arg_defaults, ret_var)
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
              method_def_expr(ctx, srcref, callex, method_table, nothing,
                              trimmed_typevar_names, trimmed_arg_names, trimmed_arg_types,
                              ret_var, body))
    end
end

# Check valid identifier/function names
function is_valid_func_name(ex)
    k = kind(ex)
    if k == K"Identifier"
        name = ex.name_val
    elseif k == K"." && numchildren(ex) == 2 && kind(ex[2]) == K"Symbol"
        # `function A.f(x,y) ...`
        name = ex[2].name_val
    else
        return false
    end
    return name != "ccall" && name != "cglobal"
end

function expand_function_def(ctx, ex, docs, rewrite_call=identity, rewrite_body=identity)
    @chk numchildren(ex) in (1,2)
    name = ex[1]
    if numchildren(ex) == 1 && is_identifier_like(name)
        # Function declaration with no methods
        if !is_valid_func_name(name)
            throw(LoweringError(name, "Invalid function name"))
        end
        return @ast ctx ex [K"method" name=>K"Symbol"]
    end

    typevar_names = SyntaxList(ctx)
    typevar_stmts = SyntaxList(ctx)
    if kind(name) == K"where"
        # `where` vars end up in two places
        # 1. Argument types - the `T` in `x::T` becomes a `TypeVar` parameter in
        #    the method sig, eg, `function f(x::T) where T ...`.  These define the
        #    static parameters of the method.
        # 2. In the method body - either explicitly or implicitly via the method
        #    return type or default arguments - where `T` turns up as the *name* of
        #    a special slot of kind ":static_parameter"
        name = _split_wheres!(ctx, typevar_names, typevar_stmts, name)
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
            name = ssavar(ctx, name, name.name_val)
            bare_func_name = name
        elseif !is_valid_func_name(name)
            throw(LoweringError(name, "Invalid function name"))
        elseif is_identifier_like(name)
            # Add methods to a global `Function` object, or local closure
            # type function f() ...
            bare_func_name = name
        else
            # Add methods to an existing Function
            # function A.B.f() ...
        end
        doc_obj = name # todo: can closures be documented?
        self_type = @ast ctx name [K"function_type" name]
    end
    # Add self argument
    if isnothing(self_name)
        self_name = new_local_binding(ctx, name, "#self#"; kind=:argument)
    end

    # Expand remaining argument names and types
    arg_names = SyntaxList(ctx)
    arg_types = SyntaxList(ctx)
    push!(arg_names, self_name)
    push!(arg_types, self_type)
    args = callex[2:end]
    body_stmts = SyntaxList(ctx)
    first_default = 0
    arg_defaults = SyntaxList(ctx)
    for (i,arg) in enumerate(args)
        info = match_function_arg(arg)
        aname = !isnothing(info.name) ? info.name : @ast ctx arg "_"::K"Placeholder"
        if kind(aname) == K"tuple"
            # Argument destructuring
            is_nospecialize = getmeta(arg, :nospecialize, false)
            n = new_local_binding(ctx, aname, "destructured_arg_$i";
                                kind=:argument, is_nospecialize=is_nospecialize)
            push!(body_stmts, @ast ctx aname [
                K"local"(meta=CompileHints(:is_destructured_arg, true))
                [K"=" aname n]
            ])
            aname = n
        end
        push!(arg_names, aname)

        atype = !isnothing(info.type) ? info.type : @ast ctx arg "Any"::K"core"
        if info.is_slurp
            if i != length(args)
                throw(LoweringError(arg, "`...` may only be used for the last function argument"))
            end
            atype = @ast ctx arg [K"curly" "Vararg"::K"core" atype]
        end
        if isnothing(info.default)
            if !isempty(arg_defaults) && !info.is_slurp
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
                throw(LoweringError(args[first_default], "optional positional arguments must occur at end"))
            end
        else
            if isempty(arg_defaults)
                first_default = i
            end
            push!(arg_defaults, info.default)
        end
        # TODO: Ideally, ensure side effects of evaluating arg_types only
        # happen once - we should create an ssavar if there's any following
        # defaults. (flisp lowering doesn't ensure this either). Beware if
        # fixing this that optional_positional_defs! depends on filtering the
        # *symbolic* representation of arg_types.
        push!(arg_types, atype)
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

    method_table_val = nothing # TODO: method overlays
    method_table = isnothing(method_table_val)            ?
                   @ast(ctx, callex, "nothing"::K"core")  :
                   ssavar(ctx, ex, "method_table")
    method_stmts = SyntaxList(ctx)

    if !isempty(arg_defaults)
        first_default += 1 # Offset for self argument
        optional_positional_defs!(ctx, method_stmts, ex, callex,
                                  method_table, typevar_names, typevar_stmts,
                                  arg_names, arg_types, first_default, arg_defaults, ret_var)
    end

    # The method with all non-default arguments
    push!(method_stmts,
          method_def_expr(ctx, ex, callex, method_table, docs,
                          typevar_names, arg_names, arg_types, ret_var, body))
    if !isnothing(docs)
        method_stmts[end] = @ast ctx docs [K"block"
            method_metadata := method_stmts[end]
            @ast ctx docs [K"call"
                bind_docs!::K"Value"
                doc_obj
                docs[1]
                method_metadata
            ]
        ]
    end

    @ast ctx ex [K"block"
        if !isnothing(bare_func_name)
            [K"function_decl"(bare_func_name) bare_func_name]
        end
        [K"scope_block"(scope_type=:hard)
            [K"method_defs"
                isnothing(bare_func_name) ? "nothing"::K"core" : bare_func_name
                [K"block"
                    typevar_stmts...
                    if !isnothing(method_table_val)
                        [K"=" method_table method_table_val]
                    end
                    method_stmts...
                ]
            ]
        ]
        if !isnothing(bare_func_name)
            # K"function_decl" ensures this name is defined
            bare_func_name
        else
            "nothing"::K"core"
        end
    ]
end

function expand_arrow_arglist(ctx, arglist, arrowname)
    k = kind(arglist)
    if k == K"where"
        @ast ctx arglist [K"where"
            expand_arrow_arglist(ctx, arglist[1], arrowname)
            argslist[2]
        ]
    else
        # The arglist can sometimes be parsed as a block, or something else, and
        # fixing this is extremely awkward when nested inside `where`. See
        # https://github.com/JuliaLang/JuliaSyntax.jl/pull/522
        if k == K"block"
            @chk numchildren(arglist) == 2
            arglist = @ast ctx arglist [K"tuple"
                ex[1]
                [K"parameters" ex[2]]
            ]
        elseif k != K"tuple"
            # `x::Int -> body`
            arglist = @ast ctx arglist [K"tuple"
                ex[1]
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
    ret = @ast ctx ex [K"function"
        [K"call"(sig)
            _make_macro_name(ctx, name)
            [K"::"
                adopt_scope(@ast(ctx, sig, "__context__"::K"Identifier"),
                            kind(name) == K"." ? name[1] : name)
                MacroContext::K"Value"
            ]
            # flisp: We don't mark these @nospecialize because all arguments to
            # new macros will be of type SyntaxTree
            args[2:end]...
        ]
        ex[2]
    ]
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
        (ex[2], ex[1], nothing)
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
                [K"local_def" name]
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
                [K"call" "_typebody!"::K"core" newtype_var]
            ]
        ]
        [K"assert" "toplevel_only"::K"Symbol" [K"inert" ex] ]
        [K"global" name]
        [K"const" name]
        [K"if"
            [K"&&"
                [K"isdefined" name]
                [K"call" "_equiv_typedef"::K"core" name newtype_var]
            ]
            nothing_(ctx, ex)
            [K"=" name newtype_var]
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
    if kind(field_type) == K"core" && field_type.name_val == "Any"
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
                                    typevar_names, field_names, field_types)
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
    maybe_non_Any_field_types = filter(field_types) do ft
        !(kind(ft) == K"core" && ft.name_val == "Any")
    end
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
                             [K"_typevars" typevar_names...]
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
                                   typevar_names, field_names, field_types)
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
            [K"_typevars" typevar_names...]
        ]
        [K"new" [K"curly" global_struct_name typevar_names...] field_names...]
    ]
end

function _new_call(ctx, ex, typevar_names, field_names, field_types)
    if has_keywords(ex)
        throw(LoweringError(""))
    end
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
        n_type_splat = sum(kind(t) == K"..." for t in new_type_params)
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
    n_splat = sum(kind(t) == K"..." for t in new_args)
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
        fields_all_Any = all(kind(ft) == K"core" && ft.name_val == "Any" for ft in field_types)
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

function _constructor_min_initalized(ex::SyntaxTree)
    if _is_new_call(ex)
        if any(kind(e) == K"..." for e in ex[2:end])
            # Lowering ensures new with splats always inits all fields
            # or in the case of splatnew this is enforced by the runtime.
            typemax(Int)
        else
            numchildren(ex) - 1
        end
    elseif !is_leaf(ex)
        minimum((_constructor_min_initalized(e) for e in children(ex)), init=typemax(Int))
    else
        typemax(Int)
    end
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
    min_initialized = minimum((_constructor_min_initalized(e) for e in inner_defs),
                              init=length(field_names))
    newtype_var = ssavar(ctx, ex, "struct_type")
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
        # able to be inferred from the list of fields passed as constuctor
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
                    # TODO: flisp lowering tests `lb` here so we also do. But
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
            [K"block"
                [K"global" global_struct_name]
                [K"const" global_struct_name]
                [K"local_def" struct_name]
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
                [K"if"
                    [K"isdefined" global_struct_name]
                    [K"if"
                        [K"call" "_equiv_typedef"::K"core" global_struct_name newtype_var]
                        [K"block"
                            # If this is compatible with an old definition, use
                            # the existing type object and throw away the new
                            # type
                            [K"=" struct_name global_struct_name]
                            if !isempty(typevar_names)
                                # And resassign the typevar_names - these may be
                                # referenced in the definition of the field
                                # types below
                                [K"="
                                    [K"tuple" typevar_names...]
                                    prev_typevars
                                ]
                            end
                        ]
                        # Otherwise do an assignment to trigger an error
                        [K"=" global_struct_name struct_name]
                    ]
                    [K"=" global_struct_name struct_name]
                ]
                [K"call"(type_body)
                    "_typebody!"::K"core"
                    struct_name
                    [K"call" "svec"::K"core" field_types...]
                ]
                # Default constructors
                if isempty(inner_defs)
                    default_inner_constructors(ctx, ex, global_struct_name,
                                               typevar_names, field_names_2, field_types)
                else
                    map!(inner_defs, inner_defs) do def
                        rewrite_new_calls(ctx, def, struct_name, global_struct_name,
                                          typevar_names, field_names, field_types)
                    end
                    [K"block" inner_defs...]
                end
                if need_outer_constructor
                    default_outer_constructor(ctx, ex, global_struct_name,
                                              typevar_names, field_names_2, field_types)
                end
            ]
        ]

        # Documentation
        if !isnothing(docs) || !isempty(field_docs)
            [K"call"(isnothing(docs) ? ex : docs)
                bind_docs!::K"Value"
                struct_name
                isnothing(docs) ? nothing_(ctx, ex) : docs[1]
                QuoteNode(source_location(LineNumberNode, ex))::K"Value"
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
        for r in reverse(children(rhs))
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

    stmts = SyntaxList(ctx)
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
            arg = emit_assign_tmp(stmts, ctx, typevar)
            push!(implicit_typevars, arg)
        else
            arg = e
        end
        push!(type_args, arg)
    end

    type = @ast ctx ex [K"call" "apply_type"::K"core" type_args...]
    if !isempty(implicit_typevars)
        type = @ast ctx ex [K"block"
            stmts...
            [K"where" type [K"_typevars" implicit_typevars...]]
        ]
    end

    return type
end

#-------------------------------------------------------------------------------
# Expand import / using / export

function _append_importpath(ctx, path_spec, path)
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
        push!(path_spec, @ast(ctx, component, name::K"String"))
    end
    path_spec
end

function expand_import(ctx, ex)
    is_using = kind(ex) == K"using"
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
        @chk kind(from) == K"importpath"
        from_path = @ast ctx from [K"call"
            "svec"::K"core"
            _append_importpath(ctx, SyntaxList(ctx), from)...
        ]
        paths = ex[1][2:end]
    else
        # import A.B
        # (using (importpath A B))
        # (call module_import true nothing (call core.svec 1 "w"))
        @chk numchildren(ex) >= 1
        from_path = nothing_(ctx, ex)
        paths = children(ex)
    end
    path_spec = SyntaxList(ctx)
    for path in paths
        as_name = nothing
        if kind(path) == K"as"
            @chk numchildren(path) == 2
            as_name = path[2]
            @chk kind(as_name) == K"Identifier"
            path = path[1]
        end
        @chk kind(path) == K"importpath"
        push!(path_spec, @ast(ctx, path, numchildren(path)::K"Integer"))
        _append_importpath(ctx, path_spec, path)
        push!(path_spec, isnothing(as_name) ? nothing_(ctx, ex) :
                         @ast(ctx, as_name, as_name.name_val::K"String"))
    end
    @ast ctx ex [K"block"
        [K"assert" "toplevel_only"::K"Symbol" [K"inert" ex]]
        [K"call"
            module_import ::K"Value"
            ctx.mod       ::K"Value"
            is_using      ::K"Value"
            from_path
            [K"call"
                "svec"::K"core"
                path_spec...
            ]
        ]
    ]
end

#-------------------------------------------------------------------------------
# Expand module definitions

function expand_module(ctx, ex::SyntaxTree)
    modname_ex = ex[1]
    @chk kind(modname_ex) == K"Identifier"
    modname = modname_ex.name_val

    std_defs = if !has_flags(ex, JuliaSyntax.BARE_MODULE_FLAG)
        @ast ctx (@HERE) [
            K"block"
            [K"using"(@HERE)
                [K"importpath"
                    "Base"           ::K"Identifier"
                ]
            ]
            [K"function"(@HERE)
                [K"call"
                    "eval"           ::K"Identifier"
                    "x"              ::K"Identifier"
                ]
                [K"call"
                    "eval"           ::K"core"      
                    modname          ::K"Identifier"
                    "x"              ::K"Identifier"
                ]
            ]
            [K"function"(@HERE)
                [K"call"
                    "include"        ::K"Identifier"
                    "x"              ::K"Identifier"
                ]
                [K"call"
                    "_call_latest"   ::K"core"
                    "include"        ::K"top"
                    modname          ::K"Identifier"
                    "x"              ::K"Identifier"
                ]
            ]
            [K"function"(@HERE)
                [K"call"
                    "include"        ::K"Identifier"
                    [K"::"
                        "mapexpr"    ::K"Identifier"
                        "Function"   ::K"top"
                    ]
                    "x"              ::K"Identifier"
                ]
                [K"call"
                    "_call_latest"   ::K"core" 
                    "include"        ::K"top" 
                    "mapexpr"        ::K"Identifier" 
                    modname          ::K"Identifier" 
                    "x"              ::K"Identifier" 
                ]
            ]
        ]
    end

    body = ex[2]
    @chk kind(body) == K"block"

    @ast ctx ex [K"block"
        [K"assert"
            "global_toplevel_only"::K"Symbol"
            [K"inert" ex]
        ]
        [K"call"
            eval_module ::K"Value"
            ctx.mod     ::K"Value"
            modname     ::K"String"
            [K"inert"(body)
                [K"toplevel"
                    std_defs
                    children(body)...
                ]
            ]
        ]
    ]
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
    elseif k == K"dotcall" || ((k == K"&&" || k == K"||") && is_dotted(ex))
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
    elseif k == K"op="
        expand_forms_2(ctx, expand_update_operator(ctx, ex))
    elseif k == K"="
        if is_dotted(ex)
            expand_forms_2(ctx, expand_fuse_broadcast(ctx, ex))
        else
            expand_assignment(ctx, ex)
        end
    elseif k == K"break"
        numchildren(ex) > 0 ? ex :
            @ast ctx ex [K"break" "loop_exit"::K"symbolic_label"]
    elseif k == K"continue"
        @ast ctx ex [K"break" "loop_cont"::K"symbolic_label"]
    elseif k == K"comparison"
        expand_forms_2(ctx, expand_compare_chain(ctx, ex))
    elseif k == K"doc"
        @chk numchildren(ex) == 2
        sig = expand_forms_2(ctx, ex[2], ex)
    elseif k == K"for"
        expand_forms_2(ctx, expand_for(ctx, ex))
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
    elseif k == K"local" || k == K"global" || k == K"const"
        if numchildren(ex) == 1 && kind(ex[1]) == K"Identifier"
            # Don't recurse when already simplified - `local x`, etc
            ex
        else
            expand_forms_2(ctx, expand_decls(ctx, ex))
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
        expand_module(ctx, ex)
    elseif k == K"import" || k == K"using"
        expand_import(ctx, ex)
    elseif k == K"export" || k == K"public"
        TODO(ex)
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
        @ast ctx ex [K"block"
            [K"assert" "toplevel_only"::K"Symbol" [K"inert" ex]]
            [K"call"
                eval          ::K"Value"
                ctx.mod       ::K"Value"
                [K"inert" ex]
            ]
        ]
    elseif k == K"vect"
        check_no_parameters(ex, "unexpected semicolon in array expression")
        check_no_assignment(children(ex))
        @ast ctx ex [K"call"
            "vect"::K"top"
            expand_forms_2(ctx, children(ex))...
        ]
    elseif k == K"hcat"
        check_no_assignment(children(ex))
        @ast ctx ex [K"call"
            "hcat"::K"top"
            expand_forms_2(ctx, children(ex))...
        ]
    elseif k == K"typed_hcat"
        check_no_assignment(children(ex))
        @ast ctx ex [K"call"
            "typed_hcat"::K"top"
            expand_forms_2(ctx, children(ex))...
        ]
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
    elseif k == K"&"
        throw(LoweringError(ex, "invalid syntax"))
    elseif k == K"$"
        throw(LoweringError(ex, "`\$` expression outside string or quote"))
    elseif k == K"..."
        throw(LoweringError(ex, "`...` expression outside call"))
    elseif is_leaf(ex)
        ex
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

function expand_forms_2(ctx::MacroExpansionContext, ex::SyntaxTree)
    ctx1 = DesugaringContext(ctx)
    ex1 = expand_forms_2(ctx1, reparent(ctx1, ex))
    ctx1, ex1
end


# Lowering Pass 2 - syntax desugaring

struct DesugaringContext{Attrs} <: AbstractLoweringContext
    graph::SyntaxGraph{Attrs}
    bindings::Bindings
    scope_layers::Vector{ScopeLayer}
    mod::Module
    expr_compat_mode::Bool
    ssa_mapping::Dict{Int, IdTag}
    world::UInt
end

# Translate a K"ssavalue" node from pre-lowered code into a normal SSA binding.
# Uses ctx.ssa_mapping to ensure the same external SSA id maps to the same binding.
function _resolve_ssavalue(ctx::DesugaringContext, ex)
    binding_id = get!(ctx.ssa_mapping, ex[1].value) do
        s = ssavar(ctx, ex)
        s.var_id
    end
    binding_ex(ctx, binding_id)
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

# Return true if `f(e)` is true for any unquoted child of `ex`, recursively.
function contains_unquoted(f::Function, ex::SyntaxTree)
    if f(ex)
        return true
    elseif !is_leaf(ex) && !(kind(ex) in KSet"quote inert inert_syntaxtree meta")
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
        k == K"inert" || k == K"inert_syntaxtree" || k == K"top" ||
        k == K"core" || k == K"Value"
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
    i = findfirst(kind(e) == K"=" || kind(e) == K"kw" for e in exs)
    if !isnothing(i)
        throw(LoweringError(exs[i], msg))
    end
end

# Generating a new_local_binding or ssaval should only be done if we can
# guarantee there's some scope it's declared in, and that it's not declared or
# used outside of that scope (binding capture is OK).  This is the alternative.
function newsym(ctx, src::SyntaxTree, name::String; unused=false)
    out = newleaf(ctx, src, unused ? K"Placeholder" : K"Identifier", name)
    hasattr(src, :meta) && setattr!(out, :meta, src.meta)
    setattr!(out, :scope_layer, new_scope_layer(ctx))
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
        if is_literal(k) || k == K"Symbol" || k == K"inert" ||
            k == K"inert_syntaxtree" || k == K"top" || k == K"core" ||
            k == K"Value"
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
    @jl_assert is_identifier_like(lhs) lhs
    if kind(rhs) == K"block" && numchildren(rhs) > 0
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
    newnode(ctx, assignment_srcref, K"block", stmts)
end

# Implement destructuring with `lhs` a tuple expression (possibly with
# slurping) and `rhs` a general expression.
#
# Destructuring in this context is done via the iteration interface, though
# calls `Base.indexed_iterate()` to allow for a fast path in cases where the
# right hand side is directly indexable.
function _destructure(ctx, assignment_srcref, stmts, lhs, rhs, is_const)
    n_lhs = numchildren(lhs)
    iterstate = n_lhs > 0 ? new_local_binding(ctx, rhs, "iterstate") : nothing

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
function expand_property_destruct(ctx, ex)
    @jl_assert numchildren(ex) == 2 ex
    lhs = ex[1]
    @jl_assert kind(lhs) == K"tuple" ex
    if numchildren(lhs) != 1
        throw(LoweringError(lhs, "Property destructuring must use a single `;` before the property names, eg `(; a, b) = rhs`"))
    end
    params = lhs[1]
    @jl_assert kind(params) == K"parameters" ex
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
    newnode(ctx, ex, K"block", stmts)
end

# Expands all cases of general tuple destructuring, eg
#   (x,y) = (a,b)
function expand_tuple_destruct(ctx, ex, is_const)
    lhs = ex[1]
    @jl_assert kind(lhs) == K"tuple" ex
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
    newnode(ctx, ex, K"block", stmts)
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
    @jl_assert kind(ex) == K"comparison" ex
    terms = children(ex)
    @jl_assert numchildren(ex) >= 3 ex
    @jl_assert isodd(numchildren(ex)) ex
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
function _arg_to_temp(ctx, stmts, ex)
    k = kind(ex)
    if is_effect_free(ex)
        ex
    elseif k == K"..."
        @ast ctx ex [k _arg_to_temp(ctx, stmts, ex[1])]
    elseif k == K"kw"
        @ast ctx ex [K"kw" ex[1] _arg_to_temp(ctx, stmts, ex[2])]
    elseif k == K"parameters"
        mapchildren(ctx, ex) do e
            _arg_to_temp(ctx, stmts, e)
        end
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
            for e in children(ex)
                push!(args, _arg_to_temp(ctx, stmts, e))
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
    elseif k == K"kw"
        # note from flisp
        # TODO: this probably should not be allowed since keyword args aren't
        # positional, but in this context we have just used their positions anyway
        @ast ctx ex [K"kw" ex[1] replace_beginend(ctx, ex[2], arr, n, splats, is_last)]
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
    @jl_assert kind(ex) == K"ref" ex
    @jl_assert numchildren(ex) >= 1 ex
    arr = ex[1]
    idxs = ex[2:end]
    if any(contains_identifier(e, "begin", "end") for e in idxs)
        arr = emit_assign_tmp(sctx, arr)
    end
    new_idxs = process_indices(sctx, arr, idxs)
    return (arr, new_idxs)
end

function expand_setindex(ctx, ex)
    @jl_assert kind(ex) == K"=" && numchildren(ex) == 2 ex
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
        @jl_assert numchildren(ex) >= 1 ex
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
        @jl_assert numchildren(ex) == 2 ex
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
    elseif !is_leaf(ex) && !(kind(ex) in KSet"quote inert inert_syntaxtree meta function ->")
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
    @jl_assert numchildren(ex) >= 2 ex
    body = ex[1]
    check_no_return(body)
    if numchildren(ex) > 2
        # Uniquify outer vars by NameKey
        outervars_by_key = Dict{NameKey,typeof(ex)}()
        for iterspecs in ex[2:end-1]
            for iterspec in children(iterspecs)
                foreach_lhs_name(iterspec[1]) do var
                    @jl_assert kind(var) == K"Identifier" ex # Todo: K"BindingId"?
                    outervars_by_key[NameKey(var)] = var
                end
            end
        end
        outervar_assignments = SyntaxList(ctx)
        for (_,v) in sort(collect(pairs(outervars_by_key)), by=first)
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
            throw(LoweringError(ex, """Expected `K"iteration"` iteration specification in generator"""))
        end
        iter_ranges = SyntaxList(ctx)
        iter_lhss = SyntaxList(ctx)
        for iterspec in children(iterspecs)
            @jl_assert kind(iterspec) == K"in" iterspec
            @jl_assert numchildren(iterspec) == 2 iterspec
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
    @jl_assert kind(ex) == K"typed_comprehension" ex
    element_type = ex[1]
    gen = ex[2]
    @jl_assert kind(gen) == K"generator" ex
    body = gen[1]
    check_no_return(body)
    # TODO: check_no_break_continue
    iterspecs = gen[2]
    @jl_assert kind(iterspecs) == K"iteration" ex
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
        parent_layout_dim != 1 || throw(LoweringError(ex,"Badly nested rows in `ncat`"))
    elseif k == K"nrow"
        dim = JuliaSyntax.numeric_flags(ex)
        dim > 0                || throw(LoweringError(ex,"Unsupported dimension $dim in ncat"))
        !row_major || dim != 2 || throw(LoweringError(ex,"2D `nrow` cannot be mixed with `row` in `ncat`"))
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
    parent_layout_dim > layout_dim || throw(LoweringError(ex, "Badly nested rows in `ncat`"))
    for e in children(ex)
        if layout_dim == 1
            kind(e) ∉ KSet"nrow row" || throw(LoweringError(e,"Badly nested rows in `ncat`"))
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
    outer_dim = JuliaSyntax.numeric_flags(ex)
    @jl_assert outer_dim > 0 (ex,"Unsupported dimension in ncat")
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
    @jl_assert !row_major || outer_dim != 2 (ex,"2D `nrow` cannot be mixed with `row` in `ncat`")
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
        @jl_assert dimspan % prev_dimspan == 0 ex
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
    @jl_assert numchildren(ex) == 2 ex
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
    elseif kl == K"ssavalue"
        sink_assignment(ctx, ex, _resolve_ssavalue(ctx, lhs), expand_forms_2(ctx, rhs))
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
        @jl_assert !is_const (ex, "cannot declare `.` form const")
        @jl_assert numchildren(lhs) == 2 lhs
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
            expand_property_destruct(ctx, ex)
        else
            expand_tuple_destruct(ctx, ex, is_const)
        end
    elseif kl == K"ref"
        # a[i1, i2] = rhs
        @jl_assert !is_const (ex, "cannot declare ref form const")
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
                if kind(x) !== K"Placeholder"
                     [K"decl" x T]
                end
                is_const ? [K"const" [K"=" x rhs]] : [K"=" x rhs]
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

    @jl_assert numchildren(ex) == 3 ex
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
        @jl_assert length(cs) > 1 ex
        test = newnode(ctx, test, k, cs)
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
    @jl_assert numchildren(ex) == 2 ex
    bindings = ex[1]
    @jl_assert kind(bindings) == K"block" bindings
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

function expand_named_tuple(ctx, ex, kws; field_name="named tuple field",
                            element_name="named tuple element")
    name_strs = Set{String}()
    names = SyntaxList(ctx)
    values = SyntaxList(ctx)
    current_nt = nothing
    for kw in kws
        k = kind(kw)
        appended_nt = nothing
        name = value = nothing
        if kind(k) == K"Identifier"
            # x  ==>  x = x
            name = to_symbol(ctx, kw)
            value = kw
        elseif k == K"kw" || k == K"="
            # syntax TODO: This should parse to K"kw"
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
        elseif k == K"call" && numchildren(kw) == 3 &&
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
        if !isnothing(name) && !isnothing(value)
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
    @jl_assert !isnothing(current_nt) ex
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

# Special rule: Any becomes core.Any regardless of the module
# scope, and don't need GC roots.
function expand_ccall_argtype(ctx, ex)
    if is_same_identifier_like(ex, "Any")
        @ast ctx ex "Any"::K"core"
    else
        expand_forms_2(ctx, ex)
    end
end

# Expand the (sym,lib) argument to ccall
function expand_C_library_symbol(ctx, ex)
    @stm ex begin
        [K"tuple" _...] -> @ast ctx ex [K"static_eval"(
            meta=name_hint("function name and library expression"))
            mapchildren(e->expand_forms_2(ctx,e), ctx, ex)
        ]
        [K"static_eval" _] -> ex # already done
        _ -> expand_forms_2(ctx, ex)
    end
end

function expand_ccall(ctx, ex)
    @jl_assert kind(ex) == K"call" ex
    if numchildren(ex) < 4
        throw(LoweringError(ex, "too few arguments to ccall"))
    end
    cfunc_name = ex[2]
    # Detect calling convention if present.
    known_conventions = ("cdecl", "stdcall", "fastcall", "thiscall", "llvmcall")
    cconv = if kind(ex[3]) === K"cconv"
        ex[3]
    elseif any(is_same_identifier_like(ex[3], id) for id in known_conventions)
        ex[3]
    else
        nothing
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
            @jl_assert numchildren(va) == 1 va
            # Ok: vararg function
            vararg_type = expand_ccall_argtype(ctx, va[1])
            arg_types = arg_types[1:end-1]
            if length(arg_types) === 0
                throw(LoweringError(va, "C ABI prohibits vararg without one required argument"))
            end
        end
    end
    # todo: use multi-range errors here
    if length(args) < length(arg_types)
        throw(LoweringError(ex, "Too few arguments in ccall compared to argument types"))
    elseif length(args) > length(arg_types) && isnothing(vararg_type)
        throw(LoweringError(ex, "More arguments than types in ccall"))
    end
    sctx = with_stmts(ctx)
    expanded_types = SyntaxList(ctx)
    for argt in arg_types
        if kind(argt) == K"..."
            throw(LoweringError(argt, "only the trailing ccall argument type should have `...`"))
        end
        push!(expanded_types, expand_ccall_argtype(ctx, argt))
    end
    for _ in length(arg_types)+1:length(args)
        push!(expanded_types, vararg_type)
    end

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
            (cconv !== nothing && kind(cconv) === K"cconv" ? cconv[2].value :
                isnothing(vararg_type) ? 0 :
                length(arg_types))::K"Integer"
            if isnothing(cconv)
                "ccall"::K"Symbol"
            elseif kind(cconv) === K"cconv"
                @ast ctx cconv [K"inert" cconv[1]]
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
        if k == K"kw"
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
    if kind(farg) === K"Identifier" && farg.name_val === "ccall"
        return expand_ccall(ctx, ex)
    elseif kind(farg) === K"Identifier" && farg.name_val === "cglobal"
        return @ast ctx ex [K"call"
            [K"static_eval" ex[1]] # just so the globalref is inlined
            expand_forms_2(ctx, ex[2])
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
    elseif kind(farg) == K"Identifier" && farg.name_val === "include"
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
    @stm ex begin
        # eg, `f = .+`
        # Upstream TODO: Remove the (. +) representation and replace with use
        # of DOTOP_FLAG? This way, `K"."` will be exclusively used for
        # getproperty.
        [K"." op] -> @ast ctx ex [K"call" "BroadcastFunction"::K"top" op]
        [K"." l r] -> begin
            @jl_assert is_leaf(r) || kind(r) in KSet"inert inert_syntaxtree" ex
            @ast ctx ex [K"call" "getproperty"::K"top" l r]
        end
    end
end

#-------------------------------------------------------------------------------
# Expand for loops

function expand_for(ctx, ex)
    iterspecs = ex[1]

    @jl_assert kind(iterspecs) == K"iteration" ex

    # Loop variables not declared `outer` are reassigned for each iteration of
    # the innermost loop in case the user assigns them to something else.
    # (Maybe we should filter these to remove vars not assigned in the loop?
    # But that would ideally happen after the variable analysis pass, not
    # during desugaring.)
    copied_vars = SyntaxList(ctx)
    for iterspec in iterspecs[1:end-1]
        @jl_assert kind(iterspec) == K"in" iterspec
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
            @ast ctx ex [K"symbolicblock"
                "loop-cont"::K"symboliclabel"
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
                    [K"call" "==="::K"core" next (::K"nothing")]
                ]
                [K"_do_while"(ex)
                    [K"block"
                        body
                        # Advance iterator
                        [K"="(iterspec) next [K"call" "iterate"::K"top" collection state]]
                    ]
                    [K"call"(iterspec)
                        "not_int"::K"top"
                        [K"call" "==="::K"core" next (::K"nothing")]
                    ]
                ]
            ]
        ]
    end

    @ast ctx ex [K"symbolicblock" "loop-exit"::K"symboliclabel"
        loop
    ]
end

#-------------------------------------------------------------------------------
# Expand try/catch/finally

function match_try(ex)
    @jl_assert numchildren(ex) > 1 (ex, "Invalid `try` form")
    try_ = ex[1]
    catch_ = nothing
    finally_ = nothing
    else_ = nothing
    for e in ex[2:end]
        k = kind(e)
        if k == K"catch" && isnothing(catch_)
            @jl_assert numchildren(e) == 2 (e, "Invalid `catch` form")
            catch_ = e
        elseif k == K"else" && isnothing(else_)
            @jl_assert numchildren(e) == 1 e
            else_ = e[1]
        elseif k == K"finally" && isnothing(finally_)
            @jl_assert numchildren(e) == 1 e
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
        # TODO: Disallow @goto from try/catch/else blocks when there's a finally clause
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
function make_lhs_decls(ctx, stmts, declkind, declmeta, ex, type_decls=true)
    declname = @stm ex begin
        [K"Identifier"] -> ex
        # TODO: consider removing support for Expr(:global, GlobalRef(...)) and
        # other Exprs that cannot be produced by the parser (tested by
        # test/precompile.jl #50538).
        ([K"Value"], when=ex.value isa GlobalRef) -> ex
        [K"Placeholder"] -> nothing
        ([K"::" [K"Identifier"] t], when=type_decls) -> let x = ex[1]
            t2 = expand_forms_2(ctx, t)
            push!(stmts, newnode(ctx, ex, K"decl", tree_ids(x, t2)))
            make_lhs_decls(ctx, stmts, declkind, declmeta, x, type_decls)
        end
        ([K"::" [K"Placeholder"] t], when=type_decls) -> let
            # TODO: Currently, this ignores the LHS in `_::T = val`.
            # We should probably do one of the following:
            # - Throw a LoweringError if that's not too breaking
            # - `convert(T, rhs)::T` and discard the result which is what
            #   `x::T = rhs` would do if x is never used again.
        end
        ([K"::" x t], when=!type_decls) ->
            make_lhs_decls(ctx, stmts, declkind, declmeta, x, type_decls)
        (_, when=kind(ex) in KSet"call curly where") ->
            make_lhs_decls(ctx, stmts, declkind, declmeta, ex[1], type_decls)
        [K"tuple" xs...] -> for x in xs
            make_lhs_decls(ctx, stmts, declkind, declmeta, x, type_decls)
        end
        [K"parameters" xs...] -> for x in xs
            make_lhs_decls(ctx, stmts, declkind, declmeta, x, type_decls)
        end
        [K"..." x] -> nothing # from recursion above
    end

    if !isnothing(declname)
        stmt = @ast ctx ex [declkind declname]
        !isnothing(declmeta) && setattr!(stmt, :meta, declmeta)
        push!(stmts, stmt)
    end
    return nothing
end

# Separate decls and assignments (which require re-expansion)
# local x, (y=2), z ==> local x; local z; y = 2
function expand_decls(ctx, ex)
    declkind = kind(ex)
    @jl_assert declkind in KSet"local global" ex
    stmts = SyntaxList(ctx)
    for c in children(ex)
        simple = kind(c) in KSet"Identifier :: Value Placeholder"
        lhs = @stm c begin
            (_, when=simple) -> c
            [K"=" x _] -> x
            [K".=" x _] -> x
            [K"op=" x _ _] -> x
            [K".op=" x _ _] -> x
            [K"function" x _] -> x
        end
        # type decls are handled elsewhere unless simple
        make_lhs_decls(ctx, stmts, declkind, get(ex, :meta, nothing), lhs, simple)
        simple || push!(stmts, expand_forms_2(ctx, c))
    end
    newnode(ctx, ex, K"block", stmts)
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
    if numchildren(ex) == 2
        # pre-desugared const
        @ast ctx ex [K"constdecl" ex[1] ex[2]]
    elseif k == K"global"
        asgn = ex[1][1]
        @jl_assert (kind(asgn) == K"=" || kind(asgn) == K"function") (ex, "expected assignment after `const`")
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
    elseif k == K"Identifier" || k == K"Value"
        # Expr(:const, v) where v is a Symbol or a GlobalRef is an unfortunate
        # remnant from the days when const-ness was a flag that could be set on
        # any global.  It creates a binding with kind PARTITION_KIND_UNDEF_CONST.
        # TODO: deprecate and delete this "feature"
        @jl_assert numchildren(ex) == 1 ex
        @ast ctx ex [K"constdecl" ex[1]]
    else
        @jl_assert false ex
    end
end

#-------------------------------------------------------------------------------
# Expansion of function definitions

# Expand `where` clause(s) of a function into (typevar_names, typevar_stmts) where
# - `typevar_names` are the names of the type's type parameters
# - `typevar_stmts` are a list of statements to define a `TypeVar` for each parameter
#   name in `typevar_names`, with exactly one per `typevar_name`. Some of these
#   may already have been emitted.
# - `new_typevar_stmts` is the list of statements which needs to to be emitted
#   prior to uses of `typevar_names`.

# (where (where x a b) c d) -> (x, [c d a b])
function flatten_wheres(ex)
    tvs = SyntaxList(ex._graph)
    while kind(ex) === K"where"
        append!(tvs, ex[2:end])
        ex = ex[1]
    end
    return ex, tvs
end

# Select static parameters which are used in function arguments `arg_types`, or
# transitively used.
#
# The transitive usage check probably doesn't guarantee that the types are
# inferable during dispatch as they may only be part of the bounds of another
# type. Thus we might get false positives here but we shouldn't get false
# negatives.
function select_used_typevars(uses::SyntaxList, typevars::SyntaxList)
    used = BitVector(undef, length(typevars))
    for (i, tv) in enumerate(typevars)
        @jl_assert kind(tv) === K"_typevar" tv
        for u in uses
            contains_identifier(u, tv[1]) && (used[i] = true)
        end
    end
    # now find transitive uses
    todo = findall(used)
    while !isempty(todo)
        tv_i = pop!(todo)
        tv = typevars[tv_i]
        # for each typevar `prev` before tv, if our bounds reference `prev` (and
        # `prev` is not already used), add it to used and todo
        for prev_i in 1:tv_i-1
            used[prev_i] && continue
            prevname = typevars[prev_i][1]
            if contains_identifier(tv[2], prevname) ||
                    contains_identifier(tv[3], prevname)
                used[prev_i] = true
                push!(todo, prev_i)
            end
        end
    end
    return used
end

used_typevars(uses::SyntaxList, tvs::SyntaxList) =
    tvs[select_used_typevars(uses, tvs)]

unused_typevars(uses::SyntaxList, tvs::SyntaxList) =
    tvs[map(!, select_used_typevars(uses, tvs))]

function make_assigns(ctx, ls::SyntaxList, rs::SyntaxList)
    out = SyntaxList(ctx.graph)
    for (l, r) in zip(ls, rs)
        push!(out, @ast ctx r [K"=" l r])
    end
    out
end

function scope_nest(ctx, assigns, body)
    for a in Iterators.reverse(assigns)
        body = @ast ctx a [K"let" [K"block" a] body]
    end
    body
end

function pos_req_args(argl::SyntaxList)
    last = lastindex(argl)
    for i in eachindex(argl)
        if kind(argl[i]) in KSet"kw ... parameters"
            last = i-1
            break
        end
    end
    argl[1:last]
end

function pos_opt_args(argl::SyntaxList)
    opt_start = length(pos_req_args(argl))+1
    opt_end = -1
    for i in opt_start:lastindex(argl)
        if kind(argl[i]) === K"kw"
            opt_end = i
        end
    end
    @jl_assert let pos = kind(argl[end]) === K"parameters" ? argl[1:end-1] : argl
        # no optargs, or optargs until the end (maybe excluding vararg, kws)
        opt_end in (-1,lastindex(pos),lastindex(pos)-1)
    end pos[end]
    argl[opt_start:opt_end]
end

# (_typevar name lb ub) -> (local (= name (call core TypeVar...)))
function assign_sparams(ctx, tvs)
    out = SyntaxList(ctx.graph)
    for tv in tvs
        @jl_assert kind(tv) === K"_typevar" tv
        push!(out, @ast ctx tv [K"local" tv[1]])
        push!(out, @ast ctx tv [K"=" tv[1] bounds_to_typevar(ctx, tv)])
    end
    out
end

# Hack: Normally just (block ex body), but needs special handling due to
# pre-quoted parts of generated function body, where we need to prepend
# desugarable AST to macro AST.  Fortunately there are only two places (meta
# nkw, and destructuring arg assignments) we do this, so handle them manually.
function prepend_function_body(ctx, body, ex)
    @stm body begin
        [K"_generated_body" [K"quote" gen] nongen] -> begin
            ex_est = @stm ex begin
                [K"meta" [K"Symbol"] n] ->
                    @ast ctx ex [K"meta" "nkw"::K"Identifier" n]
                # TODO: need to handle destructuring arg assignments
                [K"block" _... [K"nothing"]] ->
                    newleaf(ctx, ex, K"Value", nothing)
                _ -> @jl_assert false (ex, "unexpected prepend_function_body")
            end
            @ast ctx body [K"_generated_body"
                [K"quote" [K"block" ex_est gen]] [K"block" ex nongen]]
        end
        _ -> @ast ctx body [K"block" ex body]
    end
end

# Produce all `method` exprs for the given `argl`
# - one wrapper per optional positional arg
# - one containing the body
# - possibly one generated method
function method_def_expr(ctx, src, mtable, sparams, argl, body,
                         rett=@ast(ctx, src, "Any"::K"core"))
    @jl_assert length(argl) > 0 src
    @jl_assert kind(argl[end]) !== K"parameters" src argl[end]
    if length(pos_opt_args(argl)) > 0
        return optional_positional_defs(
            ctx, src, mtable, sparams, argl, body, rett)
    elseif kind(body) === K"_generated_body"
        return generated_method_defs(
            ctx, src, mtable, sparams, argl, body, rett)
    end
    # Needs to be done per method, not per function (may create ssavalues)
    arg_types = mapsyntax(a->expand_forms_2(ctx, a[2]), argl)
    @ast ctx src [K"block"
        # possible TODO: flisp assigns typevars to ssavalues and manually
        # resolves them here instead of assigning to locals
        assign_sparams(ctx, sparams)...
        method_metadata := [K"call"(src) "svec"::K"core"
            [K"call" "svec"::K"core" arg_types...]
            [K"call" "svec"::K"core" mapindex(sparams, 1)...]
            ::K"SourceLocation"(src[1])]
        [K"method"
            mtable
            method_metadata
            [K"lambda"(body, is_toplevel_thunk=false, toplevel_pure=false)
                [K"block" mapindex(argl, 1)...]
                [K"block" mapindex(sparams, 1)...]
                expand_forms_2(ctx, body)
                is_core_Any(rett) ? nothing : expand_forms_2(ctx, rett)]]
        [K"removable" method_metadata]]
end

function _untyped_arg(a)
    @jl_assert kind(a) === K"::" || kind(a) === K"_typevar" a
    aname = setmeta(a[1], :nospecialize, true)
    @ast a._graph a [K"::" aname "Any"::K"core"]
end

function _expr_arg_sym(a)
    @jl_assert kind(a) === K"::" || kind(a) === K"_typevar" a
    sym = setattr(a[1], :kind, K"Symbol")
    if kind(a[1]) === K"Placeholder"
        setattr!(sym, :name_val, UNUSED)
    end
    sym
end

# The Julia runtime associates the code generator with the non-generated method
# by adding (meta generated ...) to the non-generated body
function generated_method_defs(ctx, src, mtable, sparams, argl, body, rett)
    @jl_assert kind(body) === K"_generated_body" && numchildren(body) == 2 body
    gen_name = let mangled = reserve_module_binding_i(
        ctx.mod, string("#", kind(mtable) === K"nothing" ? "_" : mtable, "@generator#"))
        new_global_binding(ctx, src, mangled, ctx.mod)
    end

    gen_mdef = let arg1_name = newsym(ctx, argl[1], "#self#"),
         gen_argl = SyntaxList(
            @ast(ctx, src, [K"::" arg1_name [K"function_type" gen_name]]),
            @ast(ctx, src, [K"::"
                # TODO: correct scope layer?
                "__context__"::K"Identifier"(scope_layer=get(mtable, :scope_layer, 1))
                MacroContext::K"Value"
            ]),
            mapsyntax(_untyped_arg, sparams)...,
             mapsyntax(_untyped_arg, argl)...)
        @jl_assert kind(body[1]) === K"quote" body
        gen_body = est_to_dst(expand_quote(ctx, body[1][1]))

        method_def_expr(ctx, src, gen_name, SyntaxList(ctx), gen_argl, gen_body,
                        @ast(ctx, src, "Any"::K"core"))
    end

    nongen_mdef = let
        nongen_body = @ast ctx body[2] [K"block" [K"meta" "generated"::K"Symbol"
            [K"new"
                GeneratedFunctionStub::K"Value" # Use stub type from JuliaLowering
                ctx.expr_compat_mode::K"Value"
                gen_name
                # Truncate provenance to just the source file range, as this
                # will live permanently in the IR and we probably don't want
                # the full provenance tree and intermediate expressions
                # (TODO: More truncation. We certainly don't want to store the
                #  source file either.)
                sourceref(src)::K"Value"
                [K"call" "svec"::K"core" mapsyntax(_expr_arg_sym, argl)...]
                [K"call" "svec"::K"core" mapsyntax(_expr_arg_sym, sparams)...]]]
            body[2]]
        method_def_expr(ctx, src, mtable, sparams, argl, nongen_body, rett)
    end

    @ast ctx src [K"block"
        [K"global" gen_name]
        [K"function_decl" gen_name]
        [K"method_defs" gen_name gen_mdef]
        nongen_mdef]
end

# Semantically, we want each wrapper method's body to call the method with one
# additional default (on top of its args, `passed`) until we reach the body
# method with all args filled.  As an optimization, a wrapper can fill all
# remaining default values as long as we can rule out any of the additional
# default values depending on the values of non-`passed` arguments.
#
# flisp checks dependencies by searching each additional default for every
# subexpression of `arg::type` for every non-`passed` `arg` before it.  We only
# check that static params in `::type` are not referenced in later defaults, and
# use `(= arg default)` to handle references to `arg`. (possible TODO: it's
# unclear why flisp doesn't do this; too many slots?)
function optional_positional_defs(ctx, src, mtable, sparams, argl, body, rett)
    opt = pos_opt_args(argl)
    opt_decls = mapindex(opt, 1)
    opt_names = mapindex(opt_decls, 1)
    opt_defaults = mapindex(opt, 2)

    # the final optarg (index into `opt`) that might reference `sp` in its type
    sp_known_by = zeros(Int, length(sparams))
    for sp_i in eachindex(sparams)
        for (i, arg) in Iterators.reverse(enumerate(opt_decls))
            if contains_identifier(arg[2], sparams[sp_i][1])
                sp_known_by[sp_i] = i
                break
            end
        end
    end
    # `deps[i] = j` is the largest `j<i` such that `opt_decls[j]` may affect the
    # value of `opt_defaults[i]`
    deps = zeros(Int, length(opt))
    for i in eachindex(opt)
        for sp_i in eachindex(sparams)
            if contains_identifier(opt_defaults[i], sparams[sp_i][1])
                deps[i] = max(deps[i], sp_known_by[sp_i])
            end
        end
    end
    req = pos_req_args(argl)
    passed = copy(req)
    methods = SyntaxList(ctx.graph)
    for i in eachindex(opt)
        @jl_assert i == length(passed)-length(req)+1 src
        wrapper_body = if all((<)(i), deps[i+1:end])
            # fill-all-defaults case.  note that the final default may be a
            # splat, and doesn't have further args referring to it by name, so
            # we put it directly in the call (see #50563 for some notes)
            @ast ctx src [K"block"
                make_assigns(ctx, opt_names[i:end-1], opt_defaults[i:end-1])...
                [K"call" mapindex(passed, 1)... opt_names[i:end-1]... opt_defaults[end]]]
        else
            @ast ctx src [K"block"
                [K"call" mapindex(passed, 1)... opt_defaults[i]]]
        end
        push!(methods, method_def_expr(
            ctx, src, mtable, used_typevars(passed, sparams),
            passed, wrapper_body))
        push!(passed, opt_decls[i])
    end
    if length(opt) + length(req) < length(argl)
        # positional vararg
        @jl_assert length(passed) == length(opt) + length(req) == length(argl) - 1 src
        push!(passed, argl[end])
    end
    push!(methods, method_def_expr(ctx, src, mtable, sparams, passed, body, rett))
    @ast ctx src [K"block" methods...]
end

function expand_kw_args(ctx, kws)
    kargl, restkw = @stm kws begin
        [K"parameters" xs... [K"..." va]] -> (xs, va)
        [K"parameters" xs...] -> (xs, nothing)
    end
    kw_decls = SyntaxList(ctx.graph)
    kw_syms = SyntaxList(ctx.graph)
    kw_defaults = SyntaxList(ctx.graph)
    for raw_a in kargl
        a = expand_function_arg(ctx, raw_a, false)
        @stm a begin
            [K"kw" [K"::" n t] v] -> begin
                push!(kw_decls, a[1])
                push!(kw_defaults, v)
            end
            [K"::" n t] -> begin
                push!(kw_decls, a)
                push!(kw_defaults, @ast ctx a [K"call" "throw"::K"core"
                    [K"call" "UndefKeywordError"::K"core" a[1]=>K"Symbol"]])
            end
        end
    end
    kw_names = mapindex(kw_decls, 1)
    kw_syms = mapsyntax(x->setattr(x, :kind, K"Symbol"), kw_names)
    restkw_list = isnothing(restkw) ? SyntaxList(ctx.graph) :
        SyntaxList(@ast ctx restkw [K"::"
            restkw [K"call" "pairs"::K"top" "NamedTuple"::K"core"]])

    return (kw_decls, kw_names, kw_syms, kw_defaults, restkw_list)
end

function keywords_method_def_expr(ctx, src, mtable, sparams, argl, body, rett, pos_va)
    kws = argl[end]
    pargl = argl[1:end-1]
    @jl_assert kind(kws) === K"parameters" src
    pos_decls = mapsyntax(a->kind(a)===K"kw" ? a[1] : a, pargl)
    # Mark the wrapper, not the body method, as the "self" arg to @__FUNCTION__.
    # TODO: We could probably unify this with is_kwcall_self with a generic
    # "closure not on first arg" flag if we're willing to pass the closure to
    # the body method through this arg instead of the first.
    pos_decls[1] = let p = pos_decls[1]
        @ast ctx p [K"::" setmeta(p[1], :thisfunction_original, true) p[2]]
    end

    # Positional names and splatted vararg so we can `(call f forward_pargl...)`
    forward_pargl = let l = mapindex(pos_decls, 1)
        pos_va && (l[end] = @ast ctx l[end] [K"..." l[end]])
        l
    end
    (kw_decls, kw_names, kw_syms, kw_defaults, restkw) = expand_kw_args(ctx, kws)
    ordered_defaults = any(val->contains_identifier(val, kw_names), kw_defaults)
    positional_sparams = used_typevars(pargl, sparams)

    m1_name = let n = kind(mtable) === K"nothing" ? "_" : mtable.name_val,
        mangled = string(startswith(n, '#') ? "" : "#kw_body#", n, "#")
        newsym(ctx, argl[1], reserve_module_binding_i(ctx.mod, mangled))
    end
    # (1) Body method.  This contains the actual function body, and requires
    # every possible default to be filled.  `rett` is only passed here since it
    # can reference any argument.
    mdefs1 = let arg1 = @ast ctx m1_name [K"::" m1_name [K"function_type" m1_name]]
        nkw = @ast ctx kws [K"meta" "nkw"::K"Symbol" numchildren(kws)::K"Value"]
        method_def_expr(
            ctx, src, m1_name, sparams,
            SyntaxList(arg1, kw_decls..., restkw..., pos_decls...),
            prepend_function_body(ctx, body, nkw), rett)
    end
    # (2) nokw methods (one per optarg).  Lowering wouldn't know to call
    # Core.kwcall given no kws in a call, so this method initializes kw defaults
    # and calls the body method.
    mdefs2 = let rkw = isempty(restkw) ? nothing :
            @ast ctx restkw[1] [K"call"
                "pairs"::K"top" [K"call" "NamedTuple"::K"core"]]
        body2 = if !ordered_defaults
            @ast ctx src [K"call" m1_name kw_defaults... rkw forward_pargl...]
        else
            scope_nest(ctx, make_assigns(ctx, kw_names, kw_defaults),
                @ast ctx src [K"call" m1_name kw_names... rkw forward_pargl...])
        end
        method_def_expr(
            ctx, src, mtable, positional_sparams, pargl,
            @ast(ctx, src, [K"block" [K"return" body2]]))
    end
    # (3) Core.kwcall(arg2::NamedTuple, pargl...) methods (one per optarg).
    # - for each kwarg:
    #   - kw_temp = if kwname in arg2, extract and typecheck it, else use default
    # - collect excess kws (caller-provided fields in arg2 minus `kw_names`)
    # - call body method using all kw_temps
    # sig: (kwcall_self::typeof(Core.kwcall) kw_namedtuple pargl...)
    mdefs3 = let
        arg2_name = newsym(ctx, kws, "kws")
            # If kwargs don't depend on each other, and their defaults don't contain
            # assignments, then we can use ssavalues instead of slots
            use_ssa_kw_temps = !ordered_defaults &&
                !any(val->contains_unquoted(e->kind(e) == K"=", val), kw_defaults)
        kw_temps = use_ssa_kw_temps ?
            mapsyntax(x->ssavar(ctx, x, x.name_val), kw_names) : kw_names
        tempslot = newsym(ctx, kws, "kwtmp")
        keyword_only_spnames = mapindex(unused_typevars(pargl, sparams), 1)

        kw_assigns = SyntaxList(ctx.graph)
        for (tmp, sym, decl, default) in zip(kw_temps, kw_syms, kw_decls, kw_defaults)
            get_kw = @ast ctx decl [K"call" "getfield"::K"core" arg2_name sym]
            if !is_core_Any(decl[2]) &&
                    !contains_identifier(decl[2], keyword_only_spnames)
                # static parameters don't have values yet, so don't assert the
                # declared kw type here if it contains any static params.  bad
                # types will trigger a MethodError when calling body instead.
                get_kw = @ast ctx decl [K"block"
                    getkw_tmp := get_kw
                    [K"if" [K"call" "isa"::K"core" getkw_tmp decl[2]]
                        (::K"nothing")
                        [K"call" "throw"::K"core"
                            [K"new" "TypeError"::K"core"
                                "keyword argument"::K"Symbol"
                                sym decl[2] getkw_tmp]]]
                    getkw_tmp]
            end
            push!(kw_assigns, @ast ctx decl [K"=" tmp [K"block"
                [K"if" [K"call" "isdefined"::K"core" arg2_name sym]
                    [K"=" tempslot get_kw]
                    [K"=" tempslot default]]
                tempslot]])
        end

        # bundle and forward excess if there's a restkw, else throw kwerr
        handle_excess = if !isempty(restkw)
            excess_kw = ssavar(ctx, arg2_name, "excess_kw")
            @ast ctx src [K"="
                excess_kw
                [K"call" "pairs"::K"top"
                   isempty(kw_names) ? arg2_name :
                   [K"call" "structdiff"::K"top" arg2_name
                       [K"curly" "NamedTuple"::K"core" [K"tuple" kw_syms...]]]]]
        else
            @ast ctx src [K"if"
                [K"call" "isempty"::K"top"
                    [K"call" "diff_names"::K"top"
                        [K"call" "keys"::K"top" arg2_name]
                        [K"tuple" kw_syms...]]]
                (::K"nothing")
                [K"call" "kwerr"::K"top" arg2_name forward_pargl...]]
        end
        final_call = @ast ctx kws [K"call"
            m1_name
            kw_temps...
            isempty(restkw) ? nothing : excess_kw
            forward_pargl...]
        kwcall_body = if use_ssa_kw_temps
            for n in kw_names
                # If not using slots for the keyword argument values, still
                # declare them for reflection purposes
                push!(kw_assigns, @ast ctx n [K"local" setmeta(n, :is_internal, true)])
            end
            @ast(ctx, src, [K"block" kw_assigns... handle_excess final_call])
        else
            scope_nest(ctx, kw_assigns,
                       @ast ctx src [K"block" handle_excess final_call])
        end
        # Core.kwcall method has its own first argument.  Ensure closure
        # conversion knows not to put the closure there.
        let arg1_name = setmeta!(
            newsym(ctx, kws, "#kwcall_self#"; unused=length(pos_opt_args(pargl)) == 0),
            :is_kwcall_self, true)
            arg1 = @ast ctx src [K"::" arg1_name
                [K"call" "typeof"::K"core" "kwcall"::K"core"]
            ]
            arg2 = @ast ctx arg2_name [K"::" arg2_name "NamedTuple"::K"core"]
            method_def_expr(
                ctx, src, mtable, positional_sparams,
                SyntaxList(arg1, arg2, pargl...), kwcall_body)
        end
    end
    @ast ctx src [K"block"
        [K"function_decl" m1_name]
        kind(mtable) === K"nothing" ? nothing : [K"function_decl" mtable]
        [K"method_defs" m1_name mdefs1]
        [K"method_defs" mtable mdefs2]
        [K"method_defs" mtable mdefs3]
        mtable
    ]
end

_lower_destructuring_arg(stmts, ctx, ex) = @stm ex begin
    [K"tuple" _...] -> let arg2 = newsym(ctx, ex, "destructured")
        push!(stmts, @ast(ctx, ex, [K"local"(meta=CompileHints(:is_destructured_arg, true))
            [K"=" ex arg2]]))
        arg2
    end
    [K"::" x t] -> @ast ctx ex [K"::" _lower_destructuring_arg(stmts, ctx, x) t]
    [K"kw" x t] -> @ast ctx ex [K"kw" _lower_destructuring_arg(stmts, ctx, x) t]
    [K"..." x]  -> @ast ctx ex [K"..." _lower_destructuring_arg(stmts, ctx, x)]
    _ -> ex
end

function lower_destructuring_args!(ctx, args)
    stmts = SyntaxList(ctx.graph)
    for (i, a) in enumerate(args)
        args[i] = _lower_destructuring_arg(stmts, ctx, a)
    end
    # return `nothing` from the assignments (issue #26518)
    !isempty(stmts) && push!(stmts, @ast ctx stmts[1] (::K"nothing"))
    return stmts
end

# `arg` is the first arg to a function's `call`.  return (1) whether this is an
# :overlay expression, (2) the method table expression, and (3) the typed arg
# expression `(:: #self# t)`
function expand_function_arg1(ctx, arg)
    if kind(arg) === K"overlay"
        _, _, x = expand_function_arg1(ctx, arg[2])
        return true, arg[1], x
    end
    aname = @stm arg begin
        [K"::" n t] -> n
        _ -> newsym(ctx, arg, "#self#")
    end
    atype = @stm arg begin
        [K"::" t] -> t
        [K"::" _ t] -> t
        _ -> @ast ctx arg [K"function_type" arg]
    end
    # first arg to Expr(:method)
    mt = @stm arg begin
        [K"Identifier"] -> arg
        [K"Value"] -> arg # TODO delete with globalref support
        [K"Placeholder"] -> arg
        _ -> @ast ctx arg (::K"nothing")
    end
    return false, mt, @ast ctx arg [K"::" aname atype]
end

fix_argname(ctx, arg, used) = @stm arg begin
    [K"Identifier"] -> arg
    # Lowering should be able to use placeholder args as rvalues internally,
    # e.g. for kw method dispatch.
    ([K"Placeholder"], when=used) -> newsym(ctx, arg, "#arg#")
    ([K"Placeholder"], when=!used) -> arg
end

# flisp: fill-missing-argname, llist-types, llist-vars, dots->vararg
#
# Make an arg into `(:: x t)` or `(kw (:: x t) default)`.  If `used`, the caller
# specifies that even placeholder/underscore arguments might be read from
# internally.  Desugar type, but desugar default values later, since
# `default...` is unfortunately allowed, so do that in body desugaring.
expand_function_arg(ctx, arg, used) = @stm arg begin
    [K"::" x t] ->
        @ast ctx arg [K"::" fix_argname(ctx, x, used) t]
    [K"::" t] -> let aname = newsym(ctx, arg, "#arg#"; unused=true)
        hasattr(arg, :meta) && setattr!(aname, :meta, arg.meta)
        @ast ctx arg [K"::" fix_argname(ctx, aname, used) t]
    end
    [K"kw" x v] ->
        @ast ctx arg [K"kw" expand_function_arg(ctx, x, used) v]
    # note: not correct for kwargs
    [K"..." x] -> let inner = expand_function_arg(ctx, x, used)
        @jl_assert kind(inner) === K"::" inner arg
        @ast ctx x [K"::" inner[1] [K"curly" "Vararg"::K"core" inner[2]]]
    end
    _ -> @ast ctx arg [K"::" fix_argname(ctx, arg, used) "Any"::K"core"]
end

# Normalize and expand all positional arguments to (:: identifier t), then call
# a helper to create the method(s).
function expand_function_def(ctx, src, raw_args, wheres, body, rett)
    @jl_assert length(raw_args) >= 1 (body, "expected a self arg")
    let arg_stmts = lower_destructuring_args!(ctx, raw_args)
        if !isempty(arg_stmts)
            blk = @ast ctx src [K"block" arg_stmts...]
            body = prepend_function_body(ctx, body, blk)
        end
    end
    (overlay, mtable, a1) = expand_function_arg1(ctx, raw_args[1])
    argl = SyntaxList(a1)
    has_kws = kind(raw_args[end]) === K"parameters" && numchildren(raw_args[end]) > 0
    let force_used = length(pos_opt_args(raw_args)) > 0 || has_kws
        for a in raw_args[2:end]
            if kind(a) === K"parameters"
                numchildren(a) >= 1 && push!(argl, a)
            else
                push!(argl, expand_function_arg(ctx, a, force_used))
            end
        end
    end
    sparams = mapsyntax(x->typevar_bounds(ctx, x), wheres)
    # Error if there are unused sparams.  possible TODO: this is currently only
    # a warning, so may need to be relaxed
    let unused = unused_typevars(argl, sparams)
        !isempty(unused) && throw(LoweringError(
            unused[1], string(
                "method definition declares type variable but ",
                "does not use it in the type of any function parameter")))
    end
    if has_kws
        pos_va = @stm raw_args[end-1] begin
            [K"kw" [K"..." _] _...] -> true
            [K"..." _] -> true
            _ -> false
        end
        keywords_method_def_expr(ctx, src, mtable, sparams, argl, body, rett, pos_va)
    else
        @ast ctx src [K"block"
            (overlay || kind(mtable) === K"nothing") ? nothing : [K"function_decl" mtable]
            [K"method_defs" mtable [K"block"
                method_def_expr(ctx, src, mtable, sparams, argl, body, rett)]]
                # TODO: overlay should return the method
                [K"removable" mtable]]
    end
end

expand_opaque_closure(ctx, ex) = @stm ex begin
    [K"opaque_closure" argt rt_lb rt_ub allow_partial lam] -> begin
        @jl_assert kind(lam[1]) === K"tuple" ex
        check_no_parameters(ex, lam[1])
        raw_args = SyntaxList(children(lam[1])...)
        arg_stmts = lower_destructuring_args!(ctx, raw_args)

        arg_names = SyntaxList(newsym(ctx, lam[1], "#self#"))
        inner_arg_types = SyntaxList(ctx.graph)
        for a in raw_args
            if kind(argt) !== K"nothing" && kind(a) === K"::"
                throw(LoweringError(a, "opaque closure argument type may not be specified both in the method signature and separately"))
            end
            a2 = expand_function_arg(ctx, a, false)
            if kind(a) === K"kw" || kind(a) === K"parameters"
                throw(LoweringError(
                    a, "opaque closure cannot have optional or keyword arguments"))
            end
            @jl_assert kind(a2) === K"::" a2
            push!(inner_arg_types, a2[2])
            push!(arg_names, a2[1])
        end

        out_argt = kind(argt) !== K"nothing" ? argt :
            @ast ctx lam[1] [K"curly" "Tuple"::K"core" inner_arg_types...]
        out_rt_lb = kind(rt_lb) !== K"nothing" ? rt_lb :
            @ast ctx lam[1] [K"curly" "Union"::K"core"]
        out_rt_ub = kind(rt_ub) !== K"nothing" ? rt_ub :
            @ast ctx lam[1] "Any"::K"core"
        nargs = (length(arg_names)-1) # ignoring #self#
        is_va = kind(raw_args[end]) === K"..."
        body = @ast ctx lam[2] [K"block" arg_stmts... lam[2]]

    @ast ctx ex [K"_opaque_closure"
        ssavar(ctx, ex, "opaque_closure_id") # only a placeholder. Must be :local
        expand_forms_2(ctx, out_argt)
        expand_forms_2(ctx, out_rt_lb)
        expand_forms_2(ctx, out_rt_ub)
        allow_partial
        nargs::K"Integer"
        is_va::K"Bool"
        ::K"SourceLocation"(lam)
        [K"lambda"(lam, is_toplevel_thunk=false, toplevel_pure=false)
            [K"block" arg_names...]
            [K"block"]
            expand_forms_2(ctx, body)]]
    end
end

#-------------------------------------------------------------------------------
# Expand macro definitions

function _make_macro_name(ctx, ex)
    k = kind(ex)
    if k == K"Identifier" || k == K"Symbol"
        name = setattr!(mkleaf(ex), :kind, k)
        setattr!(name, :name_val, "@$(ex.name_val)")
    elseif k == K"Placeholder"
        name = setattr!(mkleaf(ex), :kind, K"Identifier")
        setattr!(name, :name_val, "@$(ex.name_val)")
    elseif is_valid_modref(ex)
        @jl_assert numchildren(ex) == 2 ex
        @ast ctx ex [K"." ex[1] _make_macro_name(ctx, ex[2])]
    else
        @jl_assert false ex
    end
end

# flisp: expand-macro-def
function expand_macro_def(ctx, ex)
    if numchildren(ex) == 1
        # macro with zero methods
        # `macro m end`
        return @ast ctx ex [K"function" _make_macro_name(ctx, ex[1])]
    end
    (sig, name, args) = @stm ex begin
        [K"macro" [K"call" n a...] _] -> (ex[1], n, remove_empty_parameters(a))
        _ -> @jl_assert false ex
    end

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
                mapsyntax(e->apply_arg_meta(e, :nospecialize), args)...
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
                args...
            ]
            ex[2]
        ]
    end
end

#-------------------------------------------------------------------------------
# Expand type definitions

# argument to where expression -> (_typevar name expanded_lb expanded_ub)
# used, e.g. in all `sparams`, where flisp generally uses a list (name, lb, ub)
function typevar_bounds(ctx, ex)
    any = @ast ctx ex "Any"::K"core"
    (name, lb, ub) = bounds = @stm ex begin
        [K"Identifier"] -> (ex, any, any)
        [K"Placeholder"] -> (ex, any, any)
        ([K"comparison" lb op x _ ub], when=op.name_val==="<:") -> (x, lb, ub)
        ([K"comparison" ub op x _ lb], when=op.name_val===">:") -> (x, lb, ub)
        [K"<:" x ub] -> (x, any, ub)
        [K">:" x lb] -> (x, lb, any)
    end
    @ast ctx ex [K"_typevar" name expand_forms_2(ctx, lb) expand_forms_2(ctx, ub)]
end

function bounds_to_typevar(ctx, ex)
    @jl_assert kind(ex) === K"_typevar" ex
    _bounds_to_typevar(ctx, ex, ex[1], ex[2], ex[3])
end

function _bounds_to_typevar(ctx, srcref, name, lb, ub)
    # Generate call to one of
    # TypeVar(name)
    # TypeVar(name, ub)
    # TypeVar(name, lb, ub)
    @ast ctx srcref [K"call"
        "TypeVar"::K"core"
        name=>K"Symbol"
        if !is_core_Any(lb)
            lb
        end
        if !is_core_Any(lb) || !is_core_Any(ub)
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
    @isdefined(type_params) || throw(LoweringError(ex, "invalid type signature"))
    @isdefined(supertype) || throw(LoweringError(ex, "invalid type signature"))

    return (name, type_params, supertype)
end

# Expand type_params into (typevar_names, typevar_stmts) where
# - `typevar_names` are the names of the type's type parameters
# - `typevar_stmts` are a list of statements to define a `TypeVar` for each parameter
#   name in `typevar_names`, to be emitted prior to uses of `typevar_names`.
#   There is exactly one statement from each typevar.
function expand_typevars!(ctx, typevar_names, typevar_stmts, type_params)
    for param in type_params
        bounds = typevar_bounds(ctx, param)
        n = bounds[1]
        push!(typevar_names, n)
        push!(typevar_stmts, @ast ctx param [K"block"
            [K"local" n]
            [K"=" n bounds_to_typevar(ctx, bounds)]
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
        @jl_assert numchildren(ex) == 1 ex
    else
        @jl_assert kind(ex) == K"primitive" ex
        @jl_assert numchildren(ex) == 2 ex
    end
    nbits = is_abstract ? nothing : ex[2]
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
        [K"assert" "toplevel_only"::K"Symbol" [K"inert_syntaxtree" ex] ]
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
                for prev in field_names
                    if prev.name_val == m.name.name_val
                        throw(LoweringError(m.name, "duplicate field name"))
                    end
                end
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
            elseif kind(e) == K"string" || is_effect_free(e)
                # effect-free code and docstrings should not add to `defs`, since
                # that would prevent inner ctors from being generated
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

function _is_new_call(ex)
    kind(ex) == K"call" &&
        ((kind(ex[1]) == K"Identifier" && ex[1].name_val == "new") ||
         (kind(ex[1]) == K"curly" && kind(ex[1][1]) == K"Identifier" && ex[1][1].name_val == "new"))
end

# Rewrite constructor signature, returning extra information needed for
# rewriting `new` calls in the body.  Returns `(sig2, ctor_self)`, where:
#
# If `sig` is a constructor of `tname` like `tname{X,Y}(...)`,
#   - sig2 is :((var"#ctor-self#"::Type{tname{X,Y}})(...))
#   - ctor_self is the symbol we generated above
#
# Otherwise, sig2 is sig, and ctor_self is nothing.
function rewrite_ctor_sig(ctx, sig, tname, global_tname, struct_typevars, wheres)
    sig2 = sig
    ctor_self = nothing
    @stm sig begin
        [K"::" x rett] -> let
            call2, ctor_self = rewrite_ctor_sig(
                ctx, x, tname, global_tname, struct_typevars, wheres)
            sig2 = @ast(ctx, sig, [K"::" call2 rett])
        end
        # recognize `(_::(Type{X{T}} where T))(...)` as an inner-style
        # constructor for X (rewrite it to `X{T}(...) where T`)
        ([K"call" [K"::" _ [K"where" _...]] args...], when=begin
             t, inner_wheres = flatten_wheres(ex[1][2])
             isempty(wheres) && kind(t) === K"curly" && get(t[1], :name_val, "") == "Type"
         end) -> let
             append!(wheres, inner_wheres)
             ex2 = @ast ctx ex [K"call" t[2] args...]
             return rewrite_ctor_sig(
                 ctx, ex2, tname, global_tname, struct_typevars, wheres)
        end
        [K"call" [K"curly" name curlyargs...] args...] -> let
            # if curlyargs is the wrong length, fall back to the ones in `new`
            # TODO: this isn't quite the same as flisp, which passes curlyargs
            # to new-call and checks there.  We print the wrong message with
            # `struct X{T}; X{T,U}() = new(); end`.
            if (kind(name) !== K"::" && is_same_identifier_like(name, tname) &&
                length(curlyargs) == length(struct_typevars))
                @jl_assert is_leaf(name) (sig, "didn't find ctor name in sig")
                ctor_self = newsym(ctx, sig, "#ctor-self#")
                sig2 = @ast ctx sig [K"call"
                    [K"::" ctor_self
                        [K"curly" "Type"::K"core"
                         [K"curly" global_tname curlyargs...]]]
                    args...]
            end
        end
        [K"call" name args...] -> let
            if kind(name) !== K"::" && is_same_identifier_like(name, tname)
                @jl_assert is_leaf(name) (sig, "didn't find ctor name in sig")
                ctor_self = newsym(ctx, sig, "#ctor-self#")
                sig2 = @ast ctx sig [K"call"
                    [K"::" ctor_self [K"curly" "Type"::K"core" global_tname]]
                    args...]
            end
        end
        # anonymous function
        [K"tuple" _...] -> (sig, nothing)
    end
    sig_out = isempty(wheres) ? sig2 : @ast ctx sig [K"where" sig2 wheres...]
    return sig_out, ctor_self
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
function rewrite_ctor(ctx, ex, tname, global_tname, struct_typevars, field_types)
    @stm ex begin
        [K"function" call body] -> let (sig, wheres) = flatten_wheres(call)
            call2, ctor_self =
                rewrite_ctor_sig(ctx, sig, tname, global_tname, struct_typevars, wheres)
            body2 = _rewrite_ctor_new_calls(
                ctx, body, global_tname,
                mapsyntax(x->typevar_bounds(ctx, x), wheres),
                struct_typevars, ctor_self, field_types)
            @ast ctx ex [K"function" call2 body2]
        end
        x -> mapchildren(e->rewrite_ctor(
            ctx, e, tname, global_tname, struct_typevars, field_types), ctx, ex)
    end
end

# possible TODO: flisp does rewrites
# new(args...) => new_call(
#     global_tname,     (), ctor_sparams, struct_typevars, map(rewrite, args), field_types, ctor_self)
# new{new_curlyargs...}(args...) => new_call(
#     global_tname, new_curlyargs, ctor_sparams, struct_typevars, map(rewrite, args), field_types, ctor_self)
#
# This function should do as much as `new-call`, but does not use curlyargs
# or ctor_sparams, so may be missing something.
function _rewrite_ctor_new_calls(ctx, ex, global_struct_name, ctor_sparams,
                                       struct_typevars, ctor_self, field_types)
    if is_leaf(ex)
        return ex
    elseif !_is_new_call(ex)
        return mapchildren(
            e->_rewrite_ctor_new_calls(ctx, e, global_struct_name, ctor_sparams,
                                       struct_typevars, ctor_self, field_types),
            ctx, ex
        )
    end
    # Rewrite a call to new()
    kw_arg_i = findfirst(e->(k = kind(e); k == K"kw" || k == K"parameters"), children(ex))
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
function _insert_fieldtype_struct_shim(ctx, name, ex)
    if kind(ex) == K"." &&
        numchildren(ex) == 2 &&
        kind(ex[2]) == K"Symbol" &&
        ex[2].name_val == name.name_val
        @ast ctx ex [K"call" "struct_name_shim"::K"core" ex[1] ex[2] ctx.mod::K"Value" name]
    elseif numchildren(ex) > 0
        mapchildren(e->_insert_fieldtype_struct_shim(ctx, name, e), ctx, ex)
    else
        ex
    end
end

function insert_struct_shim(ctx, fieldtypes, name)
    map(ex->_insert_fieldtype_struct_shim(ctx, name, ex), fieldtypes)
end

# Replace all (call core.apply_type ...) with (call core.apply_type_or_typeapp ...)
# in an expression tree. Used for typegroup to handle TypeVar/TypeApp references
# during type resolution before real DataTypes exist.
function _replace_type_constructors(ctx, ex)
    if is_leaf(ex)
        return ex
    end
    k = kind(ex)
    if k == K"call" && numchildren(ex) >= 1 && kind(ex[1]) == K"core" && ex[1].name_val == "apply_type"
        new_head = @ast ctx ex[1] "apply_type_or_typeapp"::K"core"
        new_children = SyntaxList(ctx)
        push!(new_children, new_head)
        for i in 2:numchildren(ex)
            push!(new_children, _replace_type_constructors(ctx, ex[i]))
        end
        return @ast ctx ex [K"call" new_children...]
    else
        return mapchildren(e->_replace_type_constructors(ctx, e), ctx, ex)
    end
end

struct TypeGroupEntry
    sdef            # struct definition syntax node
    docs            # nothing or K"doc" node
    typevar_names   # typevar names for this struct
    typevar_stmts   # typevar creation statements
    field_names     # field name syntax nodes
    field_types     # field type expressions
    field_attrs     # field attribute expressions
    supertype       # supertype expression
    is_mutable::Bool
    min_initialized::Int
    inner_defs      # inner constructor definitions
    field_docs      # field documentation
end

function expand_typegroup_def(ctx, ex)
    @jl_assert numchildren(ex) == 1 ex
    body = ex[1]
    if kind(body) != K"block"
        throw(LoweringError(body, "expected block for `typegroup` body"))
    end

    # Collect and analyze struct definitions from block children.
    # A child can be a bare K"struct" or a K"doc" wrapping a K"struct".
    entries = TypeGroupEntry[]
    struct_names = SyntaxList(ctx)   # local name bindings (splatted into AST)
    global_names = SyntaxList(ctx)   # global name bindings (splatted into AST)
    info_vars = SyntaxList(ctx)      # SSA vars for struct info svecs (splatted into AST)

    for child in children(body)
        if kind(child) == K"struct"
            sdef = child
            docs = nothing
        elseif kind(child) == K"doc"
            @jl_assert numchildren(child) == 2 child
            sdef = child[2]
            if kind(sdef) != K"struct"
                throw(LoweringError(sdef, "`typegroup` only supports `struct` definitions"))
            end
            docs = child
        else
            throw(LoweringError(child, "`typegroup` only supports `struct` definitions"))
        end

        @jl_assert numchildren(sdef) == 2 sdef
        type_sig = sdef[1]
        type_body = sdef[2]
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

        is_mutable = has_flags(sdef, JuliaSyntax.MUTABLE_FLAG)
        min_initialized = minimum((_constructor_min_initialized(e) for e in inner_defs),
                                  init=length(field_names))

        push!(entries, TypeGroupEntry(sdef, docs, typevar_names, typevar_stmts,
                                      field_names, field_types, field_attrs,
                                      supertype, is_mutable, min_initialized,
                                      inner_defs, field_docs))
        push!(struct_names, struct_name)
        layer = new_scope_layer(ctx, struct_name)
        push!(global_names, adopt_scope(struct_name, layer))
        push!(info_vars, ssavar(ctx, sdef, "struct_info"))
    end
    n = length(entries)
    if n == 0
        return nothing_(ctx, ex)
    end

    # Build the lowered code
    #
    # Structure:
    # 1. Assert toplevel-only
    # 2. scope_block(hard) {
    #   a. Declare all names as locals
    #   b. Create TypeVar placeholders for each name
    #   c. For each struct: create TypeVar params, collect info into svec
    #   d. Call resolve_typegroup
    #   e. Bind to global constants
    #   f. latestworld
    #   g. Constructor definitions
    # }

    stmts = SyntaxList(ctx)

    # 2a. Declare all names as locals
    for name in struct_names
        push!(stmts, @ast ctx name [K"local" name])
    end

    # 2b. Create TypeVar placeholders for each name
    for name in struct_names
        push!(stmts, @ast ctx name [K"=" name [K"call" "TypeVar"::K"core" name=>K"Symbol"]])
    end

    # 2c. For each struct: create scope_block with TypeVar params and collect info into svec
    for i in 1:n
        e = entries[i]
        typevar_names = e.typevar_names
        typevar_stmts = e.typevar_stmts
        info_var = info_vars[i]

        inner_stmts = SyntaxList(ctx)
        for tv_name in typevar_names
            push!(inner_stmts, @ast ctx e.sdef [K"local" tv_name])
        end
        append!(inner_stmts, typevar_stmts)
        push!(inner_stmts, @ast ctx e.sdef [K"assert" "toplevel_only"::K"Symbol" [K"inert_syntaxtree" e.sdef]])
        push!(inner_stmts, @ast ctx e.sdef [K"="
            info_var
            [K"call" "svec"::K"core"
                [K"call" "svec"::K"core" typevar_names...]
                [K"call" "svec"::K"core" [fname=>K"Symbol" for fname in e.field_names]...]
                [K"call" "svec"::K"core" e.field_attrs...]
                e.is_mutable::K"Bool"
                e.min_initialized::K"Integer"
                e.supertype
                [K"call" "svec"::K"core" e.field_types...]
            ]
        ])

        push!(stmts, @ast ctx e.sdef [K"scope_block"(scope_type=:hard)
            [K"block" inner_stmts...]
        ])
    end

    # 2d. Call resolve_typegroup
    push!(stmts, @ast ctx ex [K"="
        [K"tuple" struct_names...]
        [K"call" "resolve_typegroup"::K"core"
            ctx.mod::K"Value"
            [K"call" "svec"::K"core" struct_names...]
            [K"call" "svec"::K"core" info_vars...]
        ]
    ])

    # 2e. Bind to global constants
    for i in 1:n
        push!(stmts, @ast ctx entries[i].sdef [K"constdecl" global_names[i] struct_names[i]])
    end

    # 2f. latestworld
    push!(stmts, @ast ctx ex (::K"latestworld"))
    push!(stmts, nothing_(ctx, ex))

    # 2g. Constructor definitions — placed outside the scope_block so that
    # type names in constructor bodies resolve to globals, not captured locals.
    fdef_stmts = SyntaxList(ctx)
    for i in 1:n
        e = entries[i]
        if isempty(e.inner_defs)
            push!(fdef_stmts, @ast ctx e.sdef [K"call"
                "_defaultctors"::K"top"
                global_names[i]
                ::K"SourceLocation"(e.sdef)
            ])
        else
            inner_defs = e.inner_defs
            for (def_i, def) in enumerate(inner_defs)
                inner_defs[def_i] =
                    rewrite_ctor(ctx, def, struct_names[i], global_names[i],
                             e.typevar_names, e.field_types)
            end
            push!(fdef_stmts, @ast ctx e.sdef [K"scope_block"(scope_type=:hard)
                [K"block" inner_defs...]
            ])
        end
    end

    push!(fdef_stmts, @ast ctx ex (::K"latestworld"))

    # 2h. Documentation — after constructors and latestworld so types are fully defined
    for i in 1:n
        e = entries[i]
        if !isnothing(e.docs) || !isempty(e.field_docs)
            push!(fdef_stmts, @ast ctx e.sdef [K"call"(isnothing(e.docs) ? e.sdef : e.docs)
                bind_docs!::K"Value"
                struct_names[i]
                isnothing(e.docs) ? nothing_(ctx, e.sdef) : e.docs[1]
                ::K"SourceLocation"(e.sdef)
                [K"kw"
                    "field_docs"::K"Identifier"
                    [K"call" "svec"::K"core" e.field_docs...]
                ]
            ])
        end
    end

    push!(fdef_stmts, nothing_(ctx, ex))

    # Build the toplevel assertion + scope block, then do the expand and replace
    scope_block_stmts = SyntaxList(ctx)
    for name in global_names
        push!(scope_block_stmts, @ast ctx ex [K"global" name])
    end
    push!(scope_block_stmts, @ast ctx ex [K"block" stmts...])

    result = @ast ctx ex [K"block"
        [K"assert" "toplevel_only"::K"Symbol" [K"inert_syntaxtree" ex]]
        [K"scope_block"(scope_type=:hard)
            scope_block_stmts...
        ]
        fdef_stmts...
    ]

    # Expand, then replace apply_type with apply_type_or_typeapp
    expanded = expand_forms_2(ctx, result)
    return _replace_type_constructors(ctx, expanded)
end

function expand_struct_def(ctx, ex, docs)
    @jl_assert numchildren(ex) == 2 ex
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
        for i in 1:length(typevar_names)
            typevar_name = typevar_names[i]
            typevar_in_fields = any(contains_identifier(ft, typevar_name) for ft in field_types)
            if !typevar_in_fields
                typevar_in_bounds = any(type_params[i+1:end]) do param
                    # Check the bounds of subsequent type params
                    (lb,ub) = let bounds = typevar_bounds(ctx, param)
                        bounds[2], bounds[3]
                    end
                    # todo: flisp lowering tests `lb` here so we also do. But
                    # in practice this doesn't seem to constrain `typevar_name`
                    # and the generated constructor doesn't work?
                    (!isnothing(ub) && contains_identifier(ub, typevar_name)) ||
                    (!isnothing(lb) && contains_identifier(lb, typevar_name))
                end
                if !typevar_in_bounds
                    break
                end
            end
        end
    end

    # For all functions within `struct`, rewrite `new` calls and
    # constructor-like signatures
    for (def_i, def) in enumerate(inner_defs)
        inner_defs[def_i] =
            rewrite_ctor(ctx, def, struct_name, global_struct_name,
                         typevar_names, field_types)
    end

    # The following lowering covers several subtle issues in the ordering of
    # typevars when "redefining" structs.
    # See https://github.com/JuliaLang/julia/pull/36121
    @ast ctx ex [K"block"
        [K"assert" "toplevel_only"::K"Symbol" [K"inert_syntaxtree" ex] ]
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
            ]
        ]

        if isempty(inner_defs)
            # Default constructors are generated at runtime by Base._defaultctors.
            [K"block"
                [K"call"
                    "_defaultctors"::K"top"
                    global_struct_name
                    ::K"SourceLocation"(ex)
                ]
                (::K"latestworld")
            ]
        else
            # User-defined inner constructors are placed in a separate scope_block
            # so that helper functions defined in the struct body don't leak to
            # the module's global scope.
            [K"scope_block"(scope_type=:hard)
                [K"block" inner_defs...]
            ]
        end

        # Documentation
        if !isnothing(docs) || !isempty(field_docs)
            [K"call"(isnothing(docs) ? ex : docs)
                bind_docs!::K"Value"
                struct_name
                isnothing(docs) ? nothing_(ctx, ex) : docs[1]
                ::K"SourceLocation"(ex)
                [K"kw"
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
    bounds = typevar_bounds(ctx, rhs)
    v = bounds[1]
    @ast ctx srcref [K"let"
        [K"block" [K"=" v bounds_to_typevar(ctx, bounds)]]
        [K"call" "UnionAll"::K"core" v lhs]
    ]
end

function expand_wheres(ctx, ex)
    body = ex[1]
    @stm ex begin
        [K"where" _ [K"_typevars" [K"block" names...] [K"block" stmts...]]] ->
            for n in Iterators.reverse(names)
                body = @ast ctx ex [K"call" "UnionAll"::K"core" n body]
            end
        [K"where" _ tvs...] ->
            for v in Iterators.reverse(tvs)
                body = expand_where(ctx, ex, body, v)
            end
    end
    body
end

# Match implicit where parameters for `Foo{<:Bar}` ==> `Foo{T} where T<:Bar`
function expand_curly(ctx, ex)
    @jl_assert kind(ex) == K"curly" ex
    check_no_parameters(ex, "unexpected semicolon in type parameter list")
    check_no_assignment(children(ex), "misplaced assignment in type parameter list")

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
            any = @ast ctx ex "Any"::K"core"
            typevar = k == K"<:" ?
                _bounds_to_typevar(ctx, e, name, any, e[1]) :
                _bounds_to_typevar(ctx, e, name, e[1], any)
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
    @jl_assert kind(path) == K"importpath" path
    path_spec = Expr(:.)
    prev_was_dot = true
    for component in children(path)
        k = kind(component)
        if k == K"quote"
            # Permit quoted path components as in
            # import A.(:b).:c
            component = component[1]
        end
        @jl_assert kind(component) in (K"Identifier", K".") component
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
        @jl_assert numchildren(ex[1]) >= 2 ex
        from = ex[1][1]
        from_path = @ast ctx from QuoteNode(expand_importpath(from))::K"Value"
        paths = ex[1][2:end]
    else
        # import A.B
        # (using (importpath A B))
        # (call eval_import true nothing (call core.svec 1 "w"))
        @jl_assert numchildren(ex) >= 1 ex
        from_path = nothing
        paths = children(ex)
    end
    # Here we represent the paths as quoted `Expr` data structures
    path_specs = SyntaxList(ctx)
    for spec in paths
        if kind(spec) == K"as"
            @jl_assert numchildren(spec) == 2 spec
            @jl_assert kind(spec[2]) == K"Identifier" spec
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
                        (::K"nothing")
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
        [K"assert" "toplevel_only"::K"Symbol" [K"inert_syntaxtree" ex]]
        stmts...
        [K"removable" (::K"nothing")]
    ]
end

# Expand `public` or `export`
function expand_public(ctx, ex)
    identifiers = String[]
    for e in children(ex)
        @jl_assert kind(e) == K"Identifier" (ex, "Expected identifier")
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

function isquotedmacrocall(ex)
    kind(ex) == K"call" || return false
    numchildren(ex) == 3 || return false
    let (f, ex) = (ex[1], ex[3])
        kind(f) == K"Value" || return false
        kind(ex) == K"inert" || return false
        f.value === interpolate_ast || return false
        kind(ex[1]) == K"macrocall" || return false
        return true
    end
end

function expand_doc(ctx, ex, docex)
    if kind(ex) in (K"Identifier", K".")
        expand_forms_2(ctx, @ast ctx docex [K"call"
            bind_static_docs!::K"Value"
            (kind(ex) === K"." ? ex[1] : ctx.mod::K"Value")
            (kind(ex) === K"." ? ex[2] : ex).name_val::K"Symbol"
            docex[1]
            ::K"SourceLocation"(ex)
            Union{}::K"Value"
        ])
    elseif isquotedmacrocall(ex)
        # TODO: implement proper `doc!` support here
        expand_forms_2(ctx, ex, docex)
    elseif is_eventually_call(ex)
        TODO("docsystem rewrite")
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
        @jl_assert numchildren(ex) == 3 ex
        expand_forms_2(ctx, @ast ctx ex [K"if" children(ex)...])
    elseif k == K"&&" || k == K"||"
        @jl_assert numchildren(ex) > 1 ex
        cs = expand_cond_children(ctx, ex)
        # Attributing correct provenance for `cs[1:end-1]` is tricky in cases
        # like `a && (b && c)` because the expression constructed here arises
        # from the source fragment `a && (b` which doesn't follow the tree
        # structure. For now we attribute to the parent node.
        cond = length(cs) == 2 ?
            cs[1] :
            newnode(ctx, ex, k, cs[1:end-1])
        # This transformation assumes the type assertion `cond::Bool` will be
        # added by a later compiler pass (currently done in codegen)
        if k == K"&&"
            @ast ctx ex [K"if" cond cs[end] false::K"Bool"]
        else
            @ast ctx ex [K"if" cond true::K"Bool" cs[end]]
        end
    elseif k == K"::"
        @jl_assert numchildren(ex) == 2 (ex, "`::` must be written `value::type` outside function argument lists")
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
        @stm ex begin
            [K"break"] ->
                @ast ctx ex [K"break" "loop-exit"::K"symboliclabel"]
            [K"break" [K"Placeholder"]] ->
                @ast ctx ex [K"break" "loop-exit"::K"symboliclabel"]
            [K"break" [K"Identifier"]] -> begin
                @ast ctx ex [K"break" ex[1]=>K"symboliclabel"]
            end
            [K"break" [K"Placeholder"] val] ->
                @ast ctx ex [K"break" "loop-exit"::K"symboliclabel"
                             expand_forms_2(ctx, val)]
            [K"break" [K"Identifier"] val] -> begin
                @ast ctx ex [K"break" ex[1]=>K"symboliclabel"
                             expand_forms_2(ctx, val)]
            end
        end
    elseif k == K"continue"
        @stm ex begin
            [K"continue"] ->
                @ast ctx ex [K"break" "loop-cont"::K"symboliclabel"]
            [K"continue" [K"Placeholder"]] ->
                @ast ctx ex [K"break" "loop-cont"::K"symboliclabel"]
            [K"continue" [K"Identifier"]] ->
                @ast ctx ex [K"break" string(ex[1].name_val, "#cont")::K"symboliclabel"]
        end
    elseif k == K"comparison"
        expand_forms_2(ctx, expand_compare_chain(ctx, ex))
    elseif k == K"doc"
        @jl_assert numchildren(ex) == 2 ex
        expand_doc(ctx, ex[2], ex)
    elseif k == K"for"
        expand_forms_2(ctx, expand_for(ctx, ex))
    elseif k == K"comprehension"
        @jl_assert numchildren(ex) == 1 ex
        @jl_assert kind(ex[1]) == K"generator" ex
        @ast ctx ex [K"call"
            "collect"::K"top"
            expand_forms_2(ctx, ex[1])
        ]
    elseif k == K"typed_comprehension"
        @jl_assert numchildren(ex) == 2 ex
        @jl_assert kind(ex[2]) == K"generator" ex
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
    elseif k == K"function"
        if numchildren(ex) == 1
            return @ast ctx ex [K"block" [K"function_decl" ex[1]] ex[1]]
        end
        sig, wheres = flatten_wheres(ex[1])
        name, args, rett = @stm sig begin
            [K"::" [K"call" f as...] t] -> (f, as, t)
            [K"call" f as...] -> (f, as, @ast(ctx, sig, "Any"::K"core"))
            [K"tuple" as...] -> (nothing, as, @ast(ctx, sig, "Any"::K"core"))
        end
        if isnothing(name)
            name = newsym(ctx, sig, "#anon#")
            @ast ctx ex [K"block" [K"local" name] expand_function_def(
                ctx, ex, SyntaxList(name, args...), wheres, ex[2], rett)]
        else
            expand_function_def(
                ctx, ex, SyntaxList(name, args...), wheres, ex[2], rett)
        end
    elseif k == K"->"
        sig, wheres = flatten_wheres(ex[1])
        @jl_assert kind(sig) === K"tuple" ex
        name = newsym(ctx, sig, "#->#")
        rett = @ast(ctx, sig, "Any"::K"core")
        @ast ctx ex [K"block" [K"local" name] expand_function_def(
            ctx, ex, SyntaxList(name, children(sig)...), wheres, ex[2], rett)]
    elseif k == K"macro"
        @ast ctx ex [K"block"
            [K"assert"
                "global_toplevel_only"::K"Symbol"
                [K"inert_syntaxtree" ex]
            ]
            expand_forms_2(ctx, expand_macro_def(ctx, ex))
        ]
    elseif k == K"if" || k == K"elseif"
        @jl_assert numchildren(ex) >= 2 ex
        @ast ctx ex [k
            expand_condition(ctx, ex[1])
            expand_forms_2(ctx, ex[2:end])...
        ]
    elseif k == K"let"
        expand_forms_2(ctx, expand_let(ctx, ex))
    elseif k == K"const"
        expand_const_decl(ctx, ex)
    elseif k == K"local" || k == K"global"
        expand_decls(ctx, ex)
    elseif k == K"where"
        expand_forms_2(ctx, expand_wheres(ctx, ex))
    elseif k == K"string"
        if numchildren(ex) == 1 && kind(ex[1]) == K"String"
            ex[1]
        else
            expand_forms_2(ctx, @ast ctx ex [K"call"
                "string"::K"top"
                children(ex)...
            ])
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
    elseif k == K"typegroup"
        expand_typegroup_def(ctx, ex)
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
            [K"assert" "toplevel_only"::K"Symbol" [K"inert_syntaxtree" ex]]
            [K"call"
                eval                  ::K"Value"
                ctx.mod               ::K"Value"
                [K"inert_syntaxtree" ex]
                [K"parameters"
                    [K"kw"
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
        expand_opaque_closure(ctx, ex)
    elseif k == K"vcat" || k == K"typed_vcat"
        expand_forms_2(ctx, expand_vcat(ctx, ex))
    elseif k == K"ncat" || k == K"typed_ncat"
        expand_forms_2(ctx, expand_ncat(ctx, ex))
    elseif k == K"while"
        @jl_assert numchildren(ex) == 2 ex
        @ast ctx ex [K"symbolicblock" "loop-exit"::K"symboliclabel"
            [K"_while"
                expand_condition(ctx, ex[1])
                [K"symbolicblock" "loop-cont"::K"symboliclabel"
                    [K"scope_block"(scope_type=:neutral)
                         expand_forms_2(ctx, ex[2])
                    ]
                ]
            ]
        ]
    elseif k == K"inert" || k == K"inert_syntaxtree"
        ex
    elseif k == K"foreigncall"
        # Assume user macros may produce this, but static_eval means desugaring
        # has already occurred.
        args = SyntaxList(ctx)
        for i in 2:numchildren(ex)
            c = ex[i]
            if kind(c) === K"static_eval"
                push!(args, c)
            elseif i <= 3
                push!(args, @ast ctx ex [K"static_eval" expand_forms_2(ctx, c)])
            else
                push!(args, expand_forms_2(ctx, c))
            end
        end
        @ast ctx ex [K"foreigncall" expand_C_library_symbol(ctx, ex[1]) args...]
    elseif k == K"gc_preserve"
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
    elseif k == K"ssavalue"
        _resolve_ssavalue(ctx, ex)
    elseif is_leaf(ex)
        ex
    elseif k == K"return"
        if numchildren(ex) == 0
            @ast ctx ex [K"return" (::K"nothing")]
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

ensure_desugaring_attributes!(graph) = ensure_attributes!(
    ensure_macro_attributes!(graph),
    is_toplevel_thunk=Bool,
    toplevel_pure=Bool,
    scope_type=Symbol)

@fzone "JL: desugar" function expand_forms_2(ctx::MacroExpansionContext, ex::SyntaxTree)
    graph = ensure_desugaring_attributes!(copy_attrs(ctx.graph))
    ex = reparent(graph, ex)
    ctx_out = DesugaringContext(graph, ctx.bindings, ctx.scope_layers,
                                current_layer(ctx).mod, ctx.expr_compat_mode,
                                Dict{Int, IdTag}(), ctx.macro_world)
    vr = valid_st1(ex)
    # surface only one error until we have pretty-printing for multiple
    if !vr.ok
        throw(LoweringError(vr.errors[1].sts, vr.errors[1].msgs, false))
    end
    ex_out = expand_forms_2(ctx_out, est_to_dst(ex))
    if DEBUG
        vr = valid_st2(ex_out)
        !vr.ok && throw(LoweringError(vr.errors[1].sts, vr.errors[1].msgs, true))
    end
    ctx_out, ex_out
end

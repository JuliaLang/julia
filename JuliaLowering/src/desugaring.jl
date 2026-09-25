# Lowering Pass 2 - syntax desugaring

mutable struct DesugaringContext <: AbstractLoweringContext
    const layer::ScopeLayer
    const bindings::Bindings
    const ssa_mapping::Dict{Int, IdTag}
    const world::UInt
end

# Translate a :ssavalue node from pre-lowered code into a normal SSA binding.
# Uses ctx.ssa_mapping to ensure the same external SSA id maps to the same binding.
function _resolve_ssavalue(ctx::DesugaringContext, ex)
    binding_id = get!(ctx.ssa_mapping, ex[1].value) do
        syntax_id(ssavar(ctx, ex))
    end
    binding_ex(ctx, binding_id)
end

# Return true when `x` and `y` are "the same identifier", but also works with
# bindings (and hence ssa vars). See also `is_identifier_like()`
function is_same_identifier_like(ex::SyntaxTree, y::SyntaxTree)
    return (head(ex) == :identifier && head(y) == :identifier && NameKey(ex) == NameKey(y)) ||
           (head(ex) == :bindingid  && head(y) == :bindingid  && syntax_id(ex) == syntax_id(y))
end

function is_same_identifier_like(ex::SyntaxTree, name::AbstractString)
    return head(ex) == :identifier && syntax_name(ex) == name
end

# Hack.  Scopes aren't resolved, so only use this where a false positive is
# still a correct answer.
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
    elseif !is_leaf(ex) && !(head(ex) in (:quote, :inert, :syntaxinert, :meta))
        return any(contains_unquoted(f, e) for e in children(ex))
    else
        return false
    end
end

# Identify some expressions that are safe to repeat
#
# TODO: Can we use this in more places?
function is_effect_free(ex)
    k = head(ex)
    # TODO: metas
    is_identifier_like(ex) || k == :symbol ||
        k == :inert || k == :syntaxinert || k == :top ||
        k == :core || k == :value || k == :nothing
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
    i = findfirst(head(e) == :(=) || head(e) == :kw for e in exs)
    if !isnothing(i)
        throw(LoweringError(exs[i], msg))
    end
end

function new_internal_context(st::SyntaxTree)
    sc_orig = st.context
    SyntaxContext(
        ScopeLayer(syntax_module(st), nothing),
        # macro provenance: could use nothing, but this is easier for consumers
        sc_orig.unexpanded,
        # internal bindings are only used in syntax we create, so the edition
        # should be the latest one
        JL_NEW_EDITION,
        true)
end

# Generating a new_local_binding or ssaval should only be done if we can
# guarantee there's some scope it's declared in, and that it's not declared or
# used outside of that scope (binding capture is OK).  This is the alternative.
function newsym(ctx, src::SyntaxTree, name::String; unused=false)
    h = unused ? :placeholder : :identifier
    out = @mknode(; head=h, source=src, value=name, children=nothing,
                  meta=src.meta, context=new_internal_context(src))
end

# In an flisp-compatible expansion, some explicit global declarations (and any
# initialization in the same expression) are unhygienic; they are declared in
# the macrocall module (unless wrapped in a top-level form).  This is buggy
# (references in the same scope don't resolve to it, op-equal assignments don't
# work, etc.), but compatible.  flisp: `unescape`, `unescape-global-lhs`.  TODO:
# It would be cleaner to do this in compat.jl.
function relayer_global_if_unhygienic(ctx, st::SyntaxTree)
    sc = st.context
    relayered = SyntaxList()
    # TODO: is_base_layer(sc) or sc.layer == ctx.layer?
    (!is_flisp_compat(sc) || is_base_layer(sc)) && return st, relayered
    sc2 = escape_layer(sc, true)
    return _relayer_global_if_unhygienic(relayered, st, sc2), relayered
end
function _relayer_global_if_unhygienic(done::SyntaxList, st::SyntaxTree, sc::SyntaxContext)
    k = head(st)
    if k === :identifier && is_flisp_compat(st) && st.context !== sc
        push!(done, st)
        @mknode(st; context=sc)
    elseif k === :(::) || k === :kw
        n_done = length(done)
        lhs = _relayer_global_if_unhygienic(done, st[1], sc)
        n_done == length(done) ? st : (@ast _ st [k lhs st[2]])
    elseif k === :tuple || k === :parameters
        mapchildren(e->_relayer_global_if_unhygienic(done, e, sc), st)
    else
        st
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
    wrap(asgn) = is_const ? (@ast ctx ex [:const asgn]) : asgn

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
    stmts = SyntaxList()
    rhs_tmps = SyntaxList()
    for i in 1:n_rhs
        rh = rhs[i]
        r = if head(rh) == :...
            rh[1]
        else
            rh
        end
        k = head(r)
        if k == :value || k == :symbol || k == :inert ||
            k == :syntaxinert || k == :top || k == :core
            # Effect-free and nothrow right hand sides do not need a temporary
            # (we require nothrow because the order of rhs terms is observable
            #  due to sequencing, thus identifiers are not allowed)
        else
            # Example rhs which need a temporary
            # * `f()` - arbitrary side effects to any binding
            # * `z`   - might throw UndefVarError
            tmp = emit_assign_tmp(stmts, ctx, r)
            rh = head(rh) == :... ? @ast(ctx, rh, [:... tmp]) : tmp
        end
        push!(rhs_tmps, rh)
    end

    il = 0
    ir = 0
    while il < n_lhs
        il += 1
        ir += 1
        lh = lhs[il]
        if head(lh) == :...
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
                if head(lhs[jl]) == :... || head(rhs_tmps[jr]) == :...
                    break
                end
                jl -= 1
                jr -= 1
            end
            middle = emit_assign_tmp(stmts, ctx,
                @ast(ctx, rhs, [:tuple rhs_tmps[ir:jr]...]),
                "rhs_tmp"
            )
            if il == jl
                # (x, ys...) = (a,b,c)
                # (x, ys...) = (a,bs...)
                # (ys...)    = ()
                push!(stmts, wrap(@ast ctx ex [:(=) lh[1] middle]))
            else
                # (x, ys..., z) = (a, b, c, d)
                # (x, ys..., z) = (a, bs...)
                # (xs..., y)    = (a, bs...)
                push!(stmts, wrap(@ast ctx ex [:(=) [:tuple lhs[il:jl]...] middle]))
            end
            # Continue with the remainder of the list of non-splat terms
            il = jl
            ir = jr
        else
            rh = rhs_tmps[ir]
            if head(rh) == :...
                push!(stmts, wrap(@ast ctx ex [:(=) [:tuple lhs[il:end]...] rh[1]]))
                break
            else
                push!(stmts, wrap(@ast ctx ex [:(=) lh rh]))
            end
        end
    end

    @ast ctx ex [:block
        stmts...
        [:removable [:tuple rhs_tmps...]]
    ]
end

# Create an assignment `$lhs = $rhs` where `lhs` must be "simple". If `rhs` is
# a block, sink the assignment into the last statement of the block to keep
# more expressions at top level. `rhs` should already be expanded.
#
# flisp: sink-assignment
function sink_assignment(ctx, srcref, lhs, rhs)
    @jl_assert is_identifier_like(lhs) lhs
    if head(rhs) == :block && numchildren(rhs) > 0
        @ast ctx srcref [:block
            rhs[1:end-1]...
            [:(=) lhs rhs[end]]
        ]
    else
        @ast ctx srcref [:(=) lhs rhs]
    end
end

function _tuple_sides_match(lhs, rhs)
    N = max(length(lhs), length(rhs))
    for i = 1:N+1
        if i > length(lhs)
            # (x, y)        = (a, b)      # match
            # (x,)          = (a, b)      # no match
            return i > length(rhs)
        elseif head(lhs[i]) == :...
            # (x, ys..., z) = (a, b)      # match
            # (x, ys...)    = (a,)        # match
            return true
        elseif i > length(rhs)
            # (x, y)        = (a,)        # no match
            # (x, y, zs...) = (a,)        # no match
            return false
        elseif head(rhs[i]) == :...
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
    stmts = SyntaxList()
    tmp = emit_assign_tmp(stmts, ctx, rhs, "rhs_tmp")
    for (i, lh) in enumerate(lhss)
        push!(stmts, @ast ctx assignment_srcref [:(=)
            lh
            [:call "getfield"::core tmp i::value]
        ])
    end
    newnode(assignment_srcref, :block, stmts)
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

    end_stmts = SyntaxList()
    wrap(asgn) = is_const ? (@ast ctx assignment_srcref [:const asgn]) : asgn

    i = 0
    for lh in children(lhs)
        i += 1
        if head(lh) == :...
            lh1 = if is_identifier_like(lh[1]) && !is_const
                lh[1]
            else
                lhs_tmp = ssavar(ctx, lh[1], "lhs_tmp")
                push!(end_stmts, expand_forms_2(ctx, wrap(@ast ctx lh[1] [:(=) lh[1] lhs_tmp])))
                lhs_tmp
            end
            if i == n_lhs
                # Slurping as last lhs, eg, for `zs` in
                #   (x, y, zs...) = rhs
                if head(lh1) != :placeholder
                    push!(stmts, expand_forms_2(ctx,
                        @ast ctx assignment_srcref [:(=)
                            lh1
                            [:call
                                "rest"::top
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
                            @ast ctx assignment_srcref [:call
                                "split_rest"::top
                                rhs
                                (n_lhs - i)::value
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
                push!(end_stmts, expand_forms_2(ctx, wrap(@ast ctx lh [:(=) lh lhs_tmp])))
                lhs_tmp
            end
            push!(stmts,
                expand_forms_2(ctx,
                    lower_tuple_assignment(ctx,
                        assignment_srcref,
                        i == n_lhs ? (lh1,) : (lh1, iterstate),
                        @ast ctx assignment_srcref [:call
                            "indexed_iterate"::top
                            rhs
                            i::value
                            if i > 1
                                iterstate
                            end
                        ]
                    )
                )
            )
        end
    end
    # Actual assignments must happen after the whole iterator is destructured
    # (https://github.com/JuliaLang/julia/issues/40574)
    append!(stmts, end_stmts)
    stmts
end

# Expands cases of property destructuring
function expand_property_destruct(ctx, ex)
    @jl_assert numchildren(ex) == 2 ex
    lhs = ex[1]
    @jl_assert head(lhs) == :tuple ex
    if numchildren(lhs) != 1
        throw(LoweringError(lhs, "Property destructuring must use a single `;` before the property names, eg `(; a, b) = rhs`"))
    end
    params = lhs[1]
    @jl_assert head(params) == :parameters ex
    rhs = ex[2]
    stmts = SyntaxList()
    rhs1 = emit_assign_tmp(stmts, ctx, expand_forms_2(ctx, rhs))
    for prop in children(params)
        propname = head(prop) == :identifier                           ? prop    :
                   head(prop) == :(::) && head(prop[1]) == :identifier ? prop[1] :
                   throw(LoweringError(prop, "invalid assignment location"))
        push!(stmts, expand_forms_2(ctx, @ast ctx rhs1 [:(=)
            prop
            [:call
                "getproperty"::top
                rhs1
                propname=>:symbol
            ]
        ]))
    end
    push!(stmts, @ast ctx rhs1 [:removable rhs1])
    newnode(ex, :block, stmts)
end

# Expands all cases of general tuple destructuring, eg
#   (x,y) = (a,b)
function expand_tuple_destruct(ctx, ex, is_const)
    lhs = ex[1]
    @jl_assert head(lhs) == :tuple ex
    rhs = ex[2]

    num_slurp = 0
    for lh in children(lhs)
        num_slurp += (head(lh) == :...)
        if num_slurp > 1
            throw(LoweringError(lh, "multiple `...` in destructuring assignment are ambiguous"))
        end
    end

    if head(rhs) == :tuple
        num_splat = sum(head(rh) == :... for rh in children(rhs); init=0)
        if num_splat == 0 && (numchildren(lhs) - num_slurp) > numchildren(rhs)
            throw(LoweringError(ex, "More variables on left hand side than right hand in tuple assignment"))
        end

        if !any_assignment(children(rhs)) && !has_parameters(rhs) &&
                _tuple_sides_match(children(lhs), children(rhs))
            return expand_forms_2(ctx, tuple_to_assignments(ctx, ex, is_const))
        end
    end

    stmts = SyntaxList()
    rhs1 = if is_ssa(ctx, rhs) ||
            (is_identifier_like(rhs) &&
             !any(is_same_identifier_like(head(l) == :... ? l[1] : l, rhs)
                  for l in children(lhs)))
        rhs
    else
        emit_assign_tmp(stmts, ctx, expand_forms_2(ctx, rhs))
    end
    _destructure(ctx, ex, stmts, lhs, rhs1, is_const)
    push!(stmts, @ast ctx rhs1 [:removable rhs1])
    newnode(ex, :block, stmts)
end

#-------------------------------------------------------------------------------
# Expand comparison chains

function expand_scalar_compare_chain(ctx, srcref, terms, i)
    comparisons = nothing
    while i + 2 <= length(terms)
        lhs = terms[i]
        op = terms[i+1]
        rhs = terms[i+2]
        if head(op) == :. && numchildren(op) == 1
            break
        end
        comp = @ast ctx op [:call
            op
            lhs
            rhs
        ]
        if isnothing(comparisons)
            comparisons = comp
        else
            comparisons = @ast ctx srcref [:&&
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
    @jl_assert head(ex) == :comparison ex
    terms = children(ex)
    @jl_assert numchildren(ex) >= 3 ex
    @jl_assert isodd(numchildren(ex)) ex
    i = 1
    comparisons = nothing
    # Combine any number of dotted comparisons
    while i + 2 <= length(terms)
        if !(head(terms[i+1]) == :. && numchildren(terms[i+1]) == 1)
            (comp, i) = expand_scalar_compare_chain(ctx, ex, terms, i)
        else
            lhs = terms[i]
            op = terms[i+1]
            rhs = terms[i+2]
            i += 2
            comp = @ast ctx op [:dotcall
                op[1]
                lhs
                rhs
            ]
        end
        if isnothing(comparisons)
            comparisons = comp
        else
            comparisons = @ast ctx ex [:dotcall
                "&"::top
                # ^^ NB: Flisp bug. Flisp lowering essentially does
                #     adopt_scope("&"::identifier, ctx.mod)
                # here which seems wrong if the comparison chain arose from
                # a macro in a different module. One fix would be to use
                #     adopt_scope("&"::identifier, ex)
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
    k = head(ex)
    if is_effect_free(ex)
        ex
    elseif k == :...
        @ast ctx ex [k _arg_to_temp(ctx, stmts, ex[1])]
    elseif k == :kw
        @ast ctx ex [:kw ex[1] _arg_to_temp(ctx, stmts, ex[2])]
    elseif k == :parameters
        mapchildren(ex) do e
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
    if is_identifier_like(ex) || head(ex) === :value
        ex
    else
        k = head(ex)
        if k == :let
            emit_assign_tmp(stmts, ctx, ex)
        else
            args = SyntaxList()
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
    k = head(ex)
    if k == :identifier && syntax_name(ex) in ("begin", "end")
        indexfunc = @ast ctx ex (syntax_name(ex) == "begin" ? "firstindex" : "lastindex")::top
        if length(splats) == 0
            if is_last && n == 1
                @ast ctx ex [:call indexfunc arr]
            else
                @ast ctx ex [:call indexfunc arr n::value]
            end
        else
            splat_lengths = SyntaxList()
            for splat in splats
                push!(splat_lengths, @ast ctx ex [:call "length"::top splat])
            end
            @ast ctx ex [:call
                indexfunc
                arr
                [:call
                    "+"::top
                    (n - length(splats))::value
                    splat_lengths...
                ]
            ]
        end
    elseif is_leaf(ex) || is_quoted(ex)
        ex
    elseif k == :ref
        # inside ref, only replace within the first argument
        @ast ctx ex [k
            replace_beginend(ctx, ex[1], arr, n, splats, is_last)
            ex[2:end]...
        ]
    elseif k == :kw
        # note from flisp
        # TODO: this probably should not be allowed since keyword args aren't
        # positional, but in this context we have just used their positions anyway
        @ast ctx ex [:kw ex[1] replace_beginend(ctx, ex[2], arr, n, splats, is_last)]
    else
        mapchildren(e->replace_beginend(ctx, e, arr, n, splats, is_last), ex)
    end
end

# Go through indices and replace the `begin` or `end` symbol
# `arr` - array being indexed
# `idxs` - list of indices
# returns the expanded indices. Any statements that need to execute first are
# added to ctx.stmts.
function process_indices(sctx::StatementListCtx, arr, idxs)
    has_splats = any(head(i) == :... for i in idxs)
    idxs_out = SyntaxList()
    splats = SyntaxList()
    for (n, idx0) in enumerate(idxs)
        is_splat = head(idx0) == :...
        val = replace_beginend(sctx, is_splat ? idx0[1] : idx0,
                               arr, n, splats, n == length(idxs))
        idx = head(val) === :kw || !has_splats || is_simple_atom(sctx, val) ?
            val : emit_assign_tmp(sctx, val)
        if is_splat
            push!(splats, idx)
        end
        push!(idxs_out, is_splat ? @ast(sctx, idx0, [:... idx]) : idx)
    end
    return idxs_out
end

# Expand things like `f()[i,end]`, add to `sctx.stmts` (temporaries for
# computing indices) and return
# * `arr` -  The array (may be a temporary ssa value)
# * `idxs` - List of indices
function expand_ref_components(sctx::StatementListCtx, ex)
    check_no_parameters(ex, "unexpected semicolon in array expression")
    @jl_assert head(ex) == :ref ex
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
    @jl_assert head(ex) == :(=) && numchildren(ex) == 2 ex
    lhs = ex[1]
    sctx = with_stmts(ctx)
    (arr, idxs) = expand_ref_components(sctx, lhs)
    rhs = emit_assign_tmp(sctx, ex[2])
    @ast ctx ex [:block
        sctx.stmts...
        expand_forms_2(ctx, [:call
            "setindex!"::top
            arr
            rhs
            idxs...
        ])
        [:removable rhs]
    ]
end

#-------------------------------------------------------------------------------
# Expansion of broadcast notation `f.(x .+ y)`

function expand_dotcall(ctx, ex)
    k = head(ex)
    if k == :dotcall
        @jl_assert numchildren(ex) >= 1 ex
        farg = setmeta(ex[1], :is_called, true)
        args = SyntaxList()
        append!(args, ex[2:end])
        kws = remove_kw_args!(ctx, args)
        @ast ctx ex [:call
            (isnothing(kws) ? "broadcasted" : "broadcasted_kwsyntax")::top
            farg    # todo: What about (z=f).(x,y) ?
            (expand_dotcall(ctx, arg) for arg in args)...
            if !isnothing(kws)
                [:parameters
                    kws...
                ]
            end
        ]
    elseif k == :comparison
        expand_dotcall(ctx, expand_compare_chain(ctx, ex))
    elseif k == :.&& || k == :.||
        @ast ctx ex [:call
            "broadcasted"::top
            (k == :.&& ? "andand" : "oror")::top
            (expand_dotcall(ctx, arg) for arg in children(ex))...
        ]
    else
        ex
    end
end

function expand_fuse_broadcast(ctx, ex)
    if head(ex) == :.= || head(ex) == :var".op="
        @jl_assert numchildren(ex) == 2 ex
        lhs = ex[1]
        kl = head(lhs)
        rhs = expand_dotcall(ctx, ex[2])
        @ast ctx ex [:block
            dest := if kl == :ref
                sctx = with_stmts(ctx)
                (arr, idxs) = expand_ref_components(sctx, lhs)
                [:block
                    sctx.stmts...
                    [:call
                        "dotview"::top
                        arr
                        idxs...
                    ]
                ]
            elseif kl == :. && numchildren(lhs) == 2
                [:call
                    "dotgetproperty"::top
                    children(lhs)...
                ]
            else
                lhs
            end
            bc := if !(head(rhs) == :call && head(rhs[1]) == :top && syntax_name(rhs[1]) == "broadcasted")
                # Ensure the rhs of .= is always wrapped in a call to `broadcasted()`
                [:call(rhs)
                    "broadcasted"::top
                    "identity"::top
                    rhs
                ]
            else
                rhs
            end
            [:call "materialize!"::top dest bc]
            dest
        ]
    else
        @ast ctx ex [:call
            "materialize"::top
            expand_dotcall(ctx, ex)
        ]
    end
end

#-------------------------------------------------------------------------------
# Expansion of generators and comprehensions

# Return any subexpression which is a 'return` statement, not including any
# inside quoted sections or method bodies.
function find_return(ex::SyntaxTree)
    if head(ex) == :return
        return ex
    elseif !is_leaf(ex) && !(head(ex) in (:quote, :inert, :syntaxinert, :meta, :function, :->))
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

function lhs_local_defs(ctx, lhs)
    defs = SyntaxList()
    foreach_lhs_name(lhs) do var
        push!(defs, @ast ctx var [:local var])
    end
    return defs
end

# Return the anonymous function taking an iterated value, for use with the
# first argument to `Base.Generator`
function func_for_generator(ctx, body, iter_value_destructuring)
    if is_same_identifier_like(iter_value_destructuring, body)
        # Use Base.identity for generators which are filters such as
        # `(x for x in xs if f(x))`. This avoids creating a new type.
        @ast ctx body "identity"::top
    elseif !is_identifier_like(iter_value_destructuring)
        # compat: arg::T should convert, not assert, and duplicated arg is OK
        arg = newsym(ctx, iter_value_destructuring, "#generator#")
        @ast ctx body [:->
            [:tuple arg]
            [:block
                lhs_local_defs(ctx, iter_value_destructuring)...
                [:(=) iter_value_destructuring arg]
                body]]
    else
        @ast ctx body [:-> [:tuple iter_value_destructuring] [:block body]]
    end
end

function expand_generator(ctx, ex)
    @jl_assert numchildren(ex) >= 2 ex
    body = ex[1]
    check_no_return(body)
    if numchildren(ex) > 2
        outervar_assignments = SyntaxList()
        for iterspecs in ex[2:end-1]
            for iterspec in children(iterspecs)
                foreach_lhs_name(iterspec[1]) do var
                    @jl_assert head(var) == :identifier ex # Todo: :bindingid?
                    push!(outervar_assignments, @ast ctx var [:(=) var var])
                end
            end
        end
        body = @ast ctx ex [:let
            [:block
                outervar_assignments...
            ]
            [:block
                body
            ]
        ]
    end
    for iterspecs_ind in numchildren(ex):-1:2
        iterspecs = ex[iterspecs_ind]
        filter_test = nothing
        if head(iterspecs) == :filter
            filter_test = iterspecs[2]
            iterspecs = iterspecs[1]
        end
        if head(iterspecs) != :iteration
            throw(LoweringError(ex, """Expected `:iteration` iteration specification in generator"""))
        end
        iter_ranges = SyntaxList()
        iter_lhss = SyntaxList()
        for iterspec in children(iterspecs)
            @jl_assert head(iterspec) == :in iterspec
            @jl_assert numchildren(iterspec) == 2 iterspec
            push!(iter_lhss, iterspec[1])
            push!(iter_ranges, iterspec[2])
        end
        iter_value_destructuring = if numchildren(iterspecs) == 1
            iterspecs[1][1]
        else
            iter_lhss = SyntaxList()
            for iterspec in children(iterspecs)
                push!(iter_lhss, iterspec[1])
            end
            @ast ctx iterspecs [:tuple iter_lhss...]
        end
        iter = if length(iter_ranges) > 1
            @ast ctx iterspecs [:call
                "product"::top
                iter_ranges...
            ]
        else
            iter_ranges[1]
        end
        if !isnothing(filter_test)
            iter = @ast ctx ex [:call
                "Filter"::top
                func_for_generator(ctx, filter_test, iter_value_destructuring)
                iter
            ]
        end
        body = @ast ctx ex [:call
            "Generator"::top
            func_for_generator(ctx, body, iter_value_destructuring)
            iter
        ]
        if iterspecs_ind < numchildren(ex)
            body = @ast ctx ex [:call
                "Flatten"::top
                body
            ]
        end
    end
    body
end

function expand_comprehension_to_loops(ctx, ex)
    @jl_assert head(ex) == :typed_comprehension ex
    element_type = ex[1]
    gen = ex[2]
    @jl_assert head(gen) == :generator ex
    body = gen[1]
    check_no_return(body)
    # TODO: check_no_break_continue
    iterspecs = gen[2]
    @jl_assert head(iterspecs) == :iteration ex
    new_iterspecs = SyntaxList()
    iters = SyntaxList()
    iter_defs = SyntaxList()
    for iterspec in children(iterspecs)
        iter = emit_assign_tmp(iter_defs, ctx, iterspec[2], "iter")
        push!(iters, iter)
        push!(new_iterspecs, @ast ctx iterspec [:in iterspec[1] iter])
    end
    # Lower to nested for loops
    idx = new_local_binding(ctx, iterspecs, "idx")
    @ast ctx ex [:block
        iter_defs...
        full_iter := if length(iters) == 1
            iters[1]
        else
            [:call
                "product"::top
                iters...
            ]
        end
        iter_size := [:call "IteratorSize"::top full_iter]
        size_unknown := [:call "isa"::core iter_size "SizeUnknown"::top]
        result    := [:call "_array_for"::top element_type full_iter iter_size]
        [:(=) idx [:call "first"::top [:call "LinearIndices"::top result]]]
        [:for [:iteration Iterators.reverse(new_iterspecs)...]
            [:block
                val := body
                # TODO: inbounds setindex
                [:if size_unknown
                    [:call "push!"::top result val]
                    [:call "setindex!"::top result val idx]
                ]
                [:(=) idx [:call "add_int"::top idx 1::value]]
            ]
        ]
        result
    ]
end

# Mimics native lowerer's tuple-wrap function (julia-syntax.scm:2723-2736)
# Unwraps only ONE layer of `...` and wraps sequences of non-splat args in tuples.
# Example: `[a, b, xs..., c]` -> `[tuple(a, b), xs, tuple(c)]`
function _wrap_unsplatted_args(ctx, call_ex, args)
    result = SyntaxList()
    non_splat_run = SyntaxList()
    for arg in args
        if head(arg) == :...
            # Flush any accumulated non-splat args
            if !isempty(non_splat_run)
                push!(result, @ast ctx call_ex [:call "tuple"::core non_splat_run...])
                non_splat_run = SyntaxList()
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
        push!(result, @ast ctx call_ex [:call "tuple"::core non_splat_run...])
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
    result = @ast ctx ex [:call
        "_apply_iterate"::core
        "iterate"::top
        topfunc
        wrapped_args...
    ]

    # Recursively expand the entire call (matching native's expand-forms)
    return expand_forms_2(ctx, result)
end

function expand_array(ctx, ex, topfunc)
    args = children(ex)
    check_no_assignment(args)
    topfunc = @ast ctx ex topfunc::top
    if any(head(arg) == :... for arg in args)
        expand_splat(ctx, ex, topfunc, args)
    else
        @ast ctx ex [:call
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
    is_typed = head(ex) == :typed_vcat
    eltype   = is_typed ? ex[1]     : nothing
    elements = is_typed ? ex[2:end] : ex[1:end]
    for e in elements
        k = head(e)
        if k == :row
            had_row = true
            had_row_splat = had_row_splat || any(head(e1) == :... for e1 in children(e))
        end
    end
    if had_row_splat
        # In case there is splatting inside `hvcat`, collect each row as a
        # separate tuple and pass those to `hvcat_rows` instead (ref #38844)
        rows = SyntaxList()
        for e in elements
            if head(e) == :row
                push!(rows, @ast ctx e [:tuple children(e)...])
            else
                push!(rows, @ast ctx e [:tuple e])
            end
        end
        fname = is_typed ? "typed_hvcat_rows" : "hvcat_rows"
        @ast ctx ex [:call
            fname::top
            eltype
            rows...
        ]
    else
        row_sizes = SyntaxList()
        flat_elems = SyntaxList()
        for e in elements
            if head(e) == :row
                rowsize = numchildren(e)
                append!(flat_elems, children(e))
            else
                rowsize = 1
                push!(flat_elems, e)
            end
            push!(row_sizes, @ast ctx e rowsize::value)
        end
        if had_row
            fname = is_typed ? "typed_hvcat" : "hvcat"
            @ast ctx ex [:call
                fname::top
                eltype
                [:tuple row_sizes...]
                flat_elems...
            ]
        else
            fname = is_typed ? "typed_vcat" : "vcat"
            @ast ctx ex [:call
                fname::top
                eltype
                flat_elems...
            ]
        end
    end
end

function ncat_contains_row(ex)
    k = head(ex)
    if k == :row
        return true
    elseif k == :nrow
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
    k = head(ex)
    if k == :row
        layout_dim = 1
        elems = children(ex)
        parent_layout_dim != 1 || throw(LoweringError(ex,"Badly nested rows in `ncat`"))
    elseif k == :nrow
        dim = ex[1].value::Int
        elems = children(ex)[2:end]
        dim > 0                || throw(LoweringError(ex,"Unsupported dimension $dim in ncat"))
        !row_major || dim != 2 || throw(LoweringError(ex,"2D `nrow` cannot be mixed with `row` in `ncat`"))
        layout_dim = nrow_flipdim(row_major, dim)
    elseif head(ex) == :...
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
    for e in elems
        if layout_dim == 1
            head(e) ∉ (:nrow, :row) || throw(LoweringError(e,"Badly nested rows in `ncat`"))
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
    is_typed = head(ex) == :typed_ncat
    eltype      = is_typed ? ex[1]     : nothing
    outer_dim   = is_typed ? ex[2].value::Int : ex[1].value::Int
    elements    = is_typed ? ex[3:end] : ex[2:end]
    hvncat_name = is_typed ? "typed_hvncat" : "hvncat"
    @jl_assert outer_dim > 0 (ex,"Unsupported dimension in ncat")
    if !any(head(e) === :row || head(e) === :nrow for e in elements)
        # One-dimensional ncat along some dimension
        #   [a ;;; b ;;; c]
        return @ast ctx ex [:call
            hvncat_name::top
            eltype
            outer_dim::value
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
    flat_elems = SyntaxList()
    # `ncat` syntax nests lower dimensional `nrow` inside higher dimensional
    # ones (with the exception of :row when `row_major` is true). Each nrow
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
    dim_lengths = zeros(Int, outer_dim)
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
        dim_lengths[layout_dim] = Int(dimspan ÷ prev_dimspan)
        prev_dimspan = dimspan
    end
    shape_spec = SyntaxList()
    if is_balanced
        if row_major
            dim_lengths[1], dim_lengths[2] = dim_lengths[2], dim_lengths[1]
        end
        # For balanced concatenations, the shape is specified by the length
        # along each dimension.
        for dl in dim_lengths
            push!(shape_spec, @ast ctx ex dl::value)
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
                @ast ctx ex [:tuple
                    [i::value for i in groups_for_dim]...
                ]
            )
        end
    end
    @ast ctx ex [:call
        hvncat_name::top
        eltype
        [:tuple shape_spec...]
        row_major::value
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
        @ast ctx srcref [:block
            rr := [:where rhs lhs[2:end]...]
            [is_const ? :constdecl : :assign_or_constdecl_if_global name rr]
            [:removable rr]
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
    kl = head(lhs)
    if kl == :curly
        expand_unionall_def(ctx, ex, lhs, rhs, is_const)
    elseif head(rhs) == :(=)
        # Expand chains of assignments
        # a = b = rhs  ==>  rr=rhs; b=rr; a=rr
        stmts = SyntaxList()
        rhs_end = rhs; while head(rhs_end) === :(=)
            rhs_end = rhs_end[2]
        end
        if !is_identifier_like(rhs_end)
            rr = ssavar(ctx, rhs_end, "rhs")
            assign_rr = @ast ctx rhs_end [:(=) rr rhs_end]
        else
            rr = rhs_end
            assign_rr = nothing
        end
        ex_i = ex; while head(ex_i) === :(=)
            push!(stmts, @ast ctx ex_i [:(=) ex_i[1] rr])
            ex_i = ex_i[2]
        end
        # In const a = b = c, only a is const
        is_const && (stmts[1] = @mknode(stmts[1]; head=:constdecl))

        out = @ast ctx ex [:block assign_rr reverse!(stmts)... [:removable rr]]
        expand_forms_2(ctx, out)
    elseif kl == :ssavalue
        sink_assignment(ctx, ex, _resolve_ssavalue(ctx, lhs), expand_forms_2(ctx, rhs))
    elseif is_identifier_like(lhs)
        if is_const
            rr = ssavar(ctx, ex)
            @ast ctx ex [:block
                sink_assignment(ctx, ex, rr, expand_forms_2(ctx, rhs))
                [:constdecl lhs rr]
                [:removable rr]
            ]
        else
            sink_assignment(ctx, ex, lhs, expand_forms_2(ctx, rhs))
        end
    elseif kl == :.
        # a.b = rhs  ==>  setproperty!(a, :b, rhs)
        @jl_assert !is_const (ex, "cannot declare `.` form const")
        @jl_assert numchildren(lhs) == 2 lhs
        a = lhs[1]
        b = lhs[2]
        stmts = SyntaxList()
        # TODO: Do we need these first two temporaries?
        if !is_identifier_like(a)
            a = emit_assign_tmp(stmts, ctx, expand_forms_2(ctx, a), "a_tmp")
        end
        if head(b) != :symbol
            b = emit_assign_tmp(stmts, ctx, expand_forms_2(ctx, b), "b_tmp")
        end
        if !is_identifier_like(rhs) && !(head(rhs) === :value)
            rhs = emit_assign_tmp(stmts, ctx, expand_forms_2(ctx, rhs), "rhs_tmp")
        end
        @ast ctx ex [:block
            stmts...
            [:call "setproperty!"::top a b rhs]
            [:removable rhs]
        ]
    elseif kl == :tuple
        if has_parameters(lhs)
            expand_property_destruct(ctx, ex)
        else
            expand_tuple_destruct(ctx, ex, is_const)
        end
    elseif kl == :ref
        # a[i1, i2] = rhs
        @jl_assert !is_const (ex, "cannot declare ref form const")
        expand_forms_2(ctx, expand_setindex(ctx, ex))
    elseif kl == :(::) && numchildren(lhs) == 2
        x = lhs[1]
        T = lhs[2]
        res = if is_const
            expand_forms_2(ctx, @ast ctx ex [:const
                [:(=)
                     lhs[1]
                     head(lhs[1]) === :placeholder ? rhs :
                        convert_for_type_decl(ctx, ex, rhs, T, true)
                 ]])
        elseif is_identifier_like(x)
            # Identifier in lhs[1] is a variable type declaration, eg
            # x::T = rhs
            @ast ctx ex [:block
                if head(x) !== :placeholder
                     [:decl x T]
                end
                [:(=) x rhs]
            ]
        else
            # Otherwise just a type assertion, eg
            # a[i]::T = rhs  ==>  (a[i]::T; a[i] = rhs)
            # a[f(x)]::T = rhs  ==>  (tmp = f(x); a[tmp]::T; a[tmp] = rhs)
            stmts = SyntaxList()
            l1 = remove_argument_side_effects(ctx, stmts, lhs[1])
            # TODO: What about (f(z),y)::T = rhs? That's broken syntax and
            # needs to be detected somewhere but won't be detected here. Maybe
            # it shows that remove_argument_side_effects() is not the ideal
            # solution here?
            # TODO: handle underscore?
            @ast ctx ex [:block
                stmts...
                [:(::) l1 lhs[2]]
                [:(=) l1 rhs]
            ]
        end
        expand_forms_2(ctx, res)
    elseif kl == :dotcall
        throw(LoweringError(lhs, "invalid dot call syntax on left hand side of assignment"))
    elseif kl == :typed_hcat
        throw(LoweringError(lhs, "invalid spacing in left side of indexed assignment"))
    elseif kl == :typed_vcat || kl == :typed_ncat
        throw(LoweringError(lhs, "unexpected `;` in left side of indexed assignment"))
    elseif kl == :vect || kl == :hcat || kl == :vcat || kl == :ncat
        throw(LoweringError(lhs, "use `(a, b) = ...` to assign multiple values"))
    else
        throw(LoweringError(lhs, "invalid assignment location"))
    end
end

function expand_update_operator(ctx, ex)
    k = head(ex)
    dotted = k == :var".op="

    @jl_assert numchildren(ex) == 3 ex
    lhs = ex[1]
    op = ex[2]
    rhs = ex[3]

    stmts = SyntaxList()

    declT = nothing
    if head(lhs) == :(::)
        # eg `a[i]::T += 1`
        declT = lhs[2]
        decl_lhs = lhs
        lhs = lhs[1]
    end

    if head(lhs) == :ref
        # eg `a[end] = rhs`
        sctx = with_stmts(ctx, stmts)
        (arr, idxs) = expand_ref_components(sctx, lhs)
        lhs = @ast ctx lhs [:ref arr idxs...]
    end

    lhs = remove_argument_side_effects(ctx, stmts, lhs)

    if dotted
        if !(head(lhs) == :ref || (head(lhs) == :. && numchildren(lhs) == 2))
            # `f() .+= rhs`
            lhs = emit_assign_tmp(stmts, ctx, lhs)
        end
    else
        if head(lhs) == :tuple && contains_unquoted(
                e->head(e) == :bindingid && get_binding(ctx, e).is_ssa,
                lhs)
            # If remove_argument_side_effects needed to replace an expression
            # with an ssavalue, then it can't be updated by assignment
            # (JuliaLang/julia#30062)
            throw(LoweringError(lhs, "invalid multiple assignment location"))
        end
    end

    @ast ctx ex [:block
        stmts...
        [(dotted ? :.= : :(=))
            lhs
            [(dotted ? :dotcall : :call)
                op
                if isnothing(declT)
                    lhs
                else
                    [:(::)(decl_lhs) lhs declT]
                end
                rhs
            ]
        ]
    ]
end

#-------------------------------------------------------------------------------
# Expand logical conditional statements

# Flatten nested && or || nodes and expand their children
function expand_cond_children(ctx, ex, cond_kind=head(ex), flat_children=SyntaxList())
    for e in children(ex)
        if head(e) == cond_kind
            expand_cond_children(ctx, e, cond_kind, flat_children)
        else
            push!(flat_children, expand_forms_2(ctx, e))
        end
    end
    flat_children
end

# Expand condition in, eg, `if` or `while`
function expand_condition(ctx, ex)
    isblock = head(ex) == :block && numchildren(ex) >= 1
    test = isblock ? ex[end] : ex
    k = head(test)
    if k == :&& || k == :||
        # `||` and `&&` get special lowering so that they compile directly to
        # jumps rather than first computing a bool and then jumping.
        cs = expand_cond_children(ctx, test)
        test = isempty(cs) ? (@ast ctx ex (k === :&&)::value) :
            length(cs) == 1 ? (@ast ctx ex cs[1]) :
            newnode(test, k, cs)
    else
        test = expand_forms_2(ctx, test)
    end
    if isblock
        # Special handling so that the rules for `&&` and `||` can be applied
        # to the last statement of a block
        @ast ctx ex [:block mapsyntax(e->expand_forms_2(ctx,e), ex[1:end-1])... test]
    else
        test
    end
end

#-------------------------------------------------------------------------------
# Expand let blocks

function expand_let(ctx, ex)
    scope_type = numchildren(ex) == 3 && head(ex[3]) === :neutral_scope ?
        ex[3] : @ast ctx ex [:hard_scope]
    @jl_assert numchildren(ex) == 2 || head(scope_type) === :neutral_scope ex
    bindings = ex[1]
    @jl_assert head(bindings) == :block bindings
    blk = ex[2]
    if numchildren(bindings) == 0
        return @ast ctx ex [:scope_block scope_type blk]
    end
    for binding in Iterators.reverse(children(bindings))
        kb = head(binding)
        if kb == :(::) || is_identifier_like(binding)
            blk = @ast ctx ex [:scope_block scope_type
                [:local binding]
                blk
            ]
        elseif kb == :(=)
            lhs = binding[1]
            rhs = binding[2]
            if is_identifier_like(lhs)
                if head(lhs) === :placeholder
                    blk = @ast ctx binding [:block rhs blk]
                else
                    blk = @ast ctx binding [:block
                        tmp := rhs
                        [:scope_block(ex) scope_type
                            [:local(lhs) lhs]
                            [:always_defined lhs]
                            [:(=)(binding) lhs tmp]
                            blk
                        ]
                    ]
                end
            elseif head(lhs) == :(::)
                var = lhs[1]
                if !is_identifier_like(var)
                    throw(LoweringError(var, "Invalid assignment location in let syntax"))
                elseif head(var) === :placeholder
                    # do a typeassert/convert here? (this falls through flisp as
                    # a typed global...)
                    blk = @ast ctx binding [:block rhs blk]
                else
                    blk = @ast ctx binding [:block
                        tmp := rhs
                        # type := lhs[2]
                        [:scope_block(ex) scope_type
                            # n.b. the declared type is referenced directly (not
                            # hoisted into a temporary) so that the declaration
                            # works for variables captured into other lambdas, where
                            # it is re-evaluated at each assignment like flisp does
                            [:local(lhs) [:(::) var lhs[2]]]
                            [:always_defined var]
                            [:(=)(binding) var tmp]
                            blk
                        ]
                    ]
                end
            elseif head(lhs) == :tuple
                lhs_locals = SyntaxList()
                foreach_lhs_name(lhs) do var
                    push!(lhs_locals, @ast ctx var [:local var])
                    push!(lhs_locals, @ast ctx var [:always_defined var])
                end
                blk = @ast ctx binding [:block
                    tmp := rhs
                    [:scope_block(ex) scope_type
                        lhs_locals...
                        [:(=)(binding) lhs tmp]
                        blk
                    ]
                ]
            else
                throw(LoweringError(lhs, "Invalid assignment location in let syntax"))
            end
        elseif head(binding) == :function
            sig = binding[1]
            func_name = assigned_function_name(sig)
            if isnothing(func_name)
                # Some valid function syntaxes define methods on existing types and
                # don't really make sense with let:
                #    let A.f() = 1 ... end
                #    let (obj::Callable)() = 1 ... end
                throw(LoweringError(sig, "Function signature does not define a local function name"))
            end
            blk = @ast ctx binding [:block
                [:scope_block(ex) scope_type
                    [:local(func_name) func_name]
                    # note no always_defined, as it's stronger than flisp's local-def
                    binding
                    blk
                ]
            ]
        else
            @jl_assert false (binding, "invalid binding in let")
        end
    end
    return blk
end

#-------------------------------------------------------------------------------
# Expand named tuples

function _named_tuple_expr(ctx, srcref, names, values)
    if isempty(names)
        @ast ctx srcref [:call "NamedTuple"::core]
    else
        @ast ctx srcref [:call
            [:curly "NamedTuple"::core [:tuple names...]]
            # NOTE: don't use `tuple` head, so an assignment expression as a value
            # doesn't turn this into another named tuple.
            [:call "tuple"::core values...]
        ]
    end
end

function _merge_named_tuple(ctx, srcref, old, new)
    if isnothing(old)
        new
    else
        @ast ctx srcref [:call "merge"::top old new]
    end
end

function expand_named_tuple(ctx, ex, kws; field_name="named tuple field",
                            element_name="named tuple element")
    name_strs = Set{String}()
    names = SyntaxList()
    values = SyntaxList()
    current_nt = nothing
    for kw in kws
        k = head(kw)
        appended_nt = nothing
        name = value = nothing
        if k == :identifier
            # x  ==>  x = x
            name = to_symbol(ctx, kw)
            value = kw
        elseif k == :kw || k == :(=)
            # syntax TODO: This should parse to :kw
            # x = a
            if head(kw[1]) != :identifier && head(kw[1]) != :placeholder
                throw(LoweringError(kw[1], "invalid $field_name name"))
            end
            if head(kw[2]) == :...
                throw(LoweringError(kw[2], "`...` cannot be used in a value for a $field_name"))
            end
            name = to_symbol(ctx, kw[1])
            value = kw[2]
        elseif k == :.
            # a.x ==> x=a.x
            if head(kw[2]) != :symbol
                throw(LoweringError(kw, "invalid $element_name"))
            end
            name = to_symbol(ctx, kw[2])
            value = kw
        elseif k == :call && numchildren(kw) == 3 &&
                is_same_identifier_like(kw[1], "=>")
            # a=>b   ==>  $a=b
            appended_nt = _named_tuple_expr(ctx, kw, (kw[2],), (kw[3],))
            nothing, nothing
        elseif k == :...
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
            if head(name) == :symbol
                name_str = syntax_name(name)
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
    @ast ctx srcref [:block
        func := farg
        kw_container := expand_named_tuple(ctx, srcref, kws;
                                           field_name="keyword argument",
                                           element_name="keyword argument")
        if all(head(kw) == :... for kw in kws)
            # In this case need to check kws nonempty at runtime
            [:if
                [:call "isempty"::top kw_container]
                [:call func args...]
                [:call "kwcall"::core kw_container func args...]
            ]
        else
            [:call "kwcall"::core kw_container func args...]
        end
    ]
end

# Special rule: Any becomes core.Any regardless of the module
# scope, and don't need GC roots.
function expand_ccall_argtype(ctx, ex)
    if is_same_identifier_like(ex, "Any")
        @ast ctx ex "Any"::core
    else
        expand_forms_2(ctx, ex)
    end
end

# Expand the (sym, lib) argument to ccall / cglobal
function expand_csymbol(ctx, ex)
    @stm ex begin
        [:static_eval _] -> ex # already done
        _ -> expand_forms_2(ctx, ex)
    end
end

function expand_ccall(ctx, ex)
    @jl_assert head(ex) == :call ex
    if numchildren(ex) < 4
        throw(LoweringError(ex, "too few arguments to ccall"))
    end
    cfunc_name = ex[2]
    # Detect calling convention if present.
    known_conventions = ("cdecl", "stdcall", "fastcall", "thiscall", "llvmcall")
    cconv = if head(ex[3]) === :cconv
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
    if head(arg_type_tuple) != :tuple
        msg = "ccall argument types must be a tuple; try `(T,)`"
        if head(return_type) == :tuple
            throw(LoweringError(return_type, msg*" and check if you specified a correct return type"))
        else
            throw(LoweringError(arg_type_tuple, msg))
        end
    end
    arg_types = children(arg_type_tuple)
    vararg_type = nothing
    if length(arg_types) >= 1
        va = arg_types[end]
        if head(va) == :...
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
    expanded_types = SyntaxList()
    for argt in arg_types
        if head(argt) == :...
            throw(LoweringError(argt, "only the trailing ccall argument type should have `...`"))
        end
        push!(expanded_types, expand_ccall_argtype(ctx, argt))
    end
    for _ in length(arg_types)+1:length(args)
        push!(expanded_types, vararg_type)
    end

    # An improvement might be wrap the use of types in cconvert in a special
    # :global_scope expression which modifies the scope resolution. This
    # would at least make the rules self consistent if not pretty.
    #
    # One small improvement we make here is to emit temporaries for all the
    # types used during expansion so at least we don't have their side effects
    # more than once.
    types_for_conv = SyntaxList()
    for argt in expanded_types
        push!(types_for_conv, emit_assign_tmp(sctx, argt))
    end
    gc_roots = SyntaxList()
    unsafe_args  = SyntaxList()
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
                @ast ctx argt [:call
                    "cconvert"::top
                    argt
                    exarg
                ]
            )
            push!(gc_roots, cconverted_arg)
            push!(unsafe_args,
                @ast ctx argt [:call
                    "unsafe_convert"::top
                    argt
                    cconverted_arg
                ]
            )
        end
    end
    @ast ctx ex [:block
        sctx.stmts...
        [:foreigncall
            expand_csymbol(ctx, cfunc_name)
            [:static_eval(;meta=name_hint("ccall return type"))
                expand_forms_2(ctx, return_type)
            ]
            [:static_eval(;meta=name_hint("ccall argument type"))
                [:call
                    "svec"::core
                    expanded_types...
                ]
            ]
            (cconv !== nothing && head(cconv) === :cconv ? cconv[2].value :
                isnothing(vararg_type) ? 0 :
                length(arg_types))::value
            if isnothing(cconv)
                "ccall"::symbol
            elseif head(cconv) === :cconv
                @ast ctx cconv [:inert cconv[1]]
            else
                cconv=>:symbol
            end
            unsafe_args...
            gc_roots... # GC roots
        ]
    ]
end

function expand_cglobal(ctx, ex)
    if numchildren(ex) == 2
        # cglobal(name) -> foreignglobal(name)
        return @ast ctx ex [:foreignglobal
            expand_csymbol(ctx, ex[2])
        ]
    elseif numchildren(ex) == 3
        # cglobal(name, T) -> bitcast(Ptr{T}, foreignglobal(name))
        return @ast ctx ex [:call
            "bitcast"::top
            [:call "apply_type"::core "Ptr"::top expand_forms_2(ctx, ex[3])]
            [:foreignglobal expand_csymbol(ctx, ex[2])]
        ]
    else
        throw(LoweringError(ex, "wrong number of arguments to cglobal"))
    end
end

function remove_kw_args!(ctx, args::SyntaxList)
    kws = nothing
    j = 0
    num_parameter_blocks = 0
    for i in 1:length(args)
        arg = args[i]
        k = head(arg)
        if k == :kw
            if isnothing(kws)
                kws = SyntaxList()
            end
            push!(kws, arg)
        elseif k == :parameters
            num_parameter_blocks += 1
            if num_parameter_blocks > 1
                throw(LoweringError(arg, "Cannot have more than one group of keyword arguments separated with `;`"))
            end
            if numchildren(arg) == 0
                continue # ignore empty parameters (issue #18845)
            end
            if isnothing(kws)
                kws = SyntaxList()
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
    if head(farg) === :identifier && syntax_name(farg) === "ccall"
        return expand_ccall(ctx, ex)
    elseif head(farg) === :identifier && syntax_name(farg) === "cglobal"
        return expand_cglobal(ctx, ex)
    end
    args = copy(ex[2:end])
    kws = remove_kw_args!(ctx, args)
    if !isnothing(kws)
        return expand_forms_2(ctx, expand_kw_call(ctx, ex, farg, args, kws))
    end
    if any(head(arg) == :... for arg in args)
        # Splatting, eg, `f(a, xs..., b)`
        expand_splat(ctx, ex, expand_forms_2(ctx, farg), args)
    elseif head(farg) == :identifier && syntax_name(farg) === "include"
        # world age special case
        r = ssavar(ctx, ex)
        @ast ctx ex [:block
            [:(=) r [:call
                expand_forms_2(ctx, farg)
                expand_forms_2(ctx, args)...
            ]]
            (::latestworld_if_toplevel)
            r
        ]
    else
        @ast ctx ex [:call
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
        # of DOTOP_FLAG? This way, `:.` will be exclusively used for
        # getproperty.
        [:. op] -> @ast ctx ex [:call "BroadcastFunction"::top op]
        [:. l [:syntaxinert r]] ->
            @ast ctx ex [:call "getproperty"::top l [:inert r]]
        [:. l r] -> begin
            @jl_assert (is_leaf(r) || head(r) === :inert || head(r) === :syntaxinert) ex
            @ast ctx ex [:call "getproperty"::top l r]
        end
    end
end

#-------------------------------------------------------------------------------
# Expand for loops

function expand_for(ctx, ex)
    iterspecs = ex[1]

    @jl_assert head(iterspecs) == :iteration ex

    # Loop variables not declared `outer` are reassigned for each iteration of
    # the innermost loop in case the user assigns them to something else.
    # (Maybe we should filter these to remove vars not assigned in the loop?
    # But that would ideally happen after the variable analysis pass, not
    # during desugaring.)
    copied_vars = SyntaxList()
    for iterspec in iterspecs[1:end-1]
        @jl_assert head(iterspec) == :in iterspec
        lhs = iterspec[1]
        if head(lhs) != :outer
            foreach_lhs_name(lhs) do var
                push!(copied_vars, @ast ctx var [:(=) var var])
            end
        end
    end

    loop = ex[2]
    for i in numchildren(iterspecs):-1:1
        iterspec = iterspecs[i]
        lhs = iterspec[1]

        outer = head(lhs) == :outer
        lhs_local_defs = SyntaxList()
        lhs_outer_defs = SyntaxList()
        if outer
            lhs = lhs[1]
        end
        foreach_lhs_name(lhs) do var
            if outer
                push!(lhs_outer_defs, @ast ctx var var)
            else
                push!(lhs_local_defs, @ast ctx var [:local var])
            end
        end

        iter_ex = iterspec[2]
        next = new_local_binding(ctx, iterspec, "next")
        state = ssavar(ctx, iterspec, "state")
        collection = ssavar(ctx, iter_ex, "collection")

        # Assign iteration vars and next state
        body = @ast ctx iterspec [:block
            lhs_local_defs...
            lower_tuple_assignment(ctx, iterspec, (lhs, state), next)
            loop
        ]

        body = if i == numchildren(iterspecs)
            # Innermost loop gets the continue label and copied vars
            @ast ctx ex [:symbolicblock
                "loop-cont"::symboliclabel
                [:let
                     [:block
                         copied_vars...
                     ]
                     body
                    [:neutral_scope]
                ]
            ]
        else
            # Outer loops get a scope block to contain the iteration vars
            @ast ctx ex [:scope_block [:neutral_scope] body]
        end

        loop = @ast ctx ex [:block
            if outer
                [:assert
                    "require_existing_locals"::symbol
                    lhs_outer_defs...
                ]
            end
            [:(=)(iter_ex) collection iter_ex]
            # First call to iterate is unrolled
            #   next = top.iterate(collection)
            [:(=)(iterspec) next [:call "iterate"::top collection]]
            [:if(iterspec) # if next !== nothing
                [:call(iterspec)
                    "not_int"::top
                    [:call "==="::core next (::nothing)]
                ]
                [:_do_while(ex)
                    [:block
                        body
                        # Advance iterator
                        [:(=)(iterspec) next [:call "iterate"::top collection state]]
                    ]
                    [:call(iterspec)
                        "not_int"::top
                        [:call "==="::core next (::nothing)]
                    ]
                ]
            ]
        ]
    end

    @ast ctx ex [:symbolicblock "loop-exit"::symboliclabel
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
        k = head(e)
        if k == :catch && isnothing(catch_)
            @jl_assert numchildren(e) == 2 (e, "Invalid `catch` form")
            catch_ = e
        elseif k == :else && isnothing(else_)
            @jl_assert numchildren(e) == 1 e
            else_ = e[1]
        elseif k == :finally && isnothing(finally_)
            @jl_assert numchildren(e) == 1 e
            finally_ = e[1]
        else
            throw(LoweringError(ex, "Invalid clause in `try` form"))
        end
    end
    (try_, catch_, else_, finally_)
end

function _symboliclabel_defs(st, labels=Set{NameKey}())
    if head(st) === :symboliclabel
        push!(labels, NameKey(st))
    elseif !(is_leaf(st) || is_quoted(st))
        for c in children(st)
            _symboliclabel_defs(c, labels)
        end
    end
    labels
end
function _symboliclabel_refs(st, labels=Vector{SyntaxTree}())
    if head(st) === :symbolicgoto
        push!(labels, st)
    elseif !(is_leaf(st) || is_quoted(st))
        for c in children(st)
            _symboliclabel_refs(c, labels)
        end
    end
    labels
end
function error_if_unmatched_symbolicgoto(ctx, st, hint)
    refs = _symboliclabel_refs(st)
    isempty(refs) && return nothing
    defs = _symboliclabel_defs(st)
    unmatched = nothing
    for r in refs
        NameKey(r) in defs || (unmatched = r)
    end
    isnothing(unmatched) || throw(LoweringError(
        unmatched, "`goto` out of $hint block is not permitted with `finally`"))
end

function expand_try(ctx, ex)
    (try_, catch_, else_, finally_) = match_try(ex)
    if !isnothing(finally_)
        error_if_unmatched_symbolicgoto(ctx, try_, "a `try`")
        !isnothing(catch_) && error_if_unmatched_symbolicgoto(ctx, catch_, "a `catch`")
        !isnothing(else_) && error_if_unmatched_symbolicgoto(ctx, else_, "an `else`")
    end
    try_body = @ast ctx try_ [:scope_block [:neutral_scope] try_]
    if isnothing(catch_)
        try_block = try_body
    else
        exc_var = catch_[1]
        catch_block = catch_[2]
        if !is_identifier_like(exc_var)
            throw(LoweringError(exc_var, "Expected an identifier as exception variable"))
        end
        try_block = @ast ctx ex [:trycatchelse
            try_body
            [:scope_block(catch_) [:neutral_scope]
                if head(exc_var) != :placeholder
                    [:block
                        [:(=)(exc_var) exc_var [:call current_exception::value]]
                        catch_block
                    ]
                else
                    catch_block
                end
            ]
            if !isnothing(else_)
                [:scope_block(else_) [:neutral_scope] else_]
            end
        ]
    end

    if isnothing(finally_)
        try_block
    else
        @ast ctx ex [:tryfinally
            try_block
            [:scope_block(finally_) [:neutral_scope] finally_]
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
    @nospecialize declmeta
    declname = @stm ex begin
        [:identifier] -> ex
        [:placeholder] -> nothing
        ([:(::) [:identifier] t], when=type_decls) -> let x = ex[1]
            t2 = expand_forms_2(ctx, t)
            push!(stmts, newnode(ex, :decl, SyntaxList(x, t2)))
            make_lhs_decls(ctx, stmts, declkind, declmeta, x, type_decls)
        end
        ([:(::) [:placeholder] t], when=type_decls) -> let
            # TODO: Currently, this ignores the LHS in `_::T = val`.
            # We should probably do one of the following:
            # - Throw a LoweringError if that's not too breaking
            # - `convert(T, rhs)::T` and discard the result which is what
            #   `x::T = rhs` would do if x is never used again.
        end
        ([:(::) x t], when=!type_decls) ->
            make_lhs_decls(ctx, stmts, declkind, declmeta, x, type_decls)
        (_, when=head(ex) in (:call, :curly, :where)) ->
            make_lhs_decls(ctx, stmts, declkind, declmeta, ex[1], type_decls)
        [:tuple xs...] -> for x in xs
            make_lhs_decls(ctx, stmts, declkind, declmeta, x, type_decls)
        end
        [:parameters xs...] -> for x in xs
            make_lhs_decls(ctx, stmts, declkind, declmeta, x, type_decls)
        end
        [:... x] -> nothing # from recursion above
        [:ref _ _...] -> nothing # decl is ignored; syntax TODO
        [:. _ _] -> nothing # decl is ignored; syntax TODO
    end

    if !isnothing(declname)
        stmt = @ast ctx ex [declkind(;meta=declmeta) declname]
        push!(stmts, stmt)
    end
    return nothing
end

# Separate decls and assignments (which require re-expansion)
# local x, (y=2), z ==> local x; local z; y = 2
function expand_decls(ctx, ex)
    declkind = head(ex)
    @jl_assert declkind === :local || declkind === :global ex
    stmts = SyntaxList()
    val_nothing = !(numchildren(ex) == 1 && is_leaf(children(ex)[1]))
    for c in children(ex)
        simple = head(c) === :identifier || head(c) === :(::) || head(c) === :placeholder
        val_nothing &= simple
        if declkind === :global
            if head(c) === :(=)
                (lhs, relayered) = relayer_global_if_unhygienic(ctx, c[1]);
                !isempty(relayered) && (c = @ast ctx c [:(=) lhs c[2]])
            elseif simple
                (c, relayered) = relayer_global_if_unhygienic(ctx, c);
            end
            @isdefined(relayered) && for x in relayered
                push!(stmts, @ast ctx x [:relayered_global x])
            end
        end
        lhs = @stm c begin
            (_, when=simple) -> c
            [:(=) x _] -> x
            [:.= x _] -> x
            [:var"op=" x _ _] -> x
            [:var".op=" x _ _] -> x
            [:function x _...] -> x
        end
        # type decls are handled elsewhere unless simple
        make_lhs_decls(ctx, stmts, declkind, ex.meta, lhs, simple)
        simple || push!(stmts, expand_forms_2(ctx, c))
    end
    # flisp quirk: if not a plain `global x` or `local x`, value is readable
    val_nothing && push!(stmts, @ast ctx ex (::nothing))
    newnode(ex, :block, stmts)
end

# Iterate over the variable names assigned to from a "fancy assignment left hand
# side" such as nested tuple destructuring, curlies, and calls.
function foreach_lhs_name(f::Function, ex)
    k = head(ex)
    if k == :placeholder
        # Ignored
    elseif is_identifier_like(ex)
        f(ex)
    elseif (k === :(::) && numchildren(ex) === 2) || k in (:call, :curly, :where)
        foreach_lhs_name(f, ex[1])
    elseif k === :tuple || k === :parameters
        for c in children(ex)
            foreach_lhs_name(f, c)
        end
    end
    return nothing
end

function expand_const_decl(ctx, ex)
    if numchildren(ex) == 2
        # pre-desugared const
        return @ast ctx ex [:constdecl ex[1] ex[2]]
    end
    @stm ex[1] begin
        # const is ignored on function
        [:function _...] -> expand_forms_2(ctx, ex[1])
        [:global [:function _...]] -> expand_forms_2(ctx, ex[1])

        [:global x] -> let decls = SyntaxList()
            @jl_assert head(x) === :(=) ex
            (lhs, relayered) = relayer_global_if_unhygienic(ctx, x[1])
            make_lhs_decls(
                ctx, decls, :global, ex[1].meta, lhs, false)
            for x in relayered
                push!(decls, @ast ctx x [:relayered_global x])
            end
            x2 = @ast ctx x [:(=) lhs x[2]]
            @ast ctx ex [:block decls... expand_assignment(ctx, x2, true)]
        end
        [:(=) _ _] -> expand_assignment(ctx, ex[1], true)
        # Expr(:const, v) where v is a Symbol or a GlobalRef is an unfortunate
        # remnant from the days when const-ness was a flag that could be set on
        # any global.  It creates a binding with kind PARTITION_KIND_UNDEF_CONST.
        # TODO: deprecate and delete this "feature"
        [:identifier] -> @ast ctx ex [:constdecl ex[1]]
    end
end

#-------------------------------------------------------------------------------
# Expansion of function definitions

# (where (where x a b) c d) -> (x, [c d a b])
function flatten_wheres(ex)
    tvs = SyntaxList()
    while head(ex) === :where
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
        @jl_assert head(tv) === :_typevar tv
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
    out = SyntaxList()
    for (l, r) in zip(ls, rs)
        push!(out, @ast ctx r [:(=) l r])
    end
    out
end

function scope_nest(ctx, assigns, body)
    for a in Iterators.reverse(assigns)
        body = @ast ctx a [:let [:block a] body]
    end
    body
end

function pos_req_args(argl::SyntaxList)
    last = lastindex(argl)
    for i in eachindex(argl)
        if head(argl[i]) === :kw || head(argl[i]) === :... || head(argl[i]) === :parameters
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
        if head(argl[i]) === :kw
            opt_end = i
        end
    end
    @jl_assert let pos = head(argl[end]) === :parameters ? argl[1:end-1] : argl
        # no optargs, or optargs until the end (maybe excluding vararg, kws)
        opt_end in (-1,lastindex(pos),lastindex(pos)-1)
    end pos[end]
    argl[opt_start:opt_end]
end

# (_typevar name lb ub) -> (local (= name (call core TypeVar...)))
function assign_sparams(ctx, tvs)
    out = SyntaxList()
    for tv in tvs
        @jl_assert head(tv) === :_typevar tv
        push!(out, @ast ctx tv [:local tv[1]])
        push!(out, @ast ctx tv [:(=) tv[1] bounds_to_typevar(ctx, tv)])
    end
    out
end

function method_def_sparams(ctx, src, tvs)
    out = SyntaxList()
    for tv in tvs
        @jl_assert head(tv) === :_typevar tv
        push!(out, @ast ctx tv [:typevar tv[1] bounds_to_typevar(ctx, tv)])
    end
    @ast ctx src [:block out...]
end

# Hack: Normally just (block ex body), but needs special handling due to
# pre-quoted parts of generated function body, where we need to prepend
# desugarable AST to macro AST.  Fortunately there are only two places (meta
# nkw, and destructuring arg assignments) we do this, so handle them manually.
function prepend_function_body(ctx, body, ex)
    out = @stm body begin
        [:_generated_body [:syntaxquote gen] nongen] -> begin
            ex_est = @stm ex begin
                [:meta [:symbol] n] ->
                    @ast ctx ex [:meta "nkw"::identifier n]
                # destructured arg assignments
                [:block stmts... [:nothing]] ->
                    @ast ctx ex [:block stmts...]
                _ -> @jl_assert false (ex, "unexpected prepend_function_body")
            end
            @ast ctx body [:_generated_body
                [:syntaxquote [:block ex_est gen]] [:block ex nongen]]
        end
        _ -> @ast ctx body [:block ex body]
    end
    mm = getmeta(body, :method_metas, nothing)
    isnothing(mm) || setmeta!(out, :method_metas, mm)
    out
end

# Prepend method metadata and retain it through recursive wrapper generation.
function prepend_method_metas(ctx, src, body, method_metas)
    isnothing(method_metas) && return body
    out = @stm body begin
        [:block stmts...] ->
            @ast ctx src [:block [:meta method_metas...] stmts...]
        _ -> @ast ctx src [:block [:meta method_metas...] body]
    end
    setmeta(out, :method_metas, method_metas)
end

# Produce all `method` exprs for the given `argl`
# - one wrapper per optional positional arg
# - one containing the body
# - possibly one generated method
function method_def_expr(ctx, src, mtable, sparams, argl, body,
                         rett=@ast(ctx, src, "Any"::core))
    @jl_assert length(argl) > 0 src
    @jl_assert head(argl[end]) !== :parameters src argl[end]
    if length(pos_opt_args(argl)) > 0
        return optional_positional_defs(
            ctx, src, mtable, sparams, argl, body, rett)
    elseif head(body) === :_generated_body
        return generated_method_defs(
            ctx, src, mtable, sparams, argl, body, rett)
    end
    # Needs to be done per method, not per function (may create ssavalues)
    arg_types = mapsyntax(a->expand_forms_2(ctx, a[2]), argl)
    @ast ctx src [:method mtable
        [:call "svec"::core arg_types...]
        [:lambda(body)
            [:block mapindex(argl, 1)...]
            [:block mapindex(sparams, 1)...]
            expand_forms_2(ctx, body)
            is_core_Any(rett) ? nothing : expand_forms_2(ctx, rett)]]
end

function _untyped_arg(a)
    @jl_assert head(a) === :(::) || head(a) === :_typevar a
    aname = setmeta(a[1], :nospecialize, true)
    @ast _ a [:(::) aname "Any"::core]
end

function _expr_arg_syms(args)
    out = SyntaxList()
    for (i, a) in enumerate(args)
        @jl_assert head(a) === :(::) || head(a) === :_typevar a
        name = if head(a[1]) === :placeholder
            UNUSED
        elseif a[1].context.internal && i > 1
            # we lose context, so deduplicate names (ignoring #self# to be
            # safe).  HACK: destructured args must match the desugared rhs
            n = syntax_name(a[1])
            contains(n, "destructured") ? n : n*"#"*string(i)
        else
            syntax_name(a[1])
        end
        push!(out, @mknode(a[1]; head=:symbol, value=name))
    end
    out
end

# The Julia runtime associates the code generator with the non-generated method
# by adding (meta generated ...) to the non-generated body
# May need hygiene/provenance adjustments
function generated_method_defs(ctx, src, mtable, sparams, argl, body, rett)
    @jl_assert head(body) === :_generated_body && numchildren(body) == 2 body
    gen_name = let mangled = reserve_module_binding_i(
        ctx.layer.mod,
        string("#", head(mtable) === :nothing ? "_" : mtable, "@generator"))
        new_global_binding(ctx, src, mangled, ctx.layer.mod)
    end

    sc = src.context
    gen_mdef = let arg1_name = newsym(ctx, argl[1], "#self#"),
         gen_argl = SyntaxList(
             @ast(ctx, src, [:(::) arg1_name [:function_type gen_name]]),
             @ast(ctx, src, [:(::)
                 "__context__"::identifier(;context=sc)
                 SyntaxContext::value
             ]),
             mapsyntax(_untyped_arg, sparams)...,
             mapsyntax(_untyped_arg, argl)...)
        @jl_assert head(body[1]) === :syntaxquote body
        gen_body = est_to_dst(expand_syntaxquote(ctx, body[1][1]))

        method_def_expr(ctx, src, gen_name, SyntaxList(), gen_argl, gen_body,
                        @ast(ctx, src, "Any"::core))
    end
    nongen_mdef = let
        nongen_body = @ast ctx body[2] [:block [:meta "generated"::symbol
            [:new
                GeneratedFunctionStub::value # Use stub type from JuliaLowering
                SyntaxContext(ctx.layer.mod, sc.edition)::value
                gen_name
                # Truncate provenance to just the source file range, as this
                # will live permanently in the IR and we probably don't want
                # the full provenance tree and intermediate expressions
                # (TODO: More truncation. We certainly don't want to store the
                #  source file either.)
                # ::sourcelocation(lam)
                sourceref(src)::value
                [:call "svec"::core _expr_arg_syms(argl)...]
                [:call "svec"::core _expr_arg_syms(sparams)...]]]
            body[2]]
        method_def_expr(ctx, src, mtable, sparams, argl, nongen_body, rett)
    end

    @ast ctx src [:block
        [:global gen_name]
        [:function_decl gen_name]
        [:method_defs gen_name [:block] gen_mdef]
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
# use `(let (= arg default) body)` to handle references to `arg`. (flisp likely
# does this search to accomplish what we do with scope_nest)
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
    prop_metas = getmeta(body, :method_metas, nothing)
    methods = SyntaxList()
    for i in eachindex(opt)
        @jl_assert i == length(passed)-length(req)+1 src
        wrapper_body = if all((<)(i), deps[i+1:end])
            # fill-all-defaults case.  note that the final default may be a
            # splat, and doesn't have further args referring to it by name, so
            # we put it directly in the call (see #50563 for some notes)
            scope_nest(
                ctx,
                make_assigns(ctx, opt_names[i:end-1], opt_defaults[i:end-1]),
                @ast ctx src [:call mapindex(passed, 1)...
                    opt_names[i:end-1]... opt_defaults[end]])
        else
            @ast ctx src [:block
                [:call mapindex(passed, 1)... opt_defaults[i]]]
        end
        wrapper_body = prepend_method_metas(ctx, src, wrapper_body, prop_metas)
        # this function and method_def_expr need sp bounds because of this
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
    @ast ctx src [:block methods...]
end

function expand_kw_args(ctx, kws)
    kargl, restkw = @stm kws begin
        [:parameters xs... [:... va]] -> (xs, va)
        [:parameters xs...] -> (xs, nothing)
    end
    kw_decls = SyntaxList()
    kw_syms = SyntaxList()
    kw_defaults = SyntaxList()
    for raw_a in kargl
        a = expand_function_arg(ctx, raw_a, false)
        @stm a begin
            [:kw [:(::) n t] v] -> begin
                push!(kw_decls, a[1])
                push!(kw_defaults, v)
            end
            [:(::) n t] -> begin
                push!(kw_decls, a)
                push!(kw_defaults, @ast ctx a [:call "throw"::core
                    [:call "UndefKeywordError"::core a[1]=>:symbol]])
            end
        end
    end
    kw_names = mapindex(kw_decls, 1)
    kw_syms = mapsyntax(x->@mknode(x; head=:symbol), kw_names)
    restkw_list = isnothing(restkw) ? SyntaxList() :
        SyntaxList(@ast ctx restkw [:(::)
            restkw [:call "pairs"::top "NamedTuple"::core]])

    return (kw_decls, kw_names, kw_syms, kw_defaults, restkw_list)
end

# Assumes `expand_function_arg` has run.  Note that user-supplied
# "Vararg"::identifier is assumed to resolve to Core.Vararg
is_vararg_type_expr(st) = @stm st begin
    [:curly x _...] -> is_vararg_type_expr(x)
    [:where x _...] -> is_vararg_type_expr(x)
    _ -> (head(st) === :core || head(st) === :identifier) && syntax_name(st) == "Vararg"
end

function keywords_method_def_expr(ctx, src, mtable, sparams, argl, body, rett, overlay)
    kws = argl[end]
    pargl = argl[1:end-1]
    @jl_assert head(kws) === :parameters src
    pos_decls = mapsyntax(a->head(a)===:kw ? a[1] : a, pargl)
    # Mark the wrapper, not the body method, as the "self" arg to @__FUNCTION__.
    # TODO: We could probably unify this with is_kwcall_self with a generic
    # "closure not on first arg" flag if we're willing to pass the closure to
    # the body method through this arg instead of the first.
    pos_decls[1] = let p = pos_decls[1]
        @ast ctx p [:(::) setmeta(p[1], :thisfunction_original, true) p[2]]
    end

    # Positional names and splatted vararg so we can `(call f forward_pargl...)`
    forward_pargl = let l = mapindex(pos_decls, 1)
        pos_va = @stm argl[end-1] begin
            [:kw [:(::) _... t] _...] -> is_vararg_type_expr(t)
            [:(::) _... t] -> is_vararg_type_expr(t)
            _ -> false
        end
        pos_va && (l[end] = @ast ctx l[end] [:... l[end]])
        l
    end
    (kw_decls, kw_names, kw_syms, kw_defaults, restkw) = expand_kw_args(ctx, kws)
    ordered_defaults = any(val->contains_identifier(val, kw_names), kw_defaults)
    pos_sparams = used_typevars(pargl, sparams)
    prop_metas = getmeta(body, :method_metas, nothing)

    m1_name = let n = head(mtable) === :nothing ? "_" : syntax_name(mtable),
        mangled = string("#", n, "#kw_body#", module_unique_name(ctx.layer.mod))
        # probably not desirable, but fixes eval-into-closed-module
        m1_sc = escape_layer(mtable.context, true)
        @mknode(newsym(ctx, mtable, mangled);
                context=SyntaxContext(
                    m1_sc.layer, m1_sc.unexpanded, m1_sc.edition, true))
    end
    # (1) Body method.  This contains the actual function body, and requires
    # every possible default to be filled.  `rett` is only passed here since it
    # can reference any argument.
    mdefs1 = let arg1 = @ast ctx m1_name [:(::) m1_name [:function_type m1_name]]
        nkw = @ast ctx kws [:meta "nkw"::symbol numchildren(kws)::value]
        method_def_expr(
            ctx, src, m1_name, sparams,
            SyntaxList(arg1, kw_decls..., restkw..., pos_decls...),
            prepend_function_body(ctx, body, nkw), rett)
    end
    # (2) nokw methods (one per optarg).  Lowering wouldn't know to call
    # Core.kwcall given no kws in a call, so this method initializes kw defaults
    # and calls the body method.
    mdefs2 = let rkw = isempty(restkw) ? nothing :
            @ast ctx restkw[1] [:call
                "pairs"::top [:call "NamedTuple"::core]]
        body2 = if !ordered_defaults
            @ast ctx src [:call m1_name kw_defaults... rkw forward_pargl...]
        else
            scope_nest(ctx, make_assigns(ctx, kw_names, kw_defaults),
                @ast ctx src [:call m1_name kw_names... rkw forward_pargl...])
        end
        nokw_body = prepend_method_metas(
            ctx, src, @ast(ctx, src, [:block [:return body2]]), prop_metas)
        method_def_expr(
            ctx, src, mtable, pos_sparams, pargl, nokw_body)
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
                !any(val->contains_unquoted(e->head(e) == :(=), val), kw_defaults)
        kw_temps = use_ssa_kw_temps ?
            mapsyntax(x->ssavar(ctx, x, syntax_name(x)), kw_names) : kw_names
        tempslot = newsym(ctx, kws, "#kwtmp#")
        keyword_only_spnames = mapindex(unused_typevars(pargl, sparams), 1)

        kw_assigns = SyntaxList()
        for (tmp, sym, decl, default) in zip(kw_temps, kw_syms, kw_decls, kw_defaults)
            get_kw = @ast ctx decl [:call "getfield"::core arg2_name sym]
            if !is_core_Any(decl[2]) &&
                    !contains_identifier(decl[2], keyword_only_spnames)
                # static parameters don't have values yet, so don't assert the
                # declared kw type here if it contains any static params.  bad
                # types will trigger a MethodError when calling body instead.
                get_kw = @ast ctx decl [:block
                    getkw_tmp := get_kw
                    [:if [:call "isa"::core getkw_tmp decl[2]]
                        (::nothing)
                        [:call "throw"::core
                            [:new "TypeError"::core
                                "keyword argument"::symbol
                                sym decl[2] getkw_tmp]]]
                    getkw_tmp]
            end
            push!(kw_assigns, @ast ctx decl [:(=) tmp [:block
                [:if [:call "isdefined"::core arg2_name sym]
                    [:(=) tempslot get_kw]
                    [:(=) tempslot default]]
                tempslot]])
        end

        # bundle and forward excess if there's a restkw, else throw kwerr
        handle_excess = if !isempty(restkw)
            excess_kw = ssavar(ctx, arg2_name, "excess_kw")
            @ast ctx src [:(=)
                excess_kw
                [:call "pairs"::top
                   isempty(kw_names) ? arg2_name :
                   [:call "structdiff"::top arg2_name
                       [:curly "NamedTuple"::core [:tuple kw_syms...]]]]]
        else
            @ast ctx src [:if
                [:call "isempty"::top
                    [:call "diff_names"::top
                        [:call "keys"::top arg2_name]
                        [:tuple kw_syms...]]]
                (::nothing)
                [:call "kwerr"::top arg2_name forward_pargl...]]
        end
        final_call = @ast ctx kws [:call
            m1_name
            kw_temps...
            isempty(restkw) ? nothing : excess_kw
            forward_pargl...]
        kwcall_body = if use_ssa_kw_temps
            for n in kw_names
                # If not using slots for the keyword argument values, still
                # declare them for reflection purposes
                push!(kw_assigns, @ast ctx n [:local setmeta(n, :is_internal, true)])
            end
            @ast(ctx, src, [:block kw_assigns... handle_excess final_call])
        else
            scope_nest(ctx, kw_assigns,
                       @ast ctx src [:block handle_excess final_call])
        end
        kwcall_body = prepend_method_metas(ctx, src, kwcall_body, prop_metas)
        # Core.kwcall method has its own first argument.  Ensure closure
        # conversion knows not to put the closure there.
        let arg1_name = setmeta!(
            newsym(ctx, kws, "#kwcall_self#"; unused=length(pos_opt_args(pargl)) == 0),
            :is_kwcall_self, true)
            arg1 = @ast ctx src [:(::) arg1_name
                [:call "typeof"::core "kwcall"::core]
            ]
            arg2 = @ast ctx arg2_name [:(::) arg2_name "NamedTuple"::core]
            method_def_expr(
                ctx, src, mtable, pos_sparams,
                SyntaxList(arg1, arg2, pargl...), kwcall_body)
        end
    end
    @ast ctx src [:block
        [:function_decl m1_name]
        # hack: define closure type for next decl
        overlay || head(mtable) === :nothing ? nothing : [:no_method_defs m1_name]
        overlay || head(mtable) === :nothing ? nothing : [:function_decl mtable]
        [:method_defs m1_name method_def_sparams(ctx, src, sparams) mdefs1]
        [:method_defs mtable method_def_sparams(ctx, src, pos_sparams) mdefs2]
        [:method_defs mtable method_def_sparams(ctx, src, pos_sparams) mdefs3]
        mtable
    ]
end

# string mangling is necessary until generated functions know about scope layers
# (hack, see _expr_arg_syms).
_lower_destructuring_arg(stmts, ctx, i, ex) = @stm ex begin
    [:tuple _...] -> let arg2 = newsym(ctx, ex, "destructured#" * string(i))
        push!(stmts, @ast(ctx, ex, [:local(;meta=CompileHints(:is_destructured_arg, true))
            [:(=) ex arg2]]))
        arg2
    end
    [:(::) x t] -> @ast ctx ex [:(::) _lower_destructuring_arg(stmts, ctx, i, x) t]
    [:kw x t] -> @ast ctx ex [:kw _lower_destructuring_arg(stmts, ctx, i, x) t]
    [:... x]  -> @ast ctx ex [:... _lower_destructuring_arg(stmts, ctx, i, x)]
    _ -> ex
end

function lower_destructuring_args!(ctx, args)
    stmts = SyntaxList()
    for (i, a) in enumerate(args)
        args[i] = _lower_destructuring_arg(stmts, ctx, i, a)
    end
    # return `nothing` from the assignments (issue #26518)
    !isempty(stmts) && push!(stmts, @ast ctx stmts[1] (::nothing))
    return stmts
end

# `arg` is the first arg to a function's `call`.  return (1) whether this is an
# :overlay expression, (2) the method table expression, and (3) the typed arg
# expression `(:: #self# t)`
function expand_function_arg1(ctx, arg)
    if head(arg) === :overlay
        _, _, x = expand_function_arg1(ctx, arg[2])
        return true, expand_forms_2(ctx, arg[1]), x
    end
    aname = @stm arg begin
        [:(::) [:identifier] t] -> arg[1]
        _ -> newsym(ctx, arg, "#self#")
    end
    atype = @stm arg begin
        [:(::) t] -> t
        [:(::) _ t] -> t
        _ -> @ast ctx arg [:function_type arg]
    end
    # first arg to Expr(:method)
    mt = @stm arg begin
        [:identifier] -> arg
        [:value] -> arg # TODO delete with globalref support
        [:placeholder] -> arg
        _ -> @ast ctx arg (::nothing)
    end
    return false, mt, @ast ctx arg [:(::) aname atype]
end

fix_argname(ctx, arg, used) = @stm arg begin
    [:identifier] -> arg
    # Lowering should be able to use placeholder args as rvalues internally,
    # e.g. for kw method dispatch.
    ([:placeholder], when=used) -> newsym(ctx, arg, "#arg#")
    ([:placeholder], when=!used) -> arg
end

# flisp: fill-missing-argname, llist-types, llist-vars, dots->vararg
#
# Make an arg into `(:: x t)` or `(kw (:: x t) default)`.  If `used`, the caller
# specifies that even placeholder/underscore arguments might be read from
# internally.  Desugar type, but desugar default values later, since
# `default...` is unfortunately allowed, so do that in body desugaring.
expand_function_arg(ctx, arg, used) = @stm arg begin
    [:(::) x t] ->
        @ast ctx arg [:(::) fix_argname(ctx, x, used) t]
    [:(::) t] -> let aname = newsym(ctx, arg, "#arg#"; unused=true)
        @ast ctx arg [:(::) fix_argname(ctx, aname, used) t]
    end
    [:kw x v] ->
        @ast ctx arg [:kw expand_function_arg(ctx, x, used) v]
    # note: not correct for kwargs
    [:... x] -> let inner = expand_function_arg(ctx, x, used)
        @jl_assert head(inner) === :(::) inner arg
        @ast ctx x [:(::) inner[1] [:curly "Vararg"::core inner[2]]]
    end
    _ -> @ast ctx arg [:(::) fix_argname(ctx, arg, used) "Any"::core]
end

# Normalize and expand all positional arguments to (:: identifier t), then call
# a helper to create the method(s).
function expand_function_def(ctx, src, raw_args, wheres, body, rett)
    @jl_assert length(raw_args) >= 1 (body, "expected a self arg")
    let arg_stmts = lower_destructuring_args!(ctx, raw_args)
        if !isempty(arg_stmts)
            blk = @ast ctx src [:block arg_stmts...]
            body = prepend_function_body(ctx, body, blk)
        end
    end
    (overlay, mtable, a1) = expand_function_arg1(ctx, raw_args[1])
    argl = SyntaxList(a1)
    has_kws = head(raw_args[end]) === :parameters && numchildren(raw_args[end]) > 0
    let force_used = length(pos_opt_args(raw_args)) > 0 || has_kws
        for a in raw_args[2:end]
            if head(a) === :parameters
                numchildren(a) >= 1 && push!(argl, a)
            else
                push!(argl, expand_function_arg(ctx, a, force_used))
            end
        end
    end
    sparams = mapsyntax(typevar_bounds, wheres)
    if has_kws
        keywords_method_def_expr(
            ctx, src, mtable, sparams, argl, body, rett, overlay)
    elseif overlay
        mtmp = ssavar(ctx, mtable)
        @ast ctx src [:block
            [:method_defs (::nothing)
                method_def_sparams(ctx, src, sparams)
                [:block [:(=) mtmp method_def_expr(
                    ctx, src, mtable, sparams, argl, body, rett)]]]
            mtmp]
    else
        @ast ctx src [:block
            (head(mtable) === :nothing) ? nothing : [:function_decl mtable]
            [:method_defs mtable
                method_def_sparams(ctx, src, sparams)
                [:block method_def_expr(ctx, src, mtable, sparams, argl, body, rett)]]
            [:removable mtable]]
    end
end

expand_opaque_closure(ctx, ex) = @stm ex begin
    [:opaque_closure argt rt_lb rt_ub allow_partial lam] -> begin
        @jl_assert head(lam[1]) === :tuple ex
        check_no_parameters(ex, lam[1])
        raw_args = append!(SyntaxList(), children(lam[1]))
        arg_stmts = lower_destructuring_args!(ctx, raw_args)

        arg_names = SyntaxList(newsym(ctx, lam[1], "#self#"))
        inner_arg_types = SyntaxList()
        for a in raw_args
            if head(argt) !== :nothing && head(a) === :(::)
                throw(LoweringError(a, "opaque closure argument type may not be specified both in the method signature and separately"))
            end
            a2 = expand_function_arg(ctx, a, false)
            if head(a) === :kw || head(a) === :parameters
                throw(LoweringError(
                    a, "opaque closure cannot have optional or keyword arguments"))
            end
            @jl_assert head(a2) === :(::) a2
            push!(inner_arg_types, a2[2])
            push!(arg_names, a2[1])
        end

        out_argt = head(argt) !== :nothing ? argt :
            @ast ctx lam[1] [:curly "Tuple"::core inner_arg_types...]
        out_rt_lb = head(rt_lb) !== :nothing ? rt_lb :
            @ast ctx lam[1] [:curly "Union"::core]
        out_rt_ub = head(rt_ub) !== :nothing ? rt_ub :
            @ast ctx lam[1] "Any"::core
        nargs = (length(arg_names)-1) # ignoring #self#
        is_va = !isempty(raw_args) && head(raw_args[end]) === :...
        body = @ast ctx lam[2] [:block arg_stmts... lam[2]]

    @ast ctx ex [:_opaque_closure
        ssavar(ctx, ex, "opaque_closure_id") # only a placeholder. Must be :local
        expand_forms_2(ctx, out_argt)
        expand_forms_2(ctx, out_rt_lb)
        expand_forms_2(ctx, out_rt_ub)
        allow_partial
        nargs::value
        is_va::value
        ::sourcelocation(lam)
        [:lambda(lam)
            [:block arg_names...]
            [:block]
            expand_forms_2(ctx, body)]]
    end
end

#-------------------------------------------------------------------------------
# Expand macro definitions

# Name is hygienic-global in compat mode, hygienic otherwise
function _make_macro_name(ctx, ex)
    k = head(ex)
    if k == :identifier || k == :symbol
        if k === :identifier && is_flisp_compat(ex)
            @mknode(ex; head=k, value="@$(syntax_name(ex))",
                    mod=syntax_module(ex))
        else
            @mknode(ex; head=k, value="@$(syntax_name(ex))")
        end
    elseif k == :placeholder
        @mknode(ex; head=:identifier, value="@$(syntax_name(ex))")
    elseif is_valid_modref(ex)
        @jl_assert numchildren(ex) == 2 ex
        @ast ctx ex [:. ex[1] _make_macro_name(ctx, ex[2])]
    else
        @jl_assert false ex
    end
end

function expand_macro_def(ctx, ex)
    if numchildren(ex) == 1
        # macro with zero methods
        # `macro m end`
        return @ast ctx ex [:function _make_macro_name(ctx, ex[1])]
    end
    (sig, name, args) = @stm ex begin
        [:macro [:call n a...] _] -> (ex[1], n, remove_empty_parameters(a))
        _ -> @jl_assert false ex
    end

    sc_ref = (head(name) == :. ? name[1] : name)
    if is_flisp_compat(ex)
        @ast ctx ex [:function
            [:call(sig)
                _make_macro_name(ctx, name)
                [:(::)
                    adopt_scope(sc_ref, @ast(ctx, sig, "__source__"::identifier))
                    "LineNumberNode"::core
                ]
                [:(::)
                    adopt_scope(sc_ref, @ast(ctx, sig, "__module__"::identifier))
                    "Module"::core
                ]
                mapsyntax(e->apply_arg_meta(e, :nospecialize), args)...
            ]
            ex[2]
        ]
    else
        @ast ctx ex [:function
            [:call(sig)
                _make_macro_name(ctx, name)
                [:(::)
                    adopt_scope(sc_ref, @ast(ctx, sig, "__context__"::identifier))
                    MacroContext::value
                ]
                # We don't mark these @nospecialize because all arguments to
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
function typevar_bounds(ex)
    any = @ast _ ex "Any"::core
    (name, lb, ub) = bounds = @stm ex begin
        [:identifier] -> (ex, any, any)
        [:placeholder] -> (ex, any, any)
        ([:comparison lb op x _ ub], when=syntax_name(op)==="<:") -> (x, lb, ub)
        ([:comparison ub op x _ lb], when=syntax_name(op)===">:") -> (x, lb, ub)
        [:<: x ub] -> (x, any, ub)
        [:>: x lb] -> (x, lb, any)
    end
    @ast _ ex [:_typevar name lb ub]
end

function bounds_to_typevar(ctx, ex)
    @jl_assert head(ex) === :_typevar ex
    _bounds_to_typevar(ctx, ex, ex[1], ex[2], ex[3])
end

# Generate call to `TypeVar(name[, lb, ub])`.  Note the resulting expression may
# contain SSA assignments, so can't be copied.
function _bounds_to_typevar(ctx, srcref, name, lb, ub)
    @ast ctx srcref [:call
        "TypeVar"::core
        name=>:symbol
        if !is_core_Any(lb)
            expand_forms_2(ctx, lb)
        end
        if !is_core_Any(lb) || !is_core_Any(ub)
            expand_forms_2(ctx, ub)
        end
    ]
end

# Analyze type signatures such as `A{C} <: B where C`
#
# Return (name, typevar_names, typevar_stmts, supertype) where
# - `name` is the name of the type
# - `supertype` is the super type of the type
function analyze_type_sig(ctx, ex)
    k = head(ex)
    if k == :identifier
        name = ex
        type_params = ()
        supertype = @ast ctx ex "Any"::core
    elseif k == :curly && numchildren(ex) >= 1 && head(ex[1]) == :identifier
        # name{type_params}
        name = ex[1]
        type_params = ex[2:end]
        supertype = @ast ctx ex "Any"::core
    elseif k == :<: && numchildren(ex) == 2
        if head(ex[1]) == :identifier
            name = ex[1]
            type_params = ()
            supertype = ex[2]
        elseif head(ex[1]) == :curly && numchildren(ex[1]) >= 1 && head(ex[1][1]) == :identifier
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
function expand_typevars(ctx, type_params)
    typevar_names = SyntaxList()
    typevar_stmts = SyntaxList()
    for param in type_params
        bounds = typevar_bounds(param)
        n = bounds[1]
        push!(typevar_names, n)
        push!(typevar_stmts, @ast ctx param [:block
            [:local n]
            [:(=) n bounds_to_typevar(ctx, bounds)]
        ])
    end
    return (typevar_names, typevar_stmts)
end

function expand_abstract_or_primitive_type(ctx, ex)
    is_abstract = head(ex) == :abstract
    if is_abstract
        @jl_assert numchildren(ex) == 1 ex
    else
        @jl_assert head(ex) == :primitive ex
        @jl_assert numchildren(ex) == 2 ex
    end
    nbits = is_abstract ? nothing : ex[2]
    name, type_params, supertype = analyze_type_sig(ctx, ex[1])
    name, _ = relayer_global_if_unhygienic(ctx, name)
    typevar_names, typevar_stmts = expand_typevars(ctx, type_params)
    newtype_var = ssavar(ctx, ex, "new_type")
    @ast ctx ex [:block
        [:scope_block [:hard_scope]
            [:block
                [:local name]
                [:always_defined name]
                typevar_stmts...
                [:(=)
                    newtype_var
                    [:call
                        (is_abstract ? "_abstracttype" : "_primitivetype")::core
                        syntax_module(name)::value
                        name=>:symbol
                        [:call "svec"::core typevar_names...]
                        if !is_abstract
                            nbits
                        end
                    ]
                ]
                [:(=) name newtype_var]
                [:call "_setsuper!"::core newtype_var supertype]
                [:call "_typebody!"::core name]
            ]
        ]
        [:assert "toplevel_only"::symbol [:syntaxinert ex] ]
        [:global name]
        [:if
            [:&&
                [:call
                   "isdefinedglobal"::core
                   syntax_module(name)::value
                   name=>:symbol
                   false::value]
                [:call "_equiv_typedef"::core name newtype_var]
            ]
            nothing_(ctx, ex)
            [:constdecl name newtype_var]
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
        k = head(x)
        if k == :identifier || k == :placeholder
            return (name=x, type=type, atomic=atomic, _const=_const, docs=docs)
        elseif k == :(::) && numchildren(x) == 2
            isnothing(type) || throw(LoweringError(x0, "multiple types in struct field"))
            type = x[2]
            x = x[1]
        elseif k == :atomic
            atomic = true
            x = x[1]
        elseif k == :const
            _const = true
            x = x[1]
        elseif k == :doc
            docs = x[1]
            x = x[2]
        else
            return nothing
        end
    end
end

function _collect_struct_fields(ctx, field_names, field_types, field_attrs, field_docs, inner_defs, exs)
    for e in exs
        if head(e) == :block
            _collect_struct_fields(ctx, field_names, field_types, field_attrs, field_docs,
                                   inner_defs, children(e))
        else
            m = _match_struct_field(e)
            if !isnothing(m)
                # Struct field
                for prev in field_names
                    if syntax_name(prev) == syntax_name(m.name)
                        throw(LoweringError(m.name, "duplicate field name"))
                    end
                end
                push!(field_names, m.name)
                n = length(field_names)
                push!(field_types, isnothing(m.type) ? @ast(ctx, e, "Any"::core) : m.type)
                if m.atomic
                    push!(field_attrs, @ast ctx e n::value)
                    push!(field_attrs, @ast ctx e "atomic"::symbol)
                end
                if m._const
                    push!(field_attrs, @ast ctx e n::value)
                    push!(field_attrs, @ast ctx e "const"::symbol)
                end
                if !isnothing(m.docs)
                    push!(field_docs, @ast ctx e n::value)
                    push!(field_docs, @ast ctx e m.docs)
                end
            elseif head(e) == :string || is_effect_free(e)
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
    # kt = head(field_type)
    # TODO: Allow kt == :identifier && kt in static_params to avoid fieldtype call?
    @ast ctx field_type [:block
        tmp_type := [:call
            "fieldtype"::core
            full_struct_type
            field_index::value
        ]
        convert_for_type_decl(ctx, field_type, val, tmp_type, false)
    ]
end

function _is_new_call(ex)
    head(ex) == :call &&
        ((head(ex[1]) == :identifier && syntax_name(ex[1]) == "new") ||
         (head(ex[1]) == :curly && head(ex[1][1]) == :identifier && syntax_name(ex[1][1]) == "new"))
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
        [:(::) x rett] -> let
            call2, ctor_self = rewrite_ctor_sig(
                ctx, x, tname, global_tname, struct_typevars, SyntaxList())
            sig2 = @ast(ctx, sig, [:(::) call2 rett])
        end
        # recognize `(_::(Type{X{T}} where T))(...)` as an inner-style
        # constructor for X (rewrite it to `X{T}(...) where T`)
        ([:call [:(::) _ [:where _...]] args...], when=begin
             t, inner_wheres = flatten_wheres(ex[1][2])
             isempty(wheres) && head(t) === :curly && t[1].value === "Type"
         end) -> let
             append!(wheres, inner_wheres)
             ex2 = @ast ctx ex [:call t[2] args...]
             return rewrite_ctor_sig(
                 ctx, ex2, tname, global_tname, struct_typevars, wheres)
        end
        [:call [:curly name curlyargs...] args...] -> let
            # if curlyargs is the wrong length, fall back to the ones in `new`
            # TODO: this isn't quite the same as flisp, which passes curlyargs
            # to new-call and checks there.  We print the wrong message with
            # `struct X{T}; X{T,U}() = new(); end`.
            if (head(name) !== :(::) && is_same_identifier_like(name, tname) &&
                length(curlyargs) == length(struct_typevars))
                @jl_assert is_leaf(name) (sig, "didn't find ctor name in sig")
                ctor_self = newsym(ctx, sig, "#ctor-self#")
                sig2 = @ast ctx sig [:call
                    [:(::) ctor_self
                        [:curly "Type"::core
                         [:curly global_tname curlyargs...]]]
                    args...]
            end
        end
        [:call name args...] -> let
            if head(name) !== :(::) && is_same_identifier_like(name, tname)
                @jl_assert is_leaf(name) (sig, "didn't find ctor name in sig")
                ctor_self = newsym(ctx, sig, "#ctor-self#")
                sig2 = @ast ctx sig [:call
                    [:(::) ctor_self [:curly "Type"::core global_tname]]
                    args...]
            end
        end
        # anonymous function
        [:tuple _...] -> (sig, nothing)
    end
    sig_out = isempty(wheres) ? sig2 : @ast ctx sig [:where sig2 wheres...]
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
    is_leaf(ex) && return ex
    @stm ex begin
        [:inert _] -> ex
        [:function call body] -> let (sig, wheres) = flatten_wheres(call)
            call2, ctor_self =
                rewrite_ctor_sig(ctx, sig, tname, global_tname, struct_typevars, wheres)
            body2 = _rewrite_ctor_new_calls(
                ctx, body, global_tname,
                mapsyntax(typevar_bounds, wheres),
                struct_typevars, ctor_self, field_types)
            @ast ctx ex [:function call2 body2]
        end
        x -> mapchildren(e->rewrite_ctor(
            ctx, e, tname, global_tname, struct_typevars, field_types), ex)
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
function _rewrite_ctor_new_calls(ctx, ex0, global_struct_name, ctor_sparams,
                                       struct_typevars, ctor_self, field_types)
    if is_leaf(ex0)
        return ex0
    elseif !_is_new_call(ex0)
        return mapchildren(
            e->_rewrite_ctor_new_calls(ctx, e, global_struct_name, ctor_sparams,
                                       struct_typevars, ctor_self, field_types),
            ex0
        )
    end
    # Rewrite a call to new()
    e0args = children(ex0)
    kw_arg_i = findfirst(e->(k = head(e); k == :kw), e0args)
    ex = if !isnothing(kw_arg_i)
        throw(LoweringError(e0args[kw_arg_i], "`new` does not accept keyword arguments"))
    elseif head(e0args[end]) === :parameters # flisp oversight
        if is_flisp_compat(ex0)
            @mknode(ex0; children=e0args[1:end-1])
        else
            throw(LoweringError(e0args[end], "`new` does not accept keyword arguments"))
        end
    else
        ex0
    end
    full_struct_type = if head(ex[1]) == :curly
        # new{A,B}(...)
        new_type_params = ex[1][2:end]
        n_type_splat = sum(head(t) == :... for t in new_type_params; init=0)
        n_type_nonsplat = length(new_type_params) - n_type_splat
        if n_type_splat == 0 && n_type_nonsplat < length(struct_typevars)
            throw(LoweringError(ex[1], "too few type parameters specified in `new{...}`"))
        elseif n_type_nonsplat > length(struct_typevars)
            throw(LoweringError(ex[1], "too many type parameters specified in `new{...}`"))
        end
        isempty(new_type_params) ? global_struct_name :
            @ast ctx ex[1] [:curly global_struct_name new_type_params...]
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
    n_splat = sum(head(t) == :... for t in new_args; init=0)
    n_nonsplat = length(new_args) - n_splat
    n_fields = length(field_types)
    function throw_n_fields_error(desc)
        @ast ctx ex [:call
            "throw"::core
            [:call
                "ArgumentError"::top
                "too $desc arguments in `new` (expected $n_fields)"::value
            ]
        ]
    end
    if n_nonsplat > n_fields
        return throw_n_fields_error("many")
    else
        # "Too few" args are allowed in partially initialized structs
    end
    if n_splat == 0
        @ast ctx ex [:block
            struct_type := full_struct_type
            [:new
                struct_type
                [_new_call_convert_arg(ctx, struct_type, type, i, name)
                 for (i, (name,type)) in enumerate(zip(ex[2:end], field_types))]...
            ]
        ]
    else
        fields_all_Any = all(is_core_Any, field_types)
        if fields_all_Any
            @ast ctx ex [:block
                struct_type := full_struct_type
                [:splatnew
                    struct_type
                    # Note: `jl_new_structt` ensures length of this tuple is
                    # exactly the number of fields.
                    [:call "tuple"::core ex[2:end]...]
                ]
            ]
        else
            # `new` with splatted args which are symbolically not `Core.Any`
            # (might be `Any` at runtime but we can't know that here.)
            @ast ctx ex [:block
                args := [:call "tuple"::core ex[2:end]...]
                n_args := [:call "nfields"::core args]
                [:if
                    [:call "ult_int"::top n_args n_fields::value]
                    throw_n_fields_error("few")
                ]
                [:if
                    [:call "ult_int"::top n_fields::value n_args]
                    throw_n_fields_error("many")
                ]
                struct_type := full_struct_type
                [:new
                    struct_type
                    [_new_call_convert_arg(ctx, struct_type, type, i,
                         [:call "getfield"::core args i::value])
                     for (i, type) in enumerate(field_types)]...
                ]
            ]
        end
    end
end

function _constructor_min_initialized(ex::SyntaxTree)
    if _is_new_call(ex)
        if any(head(e) == :... for e in ex[2:end])
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
function _insert_fieldtype_struct_shim(ctx, sname, ex)
    if head(ex) == :. &&
        numchildren(ex) == 2 &&
        head(ex[2]) == :symbol &&
        syntax_name(ex[2]) == syntax_name(sname)
        @ast ctx ex [:call "struct_name_shim"::core ex[1] ex[2] syntax_module(ex)::value sname]
    elseif numchildren(ex) > 0
        mapchildren(e->_insert_fieldtype_struct_shim(ctx, sname, e), ex)
    else
        ex
    end
end

function insert_struct_shim(ctx, fieldtypes, name)
    map(ex->_insert_fieldtype_struct_shim(ctx, name, ex), fieldtypes)
end

# Used to handle TypeVar/TypeApp references during type resolution before real
# DataTypes exist.  flisp: "Skips method bodies since constructors should use
# plain apply_type for correct effects inference."
function _replace_type_constructors(ctx, ex)
    if is_leaf(ex)
        return ex
    end
    k = head(ex)
    if k == :call && numchildren(ex) >= 1 && head(ex[1]) == :core && syntax_name(ex[1]) == "apply_type"
        new_head = @ast ctx ex[1] "apply_type_or_typeapp"::core
        new_children = SyntaxList()
        push!(new_children, new_head)
        for i in 2:numchildren(ex)
            push!(new_children, _replace_type_constructors(ctx, ex[i]))
        end
        return @ast ctx ex [:call new_children...]
    elseif k === :method || is_quoted(ex)
        ex
    else
        return mapchildren(e->_replace_type_constructors(ctx, e), ex)
    end
end

struct TypeGroupEntry
    sdef            # struct definition syntax node
    docs            # nothing or :doc node
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
    body = flatten_blocks(ex[1])
    if head(body) != :block
        throw(LoweringError(body, "expected block for `typegroup` body"))
    end

    # Collect and analyze struct definitions from block children.
    # A child can be a bare :struct or a :doc wrapping a :struct.
    entries = TypeGroupEntry[]
    struct_names = SyntaxList()   # local name bindings (splatted into AST)
    global_names = SyntaxList()   # global name bindings (splatted into AST)
    info_vars = SyntaxList()      # SSA vars for struct info svecs (splatted into AST)
    struct_mod_prev = nothing

    for child in children(body)
        if head(child) == :struct
            sdef = child
            docs = nothing
        elseif head(child) == :doc
            @jl_assert numchildren(child) == 2 child
            sdef = child[2]
            if head(sdef) != :struct
                throw(LoweringError(sdef, "`typegroup` only supports `struct` definitions"))
            end
            docs = child
        else
            throw(LoweringError(child, "`typegroup` only supports `struct` definitions"))
        end

        @jl_assert numchildren(sdef) == 3 sdef
        is_mutable = sdef[1].value::Bool
        type_sig = sdef[2]
        type_body = sdef[3]
        if head(type_body) != :block
            throw(LoweringError(type_body, "expected block for `struct` fields"))
        end
        struct_name, type_params, supertype = analyze_type_sig(ctx, type_sig)
        typevar_names, typevar_stmts = expand_typevars(ctx, type_params)
        field_names = SyntaxList()
        field_types = SyntaxList()
        field_attrs = SyntaxList()
        field_docs = SyntaxList()
        inner_defs = SyntaxList()
        _collect_struct_fields(ctx, field_names, field_types, field_attrs, field_docs,
                               inner_defs, children(type_body))

        min_initialized = minimum((_constructor_min_initialized(e) for e in inner_defs),
                                  init=length(field_names))

        push!(entries, TypeGroupEntry(sdef, docs, typevar_names, typevar_stmts,
                                      field_names, field_types, field_attrs,
                                      supertype, is_mutable, min_initialized,
                                      inner_defs, field_docs))
        push!(struct_names, struct_name)
        global_struct_name, _ = relayer_global_if_unhygienic(ctx, struct_name)
        struct_mod = syntax_module(global_struct_name)
        isnothing(struct_mod_prev) || struct_mod == struct_mod_prev || throw(
            LoweringError(ex, "typegroup of types from multiple modules"))
        struct_mod_prev = struct_mod
        struct_globalref = @mknode(global_struct_name; mod=struct_mod)
        push!(global_names, struct_globalref)
        push!(info_vars, ssavar(ctx, sdef, "struct_info"))
    end
    n = length(entries)
    if n == 0
        return nothing_(ctx, ex)
    end
    typegroup_mod = syntax_module(global_names[1])

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

    stmts = SyntaxList()

    # 2a. Declare all names as locals
    for name in struct_names
        push!(stmts, @ast ctx name [:local name])
    end

    # 2b. Create TypeVar placeholders for each name
    for name in struct_names
        push!(stmts, @ast ctx name [:(=) name [:call "TypeVar"::core name=>:symbol]])
    end

    # 2c. For each struct: create scope_block with TypeVar params and collect info into svec
    for i in 1:n
        e = entries[i]
        typevar_names = e.typevar_names
        typevar_stmts = e.typevar_stmts
        info_var = info_vars[i]

        inner_stmts = SyntaxList()
        for tv_name in typevar_names
            push!(inner_stmts, @ast ctx e.sdef [:local tv_name])
        end
        append!(inner_stmts, typevar_stmts)
        push!(inner_stmts, @ast ctx e.sdef [:assert "toplevel_only"::symbol [:syntaxinert e.sdef]])
        push!(inner_stmts, @ast ctx e.sdef [:(=)
            info_var
            [:call "svec"::core
                [:call "svec"::core typevar_names...]
                [:call "svec"::core [fname=>:symbol for fname in e.field_names]...]
                [:call "svec"::core e.field_attrs...]
                e.is_mutable::value
                e.min_initialized::value
                e.supertype
                [:call "svec"::core e.field_types...]
            ]
        ])

        push!(stmts, @ast ctx e.sdef [:scope_block [:hard_scope]
            [:block inner_stmts...]
        ])
    end

    # 2d. Look up old types for redefinition equivalence check
    old_type_vars = SyntaxList()
    for i in 1:n
        old_var = ssavar(ctx, ex, "old_type")
        push!(stmts, @ast ctx ex [:(=)
            old_var
            [:if
                [:call "isdefinedglobal"::core
                    typegroup_mod::value
                    struct_names[i]=>:symbol
                    false::value]
                global_names[i]
                nothing_(ctx, ex)
            ]
        ])
        push!(old_type_vars, old_var)
    end
    # 2e. Call resolve_typegroup
    resolve_tmp = ssavar(ctx, ex)
    push!(stmts, @ast ctx ex [:(=) resolve_tmp
        [:call "resolve_typegroup"::core
            typegroup_mod::value
            [:call "svec"::core struct_names...]
            [:call "svec"::core info_vars...]
            [:call "svec"::core old_type_vars...]
        ]
    ])

    # 2f. Bind to global constants
    for i in 1:n
        prov = entries[i].sdef
        push!(stmts, @ast ctx prov [:(=) struct_names[i]
                [:call "getfield"::core resolve_tmp i::value]])
        push!(stmts, @ast ctx prov [:constdecl global_names[i] struct_names[i]])
    end

    # 2f. latestworld
    push!(stmts, @ast ctx ex (::latestworld))
    push!(stmts, nothing_(ctx, ex))

    # 2g. Constructor definitions — placed outside the scope_block so that
    # type names in constructor bodies resolve to globals, not captured locals.
    fdef_stmts = SyntaxList()
    for i in 1:n
        e = entries[i]
        if isempty(e.inner_defs)
            push!(fdef_stmts, @ast ctx e.sdef [:call
                "_defaultctors"::top
                global_names[i]
                ::sourcelocation(e.sdef)
            ])
        else
            inner_defs = e.inner_defs
            for (def_i, def) in enumerate(inner_defs)
                inner_defs[def_i] =
                    rewrite_ctor(ctx, def, struct_names[i], global_names[i],
                             e.typevar_names, e.field_types)
            end
            push!(fdef_stmts, @ast ctx e.sdef [:scope_block [:hard_scope]
                [:block inner_defs...]
            ])
        end
    end

    push!(fdef_stmts, @ast ctx ex (::latestworld))

    # 2h. Documentation — after constructors and latestworld so types are fully defined
    for i in 1:n
        e = entries[i]
        if !isnothing(e.docs) || !isempty(e.field_docs)
            push!(fdef_stmts, @ast ctx e.sdef [:call(isnothing(e.docs) ? e.sdef : e.docs)
                bind_docs!::value
                struct_names[i]
                isnothing(e.docs) ? nothing_(ctx, e.sdef) : e.docs[1]
                ::sourcelocation(e.sdef)
                [:kw
                    "field_docs"::identifier
                    [:call "svec"::core e.field_docs...]
                ]
            ])
        end
    end

    push!(fdef_stmts, nothing_(ctx, ex))

    result = @ast ctx ex [:block
        [:assert "toplevel_only"::symbol [:syntaxinert ex]]
        mapsyntax(x->@ast(ctx, x, [:global x]), struct_names)...
        [:scope_block [:hard_scope] [:block stmts...]]
        fdef_stmts...
    ]

    # Expand, then replace apply_type with apply_type_or_typeapp
    expanded = expand_forms_2(ctx, result)
    return _replace_type_constructors(ctx, expanded)
end

function expand_struct_def(ctx, ex, docs)
    @jl_assert numchildren(ex) == 3 ex
    is_mutable = ex[1].value::Bool
    type_sig = ex[2]
    type_body = flatten_blocks(ex[3])
    if head(type_body) != :block
        throw(LoweringError(type_body, "expected block for `struct` fields"))
    end
    struct_name, type_params, supertype = analyze_type_sig(ctx, type_sig)
    typevar_names, typevar_stmts = expand_typevars(ctx, type_params)
    field_names = SyntaxList()
    field_types = SyntaxList()
    field_attrs = SyntaxList()
    field_docs = SyntaxList()
    inner_defs = SyntaxList()
    _collect_struct_fields(ctx, field_names, field_types, field_attrs, field_docs,
                           inner_defs, children(type_body))
    min_initialized = minimum((_constructor_min_initialized(e) for e in inner_defs),
                              init=length(field_names))
    global_struct_name, _ = relayer_global_if_unhygienic(ctx, struct_name)
    struct_mod = syntax_module(global_struct_name)
    struct_globalref = @mknode(global_struct_name; mod=struct_mod)

    # Use the typegroup mechanism for ordinary structs to ensure safety
    # when accessing incomplete types during definition (issue #60919).
    # The struct name is a TypeVar placeholder during field type evaluation,
    # preventing segfaults from accessing incomplete types.
    info_var = ssavar(ctx, ex, "struct_info")

    stmts = SyntaxList()

    # Declare struct name as local and create TypeVar placeholder
    push!(stmts, @ast ctx struct_name [:local struct_name])
    push!(stmts, @ast ctx struct_name [:(=) struct_name [:call "TypeVar"::core struct_name=>:symbol]])

    # Inner scope_block for type parameters + info collection
    inner_stmts = SyntaxList()
    for tv_name in typevar_names
        push!(inner_stmts, @ast ctx ex [:local tv_name])
    end
    append!(inner_stmts, typevar_stmts)
    push!(inner_stmts, @ast ctx ex [:assert "toplevel_only"::symbol [:syntaxinert ex]])
    push!(inner_stmts, @ast ctx ex [:(=)
        info_var
        [:call "svec"::core
            [:call(type_sig) "svec"::core typevar_names...]
            [:call(type_body) "svec"::core [n=>:symbol for n in field_names]...]
            [:call(type_body) "svec"::core field_attrs...]
            is_mutable::value
            min_initialized::value
            supertype
            [:call "svec"::core insert_struct_shim(ctx, field_types, struct_name)...]
        ]
    ])
    push!(stmts, @ast ctx ex [:scope_block [:hard_scope]
        [:block inner_stmts...]
    ])

    # Look up old type for redefinition equivalence check
    old_type_var = ssavar(ctx, ex, "old_type")
    push!(stmts, @ast ctx ex [:(=)
        old_type_var
        [:if
            [:call "isdefinedglobal"::core
                struct_mod::value
                struct_name=>:symbol
                false::value]
            struct_globalref
            nothing_(ctx, ex)
        ]
    ])

    # Call resolve_typegroup and extract the single result with getfield
    push!(stmts, @ast ctx ex [:(=)
        struct_name
        [:call "getfield"::core
            [:call "resolve_typegroup"::core
                struct_mod::value
                [:call "svec"::core struct_name]
                [:call "svec"::core info_var]
                [:call "svec"::core old_type_var]
            ]
            1::value
        ]
    ])

    # Bind to global constant
    push!(stmts, @ast ctx ex [:constdecl struct_globalref struct_name])

    # latestworld + nothing
    push!(stmts, @ast ctx ex (::latestworld))
    push!(stmts, nothing_(ctx, ex))

    # Constructor definitions — placed outside the scope_block so that
    # type names in constructor bodies resolve to globals, not captured locals.
    fdef_stmts = SyntaxList()
    if isempty(inner_defs)
        push!(fdef_stmts, @ast ctx ex [:call
            "_defaultctors"::top
            struct_globalref
            ::sourcelocation(ex)
        ])
    else
        # For all functions within `struct`, rewrite `new` calls and
        # constructor-like signatures
        for (def_i, def) in enumerate(inner_defs)
            inner_defs[def_i] =
                rewrite_ctor(ctx, def, struct_name, struct_globalref,
                             typevar_names, field_types)
        end
        push!(fdef_stmts, @ast ctx ex [:scope_block [:hard_scope]
            [:block inner_defs...]
        ])
    end
    push!(fdef_stmts, @ast ctx ex (::latestworld))

    # Documentation — after constructors and latestworld so types are fully defined
    if !isnothing(docs) || !isempty(field_docs)
        push!(fdef_stmts, @ast ctx ex [:call(isnothing(docs) ? ex : docs)
            bind_docs!::value
            struct_name
            isnothing(docs) ? nothing_(ctx, ex) : docs[1]
            ::sourcelocation(ex)
            [:kw
                "field_docs"::identifier
                [:call "svec"::core field_docs...]
            ]
        ])
    end
    push!(fdef_stmts, nothing_(ctx, ex))

    result = @ast ctx ex [:block
        [:global struct_name]
        [:assert "toplevel_only"::symbol [:syntaxinert ex]]
        [:scope_block [:hard_scope] stmts...]
        fdef_stmts...
    ]

    # Expand, then replace apply_type with apply_type_or_typeapp
    expanded = expand_forms_2(ctx, result)
    return _replace_type_constructors(ctx, expanded)
end

#-------------------------------------------------------------------------------
# Expand `where` syntax

function expand_where(ctx, srcref, lhs, rhs)
    bounds = typevar_bounds(rhs)
    v = bounds[1]
    @ast ctx srcref [:let
        [:block [:(=) v bounds_to_typevar(ctx, bounds)]]
        [:call "UnionAll"::core v lhs]
    ]
end

function expand_wheres(ctx, ex)
    body = ex[1]
    @stm ex begin
        [:where _ [:_typevars [:block names...] [:block stmts...]]] ->
            for n in Iterators.reverse(names)
                body = @ast ctx ex [:call "UnionAll"::core n body]
            end
        [:where _ tvs...] ->
            for v in Iterators.reverse(tvs)
                body = expand_where(ctx, ex, body, v)
            end
    end
    body
end

# Match implicit where parameters for `Foo{<:Bar}` ==> `Foo{T} where T<:Bar`
function expand_curly(ctx, ex)
    @jl_assert head(ex) == :curly ex
    check_no_parameters(ex, "unexpected semicolon in type parameter list")
    check_no_assignment(children(ex), "misplaced assignment in type parameter list")

    typevar_stmts = SyntaxList()
    type_args = SyntaxList()
    implicit_typevars = SyntaxList()

    i = 1
    for e in children(ex)
        k = head(e)
        if (k == :<: || k == :>:) && numchildren(e) == 1
            # `X{<:A}` and `X{>:A}`
            name = @ast ctx e "#T$i"::placeholder
            i += 1
            any = @ast ctx ex "Any"::core
            typevar = k == :<: ?
                _bounds_to_typevar(ctx, e, name, any, e[1]) :
                _bounds_to_typevar(ctx, e, name, e[1], any)
            arg = emit_assign_tmp(typevar_stmts, ctx, typevar)
            push!(implicit_typevars, arg)
        else
            arg = e
        end
        push!(type_args, arg)
    end

    type = @ast ctx ex [:call "apply_type"::core type_args...]
    if !isempty(implicit_typevars)
        type = @ast ctx ex [:block
            typevar_stmts...
            [:where type [:_typevars [:block implicit_typevars...] [:block typevar_stmts...]]]
        ]
    end

    return type
end

#-------------------------------------------------------------------------------
# Expand import / using / export

function expand_importpath(ctx, path)
    @jl_assert head(path) == :importpath path
    @ast ctx path [:. mapsyntax(_unplaceholder, children(path))...]
end

function _unplaceholder(st)
    k = head(st)
    k === :placeholder || k === :symbol ? @mknode(st; head=:identifier) :
        k === :identifier ? st : @jl_assert false st
end

# importer does not obey hygiene.  Doesn't bother with relayering any imported
# items, as the runtime functions don't see hygiene anyway
function expand_import_or_using(ctx, ex)
    if head(ex[1]) == :(:)
        # import M: x.y as z, w
        # (import (: (importpath M) (as (importpath x y) z) (importpath w)))
        # =>
        # (call module_import
        #  false
        #  (call core.svec "M")
        #  (call core.svec  2 "x" "y" "z"  1 "w" "w"))
        @jl_assert numchildren(ex[1]) >= 1 ex
        from = ex[1][1]
        from_path = @ast ctx from [:inert expand_importpath(ctx, from)]
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
    path_specs = SyntaxList()
    for spec in paths
        if head(spec) == :as
            @jl_assert numchildren(spec) == 2 spec
            s2 = _unplaceholder(spec[2])
            path = @ast ctx spec [:as expand_importpath(ctx, spec[1]) s2]
        else
            path = expand_importpath(ctx, spec)
        end
        push!(path_specs, @ast ctx spec [:inert path])
    end
    is_using = head(ex) == :using
    stmts = SyntaxList()
    if isnothing(from_path)
        for spec in path_specs
            if is_using
                push!(stmts,
                    @ast ctx spec [:call
                        eval_using   ::value
                        ctx.layer.mod::value
                        spec
                    ]
                )
            else
                push!(stmts,
                    @ast ctx spec [:call
                        eval_import   ::value
                        (!is_using)   ::value
                        ctx.layer.mod::value
                        (::nothing)
                        spec
                    ]
                )
            end
            # latestworld required between imports so that previous symbols
            # become visible
            push!(stmts, @ast ctx spec (::latestworld))
        end
    else
        push!(stmts, @ast ctx ex [:call
            eval_import   ::value
            (!is_using)   ::value
            ctx.layer.mod::value
            from_path
            path_specs...
        ])
        push!(stmts, @ast ctx ex (::latestworld))
    end
    @ast ctx ex [:block
        [:assert "toplevel_only"::symbol [:syntaxinert ex]]
        stmts...
        [:removable (::nothing)]
    ]
end

# flisp: export is relayered, and no-esc public is a syntax error (we relayer)
function expand_public(ctx, ex)
    identifiers = String[]
    numchildren(ex) == 0 && return @ast ctx ex (::nothing)
    mod = syntax_module(relayer_global_if_unhygienic(ctx, ex[1])[1])
    for e in children(ex)
        @jl_assert head(e) == :identifier (ex, "Expected identifier")
        syntax_module(relayer_global_if_unhygienic(ctx, e)[1]) !== mod &&
            throw(LoweringError(
                ex, "unexpected public/export with names from multiple modules"))
        push!(identifiers, syntax_name(e))
    end
    @ast ctx ex [:call
        eval_public::value
        mod::value
        (head(ex) == :export)::value
        identifiers::value
    ]
end

#-------------------------------------------------------------------------------
# Expand docstring-annotated expressions

function isquotedmacrocall(ex)
    head(ex) == :call || return false
    numchildren(ex) == 3 || return false
    let (f, ex) = (ex[1], ex[3])
        head(f) == :value || return false
        head(ex) == :inert || return false
        f.value === interpolate_expr || return false
        head(ex[1]) == :macrocall || return false
        return true
    end
end

function expand_doc(ctx, ex, docex)
    if head(ex) === :identifier || head(ex) === :.
        expand_forms_2(ctx, @ast ctx docex [:call
            bind_static_docs!::value
            (head(ex) === :. ? ex[1] : syntax_module(ex)::value)
            syntax_name((head(ex) === :. ? ex[2] : ex))::symbol
            docex[1]
            ::sourcelocation(ex)
            Union{}::value
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
    @nospecialize docs
    k = head(ex)
    if k == :atomic
        throw(LoweringError(ex, "unimplemented or unsupported atomic declaration"))
    elseif k == :call
        expand_call(ctx, ex)
    elseif k == :dotcall || k == :.&& || k == :.|| || k == :.=
        expand_forms_2(ctx, expand_fuse_broadcast(ctx, ex))
    elseif k == :.
        expand_forms_2(ctx, expand_dot(ctx, ex))
    elseif k == :?
        @jl_assert numchildren(ex) == 3 ex
        expand_forms_2(ctx, @ast ctx ex [:if children(ex)...])
    elseif k == :&& || k == :||
        cs = expand_cond_children(ctx, ex)
        isempty(cs) && return @ast ctx ex (k === :&&)::value
        length(cs) == 1 && return @ast ctx ex cs[1]
        # Attributing correct provenance for `cs[1:end-1]` is tricky in cases
        # like `a && (b && c)` because the expression constructed here arises
        # from the source fragment `a && (b` which doesn't follow the tree
        # structure. For now we attribute to the parent node.
        cond = length(cs) == 2 ?
            cs[1] :
            newnode(ex, k, cs[1:end-1])
        # This transformation assumes the type assertion `cond::Bool` will be
        # added by a later compiler pass (currently done in codegen)
        if k == :&&
            @ast ctx ex [:if cond cs[end] false::value]
        else
            @ast ctx ex [:if cond true::value cs[end]]
        end
    elseif k == :(::)
        @jl_assert numchildren(ex) == 2 (ex, "`::` must be written `value::type` outside function argument lists")
        @ast ctx ex [:call
            "typeassert"::core
            expand_forms_2(ctx, ex[1])
            expand_forms_2(ctx, ex[2])
        ]
    elseif k == :<: || k == :>: || k == :-->
        expand_forms_2(ctx, @ast ctx ex [:call
            adopt_scope(ex, string(k)::identifier)
            children(ex)...
        ])
    elseif k == :var"op=" || k == :var".op="
        expand_forms_2(ctx, expand_update_operator(ctx, ex))
    elseif k == :(=)
        expand_assignment(ctx, ex)
    elseif k == :break
        @stm ex begin
            [:break] ->
                @ast ctx ex [:break "loop-exit"::symboliclabel]
            [:break [:placeholder]] ->
                @ast ctx ex [:break "loop-exit"::symboliclabel]
            [:break [:identifier]] -> begin
                @ast ctx ex [:break ex[1]=>:symboliclabel]
            end
            [:break [:placeholder] val] ->
                @ast ctx ex [:break "loop-exit"::symboliclabel
                             expand_forms_2(ctx, val)]
            [:break [:identifier] val] -> begin
                @ast ctx ex [:break ex[1]=>:symboliclabel
                             expand_forms_2(ctx, val)]
            end
        end
    elseif k == :continue
        @stm ex begin
            [:continue] ->
                @ast ctx ex [:break "loop-cont"::symboliclabel]
            [:continue [:placeholder]] ->
                @ast ctx ex [:break "loop-cont"::symboliclabel]
            [:continue [:identifier]] ->
                @ast ctx ex [:break string(syntax_name(ex[1]), "#cont")::symboliclabel]
        end
    elseif k == :comparison
        expand_forms_2(ctx, expand_compare_chain(ctx, ex))
    elseif k == :doc
        @jl_assert numchildren(ex) == 2 ex
        expand_doc(ctx, ex[2], ex)
    elseif k == :for
        expand_forms_2(ctx, expand_for(ctx, ex))
    elseif k == :comprehension
        @jl_assert numchildren(ex) == 1 ex
        @jl_assert head(ex[1]) == :generator ex
        @ast ctx ex [:call
            "collect"::top
            expand_forms_2(ctx, ex[1])
        ]
    elseif k == :typed_comprehension
        @jl_assert numchildren(ex) == 2 ex
        @jl_assert head(ex[2]) == :generator ex
        if numchildren(ex[2]) == 2 && head(ex[2][2]) == :iteration
            # Hack to lower simple typed comprehensions to loops very early,
            # greatly reducing the number of functions and load on the compiler
            expand_forms_2(ctx, expand_comprehension_to_loops(ctx, ex))
        else
            @ast ctx ex [:call
                "collect"::top
                expand_forms_2(ctx, ex[1])
                expand_forms_2(ctx, ex[2])
            ]
        end
    elseif k == :generator
        expand_forms_2(ctx, expand_generator(ctx, ex))
    elseif k == :function
        if numchildren(ex) == 1
            return @ast ctx ex [:block
                [:global_if_global ex[1]]
                [:function_decl ex[1]]
                [:no_method_defs ex[1]]
                ex[1]]
        end
        sig, wheres = flatten_wheres(ex[1])
        name, args, rett = @stm sig begin
            [:(::) [:call f as...] t] -> (f, as, t)
            [:call f as...] -> (f, as, @ast(ctx, sig, "Any"::core))
            [:tuple as...] -> (nothing, as, @ast(ctx, sig, "Any"::core))
        end
        if isnothing(name)
            name = newsym(ctx, sig, "#anon#")
            @ast ctx ex [:block [:local name] expand_function_def(
                ctx, ex, SyntaxList(name, args...), wheres, ex[2], rett)]
        else
            expand_function_def(
                ctx, ex, SyntaxList(name, args...), wheres, ex[2], rett)
        end
    elseif k == :->
        sig, wheres = flatten_wheres(ex[1])
        @jl_assert head(sig) === :tuple ex
        name = newsym(ctx, sig, "#->#")
        rett = @ast(ctx, sig, "Any"::core)
        @ast ctx ex [:block [:local name] expand_function_def(
            ctx, ex, SyntaxList(name, children(sig)...), wheres, ex[2], rett)]
    elseif k == :macro
        @ast ctx ex [:block
            [:assert
                "global_toplevel_only"::symbol
                [:syntaxinert ex]
            ]
            expand_forms_2(ctx, expand_macro_def(ctx, ex))
        ]
    elseif k == :if || k == :elseif
        @jl_assert numchildren(ex) >= 2 ex
        @ast ctx ex [k
            expand_condition(ctx, ex[1])
            expand_forms_2(ctx, ex[2:end])...
        ]
    elseif k == :let
        expand_forms_2(ctx, expand_let(ctx, ex))
    elseif k == :const
        expand_const_decl(ctx, ex)
    elseif k == :local || k == :global
        expand_decls(ctx, ex)
    elseif k == :where
        expand_forms_2(ctx, expand_wheres(ctx, ex))
    elseif k == :string
        expand_forms_2(ctx, @ast ctx ex [:call "string"::top children(ex)...])
    elseif k == :try
        expand_forms_2(ctx, expand_try(ctx, ex))
    elseif k == :tuple
        if has_parameters(ex)
            if numchildren(ex) > 1
                throw(LoweringError(ex[end], "unexpected semicolon in tuple - use `,` to separate tuple elements"))
            end
            expand_forms_2(ctx, expand_named_tuple(ctx, ex, children(ex[1])))
        elseif any_assignment(children(ex))
            expand_forms_2(ctx, expand_named_tuple(ctx, ex, children(ex)))
        else
            expand_forms_2(ctx, @ast ctx ex [:call
                "tuple"::core
                children(ex)...
            ])
        end
    elseif k == :$
        throw(LoweringError(ex, "`\$` expression outside string or quote block"))
    elseif k == :module
        throw(LoweringError(ex, "`module` is only allowed at top level"))
    elseif k == :import || k == :using
        expand_import_or_using(ctx, ex)
    elseif k == :export || k == :public
        expand_public(ctx, ex)
    elseif k == :abstract || k == :primitive
        expand_forms_2(ctx, expand_abstract_or_primitive_type(ctx, ex))
    elseif k == :struct
        expand_struct_def(ctx, ex, docs)
    elseif k == :typegroup
        expand_typegroup_def(ctx, ex)
    elseif k == :ref
        sctx = with_stmts(ctx)
        (arr, idxs) = expand_ref_components(sctx, ex)
        expand_forms_2(ctx,
            @ast ctx ex [:block
                sctx.stmts...
                [:call
                    "getindex"::top
                    arr
                    idxs...
                ]
            ]
        )
    elseif k == :curly
        expand_forms_2(ctx, expand_curly(ctx, ex))
    elseif k == :toplevel
        # Temporary: It would make more sense to return this unchanged once
        # toplevel iteration over SyntaxTree exists, but for now, a call to
        # `eval` lets JuliaLowering retain provenance and hygiene here.
        ex2 = @ast ctx ex [:block
            [:assert "toplevel_only"::symbol [:syntaxinert ex]]
            [:call
             eval::value
                # a macro expanding to toplevel does not change the eval module,
                # but does change the name resolution module
                ctx.layer.mod::value
                [:syntaxinert ex]
            ]
        ]
        expand_forms_2(ctx, ex2)
    elseif k == :vect
        check_no_parameters(ex, "unexpected semicolon in array expression")
        expand_array(ctx, ex, "vect")
    elseif k == :hcat
        expand_array(ctx, ex, "hcat")
    elseif k == :typed_hcat
        expand_array(ctx, ex, "typed_hcat")
    elseif k == :opaque_closure
        expand_opaque_closure(ctx, ex)
    elseif k == :vcat || k == :typed_vcat
        expand_forms_2(ctx, expand_vcat(ctx, ex))
    elseif k == :ncat || k == :typed_ncat
        expand_forms_2(ctx, expand_ncat(ctx, ex))
    elseif k == :while
        @jl_assert numchildren(ex) == 2 ex
        @ast ctx ex [:symbolicblock "loop-exit"::symboliclabel
            [:_while
                expand_condition(ctx, ex[1])
                [:symbolicblock "loop-cont"::symboliclabel
                    [:scope_block [:neutral_scope]
                         expand_forms_2(ctx, ex[2])
                    ]
                ]
            ]
        ]
    elseif k == :inert || k == :syntaxinert || k == :foreignsymbol
        ex
    elseif k == :foreignglobal
        @ast ctx ex [:foreignglobal expand_csymbol(ctx, ex[1])]
    elseif k == :foreigncall
        # Assume user macros may produce this, but static_eval means desugaring
        # has already occurred.
        args = SyntaxList()
        for i in 2:numchildren(ex)
            c = ex[i]
            if head(c) === :static_eval
                push!(args, c)
            elseif i <= 3
                push!(args, @ast ctx ex [:static_eval expand_forms_2(ctx, c)])
            else
                push!(args, expand_forms_2(ctx, c))
            end
        end
        @ast ctx ex [:foreigncall expand_csymbol(ctx, ex[1]) args...]
    elseif k == :gc_preserve
        @ast ctx ex [:block
            s := [:gc_preserve_begin children(ex)[2:end]...]
            r := expand_forms_2(ctx, children(ex)[1])
            [:gc_preserve_end s]
            r
        ]
    elseif k == :&
        throw(LoweringError(ex, "invalid syntax"))
    elseif k == :$
        throw(LoweringError(ex, "`\$` expression outside string or quote"))
    elseif k == :...
        throw(LoweringError(ex, "`...` expression outside call"))
    elseif k == :ssavalue
        _resolve_ssavalue(ctx, ex)
    elseif is_leaf(ex)
        ex
    elseif k == :return
        if numchildren(ex) == 0
            @ast ctx ex [:return (::nothing)]
        elseif numchildren(ex) == 1
            mapchildren(e->expand_forms_2(ctx,e), ex)
        else
            throw(LoweringError(ex, "More than one argument to return"))
        end
    else
        mapchildren(e->expand_forms_2(ctx,e), ex)
    end
end

function expand_forms_2(ctx::DesugaringContext, exs::Union{Tuple,AbstractVector})
    res = SyntaxList()
    for e in exs
        push!(res, expand_forms_2(ctx, e))
    end
    res
end

@fzone "JL: desugar" function expand_forms_2(ex::SyntaxTree, world::UInt)
    sl = base_layer(ex.context)
    ctx_out = DesugaringContext(sl, Bindings(), Dict{Int, IdTag}(), world)
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

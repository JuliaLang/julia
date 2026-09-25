# flisp: dot-operators
function is_dotted_operator(s::AbstractString)
    return length(s) >= 2 &&
        s[1] === '.' && s[2] !== '.' &&
        Base.isoperator(s[2:end])
end

# Messy: expr uses a different head for every op `(a op= b)` and `(a .op= b)`.
# RawGreenNode uses op= and .op= with an extra argument specifying `op`.  The
# handling of these in both parsers and lowering is buggy enough that
# hard-coding a table here is probably best.
#
# Note simple `op` and `.op` are calls to (dotted) identifiers, so this special
# handling isn't necessary there.
const _opeq_ops = Set([
    "+", "-", "*", "/", "//", "\\", "^", "÷", "%", "<<", ">>", ">>>", "|", "&",
    "⊻", "\$", "+%", "-%", "*%"])
function opeq_op(st)
    numchildren(st) == 2 || return
    s = string(head(st))
    length(s) >= 2 && s[end] === '=' || return
    dotop_s = s[1:prevind(s, end)]
    op_s = dotop_s[1] === '.' ? dotop_s[nextind(dotop_s, 1):end] : dotop_s
    op_s in _opeq_ops ? dotop_s : nothing
end

function is_eventually_call(e)
    return e isa Expr && (e.head === :call ||
        e.head in (:escape, :where, :(::)) && is_eventually_call(e.args[1]))
end

function est_syntax_name(st, default)
    head(st) === :identifier ? st.value::String : default
end

function _scavenge_lnn(e::Expr)
    e.head in (:macrocall, :quote, :inert) || for a in e.args
        a isa LineNumberNode && return a
        if a isa Expr
            a_out = _scavenge_lnn(a)
            a_out isa LineNumberNode && return a_out
        end
    end
    return nothing
end
scavenge_lnn(@nospecialize(e)) =
    something(e isa Expr ? _scavenge_lnn(e) : nothing,
              LineNumberNode(0, :none))

_unescape_lnn(@nospecialize(e)) =
    e isa LineNumberNode ? e :
    (e isa Expr &&
    (e.head === :escape || e.head === Symbol("hygienic-scope")) &&
    length(e.args) > 0) ? _unescape_lnn(e.args[1]) : nothing

function _get_inner_lnn(e::Expr, default::LineNumberNode)
    e.head in (:function, :macro, :module, :(=)) || return default
    length(e.args) >= 2 || return default
    b = e.args[end]
    b isa Expr || return default
    b.head === :block || return default
    length(b.args) >= 1 || return default
    b_lnn = _unescape_lnn(b.args[1])
    return b_lnn isa LineNumberNode ? b_lnn : default
end

# List of Expr-AST forms that are always converted to some SyntaxTree form and
# never inserted as an opaque `:value`. Note no LineNumberNode, which appears
# unwrapped in a macrocall (possibly generated functions too, TODO check)
isa_lowering_ast_node(@nospecialize(e)) =
    e isa Symbol || e isa QuoteNode || e isa Expr || e isa GlobalRef

function expr_to_est(@nospecialize(e), src::Union{LineNumberNode, SourceRef}=scavenge_lnn(e),
                     context=SyntaxContext(nothing, nothing, JL_OLD_EDITION, false))
    _expr_to_est(e, context, src, false)[1]
end
function expr_to_est(@nospecialize(e), src::SyntaxTree)
    _expr_to_est(e, src.context, src, false)[1]
end

# Adding more cases to this function is almost certainly wrong, since this
# operates on arbitrary heads and arguments throughout macro expansion, not
# well-formed syntax after expansion is done.  Most of the complexity here is
# LineNumberNode absorption logic: linenodes are always considered provenance if
# unquoted, then removed in certain forms.  If `src` is not an linenode, it is
# assumed to be a better provenance source, so linenodes in `e` are not used for
# provenance (but still removed).
function _expr_to_est(@nospecialize(e), context::SyntaxContext,
                      src::SourceAttrType, quoted::Bool)
    st = if e isa Symbol
        @mknode(;head=:identifier, value=String(e), source=src, context)
    elseif e isa QuoteNode
        cid, _ = _expr_to_est(e.value, context, src, true)
        @mknode(;head=:inert, source=src, children=SyntaxList(cid), context)
    elseif e isa Expr
        h = e.head
        if h === :value || h === :identifier
            error("expr heads :value and :identifier are reserved")
        end
        src = old_src = src isa LineNumberNode ? _get_inner_lnn(e, src) : src
        cs = SyntaxTree[]
        rm_linenodes = h in (:block, :toplevel)
        quoted |= h in (:quote, :inert)
        for arg in e.args
            if rm_linenodes && (lnn = quoted ? arg : _unescape_lnn(arg);
                                lnn isa LineNumberNode)
                src isa LineNumberNode && (src = lnn)
            else
                cid, src = _expr_to_est(arg, context, src, quoted)
                push!(cs, cid)
            end
        end
        @mknode(;head=h, source=old_src, children=cs, context)
    elseif e isa GlobalRef
        # Represent globalref as :identifier with :mod attribute
        @mknode(;head=:identifier, source=src, value=string(e.name),
                mod=e.mod, context)
    else
        # We may want additional special cases for other types where
        # `Base.isa_ast_node(e)`, but `:value` should be fine for most, since
        # most are produced in or after lowering
        if e isa LineNumberNode && src isa LineNumberNode
            # linenode outside of block or toplevel
            src = e
        end
        @mknode(;head=:value, value=e, source=src, context)
    end
    @jl_assert isa_lowering_ast_node(e) || head(st) === :value st

    return st, src
end

# @__doc__ is brittle
_is_meta_doc_block(st) = @stm st begin
    [:block [:meta [:identifier]] _] -> syntax_name(st[1][1]) == "doc"
    _ -> false
end

# `suppress_linenodes` is true if `st`'s parent knows `st` is an exception to
# normal linenode rules.  It only applies to `st`, and not transitively to its
# children.
function est_to_expr(st::SyntaxTree, suppress_linenodes=false)
    h = head(st)
    if h === :identifier
        # @jl_assert scope layer is base
        n = Symbol(syntax_name(st))
        mod = st.mod
        !isnothing(mod) ? GlobalRef(mod, n) : n
    elseif h === :value
        v = st.value
        # Let `st.value isa Symbol` (or other AST node).  Since we enforce that
        # this is never produced by the reverse Expr->SyntaxTree transformation,
        # there is no lonely Expr for which `st` is the only SyntaxTree
        # representation.  This means we can pick some other expr this
        # represents, namely Expr(`(inert ,st.value)) rather than
        # Expr(st.value).
        isa_lowering_ast_node(v) ? QuoteNode(v) : v
    elseif h === :inert
        QuoteNode(est_to_expr(st[1]))
    else
        # TODO: should handle post-lowering forms as well
        @jl_assert !is_leaf(st) (st, "est_to_expr should only be used pre-desugaring")
        # In a partially-expanded or quoted AST, there may be heads with no
        # corresponding kind
        out = Expr(h)

        # (Move the following assumptions to the docs if they turn out accurate)
        # The only mandatory LineNumberNode is the second macrocall argument.
        # Other than that, optional linenodes may show up anywhere within:
        # - `block`, unless the block is the first child of `for` or `let`
        # - `toplevel`
        # Macro authors are responsible for handling any linenodes that follow
        # the rules above (but the presence of optional linenodes can't be
        # counted upon).
        need_lnns = h in (:block, :toplevel) && !suppress_linenodes &&
            !_is_meta_doc_block(st)
        for (i, c) in enumerate(children(st))
            need_lnns && push!(out.args, source_location(LineNumberNode, c))
            let suppress_c = i == 1 && (h == :for || h == :let)
                push!(out.args, est_to_expr(c, suppress_c))
            end
        end
        # Add extra linenodes to some blocks for better provenance
        if h === :block && length(out.args) == 0 && !suppress_linenodes
            push!(out.args, source_location(LineNumberNode, st))
        elseif h in (:module, :function, :macro) && length(out.args) > 0
            let b = out.args[end]
                b isa Expr && b.head === :block && pushfirst!(
                    b.args, source_location(LineNumberNode, st))
            end
        elseif h in (:for, :while) && length(out.args) > 0
            let b = out.args[end]
                b isa Expr && b.head === :block && push!(
                    b.args, source_location(
                        LineNumberNode, sourcefile(st), last_byte(st)))
            end
        end
        out
    end
end

#-------------------------------------------------------------------------------
# EST->DST

Base.@kwdef struct SyntaxCompatContext <: AbstractLoweringContext
    toplevel::Bool=true
end
function with(ctx::SyntaxCompatContext; toplevel=ctx.toplevel)
    SyntaxCompatContext(toplevel)
end

# .op => (. op)
function dst_separate_dotop(ctx, st::SyntaxTree)
    k = head(st)
    if k === :identifier
        dotop_s = syntax_name(st)
        !is_dotted_operator(dotop_s) && return est_to_dst(ctx, st)
        op_s = dotop_s[nextind(dotop_s,1):end]
        op_leaf = newleaf(st, :identifier, op_s)
        return @ast _ st [:. op_leaf]
    elseif k === :value && st.value isa GlobalRef &&
        is_dotted_operator(string(st.value.name))
        @jl_assert false (st, "TODO: handle dotted globalref")
    else
        return est_to_dst(ctx, st)
    end
end

function dst_eq_to_in(ctx, st::SyntaxTree)
    return @stm st begin
        [:filter cond is...] ->
            @ast _ st [:filter est_to_dst(ctx, cond)
                       [:iteration map_dst_eq_to_in(ctx, is)...]]
        [:(=) l r] ->
            @ast _ st [:in est_to_dst(ctx, l) est_to_dst(ctx, r)]
    end
end
function map_dst_eq_to_in(ctx, sl)
    mapsyntax(st->dst_eq_to_in(ctx, st), sl)
end

function dst_iterspec(ctx, src::SyntaxTree, sl::AbstractVector{SyntaxTree})
    return if length(sl) === 1 && head(sl[1]) === :filter
        cond = sl[1][1]
        iters = sl[1][2:end]
        @ast _ sl[1] [:filter
            [:iteration map_dst_eq_to_in(ctx, iters)...]
            est_to_dst(ctx, cond)
        ]
    else
        @ast _ src [:iteration map_dst_eq_to_in(ctx, sl)...]
    end
end

function dst_sink_parameters(ctx, sl::AbstractVector{SyntaxTree})
    out = map_est_to_dst(ctx, sl)
    if !isempty(out) && head(out[1]) === :parameters
        push!(out, popfirst!(out))
    end
    return out
end

function dst_importpath(ctx, st::SyntaxTree)
    return @stm st begin
        [:as p name] ->
            @ast _ st [:as dst_importpath(ctx, st[1]) est_to_dst(ctx, name)]
        [:. xs...] ->
            @ast _ st [:importpath map_est_to_dst(ctx, xs)...]
    end
end

_dst_eq_to_kw(st::SyntaxTree) = @stm st begin
    [:(=) l r] -> @ast _ st [:kw l r]
    x -> x
end

# flisp: tuple-to-arglist.  Fix parsing mistakes where anon function arglist is
# parsed as a block instead of a tuple, or uses `=` instead of `kw`.  Note
# return type annotations are not possible on an anonymous function.
# (a::T)->1
# (::T)->1
# (a...)->1
# (a=1)->1
# (a=1;)->1
# (a=1;b=1)->1
function _dst_fix_arglist(st::SyntaxTree)
    @stm st begin
        [:(::) [:call _...] _] -> st
        [:call _...] -> st
        [:tuple xs...] -> let fixed = mapsyntax(_dst_eq_to_kw, xs)
            fixed == xs ? st : @ast _ st [:tuple fixed...]
        end
        [:where x tvs...] -> let fixed = _dst_fix_arglist(x)
            fixed == x ? st : @ast _ st [:where fixed tvs...]
        end
        [:block x1 x2] ->
            @ast _ st [:tuple _dst_eq_to_kw(x1)
                       [:parameters _dst_eq_to_kw(x2)]]
        [:block x] -> @ast _ st [:tuple _dst_eq_to_kw(x)]
        [:block] -> @ast _ st [:tuple]
        [:block _...] -> @jl_assert false st
        x -> @ast _ st [:tuple _dst_eq_to_kw(x)]
    end
end

_is_false(st::SyntaxTree) = head(st) === :value && st.value === false

function _expand_literal_pow(st::SyntaxTree)
    k = head(st)
    ((k === :call || k === :dotcall) &&
        numchildren(st) === 3 &&
        head(st[1]) === :identifier && syntax_name(st[1]) === "^" &&
        head(st[3]) === :value && st[3].value isa Int) || return st
    @ast _ st [k
        "literal_pow"::top
        st[1] st[2]
        [:call [:call "apply_type"::core "Val"::top st[3]]]
    ]
end

function est_to_dst_ident(ctx, st::SyntaxTree)
    s = syntax_name(st)
    if is_writeonly_est_name(s)
        @mknode(st; head=:placeholder)
    elseif is_flisp_compat(st) && s === "#self#" && !ctx.toplevel
        @mknode(st; head=:thisfunction, value=nothing, children=SyntaxList())
    else
        st
    end
end

has_if_generated(st::SyntaxTree) = @stm st begin
    (_, when=is_leaf(st)||is_quoted(st)) -> false
    [:function _...] -> false
    ([:(=) call _], when=is_eventually_call(call)) -> false
    [:-> _...] -> false
    [:if [:generated] _ _] -> true
    _ -> any(has_if_generated, children(st))
end

# The (if (generated) gen nongen) form is troublesome because everything
# surrounding it is implicitly quoted (with `gen` interpolated into it), so
# converting the function's AST before proper quoting is incorrect.
split_generated(st::SyntaxTree, gen_part) = @stm st begin
    (_, when=is_leaf(st)||is_quoted(st)) -> st
    [:if [:generated] gen nongen] -> if gen_part
        @ast(_, st, [:syntaxunquote gen])
    else
        nongen
    end
    _ -> mapchildren(x->split_generated(x, gen_part), st)
end

# Set [no]specialize on a function parameter's identifier.  `meta` is a symbol
# if we should set this arg's meta unconditionally, or a map identifier-string
# to symbol if we should only do it for some identifiers (function body >0 arg
# nospecialize), or nothing if we should just recurse to find meta forms.
# Exceptions with unconditional meta: set meta on the tuple for a destructuring
# arg, and the whole expression for (::T).
function apply_arg_meta(st, meta::Union{Nothing, Symbol, Dict{String, Symbol}})
    k = head(st)
    if k == :identifier
        if meta isa Symbol
            setmeta(st, meta, true)
        elseif isnothing(meta)
            st
        else
            sym = get(meta, syntax_name(st), nothing)
            !isnothing(sym) ? setmeta(st, sym, true) : st
        end
    elseif k == :placeholder || k == :tuple || k == :(::) && numchildren(st) == 1
        meta isa Symbol ? setmeta(st, meta, true) : st
    elseif k == :... || k == :(::) || k == :(=) || k == :kw
        c1 = st[1]
        out1 = apply_arg_meta(c1, meta)
        c1 == out1 ? st : @ast _ st [k out1 st[2:end]...]
    elseif k == :meta
        # not specified what to do here if we get conflicting
        # specialize/nospecialize
        meta2 = Symbol(syntax_name(st[1]))
        @jl_assert meta2 in (:specialize, :nospecialize) st
        apply_arg_meta(st[2], meta2)
    elseif k == :parameters
        mapchildren(x->apply_arg_meta(x, meta), st)
    else
        @jl_assert false st
    end
end

function apply_arglist_meta(st, meta::Union{Nothing, Symbol, Dict{String, Symbol}})
    @stm st begin
        [:where x tvs...] -> let fixed = apply_arglist_meta(x, meta)
            fixed == x ? st : @ast _ st [:where fixed tvs...]
        end
        [:(::) x t] ->  let fixed = apply_arglist_meta(x, meta)
            fixed == x ? st : @ast _ st [:(::) fixed t]
        end
        [:call f args...] -> mapchildren(x->
            x == f ? strip_arg_meta(f) : apply_arg_meta(x, meta), st)
        [:tuple _...] -> mapchildren(x->apply_arg_meta(x, meta), st)
    end
end

# flisp bug; underscore sparams are sometimes readable (see #60626).  Should
# return `st` unchanged 99% of the time.
function force_readable_sparams(st)
    head(st) === :where && is_flisp_compat(st) || return st
    sig, wheres = let (sig0, wheres0) = flatten_wheres(st)
        sig0, mapsyntax(typevar_bounds, wheres0)
    end
    any(w->is_flisp_compat(w) && is_writeonly_est_name(syntax_name(w[1])),
        wheres) || return st

    seen = Set{String}()
    lt = @ast _ st "<:"::identifier
    for i in eachindex(wheres)
        n = wheres[i][1]
        n_str = syntax_name(n)
        lb = _mangle_writeonly(wheres[i][2], seen)
        ub = _mangle_writeonly(wheres[i][3], seen)
        is_flisp_compat(n) && is_writeonly_est_name(n_str) && push!(seen, n_str)
        wheres[i] = @ast _ st [:comparison lb lt _mangle_writeonly(n, seen) lt ub]
    end
    mangle = args->mapsyntax(a->_mangle_writeonly_argt(a, seen), args)
    sig2 = @stm sig begin
        [:(::) [:call as...] t] -> @ast _ sig [:(::) [:call mangle(as)...] t]
        [:call as...] -> @ast _ sig [:call mangle(as)...]
        [:tuple as...] -> @ast _ sig [:tuple mangle(as)...]
    end
    @ast _ st [:where sig2 wheres...]
end
_mangle_writeonly_argt(st, seen) = @stm st begin
    [:parameters _...] -> mapchildren(c->_mangle_writeonly_argt(c, seen), st)
    [:kw x v] -> @ast _ st [:kw _mangle_writeonly_argt(x, seen) v]
    [:(=) x v] -> @ast _ st [:(=) _mangle_writeonly_argt(x, seen) v]
    [:... x] -> @ast _ st [:... _mangle_writeonly_argt(x, seen)]
    [:(::) x t] -> @ast _ st [:(::) x _mangle_writeonly(t, seen)]
    [:(::) t] -> @ast _ st [:(::) _mangle_writeonly(t, seen)]
    [:overlay mt x] -> @ast _ st [:overlay mt _mangle_writeonly(x, seen)]
    _ -> st
end
function _mangle_writeonly(st, seen)
    k = head(st)
    if k === :identifier && isnothing(st.mod) && is_flisp_compat(st)
        n = syntax_name(st)
        !(n in seen) ? st : @ast _ st (string(n, "FIXME#60626")::identifier)
    elseif is_leaf(st) || is_quoted(st) || k === :-> || k === :function
        st
    else
        mapchildren(c->_mangle_writeonly(c, seen), st)
    end
end

function _note_32026_hack!(st, expansion_sc::SyntaxContext)
    k = head(st)
    if st.context.layer === expansion_sc.layer &&
        (k === :function || k === :(=) && is_eventually_call(st[1]))
        setmeta!(st, :resolved_global_function_name, true)
    end
    st
end
_apply_32026_hack(st, sc::SyntaxContext) = @stm st begin
    ([:identifier], when=st.mod===nothing && st.context.layer===sc.layer) ->
        @mknode(st; mod=sc.layer.mod)
    [:call x args...] -> head(x) === :(::) ? st :
        @ast _ st [:call _apply_32026_hack(x, sc) args...]
    [:where x args...] -> @ast _ st [:where _apply_32026_hack(x, sc) args...]
    [:(::) x t] -> @ast _ st [:(::) _apply_32026_hack(x, sc) t]
    [:curly x args...] -> @ast _ st [:curly _apply_32026_hack(x, sc) args...]
    x -> x
end
function apply_32026_hack(st, orig)
    is_flisp_compat(st) || return st
    getmeta(orig, :resolved_global_function_name, false) || return st
    @jl_assert is_flisp_compat(orig) orig
    _apply_32026_hack(st, orig.context)
end

function collect_body_meta(st)
    argmeta_all = nothing
    argmeta = nothing
    mmetas = nothing
    for c in children(st)
        head(c) === :meta || continue
        spec = numchildren(c) >= 1 && head(c[1]) === :identifier ?
            syntax_name(c[1]) : ""
        if spec in ("specialize", "nospecialize")
            meta = Symbol(spec)
            if numchildren(c) == 1
                isnothing(argmeta_all) && (argmeta_all = meta)
            else
                isnothing(argmeta) && (argmeta = Dict{String, Symbol}())
                for id in c[2:end]
                    head(id) === :identifier && (argmeta[syntax_name(id)] = meta)
                end
            end
        else
            for m in children(c)
                isnothing(mmetas) && (mmetas = SyntaxList())
                km = head(m)
                if km === :purity
                    push!(mmetas, m)
                elseif head(m) === :identifier && syntax_name(m) in (
                    "inline", "noinline", "propagate_inbounds",
                    "nospecializeinfer", "aggressive_constprop", "no_constprop")
                    push!(mmetas, @mknode(m; head=:symbol))
                end
            end
        end
    end
    (isnothing(argmeta_all) ? argmeta : argmeta_all), mmetas
end

# Absorb `meta` nodes from arguments and the function body into syntax `.meta`
# for easier desugaring.
#
# (_generated_body (syntaxquote gen) nongen) to allow arglist-related desugaring
# to occur before the methods are created
function _dst_function_body(ctx, st, r, method_metas)
    r2 = if has_if_generated(r)
        gen, nongen = split_generated(r, true), split_generated(r, false)
        @ast _ st [:_generated_body [:syntaxquote gen] est_to_dst(ctx, nongen)]
    else
        est_to_dst(ctx, r)
    end
    isnothing(method_metas) ? r2 : setmeta(r2, :method_metas, method_metas)
end

function dst_raw_lambda(ctx, st, sps)
    argl = map(x->expr_to_est(x::Symbol, st[1]), st[1].value::Vector)
    @ast _ st [:lambda [:block argl...] [:block sps...]
        est_to_dst(with(ctx; toplevel=false), st[2])]
end

"""
Convert the Expr-like tree (EST) coming from macro expansion to the tree
desugaring expects (DST), where some forms have SyntaxNode structure and others
have Expr structure.

We may drop cases from this conversion, for example, if...

- syntax evolution changes a form in Expr and EST to use DST structure, so the
  input we receive here is "already done"

- desugaring changes to accept the EST form instead of the DST one, so we can
  leave our input unchanged

We can assume `st` has passed `valid_st1`.  Errors arising from invalid AST
(including finding `macrocall/escape/quote` forms) should be handled there.
"""
function est_to_dst(ctx::SyntaxCompatContext, st::SyntaxTree)
    rec = var"#self#"
    return @stm st begin
        [:identifier] -> est_to_dst_ident(ctx, st)
        [:value] -> st.value === nothing ? newleaf(st, :nothing) : st
        (_, when=is_leaf(st)) -> st
        [:comparison cs0...] -> let cs = copy(cs0)
            for (i, c) in enumerate(cs)
                cs[i] = iseven(i) ? dst_separate_dotop(ctx, cs[i]) : rec(ctx, cs[i])
            end
            @mknode(st; children=cs)
        end
        [:var"'" x] ->
            @ast _ st [:call "'"::identifier(st) rec(ctx, x)]
        [:. f [:tuple args...]] -> _expand_literal_pow(
            @ast _ st [:dotcall rec(ctx, f) dst_sink_parameters(ctx, args)...])
        ([:inert [:identifier]], when=isnothing(st[1].mod)) ->
            @ast _ st st[1]=>:symbol
        [:syntaxinert _] -> st
        [:inert _] -> st
        [:module _...] -> st
        [:toplevel _...] -> st
        [:for [:(=) _ _] body] ->
            @ast _ st [:for [:iteration(st[1]) dst_eq_to_in(ctx, st[1])] rec(ctx, body)]
        [:for [:block iters...] body] ->
            @ast _ st [:for
                [:iteration(st[1]) map_dst_eq_to_in(ctx, iters)...]
                rec(ctx, body)
            ]
        (_, when=(k = head(st); k === :tuple || k === :vect || k === :braces)) ->
            @ast _ st [k dst_sink_parameters(ctx, children(st))...]
        (_, when=(k = head(st); k === :curly || k === :ref)) ->
            @ast _ st [k dst_separate_dotop(ctx, st[1])
                       dst_sink_parameters(ctx, children(st)[2:end])...]
        # tuple arg should not be converted or desugared
        [:foreigncall [:tuple _...] args...] ->
            @ast _ st [:foreigncall [:foreignsymbol st[1]] args...]
        [:foreignglobal [:tuple _...]] ->
            @ast _ st [:foreignglobal [:foreignsymbol st[1]]]
        ([:call [:identifier] sym args...],
         when=(syntax_name(st[1]) === "ccall" ||
               syntax_name(st[1]) === "cglobal")) -> if head(sym) === :tuple
             @ast _ st [:call st[1] [:foreignsymbol st[2]] map_est_to_dst(ctx, args)...]
         else
             @ast _ st [:call st[1] rec(ctx, sym) map_est_to_dst(ctx, args)...]
         end
        [:call f args...] -> let
            out_k, out_f = @stm dst_separate_dotop(ctx, f) begin
                [:. op] -> (:dotcall, op)
                f_sep -> (:call, f_sep)
            end
            out = @ast _ st [out_k
                out_f dst_sink_parameters(ctx, children(st)[2:end])...
            ]
            _expand_literal_pow(out)
        end
        [:try tryb cvar catchb rest...] -> let
            has_catch = !(_is_false(cvar) && _is_false(catchb))
            cvar_out = _is_false(cvar) ?
                newleaf(cvar, :placeholder) : rec(ctx, cvar)
            has_finally = length(rest) >= 1 && !_is_false(rest[1])
            has_else = length(rest) === 2
            @ast _ st [:try rec(ctx, tryb)
                has_catch ? [:catch(catchb) cvar_out rec(ctx, catchb)] : nothing
                has_else ? [:else(rest[2]) rec(ctx, rest[2])] : nothing
                has_finally ? [:finally(rest[1]) rec(ctx, rest[1])] : nothing
            ]
        end
        [:flatten _] -> let
            out_iters = SyntaxList()
            next = st
            while head(next) === :flatten
                push!(out_iters, dst_iterspec(ctx, next, next[1][2:end]))
                next = next[1][1]
            end
            @jl_assert head(next) === :generator st next
            push!(out_iters, dst_iterspec(ctx, next, next[2:end]))
            @ast _ st [:generator rec(ctx, next[1]) out_iters...]
        end
        [:comprehension xs...] -> let
            arg = rec(ctx, length(xs) == 1 ? xs[1] :
                @ast _ st [:generator children(st)...])
            if head(arg) === :generator
                @ast _ st [:comprehension arg]
            else
                @ast _ st [:call "collect"::top arg]
            end
        end
        [:typed_comprehension t0 g] -> let
            t = rec(t0)
            arg = rec(ctx, g)
            if head(arg) === :generator
                @ast _ st [:typed_comprehension t arg]
            else
                @ast _ st [:call "collect"::top t arg]
            end
        end
        # hack: `[_ for _ in rhs]`, `[f(_) for _ in rhs]` works
        ([:generator body [:(=) u2 rhs]],
         when=is_flisp_compat(st) &&
             is_writeonly_est_name(est_syntax_name(u2, "")) && begin
                 u2name=est_syntax_name(u2, "")
                 func = @stm body begin
                     ([:call func [:identifier]],
                      when=est_syntax_name(body[2], "")===u2name &&
                          !is_dotted_operator(est_syntax_name(body[1], ""))) -> func
                     ([:identifier],
                      when=est_syntax_name(body, "")===u2name) ->
                         @ast _ st ("identity"::top)
                     _ -> nothing
                 end
                 func !== nothing
             end) -> @ast _ st [:call "Generator"::top rec(ctx, func) rec(ctx, rhs)]
        [:generator body iters...] ->
            @ast _ st [:generator rec(ctx, body) dst_iterspec(ctx, st, iters)]
        ([:(=) l r], when=(is_eventually_call(l))) -> let
            f_ctx = with(ctx; toplevel=false)
            # no fix_arglist needed, since this func can't be anonymous
            arg_meta, method_metas = collect_body_meta(r)
            l = force_readable_sparams(apply_arglist_meta(l, arg_meta))
            l = apply_32026_hack(l, st)
            @ast _ st [:function
                rec(ctx, l)
                _dst_function_body(f_ctx, st, r, method_metas)]
        end
        [:function [:identifier]] ->
            @ast _ st [:function apply_32026_hack(st[1], st)]
        [:function l r] -> let
            f_ctx = with(ctx; toplevel=false)
            arg_meta, method_metas = collect_body_meta(r)
            l = force_readable_sparams(
                apply_arglist_meta(_dst_fix_arglist(l), arg_meta))
            l = apply_32026_hack(l, st)
            @ast _ st [:function
                rec(ctx, l)
                _dst_function_body(f_ctx, st, r, method_metas)]
        end
        [:-> l r] -> let
            f_ctx = with(ctx; toplevel=false)
            arg_meta, method_metas = collect_body_meta(r)
            l = force_readable_sparams(
                apply_arglist_meta(_dst_fix_arglist(l), arg_meta))
            @ast _ st [:->
                rec(ctx, l)
                _dst_function_body(f_ctx, st, r, method_metas)]
        end
        [:macro l r] -> let
            arg_meta, method_metas = collect_body_meta(r)
            r2 = rec(with(ctx; toplevel=false), r)
            isnothing(method_metas) || (r2 = setmeta(r2, :method_metas, method_metas))
            @ast _ st [:macro rec(ctx, apply_arglist_meta(l, arg_meta)) r2]
        end
        [:do [:call f args...] lam] -> let
            @ast _ st [:call rec(ctx, f) rec(ctx, lam) dst_sink_parameters(ctx, args)...]
        end
        ([:let binds body], when=(head(binds) !== :block)) ->
            @ast _ st [:let [:block(binds) rec(ctx, binds)] rec(ctx, body)]
        (_, when=(head(st) === :using || head(st) === :import)) -> let
            # dot_importpath = (. _...)
            # as_or_dotip = dot_importpath | (as dot_importpath name)
            # replaces dot_importpath with (importpath _...) in
            # (using as_or_dotip...)
            # (using (: as_or_dotip as_or_dotip...))
            paths, maybe_colon = @stm st[1] begin
                [:(:) paths...] -> (paths, st[1])
                _ -> (children(st), nothing)
            end
            out_cs = mapsyntax(st->dst_importpath(ctx, st), paths)
            if !isnothing(maybe_colon)
                out_c1 = @ast _ maybe_colon [:(:) out_cs...]
                out_cs = SyntaxList(out_c1)
            end
            @mknode(st; children=out_cs)
        end
        # flisp macro expansion treated const as local, so names got mangled
        # throughout the thunk.  JL uses locals for this, so strip const.
        ([:const [:(=) l r]], when=ctx.toplevel && is_flisp_compat(l) &&
            !is_base_layer(l.context)) ->
            @ast _ st [:(=) rec(l) rec(r)]
        (_, when=(s = opeq_op(st); s !== nothing)) -> let
             (op_s, out_k) = s[1] === '.' ?
                 (s[nextind(s,1):end], :var".op=") :
                 (s[1:end], :var"op=")
             op_leaf = newleaf(st, :identifier, op_s)
             @ast _ st [out_k rec(ctx, st[1]) op_leaf rec(ctx, st[2])]
         end

        #-----------------------------------------------------------------------
        # Heads not emitted from parsing
        ([:meta s vs...],
         when=(meta=est_syntax_name(s, "");
               !ctx.toplevel && meta in ("nospecialize", "specialize"))) ->
             # Should be handled in the function case
             newleaf(st, :nothing)
        ([:meta s gen], when=est_syntax_name(s, "") === "generated") ->
            @ast _ st [:meta @mknode(s; head=:symbol) rec(ctx, gen)]
        [:meta syms...] ->
            @ast _ st [:meta mapsyntax(
                s->(head(s) === :identifier ? @mknode(s; head=:symbol) : s),
                syms)...
           ]
        [:boundscheck x] -> @mknode(st; children=SyntaxList())
        [:inbounds [:identifier]] -> newnode(st, :inbounds_pop, SyntaxList())
        [:core x] -> newleaf(st, :core, syntax_name(x))
        [:top x] -> newleaf(st, :top, syntax_name(x))
        [:static_parameter x] -> newleaf(st, :static_parameter, x.value::IdTag)
        [:var"with-static-parameters" lam sps...] ->
            dst_raw_lambda(ctx, lam, sps)
        [:lambda _ _] -> dst_raw_lambda(ctx, st, SyntaxTree[])
        [:copyast [:inert ex]] -> @ast _ st [:call
            interpolate_expr::value
            [:inert(st[1]) ex]
        ]
        [:symbolicgoto lab] ->
            @mknode(st; value=syntax_name(lab), children=nothing)
        [:symboliclabel lab] ->
            @mknode(st; value=syntax_name(lab), children=nothing)
        [:symbolicblock id body] -> let s = syntax_name(id)
            if is_writeonly_est_name(s)
                @ast _ st [:symbolicblock id=>:placeholder rec(ctx, body)]
            else
                @ast _ st [:symbolicblock id=>:symboliclabel rec(ctx, body)]
            end
        end
        [:var"latestworld-if-toplevel"] -> newleaf(st, :latestworld_if_toplevel)
        [:var"scope-block" cs...] ->
            @ast _ st [:scope_block [:neutral_scope] map_est_to_dst(ctx, cs)...]
        ([:latestworld], when=!is_leaf(st)) -> newleaf(st, :latestworld)
        [:cfunction typ fptr rt at sym] -> let
            # A symbol in fptr[1] does not observe hygiene or local scopes, but
            # treating this as a binding is better for e.g. JETLS.
            out_fptr = if head(fptr) == :inert && numchildren(fptr) == 1 &&
                    head(fptr[1]) == :identifier
                sc = fptr[1].context
                ident = @mknode(fptr[1]; mod=base_layer(sc).mod)
                @ast _ fptr [:static_eval(fptr) ident]
            else
                rec(ctx, fptr)
            end
            @ast _ st [:cfunction
                rec(ctx, typ) out_fptr
                [:static_eval(rt; meta=name_hint("cfunction return type")) rec(ctx, rt)]
                [:static_eval(at; meta=name_hint("cfunction argument type")) rec(ctx, at)]
                rec(ctx, sym)
            ]
        end

        # avoid creating excess nodes
        _ -> let out_cs = map_est_to_dst(ctx, children(st))
            out_cs == children(st) ? st : @mknode(st; children=out_cs)
        end
    end
end
function map_est_to_dst(ctx, sl)
    mapsyntax(st->est_to_dst(ctx, st), sl)
end

est_to_dst(st) = est_to_dst(SyntaxCompatContext(), st)

#-------------------------------------------------------------------------------
# misc

function purity_expr_to_flags(st::SyntaxTree)
    @jl_assert head(st) === :purity st
    args = Bool[x.value for x in children(st)]
    Base.encode_effects_override(Base.EffectsOverride(args...))
end

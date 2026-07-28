const JS = JuliaSyntax

function find_kind(s::String)
    out = get(JS._kind_str_to_int, s, nothing)
    return isnothing(out) ? nothing : JS.Kind(out)
end

# flisp: dot-operators
#
# We work from the operator's name here (rather than its `Kind`) because by this
# point operators are represented uniformly as identifier-like names: this code
# also runs on trees converted from `Expr`, where an operator such as `.^` is
# simply the `Symbol` `:.^` with no token or `Kind` to inspect. `Base.isoperator`
# is the same operator-name test already used for `op=` in `est_to_dst` below;
# note we can't look up a `Kind` by name (eg via `find_kind`), since most
# operators no longer have their own kind - they share `K"Operator"`.
function is_dotted_operator(s::AbstractString)
    return length(s) >= 2 &&
        s[1] === '.' && s[2] !== '.' &&
        Base.isoperator(s[2:end])
end

function is_eventually_call(e)
    return e isa Expr && (e.head === :call ||
        e.head in (:escape, :where, :(::)) && is_eventually_call(e.args[1]))
end

function est_syntax_name(st, default)
    kind(st) in KSet"Identifier unknown_head" ? st.value::String : default
end

function expr_to_est(@nospecialize(e), src::SourceAttrType=LineNumberNode(0, :none))
    _expr_to_est(e, src)[1]
end

function _get_inner_lnn(e::Expr, default::LineNumberNode)
    e.head in (:function, :macro, :module, :(=)) || return default
    length(e.args) >= 2 || return default
    b = e.args[end]
    b isa Expr || return default
    b.head === :block || return default
    length(b.args) >= 1 || return default
    b_lnn = b.args[1]
    return b_lnn isa LineNumberNode ? b_lnn : default
end

# List of Expr-AST forms that are always converted to some SyntaxTree form and
# never inserted as an opaque `K"Value"`. Note no LineNumberNode, which appears
# unwrapped in a macrocall (possibly generated functions too, TODO check)
isa_lowering_ast_node(@nospecialize(e)) =
    e isa Symbol || e isa QuoteNode || e isa Expr || e isa GlobalRef

function is_expr_value(st::SyntaxTree)
    k = kind(st)
    return JuliaSyntax.is_literal(k) || k === K"Value"
end

# Adding more cases to this function is almost certainly wrong, since this
# operates on arbitrary heads and arguments throughout macro expansion, not
# well-formed syntax after expansion is done.  Most of the complexity here is
# LineNumberNode absorption logic: linenodes are always considered provenance if
# unquoted, then removed in certain forms.  If `src` is not an linenode, it is
# assumed to be a better provenance source, so linenodes in `e` are not used for
# provenance (but still removed).
function _expr_to_est(@nospecialize(e), src::SourceAttrType)
    st = if e isa Symbol
        newleaf(src, K"Identifier", String(e))
    elseif e isa QuoteNode
        cid, _ = _expr_to_est(e.value, src)
        newnode(src, K"inert", SyntaxList(cid))
    elseif e isa Expr && e.head === :lambda && length(e.args) == 2
        argnames = e.args[1]::Vector{Any}
        arg_cs = SyntaxTree[]
        for name in argnames
            id = newleaf(src, K"Identifier", String(name::Symbol))
            push!(arg_cs, id)
        end
        body_id, src = _expr_to_est(e.args[2], src)
        args_block = newnode(src, K"block", arg_cs)
        tvars_block = newnode(src, K"block", SyntaxTree[])
        st = newnode(src, K"lambda",
                     SyntaxTree[args_block, tvars_block, body_id])
    elseif e isa Expr
        head_s = string(e.head)
        st_k = find_kind(head_s)
        src = old_src = src isa LineNumberNode ? _get_inner_lnn(e, src) : src
        cs = SyntaxTree[]
        rm_linenodes = e.head in (:block, :toplevel)
        for arg in e.args
            if rm_linenodes && arg isa LineNumberNode
                src isa LineNumberNode && (src = arg)
            else
                cid, src = _expr_to_est(arg, src)
                push!(cs, cid)
            end
        end
        if isnothing(st_k)
            @mknode(;kind=K"unknown_head", value=head_s, source=old_src,
                    children=cs, context=nothing)
        else
            @mknode(;kind=st_k, source=old_src, children=cs, context=nothing)
        end
    elseif e isa GlobalRef
        # Represent globalref as K"Identifier" with :mod attribute
        @mknode(;kind=K"Identifier", source=src, value=string(e.name),
                mod=e.mod, context=nothing)
    else
        # We may want additional special cases for other types where
        # `Base.isa_ast_node(e)`, but `K"Value"` should be fine for most, since
        # most are produced in or after lowering
        if e isa LineNumberNode && src isa LineNumberNode
            # linenode outside of block or toplevel
            src = e
        end
        newleaf(src, K"Value", e)
    end
    @jl_assert isa_lowering_ast_node(e) || is_expr_value(st) st

    return st, src
end

# @__doc__ is brittle
_is_meta_doc_block(st) = @stm st begin
    [K"block" [K"meta" [K"Identifier"]] _] -> syntax_name(st[1][1]) == "doc"
    _ -> false
end

# `suppress_linenodes` is true if `st`'s parent knows `st` is an exception to
# normal linenode rules.  It only applies to `st`, and not transitively to its
# children.
function est_to_expr(st::SyntaxTree, suppress_linenodes=false)
    k = kind(st)
    if kind(st) === K"Identifier"
        # @jl_assert scope layer is base
        n = Symbol(syntax_name(st))
        mod = st.mod
        !isnothing(mod) ? GlobalRef(mod, n) : n
    elseif is_leaf(st) && is_expr_value(st)
        v = st.value
        # Let `st.value isa Symbol` (or other AST node).  Since we enforce that
        # this is never produced by the reverse Expr->SyntaxTree transformation,
        # there is no lonely Expr for which `st` is the only SyntaxTree
        # representation.  This means we can pick some other expr this
        # represents, namely Expr(`(inert ,st.value)) rather than
        # Expr(st.value).
        isa_lowering_ast_node(v) ? QuoteNode(v) : v
    elseif k === K"inert"
        QuoteNode(est_to_expr(st[1]))
    else
        # TODO: should handle post-lowering forms as well
        @jl_assert !is_leaf(st) (st, "est_to_expr should only be used pre-desugaring")
        # In a partially-expanded or quoted AST, there may be heads with no
        # corresponding kind
        head = Symbol((k === K"unknown_head" ? syntax_name(st) : untokenize(k))::String)
        out = Expr(head)

        # (Move the following assumptions to the docs if they turn out accurate)
        # The only mandatory LineNumberNode is the second macrocall argument.
        # Other than that, optional linenodes may show up anywhere within:
        # - `block`, unless the block is the first child of `for` or `let`
        # - `toplevel`
        # Macro authors are responsible for handling any linenodes that follow
        # the rules above (but the presence of optional linenodes can't be
        # counted upon).
        need_lnns = head in (:block, :toplevel) && !suppress_linenodes &&
            !_is_meta_doc_block(st)
        for (i, c) in enumerate(children(st))
            need_lnns && push!(out.args, source_location(LineNumberNode, c))
            let suppress_c = i == 1 && (k == K"for" || k == K"let")
                push!(out.args, est_to_expr(c, suppress_c))
            end
        end
        # Add extra linenodes to some blocks for better provenance
        if head === :block && length(out.args) == 0 && !suppress_linenodes
            push!(out.args, source_location(LineNumberNode, st))
        elseif head in (:module, :function, :macro) && length(out.args) > 0
            let b = out.args[end]
                b isa Expr && b.head === :block && pushfirst!(
                    b.args, source_location(LineNumberNode, st))
            end
        elseif head in (:for, :while) && length(out.args) > 0
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

# .op => (. op)
function _dst_separate_dotop(st::SyntaxTree)
    k = kind(st)
    if k === K"Identifier"
        dotop_s = syntax_name(st)
        !is_dotted_operator(dotop_s) && return est_to_dst(st)
        op_s = dotop_s[nextind(dotop_s,1):end]
        op_leaf = newleaf(st, K"Identifier", op_s)
        return @ast _ st [K"." op_leaf]
    elseif k === K"Value" && st.value isa GlobalRef &&
        is_dotted_operator(string(st.value.name))
        @jl_assert false (st, "TODO: handle dotted globalref")
    else
        return est_to_dst(st)
    end
end

function _dst_eq_to_in(st::SyntaxTree)
    return @stm st begin
        [K"filter" cond is...] ->
            @ast _ st [K"filter" est_to_dst(cond)
                       [K"iteration" mapsyntax(_dst_eq_to_in, is)...]]
        [K"=" l r] ->
            @ast _ st [K"in" est_to_dst(l) est_to_dst(r)]
    end
end

function _dst_iterspec(src::SyntaxTree, sl::AbstractVector{SyntaxTree})
    return if length(sl) === 1 && kind(sl[1]) === K"filter"
        cond = sl[1][1]
        iters = sl[1][2:end]
        @ast _ sl[1] [K"filter"
            [K"iteration" mapsyntax(_dst_eq_to_in, iters)...]
            est_to_dst(cond)
        ]
    else
        @ast _ src [K"iteration" map(_dst_eq_to_in, sl)...]
    end
end

function _dst_sink_parameters(sl::AbstractVector{SyntaxTree})
    out = mapsyntax(est_to_dst, sl)
    if !isempty(out) && kind(out[1]) === K"parameters"
        push!(out, popfirst!(out))
    end
    return out
end

function _dst_importpath(st::SyntaxTree)
    return @stm st begin
        [K"as" p name] ->
            @ast _ st [K"as" _dst_importpath(st[1]) est_to_dst(name)]
        [K"." xs...] ->
            @ast _ st [K"importpath" mapsyntax(est_to_dst, xs)...]
    end
end

_dst_eq_to_kw(st::SyntaxTree) = @stm st begin
    [K"=" l r] -> @ast _ st [K"kw" l r]
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
        [K"::" [K"call" _...] _] -> st
        [K"call" _...] -> st
        [K"tuple" xs...] -> let fixed = mapsyntax(_dst_eq_to_kw, xs)
            fixed == xs ? st : @ast _ st [K"tuple" fixed...]
        end
        [K"where" x tvs...] -> let fixed = _dst_fix_arglist(x)
            fixed == x ? st : @ast _ st [K"where" fixed tvs...]
        end
        [K"block" x1 x2] ->
            @ast _ st [K"tuple" _dst_eq_to_kw(x1)
                       [K"parameters" _dst_eq_to_kw(x2)]]
        [K"block" x] -> @ast _ st [K"tuple" _dst_eq_to_kw(x)]
        [K"block"] -> @ast _ st [K"tuple"]
        [K"block" _...] -> @jl_assert false st
        x -> @ast _ st [K"tuple" _dst_eq_to_kw(x)]
    end
end

_is_false(st::SyntaxTree) = kind(st) === K"Value" && st.value === false

function _expand_literal_pow(st::SyntaxTree)
    k = kind(st)
    (k in KSet"call dotcall" &&
        numchildren(st) === 3 &&
        kind(st[1]) === K"Identifier" && syntax_name(st[1]) === "^" &&
        st[3].value isa Integer) || return st
    @ast _ st [k
        "literal_pow"::K"top"
        st[1] st[2]
        [K"call" [K"call" "apply_type"::K"core" "Val"::K"top" st[3]]]
    ]
end

function _est_to_dst_ident(st::SyntaxTree)
    s = syntax_name(st)
    if is_writeonly_est_name(s)
        @mknode(st; kind=K"Placeholder", children=nothing)
    else
        st
    end
end

has_if_generated(st::SyntaxTree) = @stm st begin
    (_, when=is_leaf(st)||is_quoted(st)) -> false
    [K"function" _...] -> false
    ([K"=" call _], when=is_eventually_call(call)) -> false
    [K"->" _...] -> false
    [K"if" [K"generated"] _ _] -> true
    _ -> any(has_if_generated, children(st))
end

# The (if (generated) gen nongen) form is troublesome because everything
# surrounding it is implicitly quoted (with `gen` interpolated into it), so
# converting the function's AST before proper quoting is incorrect.
split_generated(st::SyntaxTree, gen_part) = @stm st begin
    (_, when=is_leaf(st)||is_quoted(st)) -> st
    [K"if" [K"generated"] gen nongen] -> if gen_part
        @ast(_, st, [K"syntaxunquote" gen])
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
    k = kind(st)
    if k == K"Identifier"
        if meta isa Symbol
            setmeta(st, meta, true)
        elseif isnothing(meta)
            st
        else
            sym = get(meta, syntax_name(st), nothing)
            !isnothing(sym) ? setmeta(st, sym, true) : st
        end
    elseif k == K"Placeholder" || k == K"tuple" || k == K"::" && numchildren(st) == 1
        meta isa Symbol ? setmeta(st, meta, true) : st
    elseif k == K"..." || k == K"::" || k == K"=" || k == K"kw"
        c1 = st[1]
        out1 = apply_arg_meta(c1, meta)
        c1 == out1 ? st : @ast _ st [k out1 st[2:end]...]
    elseif k == K"meta"
        # not specified what to do here if we get conflicting
        # specialize/nospecialize
        meta2 = Symbol(syntax_name(st[1]))
        @jl_assert meta2 in (:specialize, :nospecialize) st
        apply_arg_meta(st[2], meta2)
    elseif k == K"parameters"
        mapchildren(x->apply_arg_meta(x, meta), st)
    else
        @jl_assert false st
    end
end

function apply_arglist_meta(st, meta::Union{Nothing, Symbol, Dict{String, Symbol}})
    @stm st begin
        [K"where" x tvs...] -> let fixed = apply_arglist_meta(x, meta)
            fixed == x ? st : @ast _ st [K"where" fixed tvs...]
        end
        [K"::" x t] ->  let fixed = apply_arglist_meta(x, meta)
            fixed == x ? st : @ast _ st [K"::" fixed t]
        end
        [K"call" f args...] -> mapchildren(x->
            x == f ? strip_arg_meta(f) : apply_arg_meta(x, meta), st)
        [K"tuple" _...] -> mapchildren(x->apply_arg_meta(x, meta), st)
    end
end

# flisp bug; underscore sparams are sometimes readable (see #60626).  Should
# return `st` unchanged 99% of the time.
function force_readable_sparams(st)
    kind(st) === K"where" && is_flisp_compat(st) || return st
    sig, wheres = let (sig0, wheres0) = flatten_wheres(st)
        sig0, mapsyntax(typevar_bounds, wheres0)
    end
    any(w->is_flisp_compat(w) && is_writeonly_est_name(syntax_name(w[1])),
        wheres) || return st

    seen = Set{String}()
    lt = @ast _ st "<:"::K"Identifier"
    for i in eachindex(wheres)
        n = wheres[i][1]
        n_str = syntax_name(n)
        lb = _mangle_writeonly(wheres[i][2], seen)
        ub = _mangle_writeonly(wheres[i][3], seen)
        is_flisp_compat(n) && is_writeonly_est_name(n_str) && push!(seen, n_str)
        wheres[i] = @ast _ st [K"comparison" lb lt _mangle_writeonly(n, seen) lt ub]
    end
    mangle = args->mapsyntax(a->_mangle_writeonly_argt(a, seen), args)
    sig2 = @stm sig begin
        [K"::" [K"call" as...] t] -> @ast _ sig [K"::" [K"call" mangle(as)...] t]
        [K"call" as...] -> @ast _ sig [K"call" mangle(as)...]
        [K"tuple" as...] -> @ast _ sig [K"tuple" mangle(as)...]
    end
    @ast _ st [K"where" sig2 wheres...]
end
_mangle_writeonly_argt(st, seen) = @stm st begin
    [K"parameters" _...] -> mapchildren(c->_mangle_writeonly_argt(c, seen), st)
    [K"kw" x v] -> @ast _ st [K"kw" _mangle_writeonly_argt(x, seen) v]
    [K"=" x v] -> @ast _ st [K"=" _mangle_writeonly_argt(x, seen) v]
    [K"..." x] -> @ast _ st [K"..." _mangle_writeonly_argt(x, seen)]
    [K"::" x t] -> @ast _ st [K"::" x _mangle_writeonly(t, seen)]
    [K"::" t] -> @ast _ st [K"::" _mangle_writeonly(t, seen)]
    [K"overlay" mt x] -> @ast _ st [K"overlay" mt _mangle_writeonly(x, seen)]
    _ -> st
end
function _mangle_writeonly(st, seen)
    k = kind(st)
    if k === K"Identifier" && isnothing(st.mod) && is_flisp_compat(st)
        n = syntax_name(st)
        !(n in seen) ? st : @ast _ st (string(n, "FIXME#60626")::K"Identifier")
    elseif is_leaf(st) || is_quoted(st) || k === K"->" || k === K"function"
        st
    else
        mapchildren(c->_mangle_writeonly(c, seen), st)
    end
end

# nothing if not found, or symbol if 0-arg [no]specialize, or dict arg->meta
function collect_body_arg_meta(st)
    out = nothing
    for c in children(st)
        k = kind(c)
        @stm c begin
            [K"meta" [K"Identifier"] idents...] -> begin
                meta = Symbol(syntax_name(c[1]))
                meta in (:specialize, :nospecialize) || continue
                length(idents) == 0 && return meta
                isnothing(out) && (out = Dict{String, Symbol}())
                for id in idents
                    kind(id) === K"Identifier" && (out[syntax_name(id)] = meta)
                end
            end
            # Only leading meta statements are recognized in lowering.  Ideally
            # meta after non-meta statements would be an error.
            _ -> break
        end
    end
    out
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
function est_to_dst(st::SyntaxTree)
    rec = var"#self#"
    return @stm st begin
        [K"Identifier"] -> _est_to_dst_ident(st)
        [K"Value"] -> st.value === nothing ? newleaf(st, K"nothing") : st
        (_, when=is_leaf(st)) -> st
        ([K"unknown_head" l r],
         when=(s=syntax_name(st); Base.isoperator(s))) -> let
             (op_s, out_k) = s[1] === '.' ?
                 (s[nextind(s,1):prevind(s,end)], K".op=") :
                 (s[1:prevind(s,end)], K"op=")

             op_leaf = newleaf(st, K"Identifier", op_s)
             @ast _ st [out_k rec(l) op_leaf rec(r)]
         end
        [K"comparison" cs0...] -> let cs = copy(cs0)
            for (i, c) in enumerate(cs)
                cs[i] = iseven(i) ? _dst_separate_dotop(cs[i]) : rec(cs[i])
            end
            @mknode(st; children=cs)
        end
        [K"'" x] ->
            @ast _ st [K"call" "'"::K"Identifier"(st) rec(x)]
        [K"." f [K"tuple" args...]] -> _expand_literal_pow(
            @ast _ st [K"dotcall" rec(f) _dst_sink_parameters(args)...])
        ([K"inert" [K"Identifier"]], when=isnothing(st[1].mod)) ->
            @ast _ st st[1]=>K"Symbol"
        [K"syntaxinert" _] -> st
        [K"inert" _] -> st
        [K"module" _...] -> st
        [K"toplevel" _...] -> st
        [K"for" [K"=" _ _] body] ->
            @ast _ st [K"for" [K"iteration"(st[1]) _dst_eq_to_in(st[1])] rec(body)]
        [K"for" [K"block" iters...] body] ->
            @ast _ st [K"for"
                [K"iteration"(st[1]) mapsyntax(_dst_eq_to_in, iters)...]
                rec(body)
            ]
        (_, when=(k = kind(st); k in KSet"tuple vect braces")) ->
            @ast _ st [k _dst_sink_parameters(children(st))...]
        (_, when=(k = kind(st); k in KSet"curly ref")) ->
            @ast _ st [k _dst_separate_dotop(st[1])
                       _dst_sink_parameters(children(st)[2:end])...]
        # tuple arg should not be converted or desugared
        [K"foreigncall" [K"tuple" _...] args...] ->
            @ast _ st [K"foreigncall" [K"foreignsymbol" st[1]] args...]
        [K"foreignglobal" [K"tuple" _...]] ->
            @ast _ st [K"foreignglobal" [K"foreignsymbol" st[1]]]
        ([K"call" [K"Identifier"] sym args...],
         when=(syntax_name(st[1]) === "ccall" ||
               syntax_name(st[1]) === "cglobal")) -> if kind(sym) === K"tuple"
             @ast _ st [K"call" st[1] [K"foreignsymbol" st[2]] mapsyntax(rec, args)...]
         else
             @ast _ st [K"call" st[1] rec(sym) mapsyntax(rec, args)...]
         end
        [K"call" f args...] -> let
            out_k, out_f = @stm _dst_separate_dotop(f) begin
                [K"." op] -> (K"dotcall", op)
                f_sep -> (K"call", f_sep)
            end
            out = @ast _ st [out_k
                out_f _dst_sink_parameters(children(st)[2:end])...
            ]
            _expand_literal_pow(out)
        end
        [K"try" tryb cvar catchb rest...] -> let
            has_catch = !(_is_false(cvar) && _is_false(catchb))
            cvar_out = _is_false(cvar) ?
                newleaf(cvar, K"Placeholder") : rec(cvar)
            has_finally = length(rest) >= 1 && !_is_false(rest[1])
            has_else = length(rest) === 2
            @ast _ st [K"try" rec(tryb)
                has_catch ? [K"catch"(catchb) cvar_out rec(catchb)] : nothing
                has_else ? [K"else"(rest[2]) rec(rest[2])] : nothing
                has_finally ? [K"finally"(rest[1]) rec(rest[1])] : nothing
            ]
        end
        [K"flatten" _] -> let
            out_iters = SyntaxList()
            next = st
            while kind(next) === K"flatten"
                push!(out_iters, _dst_iterspec(next, next[1][2:end]))
                next = next[1][1]
            end
            @jl_assert kind(next) === K"generator" st next
            push!(out_iters, _dst_iterspec(next, next[2:end]))
            @ast _ st [K"generator" rec(next[1]) out_iters...]
        end
        [K"comprehension" _ _ _...] -> let
            arg = rec(@ast _ st [K"generator" children(st)...])
            @ast _ st [K"comprehension" arg]
        end
        [K"generator" body iters...] ->
            @ast _ st [K"generator" rec(body) _dst_iterspec(st, iters)]
        ([K"=" l r], when=(is_eventually_call(l))) -> let
            # no fix_arglist needed, since this func can't be anonymous
            l = apply_arglist_meta(l, collect_body_arg_meta(r))
            l = force_readable_sparams(l)
            if has_if_generated(r)
                gen, nongen = split_generated(r, true), split_generated(r, false)
                r2 = @ast _ st [K"_generated_body" [K"syntaxquote" gen] rec(nongen)]
            else
                r2 = rec(r)
            end
            @ast _ st [K"function" rec(l) r2]
        end
        [K"function" l r] -> let
            l = apply_arglist_meta(_dst_fix_arglist(l), collect_body_arg_meta(r))
            l = force_readable_sparams(l)
            if has_if_generated(r)
                gen, nongen = split_generated(r, true), split_generated(r, false)
                r2 = @ast _ st [K"_generated_body" [K"syntaxquote" gen] rec(nongen)]
            else
                r2 = rec(r)
            end
            @ast _ st [K"function" rec(l) r2]
        end
        [K"->" l r] -> let
            l = apply_arglist_meta(_dst_fix_arglist(l), collect_body_arg_meta(r))
            l = force_readable_sparams(l)
            if has_if_generated(r)
                gen, nongen = split_generated(r, true), split_generated(r, false)
                r2 = @ast _ st [K"_generated_body" [K"syntaxquote" gen] rec(nongen)]
            else
                r2 = rec(r)
            end
            @ast _ st [K"->" rec(l) r2]
        end
        [K"macro" l r] -> let
            l = apply_arglist_meta(l, collect_body_arg_meta(r))
            @ast _ st [K"macro" rec(l) rec(r)]
        end
        [K"do" [K"call" f args...] lam] -> let
            @ast _ st [K"call" rec(f) rec(lam) _dst_sink_parameters(args)...]
        end
        ([K"let" binds body], when=(kind(binds) !== K"block")) ->
            @ast _ st [K"let" [K"block"(binds) rec(binds)] rec(body)]
        (_, when=(kind(st) in KSet"using import")) -> let
            # dot_importpath = (. _...)
            # as_or_dotip = dot_importpath | (as dot_importpath name)
            # replaces dot_importpath with (importpath _...) in
            # (using as_or_dotip...)
            # (using (: as_or_dotip as_or_dotip...))
            paths, maybe_colon = @stm st[1] begin
                [K":" paths...] -> (paths, st[1])
                _ -> (children(st), nothing)
            end
            out_cs = mapsyntax(_dst_importpath, paths)
            if !isnothing(maybe_colon)
                out_c1 = @ast _ maybe_colon [K":" out_cs...]
                out_cs = SyntaxList(out_c1)
            end
            @mknode(st; children=out_cs)
        end

        #-----------------------------------------------------------------------
        # Heads not emitted from parsing
        ([K"meta" s vs...],
         when=(meta=est_syntax_name(s, ""); meta in ("nospecialize", "specialize"))) ->
             # Should be handled in the function case
             newleaf(st, K"nothing")
        ([K"meta" s gen], when=est_syntax_name(s, "") === "generated") ->
            @ast _ st [K"meta" @mknode(s; kind=K"Symbol") rec(gen)]
        [K"meta" syms...] ->
            @ast _ st [K"meta" mapsyntax(
                s->(kind(s) === K"Identifier" ? @mknode(s; kind=K"Symbol") : s),
                syms)...
           ]
        [K"boundscheck" x] -> @mknode(st; children=SyntaxList())
        [K"inbounds" [K"Identifier"]] -> newnode(st, K"inbounds_pop", SyntaxList())
        [K"core" x] -> newleaf(st, K"core", syntax_name(x))
        [K"top" x] -> newleaf(st, K"top", syntax_name(x))
        [K"static_parameter" x] -> newleaf(st, K"static_parameter", x.value::IdTag)
        [K"lambda" args sps body] -> @mknode(st; children=[args, sps, rec(body)])
        [K"copyast" [K"inert" ex]] -> @ast _ st [K"call"
            interpolate_expr::K"Value"
            [K"inert"(st[1]) ex]
        ]
        [K"symbolicgoto" lab] ->
            @mknode(st; value=syntax_name(lab), children=nothing)
        [K"oldsymbolicgoto" lab] ->
            @mknode(st; value=syntax_name(lab), children=nothing)
        [K"symboliclabel" lab] ->
            @mknode(st; value=syntax_name(lab), children=nothing)
        [K"symbolicblock" id body] -> let s = syntax_name(id)
            if is_writeonly_est_name(s)
                @ast _ st [K"symbolicblock" id=>K"Placeholder" rec(body)]
            else
                @ast _ st [K"symbolicblock" id=>K"symboliclabel" rec(body)]
            end
        end
        [K"unknown_head" cs...] -> let head = syntax_name(st)
            if head === "latestworld-if-toplevel"
                newleaf(st, K"latestworld_if_toplevel")
            else
                @jl_assert(false, (st, string(
                    "unknown expr head (corresponding to no kind) between",
                    " macro-expansion and desugaring: ")))
            end
        end
        ([K"latestworld"], when=!is_leaf(st)) -> newleaf(st, K"latestworld")
        [K"cfunction" typ fptr rt at sym] -> let
            # A symbol in fptr[1] does not observe hygiene or local scopes, but
            # treating this as a binding is better for e.g. JETLS.
            out_fptr = if kind(fptr) == K"inert" && numchildren(fptr) == 1 &&
                    kind(fptr[1]) == K"Identifier"
                sc = fptr[1].context::SyntaxContext
                ident = @mknode(fptr[1]; mod=base_layer(sc).mod)
                @ast _ fptr [K"static_eval"(fptr) ident]
            else
                rec(fptr)
            end
            @ast _ st [K"cfunction"
                rec(typ) out_fptr
                [K"static_eval"(rt; meta=name_hint("cfunction return type")) rec(rt)]
                [K"static_eval"(at; meta=name_hint("cfunction argument type")) rec(at)]
                rec(sym)
            ]
        end

        # avoid creating excess nodes
        _ -> let out_cs = mapsyntax(rec, children(st))
            out_cs == children(st) ? st : @mknode(st; children=out_cs)
        end
    end
end

#-------------------------------------------------------------------------------
# misc

function purity_expr_to_flags(st::SyntaxTree)
    @jl_assert kind(st) === K"purity" st
    args = Bool[x.value for x in children(st)]
    Base.encode_effects_override(Base.EffectsOverride(args...))
end

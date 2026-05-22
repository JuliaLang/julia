const JS = JuliaSyntax

function find_kind(s::String)
    out = get(JS._kind_str_to_int, s, nothing)
    return isnothing(out) ? nothing : JS.Kind(out)
end

# flisp: dot-operators
function is_dotted_operator(s::AbstractString)
    return length(s) >= 2 &&
        s[1] === '.' && s[2] !== '.' &&
        JS.is_operator(something(find_kind(s[2:end]), K"None"))
end

function is_eventually_call(e)
    return e isa Expr && (e.head === :call ||
        e.head in (:escape, :where, :(::)) && is_eventually_call(e.args[1]))
end

function expr_to_est(@nospecialize(e),
                     lnn::LineNumberNode=LineNumberNode(0, :none))
    graph = ensure_desugaring_attributes!(SyntaxGraph())
    expr_to_est(graph, e, lnn)
end

function expr_to_est(graph::SyntaxGraph, @nospecialize(e), lnn::LineNumberNode)
    SyntaxTree(graph, _expr_to_est(graph, e, lnn)[1])
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

function _expr_to_est(graph::SyntaxGraph, @nospecialize(e), src::LineNumberNode)
    st = if e isa Symbol
        setattr!(newleaf(graph, src, K"Identifier"), :name_val, String(e))
    elseif e isa QuoteNode
        cid, _ = _expr_to_est(graph, e.value, src)
        newnode(graph, src, K"inert", NodeId[cid])
    elseif e isa Expr && e.head === :scope_layer
        @assert length(e.args) === 2 && e.args[1] isa Symbol
        ident = newleaf(graph, src, K"Identifier")
        setattr!(ident, :name_val, String(e.args[1]::Symbol))
        setattr!(ident, :scope_layer, e.args[2])
    elseif e isa Expr && e.head === :lambda && length(e.args) == 2
        argnames = e.args[1]::Vector{Any}
        arg_cs = NodeId[]
        for name in argnames
            id = newleaf(graph, src, K"Identifier")
            setattr!(id, :name_val, String(name::Symbol))
            push!(arg_cs, id._id)
        end
        body_id, src = _expr_to_est(graph, e.args[2], src)
        args_block = newnode(graph, src, K"block", arg_cs)
        tvars_block = newnode(graph, src, K"block", NodeId[])
        st = newnode(graph, src, K"lambda",
                     NodeId[args_block._id, tvars_block._id, body_id])
        setattr!(st, :is_toplevel_thunk, false)
        setattr!(st, :toplevel_pure, false)
    elseif e isa Expr
        head_s = string(e.head)
        st_k = find_kind(head_s)
        src = old_src = _get_inner_lnn(e, src)
        cs = NodeId[]
        rm_linenodes = e.head in (:block, :toplevel)
        for arg in e.args
            if rm_linenodes && arg isa LineNumberNode
                src = arg
            else
                cid, src = _expr_to_est(graph, arg, src)
                push!(cs, cid)
            end
        end
        if isnothing(st_k)
            setattr!(newnode(graph, old_src, K"unknown_head", cs), :name_val, head_s)
        else
            newnode(graph, old_src, st_k, cs)
        end
    elseif e isa GlobalRef
        # Represent globalref as K"Identifier" with :mod attribute
        setattr!(newleaf(graph, src, K"Identifier", string(e.name)), :mod, e.mod)
    else
        # We may want additional special cases for other types where
        # `Base.isa_ast_node(e)`, but `K"Value"` should be fine for most, since
        # most are produced in or after lowering
        if e isa LineNumberNode
            # linenode oustside of block or toplevel
            src = e
        end
        setattr!(newleaf(graph, src, K"Value"), :value, e)
    end
    @jl_assert isa_lowering_ast_node(e) || is_expr_value(st) st

    return st._id, src
end

# `suppress_linenodes` is true if `st`'s parent knows `st` is an exception to
# normal linenode rules.  It only applies to `st`, and not transitively to its
# children.
function est_to_expr(st::SyntaxTree, suppress_linenodes=false)
    k = kind(st)
    if kind(st) === K"Identifier"
        n = Symbol(st.name_val::String)
        mod = get(st, :mod, nothing)
        !isnothing(mod) ? GlobalRef(mod, n) :
            hasattr(st, :scope_layer) ? Expr(:scope_layer, n, st.scope_layer) :
            n
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
        head = Symbol((k === K"unknown_head" ? st.name_val : untokenize(k))::String)
        out = Expr(head)

        # (Move the following assumptions to the docs if they turn out accurate)
        # The only mandatory LineNumberNode is the second macrocall argument.
        # Other than that, optional linenodes may show up anywhere within:
        # - `block`, unless the block is the first child of `for` or `let`
        # - `toplevel`
        # Macro authors are responsible for handling any linenodes that follow
        # the rules above (but the presence of optional linenodes can't be
        # counted upon).
        need_lnns = head in (:block, :toplevel) && !suppress_linenodes
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
        dotop_s = st.name_val::String
        !is_dotted_operator(dotop_s) && return est_to_dst(st)
        op_s = dotop_s[nextind(dotop_s,1):end]
        op_leaf = setattr(mkleaf(st), :name_val, op_s)
        return @ast st._graph st [K"." op_leaf]
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
            @ast st._graph st [K"filter" est_to_dst(cond)
                       [K"iteration" mapsyntax(_dst_eq_to_in, is)...]]
        [K"=" l r] ->
            @ast st._graph st [K"in" est_to_dst(l) est_to_dst(r)]
    end
end

function _dst_iterspec(src::SyntaxTree, sl::SyntaxList)
    return if length(sl) === 1 && kind(sl[1]) === K"filter"
        cond = sl[1][1]
        iters = sl[1][2:end]
        @ast sl.graph sl[1] [K"filter"
            [K"iteration" mapsyntax(_dst_eq_to_in, iters)...]
            est_to_dst(cond)
        ]
    else
        @ast sl.graph src [K"iteration" map(_dst_eq_to_in, sl)...]
    end
end

function _dst_sink_parameters(sl::SyntaxList)
    out = mapsyntax(est_to_dst, sl)
    if !isempty(out) && kind(out[1]) === K"parameters"
        push!(out, popfirst!(out))
    end
    return out
end

function _dst_importpath(st::SyntaxTree)
    return @stm st begin
        [K"as" p name] ->
            @ast st._graph st [K"as" _dst_importpath(st[1]) est_to_dst(name)]
        [K"." xs...] ->
            @ast st._graph st [K"importpath" mapsyntax(est_to_dst, xs)...]
    end
end

_dst_eq_to_kw(st::SyntaxTree) = @stm st begin
    [K"=" l r] -> @ast st._graph st [K"kw" l r]
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
    g = st._graph
    @stm st begin
        [K"::" [K"call" _...] _] -> st
        [K"call" _...] -> st
        [K"tuple" xs...] -> let fixed = mapsyntax(_dst_eq_to_kw, xs)
            fixed == xs ? st : @ast g st [K"tuple" fixed...]
        end
        [K"where" x tvs...] -> let fixed = _dst_fix_arglist(x)
            fixed == x ? st : @ast g st [K"where" fixed tvs...]
        end
        [K"block" x1 x2] ->
            @ast g st [K"tuple" _dst_eq_to_kw(x1)
                       [K"parameters" _dst_eq_to_kw(x2)]]
        [K"block" x] -> @ast g st [K"tuple" _dst_eq_to_kw(x)]
        [K"block"] -> @ast g st [K"tuple"]
        [K"block" _...] -> @jl_assert false st
        x -> @ast g st [K"tuple" _dst_eq_to_kw(x)]
    end
end

_is_false(st::SyntaxTree) = kind(st) === K"Value" && st.value === false

function _expand_literal_pow(st::SyntaxTree)
    k = kind(st)
    (k in KSet"call dotcall" &&
        numchildren(st) === 3 &&
        get(st[1], :name_val, "") === "^" &&
        get(st[3], :value, nothing) isa Integer) || return st
    @ast st._graph st [k
        "literal_pow"::K"top"
        st[1] st[2]
        [K"call" [K"call" "apply_type"::K"core" "Val"::K"top" st[3]]]
    ]
end

function _est_to_dst_ident(st::SyntaxTree)
    s = st.name_val::String
    if all(==('_'), s) || s == UNUSED
        setattr!(mkleaf(st), :kind, K"Placeholder")
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
        @ast(st._graph, st, [K"$" gen])
    else
        nongen
    end
    _ -> mapchildren(x->split_generated(x, gen_part), st._graph, st)
end

# Set [no]specialize on a function parameter's identifier.  `meta` is a symbol
# if we should set this arg's meta unconditionally, or a map identifier-string
# to symbol if we should only do it for some identifiers (function body >0 arg
# nospecialize), or nothing if we should just recurse to find meta forms.
# Exceptions with unconditional meta: set meta on the tuple for a destructuring
# arg, and the whole expression for (::T).
function apply_arg_meta(st, meta::Union{Nothing, Symbol, Dict{String, Symbol}})
    g = st._graph
    k = kind(st)
    if k == K"Identifier"
        if meta isa Symbol
            setmeta(st, meta, true)
        elseif isnothing(meta)
            st
        else
            sym = get(meta, st.name_val::String, nothing)
            !isnothing(sym) ? setmeta(st, sym, true) : st
        end
    elseif k == K"Placeholder" || k == K"tuple" || k == K"::" && numchildren(st) == 1
        meta isa Symbol ? setmeta(st, meta, true) : st
    elseif k == K"..." || k == K"::" || k == K"=" || k == K"kw"
        c1 = st[1]
        out1 = apply_arg_meta(c1, meta)
        c1 == out1 ? st : @ast g st [k out1 st[2:end]...]
    elseif k == K"meta"
        # not specified what to do here if we get conflicting
        # specialize/nospecialize
        meta2 = Symbol(st[1].name_val::String)
        @jl_assert meta2 in (:specialize, :nospecialize) st
        apply_arg_meta(st[2], meta2)
    elseif k == K"parameters"
        mapchildren(x->apply_arg_meta(x, meta), st._graph, st)
    else
        @jl_assert false st
    end
end

function apply_arglist_meta(st, meta::Union{Nothing, Symbol, Dict{String, Symbol}})
    g = st._graph
    @stm st begin
        [K"where" x tvs...] -> let fixed = apply_arglist_meta(x, meta)
            fixed == x ? st : @ast g st [K"where" fixed tvs...]
        end
        [K"::" x t] ->  let fixed = apply_arglist_meta(x, meta)
            fixed == x ? st : @ast g st [K"::" fixed t]
        end
        [K"call" f args...] -> mapchildren(x->
            x == f ? f : apply_arg_meta(x, meta), st._graph, st)
        [K"tuple" _...] -> mapchildren(x->apply_arg_meta(x, meta), st._graph, st)
    end
end

# nothing if not found, or symbol if 0-arg [no]specialize, or dict arg->meta
function collect_body_arg_meta(st)
    out = nothing
    for c in children(st)
        k = kind(c)
        @stm c begin
            [K"meta" [K"Identifier"] idents...] -> begin
                meta = Symbol(c[1].name_val::String)
                meta in (:specialize, :nospecialize) || continue
                length(idents) == 0 && return meta
                isnothing(out) && (out = Dict{String, Symbol}())
                for id in idents
                    kind(id) === K"Identifier" && (out[id.name_val] = meta)
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
    g = ensure_macro_attributes!(st._graph)
    rec = var"#self#"

    return @stm st begin
        [K"Identifier"] -> _est_to_dst_ident(st)
        [K"Value"] -> st.value === nothing ? newleaf(g, st, K"nothing") : st
        (_, when=is_leaf(st)) -> st
        ([K"unknown_head" l r],
         when=(s=st.name_val; Base.isoperator(s))) -> let
             (op_s, out_k) = s[1] === '.' ?
                 (s[nextind(s,1):prevind(s,end)], K".op=") :
                 (s[1:prevind(s,end)], K"op=")

             op_leaf = newleaf(g, st, K"Identifier")
             setattr!(op_leaf, :name_val, op_s)
             setattr!(op_leaf, :scope_layer, st.scope_layer)
             @ast g st [out_k rec(l) op_leaf rec(r)]
         end
        [K"comparison" cs0...] -> let cs = copy(cs0)
            for (i, c) in enumerate(cs)
                cs[i] = iseven(i) ? _dst_separate_dotop(cs[i]) : rec(cs[i])
            end
            mknode(st, cs)
        end
        [K"'" x] ->
            @ast g st [K"call" "'"::K"Identifier"(scope_layer=st.scope_layer) rec(x)]
        [K"." f [K"tuple" args...]] -> _expand_literal_pow(
            @ast g st [K"dotcall" rec(f) _dst_sink_parameters(args)...])
        ([K"inert" [K"Identifier"]], when=!hasattr(st[1], :mod)) ->
            @ast g st st[1]=>K"Symbol"
        ([K"inert_syntaxtree" [K"Identifier"]], when=!hasattr(st[1], :mod)) ->
            @ast g st st[1]=>K"Symbol"
        [K"inert" _] -> st
        [K"inert_syntaxtree" _] -> st
        [K"module" _...] -> st
        [K"toplevel" _...] -> st
        [K"for" [K"=" _ _] body] ->
            @ast g st [K"for" [K"iteration"(st[1]) _dst_eq_to_in(st[1])] rec(body)]
        [K"for" [K"block" iters...] body] ->
            @ast g st [K"for"
                [K"iteration"(st[1]) mapsyntax(_dst_eq_to_in, iters)...]
                rec(body)
            ]
        (_, when=(k = kind(st); k in KSet"tuple vect braces")) ->
            @ast g st [k _dst_sink_parameters(children(st))...]
        (_, when=(k = kind(st); k in KSet"curly ref")) ->
            @ast g st [k _dst_separate_dotop(st[1])
                       _dst_sink_parameters(children(st)[2:end])...]
        # tuple arg should not be converted or desugared
        [K"foreigncall" [K"tuple" _...] args...] ->
            @ast g st [K"foreigncall" [K"foreigncall_arg1" st[1]] args...]
        ([K"call" [K"Identifier"] sym args...],
         when=st[1].name_val::String === "ccall") -> if kind(sym) === K"tuple"
             @ast g st [K"call" st[1] [K"foreigncall_arg1" st[2]] mapsyntax(rec, args)...]
         else
             @ast g st [K"call" st[1] rec(sym) mapsyntax(rec, args)...]
         end
        [K"call" f args...] -> let
            out_k, out_f = @stm _dst_separate_dotop(f) begin
                [K"." op] -> (K"dotcall", op)
                f_sep -> (K"call", f_sep)
            end
            out = @ast g st [out_k
                out_f _dst_sink_parameters(children(st)[2:end])...
            ]
            _expand_literal_pow(out)
        end
        [K"try" tryb cvar catchb rest...] -> let
            has_catch = !(_is_false(cvar) && _is_false(catchb))
            cvar_out = _is_false(cvar) ?
                newleaf(g, cvar, K"Placeholder") : rec(cvar)
            has_finally = length(rest) >= 1 && !_is_false(rest[1])
            has_else = length(rest) === 2
            @ast g st [K"try" rec(tryb)
                has_catch ? [K"catch"(catchb) cvar_out rec(catchb)] : nothing
                has_else ? [K"else"(rest[2]) rec(rest[2])] : nothing
                has_finally ? [K"finally"(rest[1]) rec(rest[1])] : nothing
            ]
        end
        [K"flatten" _] -> let
            out_iters = SyntaxList(g)
            next = st
            while kind(next) === K"flatten"
                push!(out_iters, _dst_iterspec(next, next[1][2:end]))
                next = next[1][1]
            end
            @jl_assert kind(next) === K"generator" st next
            push!(out_iters, _dst_iterspec(next, next[2:end]))
            @ast g st [K"generator" rec(next[1]) out_iters...]
        end
        [K"generator" body iters...] ->
            @ast g st [K"generator" rec(body) _dst_iterspec(st, iters)]
        ([K"=" l r], when=(is_eventually_call(l))) -> let
            # no fix_arglist needed, since this func can't be anonymous
            l = apply_arglist_meta(l, collect_body_arg_meta(r))
            if has_if_generated(r)
                gen, nongen = split_generated(r, true), split_generated(r, false)
                r2 = @ast g st [K"_generated_body" [K"quote" gen] rec(nongen)]
            else
                r2 = rec(r)
            end
            @ast g st [K"function" rec(l) r2]
        end
        [K"function" l r] -> let
            l = apply_arglist_meta(_dst_fix_arglist(l), collect_body_arg_meta(r))
            if has_if_generated(r)
                gen, nongen = split_generated(r, true), split_generated(r, false)
                r2 = @ast g st [K"_generated_body" [K"quote" gen] rec(nongen)]
            else
                r2 = rec(r)
            end
            @ast g st [K"function" rec(l) r2]
        end
        [K"->" l r] -> let
            l = apply_arglist_meta(_dst_fix_arglist(l), collect_body_arg_meta(r))
            if has_if_generated(r)
                gen, nongen = split_generated(r, true), split_generated(r, false)
                r2 = @ast g st [K"_generated_body" [K"quote" gen] rec(nongen)]
            else
                r2 = rec(r)
            end
            @ast g st [K"->" rec(l) r2]
        end
        [K"do" [K"call" f args...] lam] -> let
            @ast g st [K"call" rec(f) rec(lam) _dst_sink_parameters(args)...]
        end
        ([K"let" binds body], when=(kind(binds) !== K"block")) ->
            @ast g st [K"let" [K"block"(binds) rec(binds)] rec(body)]
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
                out_c1 = @ast g maybe_colon [K":" out_cs...]
                out_cs = SyntaxList(out_c1)
            end
            mknode(st, out_cs)
        end

        #-----------------------------------------------------------------------
        # Heads not emitted from parsing
        ([K"meta" [K"unknown_head" ps...]], when=st[1].name_val === "purity") ->
            @ast g st [K"meta" "purity"::K"Symbol"
                Base.EffectsOverride([x.value for x in ps]...)::K"Value"]
        ([K"meta" s vs...],
         when=(meta=get(s, :name_val, "")::String; meta in ("nospecialize", "specialize"))) ->
             # Should be handled in the function case
             newleaf(g, st, K"nothing")
        [K"meta" syms...] ->
            @ast g st [K"meta" mapsyntax(
                s->(kind(s) === K"Identifier" ? setattr(s, :kind, K"Symbol") : s),
                syms)...
           ]
        # TODO: JL doesn't support inline/noinline/inbounds
        [K"inline" _] -> newleaf(g, st, K"TOMBSTONE")
        [K"noinline" _] -> newleaf(g, st, K"TOMBSTONE")
        [K"inbounds" _] -> newleaf(g, st, K"TOMBSTONE")
        [K"core" x] -> setattr!(mkleaf(st), :name_val, x.name_val)
        [K"top" x] -> setattr!(mkleaf(st), :name_val, x.name_val)
        [K"static_parameter" x] -> setattr!(mkleaf(st), :var_id, x.value::IdTag)
        [K"copyast" [K"inert" ex]] -> @ast g st [K"call"
            interpolate_ast::K"Value"
            Expr::K"Value"
            [K"inert"(st[1]) ex]
        ]
        [K"symbolicgoto" lab] -> setattr!(mkleaf(st), :name_val, lab.name_val)
        [K"oldsymbolicgoto" lab] -> setattr!(mkleaf(st), :name_val, lab.name_val)
        [K"symboliclabel" lab] -> setattr!(mkleaf(st), :name_val, lab.name_val)
        [K"symbolicblock" id body] -> if all(==('_'), id.name_val)
            @ast g st [K"symbolicblock" id=>K"Placeholder" rec(body)]
        else
            @ast g st [K"symbolicblock" id=>K"symboliclabel" rec(body)]
        end
        [K"unknown_head" cs...] -> let head = st.name_val
            if head === "latestworld-if-toplevel"
                newleaf(g, st, K"latestworld_if_toplevel")
            else
                @jl_assert(false, (st, string(
                    "unknown expr head (corresponding to no kind) between",
                    "macro-expansion and desugaring: ")))
            end
        end
        ([K"latestworld"], when=!is_leaf(st)) -> newleaf(g, st, K"latestworld")
        [K"cfunction" typ fptr rt at sym] -> let
            # Identifier callables are scope-resolved against the outermost
            # lowering layer (which corresponds to the method module used by
            # `method.c`'s `jl_toplevel_eval`), so the IR carries a binding
            # reference matching `@cfunction`'s runtime resolution. Other
            # forms (e.g. function definitions) stay inert.
            out_fptr = if kind(fptr) == K"inert" && numchildren(fptr) == 1 &&
                          kind(fptr[1]) == K"Identifier"
                ident = setattr!(mkleaf(fptr[1]), :scope_layer, 1)
                @ast g fptr [K"static_eval"(fptr) ident]
            else
                rec(fptr)
            end
            @ast g st [K"cfunction"
                rec(typ) out_fptr
                [K"static_eval"(rt, meta=name_hint("cfunction return type")) rec(rt)]
                [K"static_eval"(at, meta=name_hint("cfunction argument type")) rec(at)]
                rec(sym)
            ]
        end

        # avoid creating excess nodes
        _ -> let out_cs::Vector{NodeId} = map(x->rec(x)._id, children(st))
            out_cs == children(st) ? st : mknode(st, out_cs)
        end
    end
end

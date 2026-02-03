const JS = JuliaSyntax

function find_kind(s::String)
    out = get(JS._kind_str_to_int, s, nothing)
    return isnothing(out) ? nothing : JS.Kind(out)
end

function is_dotted_operator(s::AbstractString)
    return length(s) >= 2 &&
        s[1] === '.' &&
        JS.is_operator(something(find_kind(s[2:end]), K"None"))
end

function is_eventually_call(e)
    return e isa Expr && (e.head === :call ||
        e.head in (:escape, :where, :(::)) && is_eventually_call(e.args[1]))
end

function expr_to_est(@nospecialize(e),
                     lnn::LineNumberNode=LineNumberNode(0, :none))
    graph = ensure_attributes!(
        SyntaxGraph(),
        kind=Kind, syntax_flags=UInt16,
        source=SourceAttrType, var_id=Int, value=Any,
        name_val=String, is_toplevel_thunk=Bool,
        scope_layer=LayerId, meta=CompileHints,
        toplevel_pure=Bool)
    expr_to_est(graph, e, lnn)
end

function expr_to_est(graph::SyntaxGraph, @nospecialize(e), lnn::LineNumberNode)
    SyntaxTree(graph, _expr_to_est(graph, e, lnn)[1])
end

function _get_inner_lnn(e::Expr, default::LineNumberNode)
    e.head in (:function, :macro, :module, :(=)) || return default
    length(e.args) >= 2 || return default
    b = e.args[end]
    b isa Expr && b.head === :block || return default
    length(b.args) >= 1 && b.args[1] isa LineNumberNode || return default
    return b.args[1]
end

# List of Expr-AST forms that are always converted to some SyntaxTree form and
# never inserted as an opaque `K"Value"`. Note no LineNumberNode, which appears
# unwrapped in a macrocall (possibly generated functions too, TODO check)
isa_lowering_ast_node(@nospecialize(e)) =
    e isa Symbol || e isa QuoteNode || e isa Expr # || e isa GlobalRef

function is_expr_value(st::SyntaxTree)
    k = kind(st)
    return JuliaSyntax.is_literal(k) || k === K"Value" ||
        k === K"core" && get(st, :name_val, nothing) === "nothing"
end

function _expr_to_est(graph::SyntaxGraph, @nospecialize(e), src::LineNumberNode)
    st = if e === Core.nothing
        # e.value can't be nothing in `K"Value"`, so represent with K"core"
        setattr!(newleaf(graph, src, K"core"), :name_val, "nothing")
    elseif e isa Symbol
        setattr!(newleaf(graph, src, K"Identifier"), :name_val, String(e))
    elseif e isa QuoteNode
        cid, _ = _expr_to_est(graph, e.value, src)
        newnode(graph, src, K"inert", NodeId[cid])
    elseif e isa Expr && e.head === :scope_layer
        @assert length(e.args) === 2 && e.args[1] isa Symbol
        ident = newleaf(graph, src, K"Identifier")
        setattr!(ident, :name_val, String(e.args[1]))
        setattr!(ident, :scope_layer, e.args[2])
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
    # elseif e isa GlobalRef
        # TODO: Better-behaved as K"globalref", but lowering doesn't know this
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
    @assert isa_lowering_ast_node(e) || is_expr_value(st)

    return st._id, src
end

function est_to_expr(st::SyntaxTree)
    k = kind(st)
    return if k === K"core" && numchildren(st) === 0 && st.name_val === "nothing"
        nothing
    elseif is_leaf(st) && hasattr(st, :name_val)
        n = Symbol(st.name_val)
        hasattr(st, :scope_layer) ? Expr(:scope_layer, n, st.scope_layer) : n
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
        @assert !is_leaf(st)
        # In a partially-expanded or quoted AST, there may be heads with no
        # corresponding kind
        head = Symbol(k === K"unknown_head" ? st.name_val : untokenize(k))
        need_lnns = head in (:block, :toplevel)
        out = Expr(head)
        for c in children(st)
            need_lnns && push!(out.args, source_location(LineNumberNode, c))
            push!(out.args, est_to_expr(c))
        end
        # extra linenodes
        n = length(out.args)
        if (k === K"module" && 3 <= n <= 4 && kind(st[end]) === K"block") ||
            (k in KSet"function macro" && n === 2 && kind(st[end]) === K"block")
            pushfirst!(out.args[end].args, source_location(LineNumberNode, st))
        end
        out
    end
end

#-------------------------------------------------------------------------------
# EST->DST

# .op => (. op)
function _dst_separate_dotop(st::SyntaxTree)
    k = kind(st)
    if k === K"Identifier" && is_dotted_operator(st.name_val)
        dotop_s = st.name_val
        op_s = dotop_s[nextind(dotop_s,1):end]
        op_leaf = setattr(mkleaf(st), :name_val, op_s)
        return @ast st._graph st [K"." op_leaf]
    elseif k === K"Value" && st.value isa GlobalRef &&
        is_dotted_operator(string(st.value.name))
        @assert false "TODO: handle dotted globalref"
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
function est_to_dst(st::SyntaxTree; all_expanded=true)
    g = st._graph
    rec = @__FUNCTION__()

    return @stm st begin
        [K"Identifier"] -> let s = st.name_val
            if s in ("ccall", "cglobal")
                setattr!(newleaf(g, st, K"core"), :name_val, st.name_val)
            elseif all(==('_'), s)
                setattr!(mkleaf(st), :kind, K"Placeholder")
            else
                st
            end
        end
        (_, when=is_leaf(st)) -> st
        ([K"unknown_head" l r],
         when=(s=st.name_val; Base.isoperator(s))) -> let
             (op_s, out_k) = s[1] === '.' ?
                 (s[nextind(s,1):prevind(s,end)], K".op=") :
                 (s[1:prevind(s,end)], K"op=")

             op_leaf = newleaf(g, st, K"Identifier")
             JS.copy_attrs!(op_leaf, st)
             setattr!(op_leaf, :name_val, op_s)
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
        [K"." l r] -> let r2 = rec(r)
            @stm r2 begin
                [K"inert" r3] -> @ast g st [K"." rec(l) r3]
                r3 -> @ast g st [K"." rec(l) r3]
            end
        end
        [K"inert" [K"Identifier"]] -> @ast g st st[1]=>K"Symbol"
        # [K"quote" [K"Identifier"]] -> @ast g st st[1]=>K"Symbol"
        [K"inert" _] -> st
        [K"inert_syntaxtree" _] -> st
        [K"module" _...] -> st
        [K"toplevel" _...] -> st
        [K"for" [K"=" _ _] body] ->
            @ast g st [K"for" [K"iteration" _dst_eq_to_in(st[1])] rec(body)]
        [K"for" [K"block" iters...] body] ->
            @ast g st [K"for"
                [K"iteration" mapsyntax(_dst_eq_to_in, iters)...]
                rec(body)
            ]
        ([K"where" t tds...],
         when=!(length(tds) === 1 && kind(tds[1]) === K"braces")) ->
             @ast g st [K"where" rec(t) [K"braces" mapsyntax(rec, tds)...]]
        (_, when=(k = kind(st); k in KSet"tuple vect braces")) ->
            @ast g st [k _dst_sink_parameters(children(st))...]
        (_, when=(k = kind(st); k in KSet"curly ref")) ->
            @ast g st [k _dst_separate_dotop(st[1])
                       _dst_sink_parameters(children(st)[2:end])...
            ]
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
                has_catch ? [K"catch" cvar_out rec(catchb)] : nothing
                has_else ? [K"else" rec(rest[2])] : nothing
                has_finally ? [K"finally" rec(rest[1])] : nothing
            ]
        end
        [K"flatten" _] -> let
            out_iters = SyntaxList(st)
            next = st
            while kind(next) === K"flatten"
                push!(out_iters, _dst_iterspec(next, next[1][2:end]))
                next = next[1][1]
            end
            @assert kind(next) === K"generator"
            push!(out_iters, _dst_iterspec(next, next[2:end]))
            @ast g st [K"generator" rec(next[1]) out_iters...]
        end
        [K"generator" body iters...] ->
            @ast g st [K"generator" rec(body) _dst_iterspec(st, iters)]
        [K"ncat" dim xs...] -> let
            out = mknode(st, mapsyntax(rec, xs))
            setattr!(out, :syntax_flags,
                     JS.flags(st) | JS.set_numeric_flags(dim.value))
        end
        [K"nrow" dim xs...] -> let
            out = mknode(st, mapsyntax(rec, xs))
            setattr!(out, :syntax_flags,
                     JS.flags(st) | JS.set_numeric_flags(dim.value))
        end
        [K"typed_ncat" t dim xs...] -> let
            out_cs = pushfirst!(mapsyntax(rec, xs), rec(t))
            out = mknode(st, out_cs)
            setattr!(out, :syntax_flags,
                     JS.flags(st) | JS.set_numeric_flags(dim.value))
        end
        ([K"=" l r], when=(is_eventually_call(l))) -> if has_if_generated(r)
            gen, nongen = split_generated(r, true), split_generated(r, false)
            @ast g st [K"generated_function" rec(l) gen nongen]
        else
            @ast g st [K"function" rec(l) rec(r)]
        end
        [K"function" l r] -> let
            if kind(l) === K"..."
                l = @ast g l [K"tuple" l]
            end
            if has_if_generated(r)
                gen, nongen = split_generated(r, true), split_generated(r, false)
                @ast g st [K"generated_function" rec(l) gen nongen]
            else
                @ast g st [K"function" rec(l) rec(r)]
            end
        end
        [K"do" [K"call" f args...] [K"->" do_args do_body]] -> let
            # Note desugaring expects first-arg do-expression, unlike RawGreenNode
            @ast g st [K"call"
                rec(f)
                @ast g st[end] [K"do" rec(do_args) rec(do_body)]
                _dst_sink_parameters(args)...
            ]
        end
        ([K"let" binds body], when=(kind(binds) !== K"block")) ->
            @ast g st [K"let" [K"block" rec(binds)] rec(body)]
        [K"struct" mut sig body] -> let
            flags = JS.flags(st) | (_is_false(mut) ? 0 : JS.MUTABLE_FLAG)
            @ast g st [K"struct"(syntax_flags=flags)
                rec(sig)
                rec(body)
            ]
        end
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
                out_cs = SyntaxList(g, tree_ids(out_c1))
            end
            mknode(st, out_cs)
        end

        #-----------------------------------------------------------------------
        # Heads are not emitted from parsing
        ([K"meta" [K"unknown_head" ps...]], when=st[1].name_val === "purity") ->
            @ast g st [K"meta" "purity"::K"Symbol"
                Base.EffectsOverride([x.value for x in ps]...)::K"Value"]
        ([K"meta" s vs...],
         when=(get(s, :name_val, "") in ("nospecialize", "specialize"))) -> let
            if length(vs) === 0
                @ast g st [K"meta" s=>K"Symbol"]
            elseif length(vs) === 1
                out = est_to_dst(vs[1])
                setmeta(out, Symbol(s), true)
            else
                # Kick the can down the road (should only be simple atoms?)
                out_cs = SyntaxList(g)
                for v in vs
                    push!(out_cs, @ast g v [K"meta" s=>K"Symbol" v])
                end
                @ast g st [K"block" out_cs...]
            end
        end
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
        [K"copyast" [K"inert" ex]] -> @ast g st [K"call"
            interpolate_ast::K"Value"
            Expr::K"Value"
            [K"inert" ex]
        ]
        [K"symbolicgoto" lab] -> setattr!(mkleaf(st), :name_val, lab.name_val)
        [K"symboliclabel" lab] -> setattr!(mkleaf(st), :name_val, lab.name_val)
        [K"unknown_head" cs...] -> let head = st.name_val
            if head === "latestworld-if-toplevel"
                newleaf(g, st, K"latestworld_if_toplevel")
            else
                @assert(false, string(
                    "unknown expr head (corresponding to no kind) between",
                    "macro-expansion and desugaring: ", st))
            end
        end
        [K"cfunction" typ fptr rt at sym] -> @ast g st [K"cfunction"
            rec(typ) rec(fptr)
            [K"static_eval"(meta=name_hint("cfunction return type")) rec(rt)]
            [K"static_eval"(meta=name_hint("cfunction argument type")) rec(at)]
            rec(sym)
        ]

        # avoid creating excess nodes
        _ -> let out_cs::Vector{NodeId} = map(x->rec(x)._id, children(st))
            out_cs == children(st) ? st : mknode(st, out_cs)
        end
    end
end

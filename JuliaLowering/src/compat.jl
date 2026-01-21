const JS = JuliaSyntax

function _insert_tree_node(graph::SyntaxGraph, k::Kind, src::SourceAttrType,
                           attrs=[], flags::UInt16=0x0000)
    id = new_id!(graph)
    setattr!(graph, id, :kind, k)
    flags !== 0 && setattr!(graph, id, :syntax_flags, flags)
    setattr!(graph, id, :source, src)
    for (k,v) in attrs
        setattr!(graph, id, k, v)
    end
    return id
end

"""
An Expr -> SyntaxTree transformation that should preserve semantics, but will
have low-quality provenance info (namely, each tree node will be associated with
the last seen LineNumberNode in the pre-order expr traversal).

Last-resort option so that, for example, we can lower the output of old
Expr-producing macros.  Always prefer re-parsing source text over using this.

Supports parsed and/or macro-expanded exprs, but not lowered exprs
"""
function expr_to_syntaxtree(@nospecialize(e), lnn::Union{LineNumberNode, Nothing}=nothing)
    graph = ensure_attributes!(
        SyntaxGraph(),
        kind=Kind, syntax_flags=UInt16,
        source=SourceAttrType, var_id=Int, value=Any,
        name_val=String, is_toplevel_thunk=Bool,
        scope_layer=LayerId, meta=CompileHints,
        toplevel_pure=Bool)
    expr_to_syntaxtree(graph, e, lnn)
end

@fzone "JL: expr_to_syntaxtree" function expr_to_syntaxtree(ctx, @nospecialize(e), lnn::Union{LineNumberNode, Nothing})
    graph = syntax_graph(ctx)
    toplevel_src = if isnothing(lnn)
        # Provenance sinkhole for all nodes until we hit a linenode
        dummy_src = SourceRef(SourceFile("No source for expression"), 1, 0)
        _insert_tree_node(graph, K"None", dummy_src)
    else
        lnn
    end
    st_id, _ = _insert_convert_expr(e, graph, toplevel_src)
    out = SyntaxTree(graph, st_id)
    return out
end

function _expr_replace(@nospecialize(e), replace_pred::Function, replacer::Function,
                        recurse_pred=(@nospecialize e)->true)
    if replace_pred(e)
        replacer(e)
    elseif e isa Expr && recurse_pred(e)
        Expr(e.head, Any[_expr_replace(a, replace_pred, replacer, recurse_pred) for a in e.args]...)
    else
        e
    end
end

function _to_iterspec(exs::Vector, is_generator::Bool)
    if length(exs) === 1 && exs[1].head === :filter
        @assert length(exs[1].args) >= 2
        return Expr(:filter, _to_iterspec(exs[1].args[2:end], true), exs[1].args[1])
    end
    outex = Expr(:iteration)
    for e in exs
        if e.head === :block && !is_generator
            for iter in e.args
                push!(outex.args, Expr(:in, iter.args...))
            end
        elseif e.head === :(=)
            push!(outex.args, Expr(:in, e.args...))
        else
            @assert false "unknown iterspec in $e"
        end
    end
    return outex
end

"""
Return `e.args`, but with any parameters in SyntaxTree (flattened, source) order.
Parameters are expected to be at `e.args[pos]`.

e.g. orderings of (a,b,c;d;e;f):
  Expr:       (tuple (parameters (parameters (parameters f) e) d) a b c)
  SyntaxTree: (tuple a b c (parameters d) (parameters e) (parameters f))
"""
function collect_expr_parameters(e::Expr, pos::Int)
    params = expr_parameters(e, pos)
    isnothing(params) && return copy(e.args)
    args = Any[e.args[1:pos-1]..., e.args[pos+1:end]...]
    return _flatten_params!(args, params)
end
function _flatten_params!(out::Vector{Any}, params::Expr)
    p,p_esc = unwrap_esc(params)
    p1 = expr_parameters(p, 1)
    if !isnothing(p1)
        push!(out, p_esc(Expr(:parameters, p.args[2:end]...)))
        _flatten_params!(out, p_esc(p1))
    else
        push!(out, params::Any)
    end
    return out
end
function expr_parameters(p::Expr, pos::Int)
    if pos <= length(p.args)
        e,_ = unwrap_esc(p.args[pos])
        if e isa Expr && e.head === :parameters
            return p.args[pos]
        end
    end
    return nothing
end

"""
If `b` (usually a block) has exactly one non-LineNumberNode argument, unwrap it.
"""
function maybe_unwrap_arg(b)
    if !(b isa Expr)
        return b
    end
    e1 = findfirst(c -> !isa(c, LineNumberNode), b.args)
    isnothing(e1) && return b
    e2 = findfirst(c -> !isa(c, LineNumberNode), b.args[e1+1:end])
    !isnothing(e2) && return b
    return b.args[e1]
end

function maybe_extract_lnn(b, default)
    !(b isa Expr) && return default
    lnn_i = findfirst(a->isa(a, LineNumberNode), b.args)
    return isnothing(lnn_i) ? default : b.args[lnn_i]
end

# Get kind by string if exists.  TODO relies on internals
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

function rewrap_escapes(hyg, ex)
    if hyg isa Expr && hyg.head in (:escape, :var"hygienic-scope")
        ex = Expr(hyg.head, rewrap_escapes(hyg.args[1], ex))
        if hyg.head === :var"hygienic-scope"
            append!(ex.args, @view hyg.args[2:end])
        end
    end
    return ex
end

# Unwrap Expr(:escape) and Expr(:hygienic-scope). Return the unwrapped
# expression and a function which will rewrap a derived expression in the
# correct hygiene wrapper.
function unwrap_esc(ex)
    orig_ex = ex
    while ex isa Expr && ex.head in (:escape, :var"hygienic-scope")
        @assert length(ex.args) >= 1
        ex = ex.args[1]
    end
    return ex, e->rewrap_escapes(orig_ex, e)
end

function unwrap_esc_(e)
    unwrap_esc(e)[1]
end

"""
Insert `e` converted to a syntaxtree into graph and recurse on children.  Return
a pair (my_node_id, last_srcloc).  Should not mutate `e`.

`src` is the latest location found in the pre-order traversal, and is the line
number node to be associated with `e`.
"""
function _insert_convert_expr(@nospecialize(e), graph::SyntaxGraph, src::SourceAttrType)
    #---------------------------------------------------------------------------
    # Non-expr types
    if isnothing(e)
        st_id = _insert_tree_node(graph, K"core", src, [:name_val=>"nothing"])
        return st_id, src
    elseif e isa LineNumberNode
        # A LineNumberNode in value position evaluates to nothing
        st_id = _insert_tree_node(graph, K"core", src, [:name_val=>"nothing"])
        return st_id, e
    elseif e isa Symbol
        st_id = _insert_tree_node(graph, K"Identifier", src, [:name_val=>String(e)])
        return st_id, src
    elseif e isa QuoteNode
        if e.value isa Symbol
            return _insert_convert_expr(Expr(:quoted_symbol, e.value), graph, src)
        elseif e.value isa Expr
            return _insert_convert_expr(Expr(:inert, e.value), graph, src)
        elseif e.value isa LineNumberNode
            return _insert_tree_node(graph, K"Value", src, [:value=>e.value]), src
        else
            return _insert_convert_expr(e.value, graph, src)
        end
    elseif e isa String
        st_id = _insert_tree_node(graph, K"string", src)
        id_inner = _insert_tree_node(graph, K"String", src, [:value=>e])
        JuliaSyntax.setchildren!(graph, st_id, [id_inner])
        return st_id, src
    elseif !(e isa Expr)
        # There are other kinds we could potentially back-convert (e.g. Float),
        # but Value should work fine.
        st_k = e isa Bool ? K"Bool" :
            e isa Integer ? K"Integer" :
            find_kind(string(typeof(e)))
        st_id = _insert_tree_node(graph, isnothing(st_k) ? K"Value" : st_k, src, [:value=>e])
        return st_id, src
    end

    #---------------------------------------------------------------------------
    # `e` is an expr.  In many cases, it suffices to
    # - guess that the kind name is the same as the expr head
    # - add no syntax flags or attrs
    # - map e.args to syntax tree children one-to-one
    e::Expr
    nargs = length(e.args)
    maybe_kind = find_kind(string(e.head))
    st_k = isnothing(maybe_kind) ? K"None" : maybe_kind
    st_flags = 0x0000
    st_attrs = Dict{Symbol, Any}()
    # Note that SyntaxTree/Node differentiate 0-child non-terminals and leaves
    child_exprs::Union{Nothing, Vector{Any}} = copy(e.args)

    # However, the following are (many) special cases where the kind, flags,
    # children, or attributes are different from what we guessed above
    if Base.isoperator(e.head) && st_k === K"None"
        # e.head is an updating assignment operator (+=, .-=, etc).  Non-=
        # dotted ops are wrapped in a call, so we don't reach this.
        s = string(e.head)
        @assert s[end] === '=' && nargs === 2
        if s[1] === '.'
            st_k = K".op="
            op = s[nextind(s,1):prevind(s,end)]
        else
            st_k = K"op="
            op = s[1:prevind(s,end)]
        end
        child_exprs = Any[e.args[1], Symbol(op), e.args[2]]
    elseif e.head === :comparison
        for i = 2:2:length(child_exprs)
            op,op_esc = unwrap_esc(child_exprs[i])
            @assert op isa Symbol
            op_s = string(op)
            if is_dotted_operator(op_s)
                child_exprs[i] = Expr(:., op_esc(Symbol(op_s[2:end])))
            end
        end
    elseif e.head === :macrocall
        @assert nargs >= 2
        a1,a1_esc = unwrap_esc(e.args[1])
        child_exprs = collect_expr_parameters(e, 3)
        if child_exprs[2] isa LineNumberNode
            src = child_exprs[2]
        end
        deleteat!(child_exprs, 2)
        if a1 isa Symbol && a1 === Symbol("@__dot__")
            child_exprs[1] = Symbol("@.")
        elseif a1 isa Expr && nargs === 2 && a1.args[2] === Symbol("@__dot__")
            child_exprs[1] = Expr(a1.head, a1.args[1], Symbol("@."))
        elseif a1 isa GlobalRef && a1.mod === Core
            # Syntax-introduced macrocalls are listed here for reference.  We
            # probably don't need to convert these.
            if a1.name === Symbol("@cmd")
            elseif a1.name === Symbol("@doc") && nargs === 4 # two macro args only
                # Single-arg @doc is a lookup not corresponding to K"doc"
                # Revise sometimes calls @doc with three args, but probably shouldn't
                st_k = K"doc"
                child_exprs = child_exprs[2:3]
            elseif a1.name === Symbol("@int128_str")
            elseif a1.name === Symbol("@int128_str")
            elseif a1.name === Symbol("@big_str")
            end
        end
    elseif e.head === Symbol("'")
        @assert nargs === 1
        st_k = K"call"
        child_exprs = Any[e.head, e.args[1]]
    elseif e.head === :. && nargs === 2
        a2, a2_esc = unwrap_esc(e.args[2])
        if a2 isa Expr && a2.head === :tuple
            st_k = K"dotcall"
            tuple_exprs = collect_expr_parameters(a2_esc(a2), 1)
            child_exprs = pushfirst!(tuple_exprs, e.args[1])
        elseif a2 isa QuoteNode
            child_exprs[2] = a2_esc(a2.value)
        end
    elseif e.head === :for
        @assert nargs === 2
        child_exprs = Any[_to_iterspec(Any[e.args[1]], false), e.args[2]]
    elseif e.head === :where
        @assert nargs >= 2
        e2,_ = unwrap_esc(e.args[2])
        if !(e2 isa Expr && e2.head === :braces)
            child_exprs = Any[e.args[1], Expr(:braces, e.args[2:end]...)]
        end
    elseif e.head in (:tuple, :vect, :braces)
        child_exprs = collect_expr_parameters(e, 1)
    elseif e.head in (:curly, :ref)
        child_exprs = collect_expr_parameters(e, 2)
    elseif e.head === :try
        child_exprs = Any[e.args[1]]
        # Expr:
        # (try (block ...) var       (block ...) [block ...] [block ...])
        # #     try        catch_var  catch       finally     else
        # SyntaxTree:
        #   (try (block ...)
        #        [catch var (block ...)]
        #        [else (block ...)]
        #        [finally (block ...)])
        e2 = unwrap_esc_(e.args[2])
        e3 = unwrap_esc_(e.args[3])
        if e2 !== false || e3 !== false
            push!(child_exprs,
                  Expr(:catch,
                       e2 === false ? Expr(:catch_var_placeholder) : e.args[2],
                       e3 === false ? nothing : e.args[3]))
        end
        if nargs >= 5
            push!(child_exprs, Expr(:else, e.args[5]))
        end
        if nargs >= 4 && unwrap_esc_(e.args[4]) !== false
            push!(child_exprs, Expr(:finally, e.args[4]))
        end
    elseif e.head === :flatten || e.head === :generator
        st_k = K"generator"
        child_exprs = Any[]
        next = e
        while next.head === :flatten
            @assert next.args[1].head === :generator
            push!(child_exprs, _to_iterspec(next.args[1].args[2:end], true))
            next = next.args[1].args[1]
        end
        @assert next.head === :generator
        push!(child_exprs, _to_iterspec(next.args[2:end], true))
        pushfirst!(child_exprs, next.args[1])
    elseif e.head === :ncat || e.head === :nrow
        dim = unwrap_esc_(popfirst!(child_exprs))
        st_flags |= JS.set_numeric_flags(dim)
    elseif e.head === :typed_ncat
        st_flags |= JS.set_numeric_flags(unwrap_esc_(e.args[2]))
        deleteat!(child_exprs, 2)
    elseif e.head === :(->)
        @assert nargs === 2
        a1, a1_esc = unwrap_esc(e.args[1])
        if a1 isa Expr && a1.head === :block
            # Expr parsing fails to make :parameters here...
            lam_args = Any[]
            lam_eqs = Any[]
            for a in a1.args
                a isa LineNumberNode && continue
                a isa Expr && a.head === :(=) ?
                    push!(lam_eqs, Expr(:kw, a.args...)) : push!(lam_args, a)
            end
            !isempty(lam_eqs) && push!(lam_args, Expr(:parameters, lam_eqs...))
            child_exprs[1] = a1_esc(Expr(:tuple, lam_args...))
        elseif !(a1 isa Expr && (a1.head in (:tuple, :where)))
            child_exprs[1] = a1_esc(Expr(:tuple, a1))
        end
        src = maybe_extract_lnn(e.args[2], src)
        child_exprs[2] = maybe_unwrap_arg(e.args[2])
    elseif e.head === :call
        child_exprs = collect_expr_parameters(e, 2)
        a1,a1_esc = unwrap_esc(child_exprs[1])
        if a1 isa Symbol
            a1s = string(a1)
            if is_dotted_operator(a1s)
                # non-assigning dotop like .+ or .==
                st_k = K"dotcall"
                child_exprs[1] = a1_esc(Symbol(a1s[2:end]))
            end
        end
    elseif e.head === :function
        if nargs >= 2
            src = maybe_extract_lnn(e.args[2], src)
        end
    elseif e.head === :(=)
        if is_eventually_call(e.args[1])
            st_k = K"function"
            st_flags |= JS.SHORT_FORM_FUNCTION_FLAG
            src = maybe_extract_lnn(e.args[2], src)
            child_exprs[2] = maybe_unwrap_arg(e.args[2])
        end
    elseif e.head === :module
        @assert nargs in (3, 4)
        has_version = !isa(e.args[1], Bool)
        if !e.args[1+has_version]
            st_flags |= JS.BARE_MODULE_FLAG
        end
        child_exprs = has_version ?
            Any[Expr(:mod_version, e.args[1]), e.args[2+has_version], e.args[3+has_version]] :
            Any[e.args[2+has_version], e.args[3+has_version]]
    elseif e.head === :do
        # Expr:
        # (do (call f args...) (-> (tuple lam_args...) (block ...)))
        # SyntaxTree:
        # (call f args... (do (tuple lam_args...) (block ...)))
        if e.args[1].head === :macrocall
            st_k = K"macrocall"
            callargs = collect_expr_parameters(e.args[1], 3)
            if callargs[2] isa LineNumberNode
                src = callargs[2]
            end
            deleteat!(callargs, 2)
        else
            callargs = collect_expr_parameters(e.args[1], 2)
            st_k = K"call"
        end
        child_exprs = Any[callargs..., Expr(:do_lambda, e.args[2].args...)]
    elseif e.head === :let
        if nargs >= 1
            a1,_ = unwrap_esc(e.args[1])
            if !(a1 isa Expr && a1.head === :block)
                child_exprs[1] = Expr(:block, e.args[1])
            end
        end
    elseif e.head === :struct
        e.args[1] && (st_flags |= JS.MUTABLE_FLAG)
        child_exprs = child_exprs[2:end]
        # TODO handle docstrings after refactor
    elseif (e.head === :using || e.head === :import)
        e2 = _expr_replace(e, (e)->(e isa Expr && e.head === :.),
                           (e)->Expr(:importpath, e.args...))
        child_exprs = e2.args
    elseif e.head in (:local, :global) && nargs > 1
        # Possible normalization
        # child_exprs = Any[Expr(:tuple, child_exprs...)]
    elseif e.head === :error
        # Zero-child errors from parsing are leaf nodes.  We could change this
        # upstream for consistency.
        if nargs === 0
            child_exprs = nothing
            st_attrs[:value] = JS.ErrorVal()
            st_flags |= JS.TRIVIA_FLAG
        end
    end

    #---------------------------------------------------------------------------
    # The following heads are not emitted from parsing, but old macros could
    # produce these and they would historically be accepted by flisp lowering.
    if e.head === Symbol("latestworld-if-toplevel")
        st_k = K"latestworld_if_toplevel"
    elseif e.head === Symbol("hygienic-scope")
        st_k = K"hygienic-scope"
    elseif e.head === :meta
        # Messy and undocumented.  Only sometimes we want a K"meta".
        if e.args[1] isa Expr && e.args[1].head === :purity
            st_k = K"meta"
            child_exprs = [Expr(:quoted_symbol, :purity), Base.EffectsOverride(e.args[1].args...)]
        elseif nargs === 0
            # pass
        elseif e.args[1] === :nospecialize
            if nargs === 1
                child_exprs[1] = Expr(:quoted_symbol, :nospecialize)
            elseif nargs > 2
                st_k = K"block"
                # Kick the can down the road (should only be simple atoms?)
                child_exprs = map(c->Expr(:meta, :nospecialize, c), child_exprs[2:end])
            elseif nargs === 2
                st_id, src = _insert_convert_expr(e.args[2], graph, src)
                setmeta!(SyntaxTree(graph, st_id); nospecialize=true)
                return st_id, src
            end
        elseif e.args[1] in (:inline, :noinline, :generated, :generated_only,
                             :max_methods, :optlevel, :toplevel, :push_loc, :pop_loc,
                             :no_constprop, :aggressive_constprop, :specialize, :compile, :infer,
                             :nospecializeinfer, :force_compile, :propagate_inbounds, :doc)
            # TODO: Some need to be handled in lowering
            for (i, ma) in enumerate(e.args)
                if ma isa Symbol
                    # @propagate_inbounds becomes (meta inline propagate_inbounds)
                    child_exprs[i] = Expr(:quoted_symbol, e.args[i])
                end
            end
        else
            # Can't throw a hard error; it is explicitly tested that meta can take arbitrary keys.
            @error("Unknown meta form at $src: `$e`\n$(sprint(dump, e))")
            child_exprs[1] = Expr(:quoted_symbol, e.args[1])
        end
    elseif e.head === :scope_layer
        @assert nargs === 2
        @assert e.args[1] isa Symbol
        @assert e.args[2] isa LayerId
        st_id, src = _insert_convert_expr(e.args[1], graph, src)
        setattr!(graph, st_id, :scope_layer, e.args[2])
        return st_id, src
    elseif e.head === :symbolicgoto || e.head === :symboliclabel
        @assert nargs === 1
        st_k = e.head === :symbolicgoto ? K"symbolic_label" : K"symbolic_goto"
        st_attrs[:name_val] = string(e.args[1])
        child_exprs = nothing
    elseif e.head in (:inline, :noinline)
        @assert nargs === 1 && e.args[1] isa Bool
        # TODO: JuliaLowering doesn't accept this (non-:meta) form yet
        st_k = K"TOMBSTONE"
        child_exprs = nothing
    elseif e.head === :inbounds
        @assert nargs === 1 && typeof(e.args[1]) in (Symbol, Bool)
        # TODO: JuliaLowering doesn't accept this form yet
        st_k = K"TOMBSTONE"
        child_exprs = nothing
    elseif e.head === :core
        @assert nargs === 1
        @assert e.args[1] isa Symbol
        st_attrs[:name_val] = string(e.args[1])
        child_exprs = nothing
    end

    #---------------------------------------------------------------------------
    # Possibly-temporary heads introduced by us converting the parent expr
    if e.head === :catch_var_placeholder
        st_k = K"Placeholder"
        st_attrs[:name_val] = ""
        child_exprs = nothing
    elseif e.head === :quoted_symbol
        st_k = K"Symbol"
        st_attrs[:name_val] = String(e.args[1])
        child_exprs = nothing
    elseif e.head === :do_lambda
        st_k = K"do"
    elseif e.head === :mod_version
        v = e.args[1]
        @assert v isa VersionNumber
        st_k = K"VERSION"
        st_flags = JS.set_numeric_flags(v.minor*10)
        st_attrs[:value] = v
        child_exprs = nothing
    end

    #---------------------------------------------------------------------------
    # Throw if this function isn't complete.  Finally, insert a new node into the
    # graph and recurse on child_exprs
    if st_k === K"None"
        error("Unknown expr head at $src: `$(e.head)`\n$(sprint(dump, e))")
    elseif st_k === K"TOMBSTONE"
        return nothing, src
    end

    st_id = _insert_tree_node(graph, st_k, src, collect(st_attrs), st_flags)

    # child_exprs === nothing means we want a leaf.  Note that setchildren! with
    # an empty list makes a node non-leaf.
    if isnothing(child_exprs)
        return st_id, src
    else
        st_child_ids, last_src = _insert_child_exprs(e.head, child_exprs, graph, src)
        JuliaSyntax.setchildren!(graph, st_id, st_child_ids)
        return st_id, last_src
    end
end

function _insert_child_exprs(head::Symbol, child_exprs::Vector{Any},
                             graph::SyntaxGraph, src::SourceAttrType)
    st_child_ids = NodeId[]
    last_src = src
    for (i, c) in enumerate(child_exprs)
        c_unwrapped, _ = unwrap_esc(c)
        # If c::LineNumberNode is anywhere in a block OR c is not in tail
        # position, we don't need to insert `nothing` here
        if c_unwrapped isa LineNumberNode && (head === :block || head === :toplevel && i != length(child_exprs))
            last_src = c_unwrapped
        else
            (c_id, last_src) = _insert_convert_expr(c, graph, last_src)
            if !isnothing(c_id)
                push!(st_child_ids, c_id)
            end
        end
    end
    return st_child_ids, last_src
end

#-------------------------------------------------------------------------------
# SyntaxTree->Expr overrides

function JuliaSyntax._expr_leaf_val(ex::SyntaxTree, _...)
    name = get(ex, :name_val, nothing)
    if !isnothing(name)
        n = Symbol(name)
        if kind(ex) === K"Symbol"
            return QuoteNode(n)
        elseif hasattr(ex, :scope_layer)
            Expr(:scope_layer, n, ex.scope_layer)
        else
            n
        end
    else
        val = get(ex, :value, nothing)
        if kind(ex) == K"Value" && val isa Expr || val isa LineNumberNode
            # Expr AST embedded in a SyntaxTree should be quoted rather than
            # becoming part of the output AST.
            QuoteNode(val)
        else
            val
        end
    end
end

function JuliaSyntax.fixup_Expr_child(::Type{<:SyntaxTree}, head::SyntaxHead,
                                      @nospecialize(arg), first::Bool)
    isa(arg, Expr) || return arg
    k = kind(head)
    coalesce_dot = k in KSet"call dotcall curly" ||
                   (k == K"quote" && has_flags(head, JuliaSyntax.COLON_QUOTE))
    if @isexpr(arg, :., 1) && arg.args[1] isa Tuple
        h, a = arg.args[1]::Tuple{SyntaxHead,Any}
        arg = ((coalesce_dot && first) || is_syntactic_operator(h)) ?
            Symbol(".", a) : Expr(:., a)
    end
    return arg
end

Base.Expr(ex::SyntaxTree) = JuliaSyntax.to_expr(ex)

#-------------------------------------------------------------------------------
# WIP: Expr<->EST

function expr_to_est(@nospecialize(e), lnn::LineNumberNode=LineNumberNode(0, :none))
    graph = ensure_attributes!(
        SyntaxGraph(),
        kind=Kind, syntax_flags=UInt16,
        source=SourceAttrType, var_id=Int, value=Any,
        name_val=String, is_toplevel_thunk=Bool,
        scope_layer=LayerId, meta=CompileHints,
        toplevel_pure=Bool)
    SyntaxTree(graph, _expr_to_est(graph, e, lnn)[1])
end

function _get_inner_lnn(e::Expr, default::LineNumberNode)
    e.head in (:function, :macro, :module) || return default
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
        k === K"core" && st.name_val === "nothing"
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
        old_src = _get_inner_lnn(e, src)
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
            setattr!(newnode(graph, src, K"unknown_head", cs), :name_val, head_s)
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

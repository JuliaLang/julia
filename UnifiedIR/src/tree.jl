# The generic tree porcelain (§3.7 Level 1).
#
# The unifying principle:
#
#   A node is `kind + tagged operands + attribute columns`. A tree is NOT a
#   second data structure — it is the substrate viewed through the
#   node-reference projection of the operand list: `children(g, id)` = the
#   node-tagged operands. A tree is the special case of IR where every value
#   has exactly one use and containment coincides with use — i.e. exactly
#   the floating layout state before scheduling. An AST is a floating body
#   in a dialect where that special case always holds; lowering is the
#   progressive introduction of ordering and regions into the same rows.
#
# Everything either consumer does with trees is therefore written ONCE here,
# against a tiny graph interface. JuliaSyntax's `SyntaxTree`/`SyntaxList` are
# aliases of `Tree`/`NodeList`; its porcelain functions are these functions.
#
# ---------------------------------------------------------------------------
# The graph interface. A "tree-viewable graph" G implements:
#
#   syntax_graph(x) -> G          resolve the graph of x (cursors, lists and
#                                 consumer contexts add methods; the name is
#                                 kept for JuliaSyntax/JuliaLowering
#                                 continuity — it is the generic resolver)
#   substrate(g) -> AttrGraph     the shared storage core
#
# and gets DEFAULT implementations (via `substrate`) of the storage layer:
#
#   new_id!(g) -> Int                          fresh leaf row
#   children(g, id) -> AbstractVector{<:Integer}   node-reference projection
#   children(g, id, r::UnitRange)              sub-range of the projection
#   child(g, id, i) -> Int
#   numchildren(g, id), is_leaf(g, id)
#   setchildren!(g, id, ids)
#   getattr(g, name) -> column                 Dict-shaped, node-id keyed
#   hasattr(g, name) -> Bool
#   attrnames(g)
#
# any of which a graph type may override (IR overrides the projection to
# select the STMT-tagged operands, skipping CONST/IMM/REGION/BLOCK tags).
#
# Level-1 non-goals (consumer-specific by design, doc §3.7): the kind
# registry search path (kind is the shared registry Kind; KIND_UNSET sentinel),
# source-text machinery (provenance walks return the terminal value; only
# the consumer knows `SourceRef`), and the leaf payload convention (the
# `payloads_isequal` hook of `≈`).
# ---------------------------------------------------------------------------

# --------------------------- interface & defaults --------------------------

"Resolve the graph of `x` (graphs are their own resolution; cursors, lists,
and consumer context types add methods)."
syntax_graph(g::AttrGraph) = g

"The shared storage core under a tree-viewable graph."
substrate(g::AttrGraph) = g

new_id!(g) = newnode!(substrate(g))

"The node-reference projection of the operand list (§3.7)."
children(g, id::Integer) = child_ids(substrate(g), id)
children(g, id::Integer, r::UnitRange) = view(children(g, id), r)
child(g, id::Integer, i::Integer) = Int(children(g, id)[i])

numchildren(g, id::Integer) = numchildren(substrate(g), id)
is_leaf(g, id::Integer) = is_leaf(substrate(g), id)

setchildren!(g, id::Integer, ids::AbstractVector{<:Integer}) =
    setchildren!(substrate(g), id, ids)

"Attribute column of `g` (Dict-shaped, node-id keyed; KeyError if absent)."
getattr(g, name::Symbol) = getattrcol(substrate(g), name)
hasattr(g, name::Symbol) = hasattrcol(substrate(g), name)
attrnames(g) = keys(substrate(g).cols)

"Raw kind access: the substrate's kind column (the shared registry's Kind)."
rawkind(g, id::Integer) = node_kind(substrate(g), id)
setrawkind!(g, id::Integer, k::Kind) = set_node_kind!(substrate(g), id, k)

# The provenance convention of the porcelain: these attribute names hold
# node-id links and are treated specially by attribute copies (skipped) and
# cross-graph copies (followed).
const PROVENANCE_ATTRS = (:source, :macro_source)


# ------------------------------ graph checks -------------------------------

"Two objects view the same graph storage iff their substrates share the
operand pool (the identity `is_compatible_graph` has always compared)."
is_compatible_graph(x, y) =
    substrate(syntax_graph(x)).operands === substrate(syntax_graph(y)).operands

function check_compatible_graph(x, y)
    if !is_compatible_graph(x, y)
        error("Incompatible syntax graphs")
    end
end

function check_same_graph(x, y)
    if syntax_graph(x) !== syntax_graph(y)
        error("Mismatching syntax graphs")
    end
end

# ------------------------------- the cursor --------------------------------

"""
    Tree{G}

The generic tree cursor over a tree-viewable graph `G`: `(graph, id)`.
Child indexing, kind, and attribute columns as properties — one cursor for
ASTs (`SyntaxTree{Attrs} = Tree{SyntaxGraph{Attrs}}`) and for IR rows viewed
through the node-reference projection.
"""
struct Tree{G}
    _graph::G
    _id::Int
end

Tree(g, id::Integer) = Tree{typeof(g)}(g, Int(id))

syntax_graph(ex::Tree) = getfield(ex, :_graph)

function Base.getproperty(ex::Tree, name::Symbol)
    name === :_graph && return getfield(ex, :_graph)
    name === :_id  && return getfield(ex, :_id)
    graph = getfield(ex, :_graph)
    val = get(getattr(graph, name), getfield(ex, :_id)) do
        error("Property `$name` not defined on node: $(node_string(ex))")
    end
    return val
end

function Base.setproperty!(ex::Tree, name::Symbol, @nospecialize(val))
    setattr!(getfield(ex, :_graph), getfield(ex, :_id), name, val)
    val
end

Base.propertynames(ex::Tree) = attrnames(ex)

function Base.get(ex::Tree, name::Symbol, default)
    graph = getfield(ex, :_graph)
    !hasattr(graph, name) && return default
    get(getattr(graph, name), getfield(ex, :_id), default)
end

function Base.getindex(ex::Tree, i::Integer)
    Tree(getfield(ex, :_graph), child(getfield(ex, :_graph), getfield(ex, :_id), i))
end

function Base.getindex(ex::Tree, r::UnitRange)
    g = getfield(ex, :_graph)
    NodeList(g, children(g, getfield(ex, :_id), r))
end

Base.firstindex(::Tree) = 1
Base.lastindex(ex::Tree) = numchildren(ex)

is_leaf(ex::Tree) = is_leaf(getfield(ex, :_graph), getfield(ex, :_id))
numchildren(ex::Tree) = numchildren(getfield(ex, :_graph), getfield(ex, :_id))

function children(ex::Tree)
    g = getfield(ex, :_graph)
    NodeList(g, children(g, getfield(ex, :_id)))
end

rawkind(ex::Tree) = rawkind(getfield(ex, :_graph), getfield(ex, :_id))
setrawkind!(ex::Tree, k::Kind) = setrawkind!(getfield(ex, :_graph), getfield(ex, :_id), k)

# ---------------------------- cursor attributes ----------------------------

function hasattr(ex::Tree, name::Symbol)
    graph = getfield(ex, :_graph)
    !hasattr(graph, name) && return false
    return haskey(getattr(graph, name), getfield(ex, :_id))
end

function attrnames(ex::Tree)
    g = getfield(ex, :_graph)
    (name::Symbol for name in attrnames(g) if haskey(getattr(g, name), getfield(ex, :_id)))
end

@noinline function setattr!(graph, id::Integer, k::Symbol, @nospecialize(v))
    getattr(graph, k)[id] = v
    id
end

function setattr!(ex::Tree, name::Symbol, @nospecialize(val))
    setattr!(getfield(ex, :_graph), getfield(ex, :_id), name, val)
    ex
end

"Non-mutating setattr: immutable-update the node, then set the attribute."
setattr(ex::Tree, name::Symbol, @nospecialize(val)) =
    setattr!(is_leaf(ex) ? mkleaf(ex) : mknode(ex, children(ex)), name, val)

deleteattr!(graph, id::Integer, name::Symbol) = delete!(getattr(graph, name), id)
deleteattr!(ex::Tree, name::Symbol) =
    deleteattr!(getfield(ex, :_graph), getfield(ex, :_id), name)

# ------------------------------ fallback print -----------------------------

"Fallback structural printing of a node: id, attributes, children."
function node_string(ex::Tree, depth=2)
    out = "(_id="*string(getfield(ex, :_id))
    for n in sort!(collect(attrnames(ex)))
        out *= ", "*string(n)*"="*repr(getproperty(ex, n))
    end
    if is_leaf(ex)
        out *= ", leaf"
    elseif depth > 1
        out *= ", children=["
        for c in children(ex)
            out *= "\n"*node_string(c, depth-1)
        end
        out *= "]"
    end
    out *= ")"
    return out
end

"Generic indented tree printer over the node-reference projection."
function print_tree(io::IO, ex::Tree; maxdepth::Int = typemax(Int), _indent::String = "")
    print(io, _indent, "#", getfield(ex, :_id))
    k = rawkind(ex)
    print(io, " kind=", k == KIND_UNSET ? "?" : repr(get(ex, :kind, k)))
    for n in sort!(collect(attrnames(ex)))
        n === :kind && continue
        n in PROVENANCE_ATTRS && continue
        print(io, " ", n, "=", repr(getproperty(ex, n)))
    end
    println(io)
    if !is_leaf(ex) && maxdepth > 0
        for c in children(ex)
            print_tree(io, c; maxdepth = maxdepth - 1, _indent = _indent * "  ")
        end
    end
    nothing
end
print_tree(ex::Tree; kws...) = print_tree(stdout, ex; kws...)

"Preorder fold over the node-reference projection: `acc = f(acc, node)`."
function foldtree(f, acc, ex::Tree)
    acc = f(acc, ex)
    if !is_leaf(ex)
        for c in children(ex)
            acc = foldtree(f, acc, c)
        end
    end
    return acc
end

"Preorder traversal: `f(node)` on every node."
traverse(f, ex::Tree) = (foldtree((_, n) -> (f(n); nothing), nothing, ex); nothing)

# ---------------------------- structural equality --------------------------

"Leaf payload comparison hook for `≈`. The payload convention is
consumer-specific (doc §3.7 non-goal): the AST dialect compares
`:value`/`:name_val` columns; the default compares nothing beyond kind."
payloads_isequal(ex1::Tree, ex2::Tree) = true

function Base.:≈(ex1::Tree, ex2::Tree)
    if rawkind(ex1) != rawkind(ex2) || is_leaf(ex1) != is_leaf(ex2)
        return false
    end
    if is_leaf(ex1)
        return payloads_isequal(ex1, ex2)
    else
        if numchildren(ex1) != numchildren(ex2)
            return false
        end
        return all(c1 ≈ c2 for (c1, c2) in zip(children(ex1), children(ex2)))
    end
end

# --------------------------------- lists -----------------------------------

"""
    NodeList{G, V} <: AbstractVector{Tree{G}}

Lightweight vector of node ids with the graph stored separately
(`SyntaxList{Attrs, V} = NodeList{SyntaxGraph{Attrs}, V}`). The element type
is `Tree{G}`, so `NodeList{SyntaxGraph{A}} <: AbstractVector{<:SyntaxTree}` —
consumer dispatch on tree-vector element types keeps working.
"""
struct NodeList{G, V} <: AbstractVector{Tree{G}}
    graph::G
    ids::V
end

NodeList(g, ids::AbstractVector{<:Integer}) = NodeList{typeof(g), typeof(ids)}(g, ids)
NodeList(g) = NodeList(g, Vector{Int}())
NodeList(st::Tree, rest::Tree...) = NodeList(getfield(st, :_graph), tree_ids(st, rest...))

tree_ids(sts::Tree...) = Int[getfield(st, :_id) for st in sts]

syntax_graph(lst::NodeList) = lst.graph

setchildren!(g, id::Integer, children::NodeList) = setchildren!(g, id, children.ids)
setchildren!(g::AttrGraph, id::Integer, children::NodeList) =
    _setchildren_impl!(g, id, children.ids)   # disambiguator

Base.size(v::NodeList) = size(v.ids)
Base.IndexStyle(::Type{<:NodeList}) = IndexLinear()
Base.getindex(v::NodeList, i::Int) = Tree(v.graph, v.ids[i])
Base.getindex(v::NodeList, r::UnitRange) = NodeList(v.graph, view(v.ids, r))

function Base.setindex!(v::NodeList, ex::Tree, i::Int)
    check_compatible_graph(v, ex)
    v.ids[i] = getfield(ex, :_id)
end
Base.setindex!(v::NodeList, id::Integer, i::Int) = (v.ids[i] = id)

function Base.push!(v::NodeList, ex::Tree)
    check_compatible_graph(v, ex)
    push!(v.ids, getfield(ex, :_id))
    v
end
Base.push!(v::NodeList, id::Integer) = push!(v.ids, id)

function Base.pushfirst!(v::NodeList, ex::Tree)
    check_compatible_graph(v, ex)
    pushfirst!(v.ids, getfield(ex, :_id))
    v
end

Base.similar(v::NodeList, size::Tuple=Base.size(v.ids)) = NodeList(v.graph, zeros(Int, size))
Base.isassigned(v::NodeList, i::Integer) = v.ids[i] > 0

function Base.append!(v::NodeList, exs)
    for e in exs
        push!(v, e)
    end
    v
end
function Base.append!(v::NodeList, exs::NodeList)
    check_compatible_graph(v, exs)
    append!(v.ids, exs.ids)
    v
end

Base.pop!(v::NodeList) = Tree(v.graph, pop!(v.ids))
Base.popfirst!(v::NodeList) = Tree(v.graph, popfirst!(v.ids))
Base.popat!(v::NodeList, i::Integer) = Tree(v.graph, popat!(v.ids, i))
Base.insert!(v::NodeList, i::Integer, st::Tree) = (insert!(v.ids, i, getfield(st, :_id)); v)
Base.resize!(v::NodeList, n) = (resize!(v.ids, n); v)
Base.empty!(v::NodeList) = (empty!(v.ids); v)
Base.deleteat!(v::NodeList, inds) = (deleteat!(v.ids, inds); v)
Base.copy(v::NodeList) = NodeList(v.graph, copy(v.ids))

function Base.filter(f, exs::NodeList)
    out = NodeList(syntax_graph(exs))
    for ex in exs
        if f(ex)
            push!(out, ex)
        end
    end
    out
end

"Map over a NodeList producing a NodeList (of trees in the same graph)."
function mapsyntax(f, exs::NodeList)
    out = NodeList(syntax_graph(exs))
    for ex in exs
        push!(out, f(ex))
    end
    out
end

"Index each tree of the list at `i`, producing a NodeList."
function mapindex(sl::NodeList, i::Int)
    out = NodeList(syntax_graph(sl))
    for st in sl
        push!(out, getindex(st, i))
    end
    out
end

# ------------------------------- construction ------------------------------

"Kind-typed value of a kind argument (Level 2: one Kind type end to end;
integers convert for raw/registry-free graph use)."
kind_bits(k::Kind) = k
kind_bits(k::Integer) = Kind(k)

"""
    newleaf(g, prov, k)

Add a fresh leaf node with kind `k` and provenance `prov` (a `Tree` records
its node id; anything else — a consumer source reference — is stored as-is).
The kind is written to the core kind column (for graphs exposing `:kind` as
an attribute column, that column is a view of the same storage).
"""
function newleaf(g, prov, k)
    st = Tree(g, new_id!(g))
    setrawkind!(st, kind_bits(k))
    setattr!(st, :source, prov isa Tree ? getfield(prov, :_id) : prov)
end

"""
    newnode(g, prov, k, children)

Add a fresh node with kind `k`, provenance `prov`, and the given children.
"""
function newnode(g, prov, k, children)
    st = newleaf(g, prov, k)
    setchildren!(g, getfield(st, :_id), children)
    return st
end

"""
    mkleaf(old::Tree)

Copy-on-write leaf: a fresh node in `old`'s graph carrying `old`'s kind and
attributes, with `old` as its provenance. With `mknode`, the primitive every
lowering transformation bottoms out in.
"""
function mkleaf(old::Tree)
    graph = syntax_graph(old)
    st = Tree(graph, new_id!(graph))
    copy_attrs!(st, old)
    setattr!(st, :source, getfield(old, :_id))
end

"""
    mknode(old::Tree, children)

Create a node in `old`'s graph that is an immutable update of `old`, but
setting `old` as its provenance. This is the main operation used by syntax
transformations such as lowering.
"""
function mknode(old::Tree, children)
    st = mkleaf(old)
    setchildren!(getfield(st, :_graph), getfield(st, :_id), children)
    return st
end

"Recursively `mknode`/`mkleaf` a whole tree."
function mktree(old::Tree)
    if is_leaf(old)
        mkleaf(old)
    else
        cs = mapsyntax(mktree, children(old))
        mknode(old, cs)
    end
end

"""
    copy_attrs!(dest::Tree, src::Tree)

Copy `src`'s attributes (except the provenance links) and its core kind onto
`dest` (possibly in another graph).
"""
function copy_attrs!(dest, src)
    # the core kind column travels too (a consumer may or may not also
    # expose it as an attribute column; the double write is idempotent)
    setrawkind!(dest, rawkind(src))
    sg = syntax_graph(src)
    sid = getfield(src, :_id)
    for name in attrnames(sg)
        name in PROVENANCE_ATTRS && continue
        attr = getattr(sg, name)
        if haskey(attr, sid)
            setattr!(dest, name, attr[sid])
        end
    end
end

# ------------------------- traversal and rewriting -------------------------

"""
    mapchildren(f, ctx, ex::Tree)

Map `f` over `ex`'s children, immutable-updating `ex` iff any child changed
(allocation-free when nothing changes).
"""
function mapchildren(f::Function, ctx, ex::Tree)
    if is_leaf(ex)
        return ex
    end
    orig_children = children(ex)
    cs = nothing
    for (i, e) in enumerate(orig_children)
        newchild = f(e)
        if isnothing(cs)
            if newchild == e
                continue
            else
                cs = NodeList(syntax_graph(ctx))
                append!(cs, orig_children[1:i-1])
            end
        end
        push!(cs::NodeList, newchild)
    end
    if isnothing(cs)
        # This function should be allocation-free if no children were changed
        # by the mapping
        return ex
    end
    cs::NodeList
    ex2 = mknode(ex, cs)
    return ex2
end

"""
Recursively copy tree `ex` into `ctx`'s graph. Every node in `ex` is copied
at most once (DAG-preserving); provenance links (`:source`/`:macro_source`)
are followed and copied.
"""
function copy_ast(ctx, ex::Tree)
    graph1 = syntax_graph(ex)
    graph2 = syntax_graph(ctx)
    @assert graph1 !== graph2 "use mktree(ex) for this"
    id2 = _copy_ast(graph2, graph1, getfield(ex, :_id), Dict{Int,Int}())
    return Tree(graph2, id2)
end

function _copy_ast(graph2, graph1, id1::Integer, seen)
    id1 = Int(id1)
    let copied = get(seen, id1, nothing)
        isnothing(copied) || return copied
    end
    id2 = new_id!(graph2)
    seen[id1] = id2
    if !is_leaf(graph1, id1)
        cs = Int[]
        for cid in children(graph1, id1)
            push!(cs, _copy_ast(graph2, graph1, cid, seen))
        end
        setchildren!(graph2, id2, cs)
    end
    for src_attr in PROVENANCE_ATTRS
        src1 = get(Tree(graph1, id1), src_attr, nothing)
        if src1 isa Int
            src2 = _copy_ast(graph2, graph1, src1, seen)
            setattr!(graph2, id2, src_attr, src2)
        elseif src_attr === :source
            setattr!(graph2, id2, src_attr, src1)
        end
    end
    copy_attrs!(Tree(graph2, id2), Tree(graph1, id1))
    return id2
end

"Reparent `ex` onto `ctx`'s graph handle (same underlying storage required)."
function reparent(ctx, ex::Tree)
    graph = syntax_graph(ctx)
    @assert substrate(graph).ops === substrate(getfield(ex, :_graph)).ops
    Tree(graph, getfield(ex, :_id))
end

# -------------------------------- provenance -------------------------------

# The provenance walk (§3.7 Level 2 — graph-qualified provenance). A chain
# value (`:source` attribute) is one of:
#   (a) a node id (Int)     — a node in the SAME graph (the classic case);
#   (b) a `Tree` cursor     — a graph-qualified reference: hop into the
#                             cursor's graph and continue the walk there
#                             (how an IR statement's :source column reaches
#                             back into the syntax graph with zero seams);
#   (c) anything else       — an opaque terminal (a consumer source
#                             reference: SourceRef, LineNumberNode, ...).

"One provenance step of the chain (hop-aware); `st` itself if terminal."
function prov(st::Tree)
    s = st.source
    s isa Int && return Tree(getfield(st, :_graph), s)
    s isa Tree && return s
    return st
end

"""
    provenance(st::Tree)

The `:source` chain of `st`: `[st.source, st.source.source, ...]` while the
values are provenance links — node ids in the same graph, or `Tree` cursors
hopping into another graph (an IR statement's chain starts with a hop into
the syntax graph and continues there). The returned list lives in the graph
where the chain runs; a cross-graph hop after entries have accumulated ends
the list (use `prov_end`/`provenance_terminal` to follow chains all the way).
"""
function provenance(st::Tree)
    g = getfield(st, :_graph)
    out = NodeList(g)
    s = st.source
    while true
        if s isa Int
            t = Tree(g, s)
        elseif s isa Tree
            t = s
            g2 = getfield(t, :_graph)
            if g2 !== g
                isempty(out.ids) || return out   # boundary after entries: stop the list
                g = g2
                out = NodeList(g)
            end
        else
            return out
        end
        push!(out, t)
        s = t.source
    end
end

"The end of `st`'s `:source` chain — the node (in whichever graph the chain
ends) whose source is neither a node id nor a graph-qualified cursor."
function prov_end(st::Tree)
    while true
        s = st.source
        if s isa Int
            st = Tree(getfield(st, :_graph), s)
        elseif s isa Tree
            st = s
        else
            return st
        end
    end
end

"The terminal (case (c)) `:source` value of `st`'s chain, across graph hops.
The porcelain does not know source-reference types; consumers wrap this
(e.g. JuliaSyntax's `sourceref` asserts `SourceRef`/`LineNumberNode`). This
is the walk that gives IR diagnostics surface-text highlighting: IR stmt →
syntax node → syntax chain → source reference."
provenance_terminal(st::Tree) = prov_end(st).source

# ---------------------------------------------------------------------------
# IR as a tree-viewable graph (§3.7): an AST is a floating body in a dialect
# where every operand is a node reference; the cursor/porcelain above works
# on IR rows through the STMT-tagged projection of the operand list (CONST/
# IMM/GLOBAL/SPARAM operands are non-node payload the projection ignores;
# REGION/BLOCK operands are control structure, likewise skipped).
# ---------------------------------------------------------------------------

syntax_graph(ir::IR) = ir
substrate(ir::IR) = getfield(ir.body, :graph)

"The node-reference projection of a statement's operands."
function children(ir::IR, id::Integer)
    s = StmtId(Int32(id))
    n = nops(ir, s)
    out = Int[]
    for i in 1:n
        o = getop(ir, s, i)
        optag(o) == TAG_STMT && push!(out, Int(asstmt(o).id))
    end
    return out
end
numchildren(ir::IR, id::Integer) = length(children(ir, id))
is_leaf(ir::IR, id::Integer) = numchildren(ir, id) == 0

"Fresh leaf row through the tree view. Legal while the body is unordered
(builder/floating): appends the row with neutral IR-only columns."
function new_id!(ir::IR)
    check_state(ir, (LAYOUT_BUILDER, LAYOUT_FLOATING), "new_id! (tree view)")
    body = ir.body
    id = newrow!(getfield(body, :graph), KIND_UNSET, OPS_LEAF)
    push!(body.type, Any)
    push!(body.flag, UInt32(0))
    push!(body.debug, (Int32(0), Int32(0), Int32(0)))
    push!(body.region, root_region(ir))
    ir.cache.stmt_epoch += 1
    return Int(id)
end

"Tree-view child store: all operands become STMT-tagged node references
(the AST-dialect case where the projection is total)."
function setchildren!(ir::IR, id::Integer, ids::AbstractVector{<:Integer})
    check_state(ir, (LAYOUT_BUILDER, LAYOUT_FLOATING), "setchildren! (tree view)")
    store_row_ops!(substrate(ir), id, Operand[op_stmt(StmtId(Int32(c))) for c in ids])
    ir.cache.stmt_epoch += 1
    return nothing
end

getattr(ir::IR, name::Symbol) = _ir_attrcol(ir.body.cols, name)
hasattr(ir::IR, name::Symbol) = _ir_hasattr(ir.body.cols, name)
attrnames(ir::IR) = _ir_attrnames(ir.body.cols)
_ir_attrcol(d::DictColumns, name::Symbol) = d.cols[name]
_ir_attrcol(nt::NamedTuple, name::Symbol) = getfield(nt, name)
_ir_hasattr(d::DictColumns, name::Symbol) = haskey(d.cols, name)
_ir_hasattr(nt::NamedTuple, name::Symbol) = haskey(nt, name)
_ir_attrnames(d::DictColumns) = keys(d.cols)
_ir_attrnames(nt::NamedTuple) = keys(nt)

# SparseCol is Dict-shaped for the porcelain (it already indexes by Integer)
Base.haskey(c::SparseCol, k::Integer) = haskey(c.data, Int32(k))
Base.get(c::SparseCol, k::Integer, default) = get(c.data, Int32(k), default)
Base.get(f::Base.Callable, c::SparseCol, k::Integer) = get(f, c.data, Int32(k))

# ---------------------------------------------------------------------------
# AST-lifetime GC (§3.7 Level 2 step 3): the CodeInstance-finalization policy
# prototype — collect syntax nodes no surviving provenance mentions.
# ---------------------------------------------------------------------------

"""
    collect_syntax!(graph, live_irs; policy = :conservative, extra_roots = ()) -> remap

Garbage-collect the syntax `graph` against the provenance of `live_irs`: the
root set is the union of `:source` cursor ids across ALL handed-in IRs
(multiple function bodies routinely share one lowering graph), plus
`extra_roots` (module-level trees the caller keeps), closed over the
`:source`/`:macro_source` chains. Runs `compact_graph!`, then sweeps the
handed-in IRs' `:source` columns rewriting cursor ids through the remap in
place (graph identity unchanged, values stay cursors) — every provenance
walk from a live statement is byte-identical before and after.

Policies:
  - `:conservative` (default) — keep full provenance chains, including all
    macro-expansion frames;
  - `:prune` — shorten `:source` chains through dead frame-less intermediate
    nodes first (`sourceref`- and macro-frame-preserving; the `prune` policy
    of JuliaSyntax expressed on the substrate), then collect. Reclaims much
    more; keeps terminals and macro frames, drops intermediate rewrite trees.

THE STALENESS CONTRACT (design question #9's node-remap subscription): being
in `live_irs` IS the subscription. Cursor columns of handed-in IRs are
rewritten through the remap; ANY other holder of node ids or cursors into
this graph — an IR not handed in, a saved `Tree`, a NodeList — is stale
after collection, exactly as stale `StmtId`s after `compact!` (the RemapSet
discipline): translate through the returned remap or re-derive.
"""
function collect_syntax!(graph, live_irs; policy::Symbol = :conservative,
                         extra_roots = ())
    policy === :conservative || policy === :prune ||
        throw(ArgumentError("collect_syntax!: unknown policy $policy (use :conservative or :prune)"))
    sub = substrate(graph)

    # Root set: union of :source cursor ids across the handed-in IRs
    # (cursors into other graphs are not this collection's roots).
    roots = Int[]
    for ir in live_irs
        hasattr(ir, :source) || continue
        col = getattr(ir, :source)
        for (_, v) in col
            v isa Tree || continue
            substrate(getfield(v, :_graph)).ops === sub.ops || continue
            push!(roots, getfield(v, :_id))
        end
    end
    for r in extra_roots
        push!(roots, r isa Tree ? getfield(r, :_id) : Int(r))
    end

    policy === :prune && _shorten_provenance_chains!(graph, roots)

    is_ref(name, v) = name in PROVENANCE_ATTRS && v isa Int
    remap = compact_graph!(sub, roots;
        attr_refs  = (n, v) -> is_ref(n, v) ? (v,) : nothing,
        remap_attr = (n, v, m) -> is_ref(n, v) ? Int(m[v]) : v)

    # The subscription sweep: rewrite the handed-in IRs' cursors in place.
    for ir in live_irs
        hasattr(ir, :source) || continue
        col = getattr(ir, :source)
        for k in collect(keys(col))
            v = col[k]
            v isa Tree || continue
            g = getfield(v, :_graph)
            substrate(g).ops === sub.ops || continue
            nid = remap[getfield(v, :_id)]
            nid == 0 && error("collect_syntax!: live cursor's node was collected (root marking bug)")
            col[k] = Tree(g, Int(nid))
        end
    end
    return remap
end

# The :prune policy's chain shortening: compute the live closure (roots +
# children + :macro_source targets + :source-chain nodes bearing
# :macro_source — provenance FRAMES, which macro_prov/unexpanded_sourceref
# walk and debug info depends on), then re-point live nodes' :source through
# dead frame-less intermediates (sourceref-preserving by construction).
function _shorten_provenance_chains!(graph, roots)
    sub = substrate(graph)
    hasattr(graph, :source) || return nothing
    srccol = getattr(graph, :source)
    msrccol = hasattr(graph, :macro_source) ? getattr(graph, :macro_source) : nothing
    n = nnodes(sub)
    live = falses(n)
    stack = Int[]
    function mark!(id::Int)
        if 1 <= id <= n && !live[id]
            live[id] = true
            push!(stack, id)
        end
        nothing
    end
    for r in roots
        mark!(Int(r))
    end
    while !isempty(stack)
        id = pop!(stack)
        for c in child_ids(sub, id)
            mark!(Int(c))
        end
        if msrccol !== nothing
            m = get(msrccol, id, nothing)
            m isa Int && mark!(m)
        end
        s = get(srccol, id, nothing)
        while s isa Int && !live[s]
            if msrccol !== nothing && haskey(msrccol, s)
                mark!(s)          # provenance frame: keep
                break
            end
            s = get(srccol, s, nothing)
        end
    end
    for id in 1:n
        live[id] || continue
        s = get(srccol, id, nothing)
        s === nothing && continue
        while s isa Int && !live[s]
            s = srccol[s]
        end
        srccol[id] = s
    end
    return nothing
end

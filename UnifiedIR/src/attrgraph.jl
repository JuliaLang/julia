# AttrGraph: THE shared storage core of §3.7 Level 1 — one data structure
# under both JuliaSyntax's SyntaxGraph and UnifiedIR's statement table:
#
#     SyntaxGraph = AttrGraph + tree conventions
#     UnifiedIR   = AttrGraph + core columns (type/flag/debug/region) +
#                   constant/global pools + regions + layout states
#
# Fields (row-indexed; identical names to the historical IRBody columns so
# IRBody forwards to them directly):
#   len      — row counter (authoritative for the IR handle owner; graph-mode
#              sibling handles created by `with_cols` share the storage
#              vectors and derive counts via `nnodes` = vector length)
#   kind     — the kind core column: the ONE `Kind` type of the shared
#              §3.4 registry (Level 2: syntax and IR kinds are dialects of
#              one numbering space).
#   ops      — the §3.2 two-mode packed word per row: pool range
#              [0|offset:39|len:24] or inline [1|arity:2|...]. Graph rows use
#              pool mode; a LEAF (never-set children, distinct from an empty
#              child list) is the inline-mode word with zero arity: "no
#              operand storage at all" (OPS_LEAF). IR rows never produce a
#              zero-arity inline word, so the vocabularies coexist.
#   operands — the shared tagged pool (§3.2). AST edges are stored as
#              STMT-tagged words — the plain node-reference tag case of the
#              pool word; `child_ids`/`edges_view` decode the payloads.
#   cols     — open attribute columns through the §3.5 machinery
#              (Dict-of-columns dynamic mode, or a frozen NamedTuple).
#
# `compact_graph!` is compact!-as-GC over a root set, sharing the
# pool-rebuild helper (`append_remapped_range!`) and the Dict-column
# permutation machinery with `compact!`.

"Sentinel for a row whose kind has not been set (kind is sparse in the graph
API). The top dialect id is never allocated (kinds.jl), so this bit pattern
is never a valid kind."
const KIND_UNSET = Base.bitcast(Kind, typemax(UInt16))

"Graph LEAF marker: the inline-mode ops word with zero arity — no operand
storage at all, distinct from an empty pool range (= empty child list).
IR statements never use zero-arity inline words (`ops_inline` requires
arity ≥ 1), so this does not collide with statement encodings."
const OPS_LEAF = OPS_INLINE_BIT

# ---------------------------------------------------------------------------
# AttrGraph
# ---------------------------------------------------------------------------

"""
    AttrGraph{Cols}

The Level-1 shared storage core (§3.7): row table (`kind` column + packed
`ops` words), the shared tagged operand pool, and open attribute columns
`Cols` (§3.5). `IRBody` composes it (plus IR-only columns and pools);
`SyntaxGraph` wraps it directly (plus tree conventions).

`with_cols` returns a sibling handle over the same storage with a different
column set (the frozen/unfrozen transition of SyntaxGraph).
"""
mutable struct AttrGraph{Cols}
    len::Int32
    kind::Vector{Kind}
    ops::Vector{UInt64}
    operands::Vector{Operand}
    cols::Cols
end

AttrGraph(cols) = AttrGraph{typeof(cols)}(0, Kind[], UInt64[], Operand[], cols)
AttrGraph() = AttrGraph(Dict{Symbol,Any}())

"Sibling handle sharing row/pool storage, with a different column set."
with_cols(g::AttrGraph, cols) =
    AttrGraph{typeof(cols)}(g.len, g.kind, g.ops, g.operands, cols)

"Row count, derived from storage (valid across sibling handles)."
nnodes(g::AttrGraph) = length(g.ops)

function Base.show(io::IO, g::AttrGraph)
    print(io, "AttrGraph(", nnodes(g), " rows, ", length(g.operands), " pool words)")
end

# ---------------------------------------------------------------------------
# The shared row-append primitive
# ---------------------------------------------------------------------------

"""
    newrow!(g, kind, opsword) -> id

THE row-append primitive: push a row with the given kind and packed ops
word, bump `len`, and grow any dense extension columns (§3.5 hooks). IR's
`_append_row!` and the graph's `newnode!` both delegate here.
"""
function newrow!(g::AttrGraph, kind::Kind, opsword::UInt64)
    push!(g.kind, kind)
    push!(g.ops, opsword)
    g.len += Int32(1)
    n = length(g.ops)
    _grow_attr_cols!(g.cols, n)
    return n
end

"Append a fresh leaf node with unset kind; returns its id."
newnode!(g::AttrGraph) = newrow!(g, KIND_UNSET, OPS_LEAF)

# Sparse (Dict-shaped) columns need no per-row growth; a static NamedTuple
# universe is grown through the §3.5 hooks (unrolled, no-ops for sparse cols).
_grow_attr_cols!(cols::AbstractDict, n::Integer) = nothing
_grow_attr_cols!(cols, n::Integer) = grow_cols!(cols, n, n - 1)
col_grow!(::AbstractDict, n::Integer, oldlen::Integer) = nothing

node_kind(g::AttrGraph, id::Integer) = g.kind[id]
set_node_kind!(g::AttrGraph, id::Integer, k::Kind) = (g.kind[id] = k; k)

# ---------------------------------------------------------------------------
# The shared pool-write family (store_ops! discipline, §3.2)
# ---------------------------------------------------------------------------

"""
    pool_append!(g, ops) -> ops word

Append an operand list to the shared pool and return its pool-mode word
(`OPS_EMPTY` for an empty list). The low-level append used by IR's
`encode_ops!` and the graph's `setchildren!` growth path.
"""
function pool_append!(g::AttrGraph, ops)::UInt64
    len = length(ops)
    len == 0 && return OPS_EMPTY
    off = length(g.operands)
    append!(g.operands, ops)
    return ops_pool(off, len)
end

"""
    store_row_ops!(g, id, ops)

Store a fresh (pool-mode) operand list for row `id` with the same-or-smaller
in-place reuse discipline (§3.2): a list no longer than the row's existing
pool range overwrites it in place; growth appends a fresh range and orphans
the old slots (reclaimed at the compaction points).
"""
function store_row_ops!(g::AttrGraph, id::Integer, ops::AbstractVector{Operand})
    len = length(ops)
    w = g.ops[id]
    if !is_ops_inline(w) && Int(ops_len(w)) >= len
        off = Int(ops_offset(w))
        for i in 1:len
            g.operands[off + i] = ops[i]
        end
        g.ops[id] = ops_pool(off, len)
    else
        g.ops[id] = pool_append!(g, ops)
    end
    return nothing
end

# ---------------------------------------------------------------------------
# Tree conventions: children as STMT-tagged pool words
# ---------------------------------------------------------------------------

_edge_word(c::Integer) = op_stmt(StmtId(Int32(c)))
_edge_id(o::Operand) = Int(asstmt(o).id)

is_leaf(g::AttrGraph, id::Integer) = is_ops_inline(g.ops[id])

function numchildren(g::AttrGraph, id::Integer)
    w = g.ops[id]
    is_ops_inline(w) ? 0 : Int(ops_len(w))
end

"""
    children_range(g, id) -> UnitRange{Int}

The node's range into the shared pool. Leaves report `0:-1` (the SyntaxGraph
leaf convention: `first == 0` marks a leaf, distinct from an empty child
list whose range is `(off+1):off` with `off ≥ 0`).
"""
function children_range(g::AttrGraph, id::Integer)
    w = g.ops[id]
    is_ops_inline(w) && return 0:-1
    off = Int(ops_offset(w))
    return (off + 1):(off + Int(ops_len(w)))
end

"""
    setchildren!(g, id, children)

Store the node's child list as STMT-tagged words in the shared pool. This is
the streaming form of `store_row_ops!` (same same-or-smaller reuse
discipline, encoding child ids to tagged words in place instead of
materializing an operand list).
"""
setchildren!(g::AttrGraph, id::Integer, children) = _setchildren_impl!(g, id, children)
# (explicit method: disambiguates against the generic tree-porcelain default
# `setchildren!(g, id, ids::AbstractVector{<:Integer})` in tree.jl)
setchildren!(g::AttrGraph, id::Integer, children::AbstractVector{<:Integer}) =
    _setchildren_impl!(g, id, children)

function _setchildren_impl!(g::AttrGraph, id::Integer, children)
    len = length(children)
    w = g.ops[id]
    if !is_ops_inline(w) && Int(ops_len(w)) >= len
        off = Int(ops_offset(w))              # same-or-smaller: reuse in place
    else
        off = length(g.operands)              # growth: append, orphan old range
        resize!(g.operands, off + len)
    end
    i = 0
    for c in children
        i += 1
        g.operands[off + i] = _edge_word(c)
    end
    g.ops[id] = ops_pool(off, len)
    return nothing
end

"""
    set_children_range!(g, id, r)

Point the node at an explicit pool range (`0:-1` = leaf). Escape hatch for
consumers that lay out the pool themselves (e.g. SyntaxGraph's `prune`).
"""
function set_children_range!(g::AttrGraph, id::Integer, r::UnitRange{<:Integer})
    g.ops[id] = first(r) == 0 ? OPS_LEAF : ops_pool(Int(first(r)) - 1, length(r))
    return g
end

function child_id(g::AttrGraph, id::Integer, i::Integer)
    r = children_range(g, id)
    return _edge_id(g.operands[r[i]])
end

# ---------------------------------------------------------------------------
# Views: node-id-decoding pool view and the edge_ranges view. Both are
# immutable wrappers of the shared storage vectors, so two views over the
# same substrate are `===` (egal) — preserving SyntaxGraph's identity-based
# graph checks (`is_compatible_graph`, `reparent`).
# ---------------------------------------------------------------------------

"NodeId-decoding read/write view over the tagged pool (SyntaxGraph's `edges`)."
struct EdgeView <: AbstractVector{Int}
    operands::Vector{Operand}
end

Base.size(v::EdgeView) = (length(v.operands),)
Base.IndexStyle(::Type{EdgeView}) = IndexLinear()
@inline Base.getindex(v::EdgeView, i::Int) = _edge_id(v.operands[i])
@inline Base.setindex!(v::EdgeView, x::Integer, i::Int) =
    (v.operands[i] = _edge_word(x); v)
Base.push!(v::EdgeView, x::Integer) = (push!(v.operands, _edge_word(x)); v)
Base.resize!(v::EdgeView, n::Integer) = (resize!(v.operands, n); v)

edges_view(g::AttrGraph) = EdgeView(g.operands)

child_ids(g::AttrGraph, id::Integer) = view(edges_view(g), children_range(g, id))

"Vector{UnitRange{Int}}-shaped read/write view over the packed ops words
(SyntaxGraph's `edge_ranges`)."
struct EdgeRangesView <: AbstractVector{UnitRange{Int}}
    ops::Vector{UInt64}
end

Base.size(v::EdgeRangesView) = (length(v.ops),)
Base.IndexStyle(::Type{EdgeRangesView}) = IndexLinear()

@inline function Base.getindex(v::EdgeRangesView, id::Int)
    w = v.ops[id]
    is_ops_inline(w) && return 0:-1
    off = Int(ops_offset(w))
    return (off + 1):(off + Int(ops_len(w)))
end

@inline function Base.setindex!(v::EdgeRangesView, r::UnitRange{<:Integer}, id::Int)
    v.ops[id] = first(r) == 0 ? OPS_LEAF : ops_pool(Int(first(r)) - 1, length(r))
    return v
end

edge_ranges_view(g::AttrGraph) = EdgeRangesView(g.ops)

# ---------------------------------------------------------------------------
# Attribute columns (§3.5 dynamic mode conveniences)
# ---------------------------------------------------------------------------

"Column marked core-backed (a view over a core column) is skipped by column
bookkeeping — the core column is compacted/grown directly."
is_core_attr(@nospecialize(col)) = false

foreachcol(f, cols::AbstractDict{Symbol}) = (for (name, c) in cols; f(name, c); end)

# @noinline works around codegen producing a trampoline for `getindex`
# (carried over from JuliaSyntax's Dict-mode getattr)
@noinline getattrcol(g::AttrGraph{<:AbstractDict{Symbol}}, name::Symbol) = g.cols[name]
getattrcol(g::AttrGraph{<:NamedTuple}, name::Symbol) = getfield(g.cols, name)
hasattrcol(g::AttrGraph{<:AbstractDict{Symbol}}, name::Symbol) = haskey(g.cols, name)
hasattrcol(g::AttrGraph{<:NamedTuple}, name::Symbol) = haskey(g.cols, name)

"Ensure a Dict-mode attribute column exists (`mk()` constructs the container)."
ensure_attrcol!(g::AttrGraph{<:AbstractDict{Symbol}}, name::Symbol,
                mk = () -> Dict{Int,Any}()) = get!(mk, g.cols, name)

delete_attrcol!(g::AttrGraph{<:AbstractDict{Symbol}}, name::Symbol) = delete!(g.cols, name)

setattrnode!(g::AttrGraph, id::Integer, name::Symbol, @nospecialize(v)) =
    (getattrcol(g, name)[Int(id)] = v)
getattrnode(g::AttrGraph, id::Integer, name::Symbol, default) =
    get(getattrcol(g, name), Int(id), default)
delattrnode!(g::AttrGraph, id::Integer, name::Symbol) =
    delete!(getattrcol(g, name), Int(id))

# Dict-shaped columns participate in the standard §3.5 compaction protocol
# too (a Dict column in an IR universe compacts like SparseCol does).
function col_compact!(c::AbstractDict{<:Integer}, old_of_new::Vector{Int32})
    new_of_old = Dict{Int,Int}(Int(old_of_new[i]) => i for i in 1:length(old_of_new))
    entries = collect(c)
    empty!(c)
    for (k, v) in entries
        nk = get(new_of_old, Int(k), 0)
        nk != 0 && (c[nk] = v)
    end
    return c
end

# ---------------------------------------------------------------------------
# Shared pool-rebuild helper (used by compact! and compact_graph!)
# ---------------------------------------------------------------------------

"""
    append_remapped_range!(newpool, oldpool, w, f) -> new ops word

Copy the pool range of word `w` from `oldpool` into `newpool`, mapping each
word through `f`; returns the rebuilt pool-mode word. The pool-rebuild step
shared by the two compaction points.
"""
function append_remapped_range!(newpool::Vector{Operand}, oldpool::Vector{Operand},
                                w::UInt64, f)::UInt64
    len = Int(ops_len(w))
    len == 0 && return OPS_EMPTY
    off = length(newpool)
    o0 = Int(ops_offset(w))
    for j in 1:len
        push!(newpool, f(oldpool[o0 + j]))
    end
    return ops_pool(off, len)
end

# ---------------------------------------------------------------------------
# compact_graph!: compact!-as-GC (§3.7)
# ---------------------------------------------------------------------------

"""
    compact_graph!(g, roots; attr_refs=nothing, remap_attr=nothing) -> Vector{Int32}

Garbage-collect the graph against a root set: drop every node unreachable
from `roots`, renumber survivors densely (relative order preserved), rewrite
the shared pool (orphaned ranges from `setchildren!` growth are reclaimed
too), and remap every attribute column — the RemapSet discipline of
`compact!` specialized to the single node namespace. Returns the remap
vector (`map[oldid] == newid`, `0` = dropped).

Reachability follows child edges. Attribute values may embed node ids (e.g.
a provenance attribute); declare those with `attr_refs(name, value)`,
returning an iterable of referenced ids (or `nothing`) — they are traced as
GC edges and rewritten via `remap_attr(name, value, map) -> newvalue`.

Mutates the shared storage in place, so sibling views (`with_cols`) observe
the compacted graph; call it on the view whose column set is complete
(columns absent from `g.cols` but present on siblings are NOT remapped).
Node ids held outside the graph are stale afterwards — translate them
through the returned map.
"""
function compact_graph!(g::AttrGraph, roots;
                        attr_refs = nothing, remap_attr = nothing)
    n = nnodes(g)
    live = falses(n)
    stack = Int[]
    function mark!(id::Int)
        (1 <= id <= n) || throw(ArgumentError("node id $id out of range 1:$n"))
        if !live[id]
            live[id] = true
            push!(stack, id)
        end
    end
    for r in roots
        mark!(Int(r))
    end
    while !isempty(stack)
        id = pop!(stack)
        for c in child_ids(g, id)
            mark!(Int(c))
        end
        if attr_refs !== nothing
            foreachcol(g.cols) do name, col
                is_core_attr(col) && return
                haskey(col, id) || return
                refs = attr_refs(name, col[id])
                refs === nothing && return
                for r in refs
                    mark!(Int(r))
                end
            end
        end
    end

    remap = zeros(Int32, n)
    nn = 0
    for i in 1:n
        if live[i]
            nn += 1
            remap[i] = nn
        end
    end

    # Rewrite core columns. old ids ascend with new ids and newid <= oldid, so
    # kind/ops rewrite forward in place; the pool is rebuilt densely through
    # the shared helper.
    newpool = Operand[]
    remap_edge = o -> begin
        m = remap[_edge_id(o)]
        m == 0 && error("compact_graph!: live node has dead child $(_edge_id(o))")
        _edge_word(m)
    end
    for old in 1:n
        newid = Int(remap[old])
        newid == 0 && continue
        w = g.ops[old]
        g.ops[newid] = is_ops_inline(w) ? w :
            append_remapped_range!(newpool, g.operands, w, remap_edge)
        g.kind[newid] = g.kind[old]
    end
    resize!(g.kind, nn)
    resize!(g.ops, nn)
    resize!(g.operands, length(newpool))
    copyto!(g.operands, newpool)
    g.len = Int32(nn)

    # Attribute columns: drop dead rows, rekey survivors, rewrite embedded refs.
    foreachcol(g.cols) do name, col
        is_core_attr(col) && return
        compact_attrcol!(col, remap, name, remap_attr)
    end
    return remap
end

function compact_attrcol!(col::AbstractDict, remap::Vector{Int32}, name::Symbol,
                          remap_attr)
    entries = collect(col)
    empty!(col)
    for (k, v) in entries
        nk = remap[Int(k)]
        nk == Int32(0) && continue
        col[Int(nk)] = remap_attr === nothing ? v : remap_attr(name, v, remap)
    end
    return col
end

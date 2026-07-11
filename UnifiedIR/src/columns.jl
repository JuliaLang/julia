# Extension columns: static universes + dispatch (§3.5).
#
# The column set is a type parameter of `IR` — a NamedTuple of column
# containers fixed per consumer. Column semantics attach by multiple dispatch
# on the column type. A Dict-backed dynamic container (`DictColumns`) is one
# legal parameter value, used by open-attribute consumers (lowering).

abstract type SemClass end
struct Semantic <: SemClass end
struct Annotation <: SemClass end
struct Derived <: SemClass end

# ------------------------- dispatch hooks (defaults) -----------------------

"Does this column type embed statement/region references? (must override remap_refs!)"
hasrefs(::Type) = false

"Remap embedded references after a renaming point. Mandatory iff hasrefs."
remap_refs!(col, rs) = hasrefs(typeof(col)) ?
    error("column $(typeof(col)) declares hasrefs but does not implement remap_refs!") : col

"Semantic class: Semantic() | Annotation() | Derived() (conservative default)."
semclass(::Type) = Derived()

"Optional refinement hook fired by splice_body! for each spliced statement."
on_splice!(col, args...) = nothing

# ------------------------- column containers -------------------------------

"""
    DenseCol{T}(default)

A dense per-statement column with a default for fresh rows.
"""
mutable struct DenseCol{T}
    data::Vector{T}
    default::T
end
DenseCol{T}(default) where {T} = DenseCol{T}(T[], convert(T, default))
DenseCol(default::T) where {T} = DenseCol{T}(T[], default)

Base.getindex(c::DenseCol, i::Integer) = c.data[i]
Base.getindex(c::DenseCol, s::StmtId) = c.data[s.id]
Base.setindex!(c::DenseCol, v, i::Integer) = (c.data[i] = v)
Base.setindex!(c::DenseCol, v, s::StmtId) = (c.data[s.id] = v)
Base.length(c::DenseCol) = length(c.data)

"""
    SparseCol{T}()

A sparse per-statement column (Dict-backed); absent rows read `nothing`.
"""
mutable struct SparseCol{T}
    data::Dict{Int32,T}
end
SparseCol{T}() where {T} = SparseCol{T}(Dict{Int32,T}())

Base.getindex(c::SparseCol, s::StmtId) = get(c.data, s.id, nothing)
Base.getindex(c::SparseCol, i::Integer) = get(c.data, Int32(i), nothing)
Base.setindex!(c::SparseCol, v, s::StmtId) = (c.data[s.id] = v)
Base.setindex!(c::SparseCol, v, i::Integer) = (c.data[Int32(i)] = v)
Base.haskey(c::SparseCol, s::StmtId) = haskey(c.data, s.id)
Base.delete!(c::SparseCol, s::StmtId) = delete!(c.data, s.id)

"""
    DictColumns()

The dynamic universe: an open set of named sparse columns. Deliberately
dynamic (lowering's macro-era attributes); hook calls through it are cold.
"""
mutable struct DictColumns
    cols::Dict{Symbol,SparseCol{Any}}
end
DictColumns() = DictColumns(Dict{Symbol,SparseCol{Any}}())

getcol!(d::DictColumns, name::Symbol) = get!(() -> SparseCol{Any}(), d.cols, name)
Base.haskey(d::DictColumns, name::Symbol) = haskey(d.cols, name)

# ------------------------- structural bookkeeping --------------------------
# What is automatic for all classes (§3.5): growth on insertion, permutation
# at compaction, reference remapping through hasrefs/remap_refs!, and
# column-wide conservative invalidation of Derived columns on semantic events.

col_grow!(c::DenseCol, n::Integer) = (resize!(c.data, max(length(c.data), n));
                                      for i in (length(c.data)-max(0, n-length(c.data))+1):n
                                          c.data[i] = c.default
                                      end)
function col_grow!(c::DenseCol, n::Integer, oldlen::Integer)
    resize!(c.data, n)
    for i in (oldlen+1):n
        c.data[i] = c.default
    end
end
col_grow!(::SparseCol, n::Integer, oldlen::Integer) = nothing
col_grow!(d::DictColumns, n::Integer, oldlen::Integer) = nothing

"Compact a column: `old_of_new[newid] = oldid`."
function col_compact!(c::DenseCol{T}, old_of_new::Vector{Int32}) where {T}
    c.data = T[c.data[old_of_new[i]] for i in 1:length(old_of_new)]
    return c
end
function col_compact!(c::SparseCol{T}, old_of_new::Vector{Int32}) where {T}
    new_of_old = Dict{Int32,Int32}(old_of_new[i] => Int32(i) for i in 1:length(old_of_new))
    nd = Dict{Int32,T}()
    for (k, v) in c.data
        nk = get(new_of_old, k, Int32(0))
        nk != 0 && (nd[nk] = v)
    end
    c.data = nd
    return c
end
function col_compact!(d::DictColumns, old_of_new::Vector{Int32})
    for c in values(d.cols)
        col_compact!(c, old_of_new)
    end
    return d
end

col_clear!(c::DenseCol) = (for i in 1:length(c.data); c.data[i] = c.default; end)
col_clear!(c::SparseCol) = empty!(c.data)
col_clear!(d::DictColumns) = foreach(col_clear!, values(d.cols))

"Conservative Derived invalidation (§3.5): column-wide clear on semantic events."
function invalidate_derived!(cols::NamedTuple)
    foreachcol(cols) do _, c
        semclass(typeof(c)) isa Derived && col_clear!(c)
    end
end
invalidate_derived!(d::DictColumns) = nothing  # dynamic mode: no class info; caller-managed

"Iterate (name, container) pairs of a universe."
function foreachcol(f, cols::NamedTuple)
    for name in keys(cols)
        f(name, cols[name])
    end
end
foreachcol(f, d::DictColumns) = (for (name, c) in d.cols; f(name, c); end)

function grow_cols!(cols, n::Integer, oldlen::Integer)
    foreachcol(cols) do _, c
        col_grow!(c, n, oldlen)
    end
end

# ---- staged column compaction (strong exception guarantee, §4.1) ----
# Renaming points must either complete or leave the IR logically unchanged, so
# column callbacks that can throw (`col_compact!`, `remap_refs!`) run against a
# staged copy of the universe; the copy is published only after every hook has
# succeeded.

stage_copy_cols(cols::NamedTuple) = map(stage_copy_col, cols)
stage_copy_cols(d::DictColumns) =
    DictColumns(Dict{Symbol,SparseCol{Any}}(k => stage_copy_col(v) for (k, v) in d.cols))
stage_copy_col(c::DenseCol{T}) where {T} = DenseCol{T}(copy(c.data), c.default)
stage_copy_col(c::SparseCol{T}) where {T} = SparseCol{T}(copy(c.data))
stage_copy_col(c) = deepcopy(c)   # user columns: conservative structural copy

"""
    compact_cols_staged(cols, old_of_new, rs) -> new cols

Compact and remap a staged copy of the universe, leaving `cols` untouched.
The caller publishes the result together with the rest of the compacted body
(§4.1 strong exception guarantee: a throwing column hook aborts the whole
renaming point with the IR logically unchanged).
"""
function compact_cols_staged(cols, old_of_new::Vector{Int32}, rs)
    newcols = stage_copy_cols(cols)
    foreachcol(newcols) do _, c
        col_compact!(c, old_of_new)
        hasrefs(typeof(c)) && remap_refs!(c, rs)
    end
    return newcols
end

# ------------------------- cross-universe conversion -----------------------

"""
    convert_universe(target_cols, ir) -> IR

Rebuild `ir` with the `target_cols` universe. Columns present in both (by
name) are copied; columns only in the target get defaults; columns only in
the source are dropped **only** when `drop` lists them — a hard error
otherwise (§3.5).
"""
function convert_universe(target_cols::NamedTuple, ir; drop::Tuple{Vararg{Symbol}} = ())
    src = ir.body.cols
    if src isa NamedTuple
        for name in keys(src)
            name in keys(target_cols) || name in drop ||
                error("convert_universe: source column :$name missing from target universe; list it in `drop` to drop explicitly")
        end
    end
    n = Int(ir.body.len)
    grow_cols!(target_cols, n, 0)
    if src isa NamedTuple
        for name in keys(src)
            name in keys(target_cols) || continue
            s = src[name]; t = target_cols[name]
            copy_col!(t, s, n)
        end
    end
    body = ir.body
    # share the substrate storage under the new universe (with_cols); keep the
    # IR-only columns/pools by reference
    newgraph = with_cols(getfield(body, :graph), target_cols)
    newbody = IRBody{typeof(target_cols)}(newgraph, body.type,
                                          body.flag, body.debug, body.region,
                                          body.constants, body.constmap,
                                          body.globals, body.globalmap)
    return IR{typeof(target_cols)}(ir.owner, newbody, ir.regions, ir.argtypes,
                                   ir.sptypes, ir.valid_worlds, ir.edit,
                                   ir.pending, ir.cache, ir.meta)
end

copy_col!(t::DenseCol, s::DenseCol, n) = (for i in 1:n; t.data[i] = s.data[i]; end)
copy_col!(t::SparseCol, s::SparseCol, n) = (for (k, v) in s.data; t.data[k] = v; end)
copy_col!(t, s, n) = error("no column copy defined between $(typeof(s)) and $(typeof(t))")

# ------------------------- provenance column (§3.7 Level 2) ----------------

"""
    ProvenanceCol()

Sparse per-statement provenance column: values are graph-qualified references
into a FOREIGN namespace — tree cursors (`Tree`) into a syntax graph, node
ids, or terminal source references. Annotation class (§3.5): keys are
remapped like any column at the renaming points (`col_compact!`), but values
are NEVER rewritten by IR renaming (`hasrefs` is false — they reference a
different namespace) and never invalidated (not `Derived`).
"""
mutable struct ProvenanceCol
    data::Dict{Int32,Any}
end
ProvenanceCol() = ProvenanceCol(Dict{Int32,Any}())

Base.getindex(c::ProvenanceCol, i::Integer) = c.data[Int32(i)]
Base.getindex(c::ProvenanceCol, s::StmtId) = c.data[s.id]
Base.setindex!(c::ProvenanceCol, @nospecialize(v), i::Integer) = (c.data[Int32(i)] = v)
Base.setindex!(c::ProvenanceCol, @nospecialize(v), s::StmtId) = (c.data[s.id] = v)
Base.haskey(c::ProvenanceCol, i::Integer) = haskey(c.data, Int32(i))
Base.haskey(c::ProvenanceCol, s::StmtId) = haskey(c.data, s.id)
Base.get(c::ProvenanceCol, i::Integer, default) = get(c.data, Int32(i), default)
Base.get(f::Base.Callable, c::ProvenanceCol, i::Integer) = get(f, c.data, Int32(i))
Base.delete!(c::ProvenanceCol, i::Integer) = (delete!(c.data, Int32(i)); c)
Base.length(c::ProvenanceCol) = length(c.data)
Base.isempty(c::ProvenanceCol) = isempty(c.data)
Base.iterate(c::ProvenanceCol, st...) = iterate(c.data, st...)
Base.keys(c::ProvenanceCol) = keys(c.data)

semclass(::Type{ProvenanceCol}) = Annotation()

col_grow!(::ProvenanceCol, n::Integer, oldlen::Integer) = nothing
function col_compact!(c::ProvenanceCol, old_of_new::Vector{Int32})
    nd = Dict{Int32,Any}()
    for i in 1:length(old_of_new)
        v = get(c.data, old_of_new[i], nothing)
        v === nothing || (nd[Int32(i)] = v)
    end
    c.data = nd
    return c
end
stage_copy_col(c::ProvenanceCol) = ProvenanceCol(copy(c.data))

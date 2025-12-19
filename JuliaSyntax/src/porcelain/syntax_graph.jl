const NodeId = Int

"""
Directed graph with arbitrary attributes on nodes. Used here for representing
one or several syntax trees.

TODO: Global attributes!
"""
mutable struct SyntaxGraph{Attrs}
    edge_ranges::Vector{UnitRange{Int}}
    edges::Vector{NodeId}
    attributes::Attrs
end

SyntaxGraph() = SyntaxGraph{Dict{Symbol,Any}}(Vector{UnitRange{Int}}(),
                                              Vector{NodeId}(), Dict{Symbol,Any}())

# "Freeze" attribute names and types, encoding them in the type of the returned
# SyntaxGraph.
function freeze_attrs(graph::SyntaxGraph)
    frozen_attrs = (; pairs(graph.attributes)...)
    SyntaxGraph(graph.edge_ranges, graph.edges, frozen_attrs)
end

# Create a copy of `graph` where the attribute list is mutable
function unfreeze_attrs(graph::SyntaxGraph)
    unfrozen_attrs = Dict{Symbol,Any}(pairs(graph.attributes)...)
    SyntaxGraph(graph.edge_ranges, graph.edges, unfrozen_attrs)
end

function _show_attrs(io, attributes::Dict)
    show(io, MIME("text/plain"), attributes)
end
function _show_attrs(io, attributes::NamedTuple)
    show(io, MIME("text/plain"), Dict(pairs(attributes)...))
end

function attrnames(graph::SyntaxGraph)
    keys(graph.attributes)
end

function attrdefs(graph::SyntaxGraph)
    [(k=>typeof(v).parameters[2]) for (k, v) in pairs(graph.attributes)]
end

function Base.show(io::IO, ::MIME"text/plain", graph::SyntaxGraph)
    print(io, typeof(graph),
          " with $(length(graph.edge_ranges)) vertices, $(length(graph.edges)) edges, and attributes:\n")
    _show_attrs(io, graph.attributes)
end

function ensure_attributes!(graph::SyntaxGraph; kws...)
    for (k,v) in pairs(kws)
        @assert k isa Symbol
        @assert v isa Type
        if haskey(graph.attributes, k)
            v0 = valtype(graph.attributes[k])
            v == v0 || throw(ErrorException("Attribute type mismatch $v != $v0"))
        elseif graph.attributes isa NamedTuple
            throw(ErrorException("""
                ensure_attributes!: $k is not an existing attribute, and the graph's attributes are frozen. \
                Consider calling non-mutating `ensure_attributes` instead."""))
        else
            graph.attributes[k] = Dict{NodeId,v}()
        end
    end
    graph
end

function ensure_attributes(graph::SyntaxGraph{<:Dict}; kws...)
    g = unfreeze_attrs(graph)
    ensure_attributes!(g; kws...)
end

function ensure_attributes(graph::SyntaxGraph{<:NamedTuple}; kws...)
    g = unfreeze_attrs(graph)
    ensure_attributes!(g; kws...)
    freeze_attrs(g)
end

function delete_attributes!(graph::SyntaxGraph{<:Dict}, attr_names::Symbol...)
    for name in attr_names
        delete!(graph.attributes, name)
    end
    graph
end

function delete_attributes(graph::SyntaxGraph{<:Dict}, attr_names::Symbol...)
    delete_attributes!(unfreeze_attrs(graph), attr_names...)
end

function delete_attributes(graph::SyntaxGraph{<:NamedTuple}, attr_names::Symbol...)
    g = delete_attributes!(unfreeze_attrs(graph), attr_names...)
    freeze_attrs(g)
end

function new_id!(graph::SyntaxGraph)
    push!(graph.edge_ranges, 0:-1) # Invalid range start => leaf node
    return length(graph.edge_ranges)
end

function setchildren!(graph::SyntaxGraph, id::NodeId,
                      children::AbstractVector{NodeId})
    n = length(graph.edges)
    graph.edge_ranges[id] = n+1:(n+length(children))
    # TODO: Reuse existing edges if possible
    append!(graph.edges, children)
end

function is_leaf(graph::SyntaxGraph, id)
    first(graph.edge_ranges[id]) == 0
end

function numchildren(graph::SyntaxGraph, id)
    length(graph.edge_ranges[id])
end

function children(graph::SyntaxGraph, id)
    @view graph.edges[graph.edge_ranges[id]]
end

function children(graph::SyntaxGraph, id, r::UnitRange)
    @view graph.edges[graph.edge_ranges[id][r]]
end

function child(graph::SyntaxGraph, id::NodeId, i::Integer)
    graph.edges[graph.edge_ranges[id][i]]
end

function getattr(graph::SyntaxGraph{<:Dict}, name::Symbol)
    getfield(graph, :attributes)[name]
end

function getattr(graph::SyntaxGraph{<:NamedTuple}, name::Symbol)
    getfield(getfield(graph, :attributes), name)
end

function getattr(graph::SyntaxGraph, name::Symbol, default)
    get(getfield(graph, :attributes), name, default)
end

function hasattr(graph::SyntaxGraph, name::Symbol)
    getattr(graph, name, nothing) !== nothing
end

# TODO: Probably terribly non-inferable?
function setattr!(graph::SyntaxGraph, id::NodeId, k::Symbol, @nospecialize(v))
    if !isnothing(v)
        getattr(graph, k)[id] = v
    end
    id
end

function deleteattr!(graph::SyntaxGraph, id::NodeId, name::Symbol)
    delete!(getattr(graph, name), id)
end

function Base.getproperty(graph::SyntaxGraph, name::Symbol)
    # TODO: Remove access to internals?
    name === :edge_ranges && return getfield(graph, :edge_ranges)
    name === :edges       && return getfield(graph, :edges)
    name === :attributes  && return getfield(graph, :attributes)
    return getattr(graph, name)
end

"""
    syntax_graph(ctx)

Return `SyntaxGraph` associated with `ctx`
"""
syntax_graph(graph::SyntaxGraph) = graph

function check_same_graph(x, y)
    if syntax_graph(x) !== syntax_graph(y)
        error("Mismatching syntax graphs")
    end
end

function check_compatible_graph(x, y)
    if !is_compatible_graph(x, y)
        error("Incompatible syntax graphs")
    end
end

function is_compatible_graph(x, y)
    syntax_graph(x).edges === syntax_graph(y).edges
end

"""
    struct SyntaxTree

An ECS-style AST used in JuliaLowering.  Unstable, but may eventually replace
SyntaxNode.
"""
struct SyntaxTree{Attrs}
    _graph::SyntaxGraph{Attrs}
    _id::NodeId
end

function Base.getproperty(ex::SyntaxTree, name::Symbol)
    name === :_graph && return getfield(ex, :_graph)
    name === :_id  && return getfield(ex, :_id)
    _id = getfield(ex, :_id)
    return get(getproperty(getfield(ex, :_graph), name), _id) do
        attrstr = join(["\n    $n = $(getproperty(ex, n))"
                        for n in attrnames(ex)], ",")
        error("Property `$name[$_id]` not found. Available attributes:$attrstr")
    end
end

function Base.setproperty!(ex::SyntaxTree, name::Symbol, @nospecialize(val))
    setattr!(ex._graph, ex._id, name, val)
    val
end

function Base.propertynames(ex::SyntaxTree)
    attrnames(ex)
end

function Base.get(ex::SyntaxTree, name::Symbol, default)
    attr = getattr(getfield(ex, :_graph), name, nothing)
    return isnothing(attr) ? default :
           get(attr, getfield(ex, :_id), default)
end

function Base.getindex(ex::SyntaxTree, i::Integer)
    SyntaxTree(ex._graph, child(ex._graph, ex._id, i))
end

function Base.getindex(ex::SyntaxTree, r::UnitRange)
    SyntaxList(ex._graph, children(ex._graph, ex._id, r))
end

Base.firstindex(ex::SyntaxTree) = 1
Base.lastindex(ex::SyntaxTree) = numchildren(ex)

function Base.:≈(ex1::SyntaxTree, ex2::SyntaxTree)
    if kind(ex1) != kind(ex2) || is_leaf(ex1) != is_leaf(ex2)
        return false
    end
    if is_leaf(ex1)
        return get(ex1, :value,    nothing) == get(ex2, :value,    nothing) &&
               get(ex1, :name_val, nothing) == get(ex2, :name_val, nothing)
    else
        if numchildren(ex1) != numchildren(ex2)
            return false
        end
        return all(c1 ≈ c2 for (c1,c2) in zip(children(ex1), children(ex2)))
    end
end

function hasattr(ex::SyntaxTree, name::Symbol)
    attr = getattr(ex._graph, name, nothing)
    return !isnothing(attr) && haskey(attr, ex._id)
end

function attrnames(ex::SyntaxTree)
    attrs = ex._graph.attributes
    [name for (name, value) in pairs(attrs) if haskey(value, ex._id)]
end

function copy_node(ex::SyntaxTree)
    graph = syntax_graph(ex)
    id = new_id!(graph)
    if !is_leaf(ex)
        setchildren!(graph, id, children(ex._graph, ex._id))
    end
    ex2 = SyntaxTree(graph, id)
    copy_attrs!(ex2, ex, true)
    ex2
end

function setattr!(ex::SyntaxTree, name::Symbol, @nospecialize(val))
    setattr!(ex._graph, ex._id, name, val)
    ex
end
setattr(ex::SyntaxTree, name::Symbol, @nospecialize(val)) =
    setattr!(copy_node(ex), name, val)

function deleteattr!(ex::SyntaxTree, name::Symbol)
    deleteattr!(ex._graph, ex._id, name)
end

# JuliaSyntax tree API

function is_leaf(ex::SyntaxTree)
    is_leaf(ex._graph, ex._id)
end

function numchildren(ex::SyntaxTree)
    numchildren(ex._graph, ex._id)
end

function children(ex::SyntaxTree)
    SyntaxList(ex._graph, children(ex._graph, ex._id))
end

function head(ex::SyntaxTree)
    SyntaxHead(kind(ex), flags(ex))
end

function kind(ex::SyntaxTree)
    ex.kind::Kind
end

function flags(ex::SyntaxTree)
    get(ex, :syntax_flags, 0x0000)
end


# Reference to bytes within a source file
struct SourceRef
    file::SourceFile
    first_byte::Int
    last_byte::Int
end

sourcefile(src::SourceRef) = src.file
byte_range(src::SourceRef) = src.first_byte:src.last_byte

# TODO: Adding these methods to support LineNumberNode is kind of hacky but we
# can remove these after JuliaLowering becomes self-bootstrapping for macros
# and we a proper SourceRef for @ast's @HERE form.
byte_range(src::LineNumberNode) = 0:0
source_location(src::LineNumberNode) = (src.line, 0)
source_location(::Type{LineNumberNode}, src::LineNumberNode) = src
source_line(src::LineNumberNode) = src.line
# The follow somewhat strange cases are for where LineNumberNode is standing in
# for SourceFile because we've only got Expr-based provenance info
sourcefile(src::LineNumberNode) = src
sourcetext(src::LineNumberNode) = SubString("")
source_location(src::LineNumberNode, byte_index::Integer) = (src.line, 0)
source_location(::Type{LineNumberNode}, src::LineNumberNode, byte_index::Integer) = src
filename(src::LineNumberNode) = string(src.file)

function highlight(io::IO, src::LineNumberNode; note="")
    print(io, src, " - ", note)
end

function highlight(io::IO, src::SourceRef; kws...)
    highlight(io, src.file, first_byte(src):last_byte(src); kws...)
end

function Base.show(io::IO, ::MIME"text/plain", src::SourceRef)
    highlight(io, src; note="these are the bytes you're looking for 😊", context_lines_inner=20)
end


function provenance(ex::SyntaxTree)
    s = ex.source
    if s isa NodeId
        return (SyntaxTree(ex._graph, s),)
    elseif s isa Tuple
        return SyntaxTree.((ex._graph,), s)
    else
        return (s,)
    end
end


function _sourceref(sources, id)
    i = 1
    while true
        i += 1
        s = sources[id]
        if s isa NodeId
            id = s
        else
            return s, id
        end
    end
end

function sourceref(ex::SyntaxTree)
    sources = ex._graph.source
    id::NodeId = ex._id
    while true
        s, _ = _sourceref(sources, id)
        if s isa Tuple
            s = s[1]
        end
        if s isa NodeId
            id = s
        else
            return s
        end
    end
end

function _flattened_provenance(refs, graph, sources, id)
    # TODO: Implement in terms of `provenance()`?
    s, id2 = _sourceref(sources, id)
    if s isa Tuple
        for i in s
            _flattened_provenance(refs, graph, sources, i)
        end
    else
        push!(refs, SyntaxTree(graph, id2))
    end
end

function flattened_provenance(ex::SyntaxTree)
    refs = SyntaxList(ex)
    _flattened_provenance(refs, ex._graph, ex._graph.source, ex._id)
    return reverse(refs)
end


function is_ancestor(ex, ancestor)
    if !is_compatible_graph(ex, ancestor)
        return false
    end
    sources = ex._graph.source
    id::NodeId = ex._id
    while true
        s = get(sources, id, nothing)
        if s isa NodeId
            id = s
            if id == ancestor._id
                return true
            end
        else
            return false
        end
    end
end

const SourceAttrType = Union{SourceRef,LineNumberNode,NodeId,Tuple}

function reparent(ctx, ex::SyntaxTree)
    # Ensure `ex` has the same parent graph, in a somewhat loose sense.
    # Could relax by copying if necessary?
    # In that case, would we copy all the attributes? That would have slightly
    # different semantics.
    graph = syntax_graph(ctx)
    @assert graph.edge_ranges === ex._graph.edge_ranges
    SyntaxTree(graph, ex._id)
end

function ensure_attributes(ex::SyntaxTree; kws...)
    reparent(ensure_attributes(syntax_graph(ex); kws...), ex)
end

syntax_graph(ex::SyntaxTree) = ex._graph

sourcefile(ex::SyntaxTree) = sourcefile(sourceref(ex))
byte_range(ex::SyntaxTree) = byte_range(sourceref(ex))

#-------------------------------------------------------------------------------
# Lightweight vector of nodes ids with associated pointer to graph stored separately.
mutable struct SyntaxList{Attrs, NodeIdVecType} <: AbstractVector{SyntaxTree}
    graph::SyntaxGraph{Attrs}
    ids::NodeIdVecType
end

function SyntaxList(graph::SyntaxGraph{T}, ids::AbstractVector{NodeId}) where {T}
    SyntaxList{T, typeof(ids)}(graph, ids)
end

SyntaxList(graph::SyntaxGraph) = SyntaxList(graph, Vector{NodeId}())
SyntaxList(ctx) = SyntaxList(syntax_graph(ctx))

tree_ids(sts::SyntaxTree...) = NodeId[st._id for st in sts]

syntax_graph(lst::SyntaxList) = lst.graph

setchildren!(graph::SyntaxGraph, id::NodeId, children::SyntaxList) =
    setchildren!(graph, id, children.ids)

Base.size(v::SyntaxList) = size(v.ids)

Base.IndexStyle(::Type{<:SyntaxList}) = IndexLinear()

Base.getindex(v::SyntaxList, i::Int) = SyntaxTree(v.graph, v.ids[i])

function Base.getindex(v::SyntaxList, r::UnitRange)
    SyntaxList(v.graph, view(v.ids, r))
end

function Base.setindex!(v::SyntaxList, ex::SyntaxTree, i::Int)
    check_compatible_graph(v, ex)
    v.ids[i] = ex._id
end

function Base.setindex!(v::SyntaxList, id::NodeId, i::Int)
    v.ids[i] = id
end

function Base.push!(v::SyntaxList, ex::SyntaxTree)
    check_compatible_graph(v, ex)
    push!(v.ids, ex._id)
end

function Base.pushfirst!(v::SyntaxList, ex::SyntaxTree)
    check_compatible_graph(v, ex)
    pushfirst!(v.ids, ex._id)
end

function Base.similar(v::SyntaxList, size::Tuple=Base.size(v.ids))
    SyntaxList(v.graph, zeros(NodeId, size))
end

function Base.isassigned(v::SyntaxList, i::Integer)
    v.ids[i] > 0
end

function Base.append!(v::SyntaxList, exs)
    for e in exs
        push!(v, e)
    end
    v
end

function Base.append!(v::SyntaxList, exs::SyntaxList)
    check_compatible_graph(v, exs)
    append!(v.ids, exs.ids)
    v
end

function Base.push!(v::SyntaxList, id::NodeId)
    push!(v.ids, id)
end

function Base.pop!(v::SyntaxList)
    SyntaxTree(v.graph, pop!(v.ids))
end

function Base.popat!(v::SyntaxList, i::Integer)
    SyntaxTree(v.graph, popat!(v.ids, i))
end

function Base.resize!(v::SyntaxList, n)
    resize!(v.ids, n)
    v
end

function Base.empty!(v::SyntaxList)
    empty!(v.ids)
    v
end

function Base.deleteat!(v::SyntaxList, inds)
    deleteat!(v.ids, inds)
    v
end

function Base.copy(v::SyntaxList)
    SyntaxList(v.graph, copy(v.ids))
end

function Base.filter(f, exs::SyntaxList)
    out = SyntaxList(syntax_graph(exs))
    for ex in exs
        if f(ex)
            push!(out, ex)
        end
    end
    out
end

# Would like the following to be an overload of Base.map() ... but need
# somewhat arcane trickery to ensure that this only tries to collect into a
# SyntaxList when `f` yields a SyntaxTree.
#
# function mapsyntax(f, exs::SyntaxList)
#     out = SyntaxList(syntax_graph(exs))
#     for ex in exs
#         push!(out, f(ex))
#     end
#     out
# end


#-------------------------------------------------------------------------------
# AST creation utilities

"""
    newnode(graph::SyntaxGraph, prov::SourceAttrType, k::Kind, children)

Add a new node to `graph` with reference to parsed source text `prov`.
"""
function newnode(graph::SyntaxGraph, prov::SourceAttrType, k::Kind, children)
    st = newleaf(graph, prov, k)
    setchildren!(graph, st._id, children)
    return st
end
function newleaf(graph::SyntaxGraph, prov::SourceAttrType, k::Kind)
    st = SyntaxTree(graph, new_id!(graph))
    setattr!(st, :kind, k)
    setattr!(st, :source, prov)
end

newnode(graph::SyntaxGraph, prov::SyntaxTree, k::Kind, children) =
    newnode(graph, prov._id, k, children)

newleaf(graph::SyntaxGraph, prov::SyntaxTree, k::Kind) =
    newleaf(graph, prov._id, k)

"""
    mknode(old::SyntaxTree, children)

Create a node in `old`'s graph that is an immutable update of `old`, but setting
`old` as its provenance.  This is the main operation used by syntax
transformations such as lowering.
"""
function mknode(old::SyntaxTree, children)
    st = mkleaf(old)
    setchildren!(st._graph, st._id, children)
    return st
end
function mkleaf(old::SyntaxTree)
    graph = syntax_graph(old)
    st = SyntaxTree(graph, new_id!(graph))
    copy_attrs!(st, old, true)
    setattr!(st, :source, old._id)
end

#-------------------------------------------------------------------------------
# Mapping and copying of AST nodes
function copy_attrs!(dest, src, all=false)
    # TODO: Make this faster?
    for (name, attr) in pairs(src._graph.attributes)
        if (all || (name !== :source && name !== :kind && name !== :syntax_flags)) &&
                haskey(attr, src._id)
            dest_attr = getattr(dest._graph, name, nothing)
            if !isnothing(dest_attr)
                dest_attr[dest._id] = attr[src._id]
            end
        end
    end
end

function copy_attrs!(dest, head::Union{Kind,SyntaxHead}, all=false)
    if all
        setattr!(dest._graph, dest._id, :kind, kind(head))
        !(head isa Kind) && setattr!(dest._graph, dest._id, :syntax_flags, flags(head))
    end
end

function mapchildren(f::Function, ctx, ex::SyntaxTree, do_map_child::Function)
    if is_leaf(ex)
        return ex
    end
    orig_children = children(ex)
    cs = nothing
    for (i,e) in enumerate(orig_children)
        newchild = do_map_child(i) ? f(e) : e
        if isnothing(cs)
            if newchild == e
                continue
            else
                cs = SyntaxList(ctx)
                append!(cs, orig_children[1:i-1])
            end
        end
        push!(cs::SyntaxList, newchild)
    end
    if isnothing(cs)
        # This function should be allocation-free if no children were changed
        # by the mapping and there's no extra_attrs
        return ex
    end
    cs::SyntaxList
    ex2 = mknode(ex, cs)
    return ex2
end

function mapchildren(f::Function, ctx, ex::SyntaxTree,
                     mapped_children::AbstractVector{<:Integer})
    j = Ref(firstindex(mapped_children))
    function do_map_child(i)
        ind = j[]
        if ind <= lastindex(mapped_children) && mapped_children[ind] == i
            j[] += 1
            true
        else
            false
        end
    end
    mapchildren(f, ctx, ex, do_map_child)
end

function mapchildren(f::Function, ctx, ex::SyntaxTree)
    mapchildren(f, ctx, ex, i->true)
end


"""
Recursively copy AST `ex` into `ctx`.

Special provenance handling: If `copy_source` is true, treat the `.source`
attribute as a reference and recurse on its contents.  Otherwise, treat it like
any other attribute.
"""
function copy_ast(ctx, ex::SyntaxTree; copy_source=true)
    graph1 = syntax_graph(ex)
    graph2 = syntax_graph(ctx)
    !copy_source && check_same_graph(graph1, graph2)
    id2 = _copy_ast(graph2, graph1, ex._id, Dict{NodeId, NodeId}(), copy_source)
    return SyntaxTree(graph2, id2)
end

function _copy_ast(graph2::SyntaxGraph, graph1::SyntaxGraph,
                   id1::NodeId, seen, copy_source)
    let copied = get(seen, id1, nothing)
        isnothing(copied) || return copied
    end
    id2 = new_id!(graph2)
    seen[id1] = id2
    src1 = get(SyntaxTree(graph1, id1), :source, nothing)
    src2 = if !copy_source
        src1
    elseif src1 isa NodeId
        _copy_ast(graph2, graph1, src1, seen, copy_source)
    elseif src1 isa Tuple
        map(i->_copy_ast(graph2, graph1, i, seen, copy_source), src1)
    else
        src1
    end
    copy_attrs!(SyntaxTree(graph2, id2), SyntaxTree(graph1, id1), true)
    setattr!(graph2, id2, :source, src2)
    if !is_leaf(graph1, id1)
        cs = NodeId[]
        for cid in children(graph1, id1)
            push!(cs, _copy_ast(graph2, graph1, cid, seen, copy_source))
        end
        setchildren!(graph2, id2, cs)
    end
    return id2
end

#-------------------------------------------------------------------------------
# RawGreenNode->SyntaxTree

function build_tree(::Type{SyntaxTree}, stream::ParseStream;
                                filename=nothing, first_line=1)
    cursor = RedTreeCursor(stream)
    graph = SyntaxGraph()
    sf = SourceFile(stream; filename, first_line)
    source = SourceRef(sf, first_byte(stream), last_byte(stream))
    cs = SyntaxList(graph)
    for c in reverse_toplevel_siblings(cursor)
        is_trivia(c) && !is_error(c) && continue
        push!(cs, SyntaxTree(graph, sf, c))
    end
    # There may be multiple non-trivia toplevel nodes (e.g. parse error)
    length(cs) === 1 && return only(cs)
    id = new_id!(graph)
    setchildren!(graph, id, reverse(cs).ids)
    setattr!(graph, id, :source, source)
    setattr!(graph, id, :kind, K"wrapper")
    return SyntaxTree(graph, id)
end

function SyntaxTree(graph::SyntaxGraph, sf::SourceFile, cursor::RedTreeCursor)
    ensure_attributes!(graph, kind=Kind, syntax_flags=UInt16,
                       source=SourceAttrType, value=Any, name_val=String)
    green_id = GC.@preserve sf begin
        raw_offset, txtbuf = _unsafe_wrap_substring(sf.code)
        offset = raw_offset - sf.byte_offset
        _insert_green(graph, sf, txtbuf, offset, cursor)
    end
    out = _green_to_ast(K"None", SyntaxTree(graph, green_id))
    @assert !isnothing(out) "SyntaxTree requires >0 nontrivia nodes"
    return out
end

# TODO: Do we really need all trivia?  K"parens" can be good to keep, but things
# like K"(" and whitespace might not be useful.
function _insert_green(graph::SyntaxGraph, sf::SourceFile,
                       txtbuf::Vector{UInt8}, offset::Int,
                       cursor::RedTreeCursor)
    id = new_id!(graph)
    setattr!(graph, id, :kind, kind(cursor))
    setattr!(graph, id, :syntax_flags, flags(cursor))
    setattr!(graph, id, :source, SourceRef(sf, first_byte(cursor), last_byte(cursor)))
    if !is_leaf(cursor)
        cs = NodeId[]
        for c in reverse(cursor)
            push!(cs, _insert_green(graph, sf, txtbuf, offset, c))
        end
        setchildren!(graph, id, reverse(cs))
    else
        v = parse_julia_literal(txtbuf, head(cursor), byte_range(cursor) .+ offset)
        if v isa Symbol
            # TODO: Fixes in JuliaSyntax to avoid ever converting to Symbol
            setattr!(graph, id, :name_val, string(v))
        elseif !isnothing(v)
            setattr!(graph, id, :value, v)
        end
    end
    return id
end

# Leaves are shared.  Unlike `mapchildren`, doesn't bother checking for
# unchanged children so internal nodes can be shared, since the likelihood of
# not deleting trivia under an internal node is practically zero.
function _green_to_ast(parent::Kind, ex::SyntaxTree; eq_to_kw=false)
    is_trivia(ex) && !is_error(ex) && return nothing
    graph = syntax_graph(ex)
    k = kind(ex)
    if k === K"ref" ||
        (k in KSet"call dotcall" && (
            is_prefix_call(ex) || is_prefix_op_call(ex) && numchildren(ex) > 2))
        cs = SyntaxList(ex)
        for c in children(ex)
            c2 = _green_to_ast(k, c; eq_to_kw=length(cs)>0)
            !isnothing(c2) && push!(cs, c2)
        end
        mknode(ex, cs)
    elseif k === K"parameters"
        eq_to_kw = parent != K"vect"   && parent != K"curly" &&
                   parent != K"braces" && parent != K"ref"
        mknode(ex, _map_green_to_ast(k, children(ex); eq_to_kw))
    elseif k === K"parens"
        cs = _map_green_to_ast(parent, children(ex); eq_to_kw)
        length(cs) === 1 ? cs[1] : mknode(ex, cs)
    elseif k in KSet"var char"
        cs = _map_green_to_ast(parent, children(ex))
        length(cs) === 1 ? cs[1] : mknode(ex, cs)
    elseif k === K"=" && eq_to_kw
        setattr!(mknode(ex, _map_green_to_ast(k, children(ex))),
                 :kind, K"kw")
    elseif k === K"CmdMacroName" || k === K"StrMacroName"
        name = lower_identifier_name(ex.name_val, k)
        setattr!(newleaf(graph, ex, K"Identifier"),
                 :name_val, name)
    elseif k === K"macro_name"
        # M.@x parses to (. M (macro_name x))
        # @M.x parses to (macro_name (. M x))
        # We want (. M @x) (both identifiers) in either case
        cs = _map_green_to_ast(k, children(ex))
        if length(cs) !== 1 || !(kind(cs[1]) in KSet". Identifier")
            return mknode(ex, cs)
        end
        id = cs[1]
        mname_raw = (kind(id) === K"." ? id[2] : id).name_val
        mac_id = setattr!(newleaf(graph, ex, K"Identifier"), :name_val,
                          lower_identifier_name(mname_raw, K"macro_name"))
        if kind(id) === K"."
            mknode(ex, tree_ids(id[1], mac_id))
        else
            mac_id
        end
    elseif is_leaf(ex)
        return ex
    else
        mknode(ex, _map_green_to_ast(k, children(ex)))
    end
end

function _map_green_to_ast(parent::Kind, cs::SyntaxList; eq_to_kw=false)
    out = SyntaxList(cs)
    for c in cs
        c2 = _green_to_ast(parent, c; eq_to_kw)
        !isnothing(c2) && push!(out, c2)
    end
    return out
end

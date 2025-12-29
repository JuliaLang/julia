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
    v
end

function Base.pushfirst!(v::SyntaxList, ex::SyntaxTree)
    check_compatible_graph(v, ex)
    pushfirst!(v.ids, ex._id)
    v
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

function Base.popfirst!(v::SyntaxList)
    SyntaxTree(v.graph, popfirst!(v.ids))
end

function Base.popat!(v::SyntaxList, i::Integer)
    SyntaxTree(v.graph, popat!(v.ids, i))
end

function Base.insert!(v::SyntaxList, i::Integer, st::SyntaxTree)
    insert!(v.ids, i, st._id)
    v
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
# WIP: expr_structure param will be deleted

function build_tree(::Type{SyntaxTree}, stream::ParseStream;
                    filename=nothing, first_line=1,
                    expr_structure=false)
    cursor = RedTreeCursor(stream)
    graph = SyntaxGraph()
    sf = SourceFile(stream; filename, first_line)
    source = SourceRef(sf, first_byte(stream), last_byte(stream))
    cs = SyntaxList(graph)
    for c in reverse_toplevel_siblings(cursor)
        is_trivia(c) && !is_error(c) && continue
        push!(cs, SyntaxTree(graph, sf, c; expr_structure))
    end
    # There may be multiple non-trivia toplevel nodes (e.g. parse error)
    length(cs) === 1 && return only(cs)
    id = new_id!(graph)
    setchildren!(graph, id, reverse(cs).ids)
    setattr!(graph, id, :source, source)
    setattr!(graph, id, :kind, K"wrapper")
    return SyntaxTree(graph, id)
end

function SyntaxTree(graph::SyntaxGraph, sf::SourceFile, cursor::RedTreeCursor; expr_structure)
    ensure_attributes!(graph, kind=Kind, syntax_flags=UInt16,
                       source=SourceAttrType, value=Any, name_val=String)
    green_id = GC.@preserve sf begin
        raw_offset, txtbuf = _unsafe_wrap_substring(sf.code)
        offset = raw_offset - sf.byte_offset
        _insert_green(graph, sf, txtbuf, offset, cursor)
    end
    gst = SyntaxTree(graph, green_id)
    if expr_structure
        out = _green_to_est(gst, 0, gst)
    else
        out = _green_to_ast(K"None", gst)
    end
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

#-------------------------------------------------------------------------------
# WIP: RawGreenNode->EST.  This will replace `_green_to_ast` above, and could
# replace `node_to_expr` later.

"""
Convert green `st` to a SyntaxTree with Expr structure.  `parent_i` is the final
position of `convert(st)` (our return value) within `convert(parent)`.  If
`parent_i == 0`, neither it nor our `parent` are known or relevant to this
conversion.

We can't assume much about `st` since it's anything the parser produces.  Our
correctness is defined against existing text->Expr transformations.

All node rearrangements and head changes are determined before recursing on
children, unlike in `node_to_expr`.  This is because these nodes are not mutable
and filling the graph with temporary nodes to fix up later is less desirable,
and also because knowing our parent's kind and our position within it
ahead-of-time makes conversion simpler.  By default, for each node `st`, we
  1. let `cs` be `children(st)` minus (non-recursively) all trivia and parens
  2. rearrange `cs` based on length(cs), their/our/parent's kind/flags, etc.
  3. let `ret_cs` be `map(convert, cs)`
  4. return our new node `convert(st)` with `ret_cs` as children.
However, we can stop and return an answer between any of these steps.  For
example, deleting a child is easy in (2), but new non-leaf children we insert
should be added to `ret_cs` rather than `cs` (unless the new child has
pre-transformation structure and we're OK with step 3 creating it again).
"""
function _green_to_est(parent::SyntaxTree, parent_i::Int,
                       st::SyntaxTree; kw_in_params=false)
    if !should_include_node(st)
        @assert kind(parent) === K"None" && parent_i === 0
        return nothing
    end

    graph = syntax_graph(st)
    k = kind(st)
    coreref(s::String) = setattr!(newleaf(graph, st, K"core"), :name_val, s)
    symleaf(s::String) = setattr!(newleaf(graph, st, K"Identifier"), :name_val, s)
    valleaf(@nospecialize(v)) = setattr!(newleaf(graph, st, K"Value"), :value, v)

    if is_leaf(st)
        return if k === K"CmdMacroName" || k === K"StrMacroName"
            name = lower_identifier_name(st.name_val, k)
            symleaf(name)
        elseif k === K"VERSION"
            valleaf(version_to_expr(st))
        elseif (v = get(st, :value, nothing); v isa Union{Int128,UInt128,BigInt})
            # syntax TODO: likely unnecessary; this is just to match RGN->Expr,
            # which added this to match flisp parsing text->Expr.
            macname = v isa Int128 ? "@int128_str" :
                v isa UInt128 ? "@uint128_str" : "@big_str"
            mac = valleaf(GlobalRef(Core, Symbol(macname)))
            arg = valleaf(replace(sourcetext(st), '_'=>""))
            ret_cids = tree_ids(mac, coreref("nothing"), arg)
            newnode(graph, st, K"macrocall", ret_cids)
        else
            st
        end
    end

    # Non-leaf cases: each branch should either set `ret_k` and `cs` or recurse
    # manually and return a finished SyntaxTree
    ret_k::Kind = k
    cs = preprocessed_green_children(st)
    n_cs = length(cs)

    if k === K"string" && n_cs > 0
        return _string_to_est(st, cs; unwrap_literal=true)
    elseif k === K"cmdstring" && n_cs > 0
        # (cmdstring _...) => (macrocall Core.@cmd lno joined_str)
        cmd_arg = _string_to_est(st, cs; unwrap_literal=true)
        loc_st = valleaf(source_location(LineNumberNode, st))
        return newnode(graph, st, K"macrocall", tree_ids(
            valleaf(GlobalRef(Core, Symbol("@cmd"))), loc_st, cmd_arg))
    elseif k === K"macro_name" && n_cs === 1
        # "M.@x" => (. M (macro_name x)) => (. M @x)
        # "@M.x" => (macro_name (. M x)) => (. M @x)
        #           (macro_name else) => else
        if kind(cs[1]) === K"Identifier"
            return symleaf(lower_identifier_name(cs[1].name_val, K"macro_name"))
        else
            inner_st = cs[1]
            inner_cs = preprocessed_green_children(inner_st)
            if (length(inner_cs) === 2 && kind(inner_st) === K"." &&
                kind(inner_cs[2]) === K"Identifier")
                (lhs, raw_m) = _green_to_est(cs[1], 1, inner_cs[1]), inner_cs[2]
                mname_s = lower_identifier_name(raw_m.name_val, K"macro_name")
                mname = setattr!(mkleaf(raw_m), :name_val, mname_s)
                mname_inert = newnode(graph, raw_m, K"inert", tree_ids(mname))
                return mknode(inner_st, tree_ids(lhs, mname_inert))
            else
                return _green_to_est(parent, 1, inner_st)
            end
        end
    elseif k === K"?"
        ret_k = K"if"
    elseif k === K"op=" && n_cs === 3
        # (op= a + b) => (+= a b)
        # (.op= a + b) => (.+= a b) below
        op_s = string(cs[2]) * '='
        lhs = _green_to_est(st, 0, cs[1])
        rhs = _green_to_est(st, 0, cs[3])
        out = newnode(graph, st, K"unknown_head", tree_ids(lhs, rhs))
        return setattr!(out, :name_val, op_s)
    elseif k === K".op=" && n_cs === 3
        op_s = '.' * string(cs[2]) * '='
        lhs = _green_to_est(st, 0, cs[1])
        rhs = _green_to_est(st, 0, cs[3])
        out = newnode(graph, st, K"unknown_head", tree_ids(lhs, rhs))
        return setattr!(out, :name_val, op_s)
    elseif k === K"macrocall" && n_cs > 0
        # LineNumberNodes are not usually added to the tree as they are in Expr,
        # but this specifically inserts the macrocall child for compatibility
        loc_st = let loc = source_location(LineNumberNode, st)
            if n_cs >= 2 && kind(cs[2]) === K"VERSION"
                v = version_to_expr(popat!(cs, 2))
                loc = Core.MacroSource(loc, v)
            end
            valleaf(loc)
        end
        insert!(cs, 2, loc_st)
        # foo`x` parses to (macrocall foo::CmdMacroName (cmdstring ::CmdString))
        # so we need to unwrap the CmdString or else we get two macrocalls
        if n_cs >= 2 && kind(cs[1]) === K"CmdMacroName"
            ret_cs = _map_green_to_est(st, cs)
            ret_cs[3] = ret_cs[3][3] # node leak
            return mknode(st, ret_cs)
        end
        do_ex = kind(cs[end]) === K"do" ? pop!(cs) : nothing
        _reorder_parameters!(cs, 3)
        !isnothing(do_ex) && return _make_do_expression(st, cs, do_ex)
    elseif k === K"doc"
        # (doc str obj) => (macrocall Core.@doc lno str obj)
        ret_k = K"macrocall"
        pushfirst!(cs, valleaf(source_location(LineNumberNode, st)))
        pushfirst!(cs, valleaf(GlobalRef(Core, Symbol("@doc"))))
    elseif k === K"dotcall" || k === K"call" && n_cs > 0
        if is_infix_op_call(st) || is_postfix_op_call(st)
            cs[2], cs[1] = cs[1], cs[2]
        end
        if is_postfix_op_call(st) && kind(cs[1]) == K"Identifier" &&
            cs[1].name_val === "'"
            popfirst!(cs)
            ret_k = K"'"
        end
        do_ex = kind(cs[end]) === K"do" ? pop!(cs) : nothing
        _reorder_parameters!(cs, 2)
        if k === K"dotcall"
            if is_prefix_call(st)
                # (dotcall f args...) => (. f (tuple args...))
                ret_cs = _map_green_to_est(st, cs)
                tuple = newnode(graph, st, K"tuple", ret_cs[2:end])
                return newnode(graph, st, K".", tree_ids(ret_cs[1], tuple))
            else
                # (dotcall + args...) => (call .+ args...)
                ret_k = K"call"
                if kind(cs[1]) === K"Identifier"
                    cs[1] = symleaf('.' * cs[1].name_val)
                end
            end
        end
        !isnothing(do_ex) && return _make_do_expression(st, cs, do_ex)
    elseif k === K"."
        if n_cs === 2
            # (. lhs rhs) => (. lhs (inert rhs))
            lhs = _green_to_est(st, 1, cs[1])
            rhs = _green_to_est(st, 2, cs[2])
            inert_rhs = kind(rhs) in KSet"quote inert" ? rhs :
                newnode(graph, cs[2], K"inert", tree_ids(rhs))
            return mknode(st, tree_ids(lhs, inert_rhs))
        elseif n_cs === 1
            # (. x) => (. x) or .x
            # TODO: This is the one place where K"parens" change the result,
            # meaning that either Expr is doing something wrong or SyntaxNode is
            # deleting semantics.
            paren_st = filter(should_include_node, children(parent))[1]
            coalesce_dot = !(kind(paren_st) === K"parens") && parent_i === 1 &&
                kind(parent) in KSet"call dotcall curly quote"

            if (coalesce_dot || is_syntactic_operator(kind(cs[1])) ||
                kind(parent) === K"comparison" && iseven(parent_i))
                return symleaf('.' * cs[1].name_val)
            end
        end
    elseif k === K"ref" || k === K"curly"
        _reorder_parameters!(cs, 2)
    elseif k === K"for" && n_cs === 2
        # (for (iteration iter1) body) => (for iter1 body)
        iters = preprocessed_green_children(cs[1])
        if length(iters) === 1
            cs[1] = iters[1]
        end
    elseif k === K"iteration"
        # (for (iteration iter1 iters...) body) => (for (block iter1 iters...) body)
        @assert kind(parent) === K"for" && parent_i === 1
        ret_k = K"block"
    elseif k === K"vect" || k === K"braces"
        _reorder_parameters!(cs, 1)
    elseif k === K"tuple"
        # Unwrap singleton, no-trailing-comma tuple in a couple cases:
        # (function (tuple (... xs)) body) => (function (... xs) body)
        # (-> (tuple _) body) => (-> _ body), assuming _ not parameters
        if n_cs === 1 && parent_i === 1 &&
            !has_flags(st, TRAILING_COMMA_FLAG)
            p_k = kind(parent)
            c_k = kind(cs[1])
            if (p_k === K"function" && c_k === K"...") ||
                (p_k === K"->" && c_k !== K"parameters")
                return _green_to_est(parent, parent_i, cs[1])
            end
        elseif n_cs === 2 && kind(parent) === K"->" && parent_i === 1 &&
            kind(cs[2]) === K"parameters" && kind(cs[1]) !== K"..."
            # This case should really be deleted.
            # (-> (tuple x (parameters y)) _) => (-> (block x y) _)
            c2_cs = preprocessed_green_children(cs[2])
            if length(c2_cs) === 0
                ret_k = K"block"
                pop!(cs)
            elseif length(c2_cs) === 1
                ret_k = K"block"
                cs[2] = c2_cs[1]
            end
        end
        _reorder_parameters!(cs, 1)
    elseif k === K"where" && n_cs === 2
        # (where lhs (braces a b c)) => (where lhs a b c)
        if kind(cs[2]) === K"braces"
            rhs = pop!(cs)
            append!(cs, preprocessed_green_children(rhs))
            _reorder_parameters!(cs, 2)
        end
    elseif k === K"try"
        # anything => (try try_block e catch_block [finally_block] [else_block])
        try_ = cs[1]
        st_false = valleaf(false)
        catch_var = catch_ = else_ = finally_ = st_false
        for c in cs[2:end]
            inner_cs = preprocessed_green_children(c)
            if kind(c) === K"catch"
                if kind(inner_cs[1]) !== K"Placeholder"
                    catch_var = inner_cs[1]
                end
                catch_ = inner_cs[2]
            elseif kind(c) === K"else"
                else_ = only(inner_cs)
            elseif kind(c) === K"finally"
                finally_ = only(inner_cs)
            elseif kind(c) === K"error"
                return mknode(st, cs) # give up
            else
                @assert false "Illegal subclause in `try`"
            end
        end
        empty!(cs)
        push!(cs, try_, catch_var, catch_)
        if finally_ != st_false || else_ != st_false
            push!(cs, finally_)
            if else_ != st_false
                push!(cs, else_)
            end
        end
    elseif k === K"generator" && n_cs >= 2
        # let (g2 x iter) mean (generator x iter.children...)
        # (generator val iter_1 ... iter_n) =>
        # (flatten (g2 (... (flatten (g2 (g2 val i_n) i_{n-1})) ...) i_1))
        g_out = _green_to_est(st, 1, popfirst!(cs))
        for c in Iterators.reverse(cs)
            gen_cs = let rest = kind(c) === K"iteration" ?
                preprocessed_green_children(c) : SyntaxList(graph, tree_ids(c))
                rest = _map_green_to_est(st, rest; undef_parent=true)
                pushfirst!(rest, g_out)
            end
            g_out = mknode(st, gen_cs)
            if c !== cs[end]
                g_out = newnode(graph, c, K"flatten", tree_ids(g_out))
            end
        end
        return setattr!(g_out, :source, st._id) # outermost provenance
    elseif k === K"filter"
        @assert n_cs === 2
        # (filter (iteration is...) cond) => (filter cond is...)
        cond = pop!(cs)
        cs = preprocessed_green_children(cs[1])
        pushfirst!(cs, cond)
    elseif k === K"in"
        ret_k = K"="
    elseif k === K"nrow" || k === K"ncat"
        pushfirst!(cs, valleaf(numeric_flags(flags(st))))
    elseif k === K"typed_ncat"
        insert!(cs, 2, valleaf(numeric_flags(flags(st))))
    elseif k === K"elseif"
        # (elseif cond body) => (elseif (block cond) body)
        # RGN->Expr block-wraps for linenodes; we do it for parity
        ret_cs = _map_green_to_est(st, cs)
        ret_cs[1] = newnode(graph, cs[1], K"block", tree_ids(ret_cs[1]))
        return mknode(st, ret_cs)
    elseif k === K"->" && kind(cs[2]) !== K"block"
        ret_cs = _map_green_to_est(st, cs)
        ret_cs[2] = newnode(graph, cs[2], K"block", tree_ids(ret_cs[2]))
        return mknode(st, ret_cs)
    elseif k === K"function" && n_cs >= 2 &&
        has_flags(st, SHORT_FORM_FUNCTION_FLAG)
        # (function-= callex body) => (= callex (block body))
        # exception: no block on "x' = y", or if body is already a block
        if kind(cs[2]) !== K"block" && !is_postfix_op_call(cs[1])
            ret_cs = _map_green_to_est(st, cs)
            ret_cs[2] = newnode(graph, cs[2], K"block", tree_ids(ret_cs[2]))
            return newnode(graph, st, K"=", ret_cs)
        end
        ret_k = K"="
    elseif k === K"module"
        not_bare = valleaf(!has_flags(st, BARE_MODULE_FLAG))
        insert!(cs, kind(cs[1]) === K"VERSION" ? 2 : 1, not_bare)
    elseif k === K"quote" && n_cs === 1
        # (quote something_simple) => (inert something_simple)
        ret_c = _green_to_est(st, 1, cs[1])
        return is_leaf(ret_c) && kind(ret_c) !== K"Bool" ?
            newnode(graph, st, K"inert", tree_ids(ret_c)) :
            mknode(st, tree_ids(ret_c))
    elseif k === K"do"
        ret_k = K"->"
    elseif k === K"block"
        # (let (block x) _...) => (let x _...)
        # (let (block (= x y)) _...) => (let (= x y) _...)
        # (let (block (:: x y)) _...) => (let (:: x y) _...)
        # (struct _ (block (doc "foo" field1) (doc "bar" field2))) =>
        # (struct _ (block "foo" field1 "bar" field2))
        if kind(parent) === K"let" && parent_i === 1 && n_cs === 1
            out = _green_to_est(st, 1, cs[1])
            return kind(out) in KSet"Identifier = ::" ? out :
                mknode(st, tree_ids(out))
        elseif kind(parent) === K"struct" && parent_i === 3
            cs_tmp = SyntaxList(cs)
            for c in cs
                kind(c) === K"doc" ?
                    append!(cs_tmp, preprocessed_green_children(c)) :
                    push!(cs_tmp, c)
            end
            cs = cs_tmp
        end
    elseif (k === K"local" || k === K"global") && n_cs === 1
        # (local (const _)) => (const (local _))
        # (local (tuple a b c)) => (local a b c)
        if kind(cs[1]) === K"const"
            ret_c1_cs = _map_green_to_est(st, preprocessed_green_children(cs[1]))
            ret_cs = tree_ids(mknode(st, ret_c1_cs))
            return mknode(cs[1], ret_cs)
        elseif kind(cs[1]) === K"tuple"
            cs = preprocessed_green_children(cs[1])
        end
    elseif k === K"return" && n_cs === 0
        push!(cs, coreref("nothing"))
    elseif k === K"juxtapose"
        ret_k = K"call"
        pushfirst!(cs, symleaf("*"))
    elseif k === K"struct"
        is_mutable = valleaf(has_flags(st, MUTABLE_FLAG))
        pushfirst!(cs, is_mutable)
    elseif k === K"importpath"
        ret_k = K"."
        for i in eachindex(cs)
            if kind(cs[i]) === K"inert"
                inner_cs = preprocessed_green_children(cs[i])
                length(inner_cs) === 1 && (cs[i] = only(inner_cs))
            end
        end
    elseif k === K"wrapper" # parse errors only
        ret_k = K"block"
    elseif k === K"parameters"
        kw_in_params = kind(parent) === K"parameters" && parent_i === 1 ?
            kw_in_params : !(kind(parent) in KSet"vect curly braces ref")
    elseif k === K"="
        p_k = kind(parent)
        because_params = p_k === K"parameters" && parent_i >= 1 && kw_in_params
        because_call = parent_i > 1 && (p_k == K"ref" ||
            p_k in KSet"call dotcall" && is_prefix_call(parent))
        ret_k = because_params || because_call ? K"kw" : K"="
    end

    # Recurse on `cs`.  If no children change, just return `st`.
    ret_cs = _map_green_to_est(st, cs; kw_in_params)
    return ret_cs.ids == children(st).ids && ret_k == kind(st) ?
        st : setattr!(mknode(st, ret_cs), :kind, ret_k)
end

function _map_green_to_est(parent::SyntaxTree, cs::SyntaxList;
                           kw_in_params=false, undef_parent=false)
    ret_cs = SyntaxList(cs.graph)
    for (i, c) in enumerate(cs)
        new_c = _green_to_est(parent, undef_parent ? 0 : i, c; kw_in_params)
        @assert should_include_node(new_c)
        push!(ret_cs, new_c)
    end
    ret_cs
end

# When converting, first delete trivia and wrapper nodes in children so we can
# observe child kinds before recursing, thus creating fewer "temporary" nodes
function preprocessed_green_children(st::SyntaxTree)
    cs = filter(should_include_node, children(st))
    for i in eachindex(cs)
        while kind(cs[i]) in KSet"var char parens"
            inner_cs = preprocessed_green_children(cs[i])
            if length(inner_cs) === 1
                cs[i] = inner_cs[1]
            end
        end
    end
    return cs
end

# (call f a b (parameters c d) (parameters e)) =>
# (call f (parameters (parameters e) c d) a b)
function _reorder_parameters!(cs::SyntaxList, params_pos::Int)
    (length(cs) > params_pos && kind(cs[end]) === K"parameters") || return cs
    local param_ball = pop!(cs)
    while length(cs) >= 1 && kind(cs[end]) === K"parameters"
        next_ball_cs = pushfirst!(copy(children(cs[end])), param_ball)
        # `mknode` leaks nodes, but having multiple `parameters` blocks is
        # extremely rare nonsense syntax (`f(a,b;c=d;e)`)
        param_ball = mknode(cs[end], next_ball_cs)
        pop!(cs)
    end
    insert!(cs, params_pos, param_ball)
    nothing
end

# (call args... (do _...)) -> (do (call args...) (-> _...))
#
# Expects preprocessed and rearranged `args`
function _make_do_expression(st::SyntaxTree, args::SyntaxList, doex::SyntaxTree)
    ret_doex = _green_to_est(st, 0, doex)
    ret_callex = mknode(st, _map_green_to_est(st, args))
    return newnode(st._graph, st, K"do", tree_ids(ret_callex, ret_doex))
end

# A `string` or `cmdstring` may have multiple literal strings within (from
# newlines when triple-quoting).  A `string` may have interpolated values.
#
# (string "a" "b" "c") => "abc" # unwrap_literal=true
# (string "a" "b" "c" 1) => (string "abc" 1)
# (string "a" "b" (string "c" "d")) => (string "ab" (string "cd"))
#
# (cmdstring "a"::CmdString "b"::CmdString) => "ab"::CmdString
#
# Converting children-first (as _string_to_Expr does) would make this much
# harder by converting literal strings without the parent's knowledge
function _string_to_est(st::SyntaxTree, cs::SyntaxList; unwrap_literal)
    ret_cs = SyntaxList(st)
    literal_k = kind(st) === K"cmdstring" ? K"CmdString" : K"String"
    prev_str = cur_str = false
    next_str = length(cs) > 0 && kind(cs[1]) === literal_k
    buf = IOBuffer()
    for i in eachindex(cs)
        c = cs[i]
        (prev_str, cur_str) = (cur_str, next_str)
        next_str = i != lastindex(cs) && kind(cs[i+1]) === literal_k
        # optimization: push the current child mostly unchanged if the following
        # one isn't a literal string
        if !prev_str && cur_str && !next_str
            push!(ret_cs, c)
        elseif cur_str
            write(buf, c.value)
            if !next_str
                ret_c = newleaf(st._graph, st, literal_k)
                setattr!(ret_c, :value, String(take!(buf)))
                push!(ret_cs, ret_c)
            end
        else
            ret_c = kind(c) === K"string" ?
                _string_to_est(c, preprocessed_green_children(c);
                               unwrap_literal=false) :
                _green_to_est(st, i, c)

            push!(ret_cs, ret_c)
        end
    end
    if unwrap_literal && length(ret_cs) === 1 && kind(ret_cs[1]) === literal_k
        return ret_cs[1]
    end
    return mknode(st, ret_cs)
end

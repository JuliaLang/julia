const NodeId = Int

"""
Directed graph with arbitrary attributes on nodes. Used here for representing
one or several syntax trees.
"""
struct SyntaxGraph{Attrs}
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

function _show_attrs(io, attributes::Dict)
    show(io, MIME("text/plain"), attributes)
end
function _show_attrs(io, attributes::NamedTuple)
    show(io, MIME("text/plain"), Dict(pairs(attributes)...))
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
        else
            graph.attributes[k] = Dict{NodeId,v}()
        end
    end
    graph
end

function ensure_attributes(graph::SyntaxGraph; kws...)
    g = SyntaxGraph(graph.edge_ranges, graph.edges, Dict(pairs(graph.attributes)...))
    ensure_attributes!(g; kws...)
    freeze_attrs(g)
end

function newnode!(graph::SyntaxGraph)
    push!(graph.edge_ranges, 0:-1) # Invalid range start => leaf node
    return length(graph.edge_ranges)
end

function setchildren!(graph::SyntaxGraph, id, children::NodeId...)
    setchildren!(graph, id, children)
end

function setchildren!(graph::SyntaxGraph, id, children)
    n = length(graph.edges)
    graph.edge_ranges[id] = n+1:(n+length(children))
    # TODO: Reuse existing edges if possible
    append!(graph.edges, children)
end

function JuliaSyntax.haschildren(graph::SyntaxGraph, id)
    first(graph.edge_ranges[id]) > 0
end

function JuliaSyntax.numchildren(graph::SyntaxGraph, id)
    length(graph.edge_ranges[id])
end

function JuliaSyntax.children(graph::SyntaxGraph, id)
    @view graph.edges[graph.edge_ranges[id]]
end

function JuliaSyntax.children(graph::SyntaxGraph, id, r::UnitRange)
    @view graph.edges[graph.edge_ranges[id][r]]
end

function JuliaSyntax.child(graph::SyntaxGraph, id::NodeId, i::Integer)
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

# FIXME: Probably terribly non-inferrable?
function setattr!(graph::SyntaxGraph, id; attrs...)
    for (k,v) in pairs(attrs)
        getattr(graph, k)[id] = v
    end
end

function Base.getproperty(graph::SyntaxGraph, name::Symbol)
    # FIXME: Remove access to internals
    name === :edge_ranges && return getfield(graph, :edge_ranges)
    name === :edges       && return getfield(graph, :edges)
    name === :attributes  && return getfield(graph, :attributes)
    return getattr(graph, name)
end

function sethead!(graph, id::NodeId, h::JuliaSyntax.SyntaxHead)
    graph.kind[id] = kind(h)
    f = flags(h)
    if f != 0
        graph.syntax_flags[id] = f
    end
end

function sethead!(graph, id::NodeId, k::Kind)
    graph.kind[id] = k
end

function _convert_nodes(graph::SyntaxGraph, node::SyntaxNode)
    id = newnode!(graph)
    sethead!(graph, id, head(node))
    if !isnothing(node.val)
        v = node.val
        if v isa Symbol
            # TODO: Fixes in JuliaSyntax to avoid ever converting to Symbol
            setattr!(graph, id, name_val=string(v))
        else
            setattr!(graph, id, value=v)
        end
    end
    setattr!(graph, id, source=SourceRef(node.source, node.position, node.raw))
    if haschildren(node)
        cs = map(children(node)) do n
            _convert_nodes(graph, n)
        end
        setchildren!(graph, id, cs)
    end
    return id
end

"""
    syntax_graph(ctx)

Return `SyntaxGraph` associated with `ctx`
"""
syntax_graph(graph::SyntaxGraph) = graph

function check_same_graph(x, y)
    if syntax_graph(x) !== syntax_graph(y)
        @info "" syntax_graph(x) syntax_graph(y) x y
        error("Mismatching syntax graphs")
    end
end

function similar_graph(x, y)
    syntax_graph(x).edges === syntax_graph(y).edges
end

#-------------------------------------------------------------------------------
struct SyntaxTree{GraphType}
    graph::GraphType
    id::NodeId
end

function Base.getproperty(tree::SyntaxTree, name::Symbol)
    # FIXME: Remove access to internals
    name === :graph && return getfield(tree, :graph)
    name === :id  && return getfield(tree, :id)
    id = getfield(tree, :id)
    return get(getproperty(getfield(tree, :graph), name), id) do
        error("Property `$name[$id]` not found")
    end
end

function Base.setproperty!(tree::SyntaxTree, name::Symbol, val)
    return setattr!(tree.graph, tree.id; name=>val)
end

function Base.propertynames(tree::SyntaxTree)
    attrnames(tree)
end

function Base.get(tree::SyntaxTree, name::Symbol, default)
    attr = getattr(getfield(tree, :graph), name, nothing)
    return isnothing(attr) ? default :
           get(attr, getfield(tree, :id), default)
end

function Base.getindex(tree::SyntaxTree, i::Integer)
    child(tree, i)
end

function Base.getindex(tree::SyntaxTree, r::UnitRange)
    SyntaxList(tree.graph, children(tree.graph, tree.id, r))
end

Base.firstindex(tree::SyntaxTree) = 1
Base.lastindex(tree::SyntaxTree) = numchildren(tree)

function hasattr(tree::SyntaxTree, name::Symbol)
    attr = getattr(tree.graph, name, nothing)
    return !isnothing(attr) && haskey(attr, tree.id)
end

function attrnames(tree::SyntaxTree)
    attrs = tree.graph.attributes
    [name for (name, value) in pairs(attrs) if haskey(value, tree.id)]
end

function setattr!(ex::SyntaxTree; attrs...)
    setattr!(ex.graph, ex.id; attrs...)
end

# JuliaSyntax tree API

function JuliaSyntax.haschildren(tree::SyntaxTree)
    haschildren(tree.graph, tree.id)
end

function JuliaSyntax.numchildren(tree::SyntaxTree)
    numchildren(tree.graph, tree.id)
end

function JuliaSyntax.children(tree::SyntaxTree)
    SyntaxList(tree.graph, children(tree.graph, tree.id))
end

function JuliaSyntax.child(tree::SyntaxTree, i::Integer)
    SyntaxTree(tree.graph, child(tree.graph, tree.id, i))
end

function JuliaSyntax.head(tree::SyntaxTree)
    JuliaSyntax.SyntaxHead(kind(tree), flags(tree))
end

function JuliaSyntax.kind(tree::SyntaxTree)
    tree.kind
end

function JuliaSyntax.flags(tree::SyntaxTree)
    get(tree, :syntax_flags, 0x0000)
end


# Reference to bytes within a source file
struct SourceRef
    file::SourceFile
    first_byte::Int
    # TODO: Do we need the green node, or would last_byte suffice?
    green_tree::JuliaSyntax.GreenNode
end

JuliaSyntax.first_byte(src::SourceRef) = src.first_byte
JuliaSyntax.last_byte(src::SourceRef) = src.first_byte + span(src.green_tree) - 1
JuliaSyntax.filename(src::SourceRef) = filename(src.file)
JuliaSyntax.source_location(::Type{LineNumberNode}, src::SourceRef) = source_location(LineNumberNode, src.file, src.first_byte)
JuliaSyntax.source_location(src::SourceRef) = source_location(src.file, src.first_byte)
JuliaSyntax.sourcetext(src::SourceRef) = src.file[first_byte(src):last_byte(src)]

# TODO: Adding these methods to support LineNumberNode is kind of hacky but we
# can remove these after JuliaLowering becomes self-bootstrapping for macros
# and we a proper SourceRef for @ast's @HERE form.
JuliaSyntax.first_byte(src::LineNumberNode) = 0
JuliaSyntax.last_byte(src::LineNumberNode) = 0
JuliaSyntax.filename(src::LineNumberNode) = string(src.file)
JuliaSyntax.source_location(::Type{LineNumberNode}, src::LineNumberNode) = src
JuliaSyntax.source_location(src::LineNumberNode) = (src.line, 0)

function JuliaSyntax.highlight(io::IO, src::LineNumberNode; note="")
    print(io, src, " - ", note, "\n")
end

function JuliaSyntax.highlight(io::IO, src::SourceRef; kws...)
    highlight(io, src.file, first_byte(src):last_byte(src); kws...)
end

function Base.show(io::IO, ::MIME"text/plain", src::SourceRef)
    highlight(io, src; note="these are the bytes you're looking for 😊", context_lines_inner=20)
end

function sourceref(tree::SyntaxTree)
    sources = tree.graph.source
    id::NodeId = tree.id
    while true
        s = get(sources, id, nothing)
        if s isa NodeId
            id = s
        else
            return s
        end
    end
end

function is_ancestor(ex, ancestor)
    if !similar_graph(ex, ancestor)
        return false
    end
    sources = ex.graph.source
    id::NodeId = ex.id
    while true
        s = get(sources, id, nothing)
        if s isa NodeId
            id = s
            if id == ancestor.id
                return true
            end
        else
            return false
        end
    end
end

JuliaSyntax.filename(tree::SyntaxTree) = filename(sourceref(tree))
JuliaSyntax.source_location(::Type{LineNumberNode}, tree::SyntaxTree) = source_location(LineNumberNode, sourceref(tree))
JuliaSyntax.source_location(tree::SyntaxTree) = source_location(sourceref(tree))
JuliaSyntax.first_byte(tree::SyntaxTree) = first_byte(sourceref(tree))
JuliaSyntax.last_byte(tree::SyntaxTree) = last_byte(sourceref(tree))
JuliaSyntax.sourcetext(tree::SyntaxTree) = sourcetext(sourceref(tree))

const SourceAttrType = Union{SourceRef,LineNumberNode,NodeId}

function SyntaxTree(graph::SyntaxGraph, node::SyntaxNode)
    ensure_attributes!(graph, kind=Kind, syntax_flags=UInt16, source=SourceAttrType,
                       value=Any, name_val=String)
    id = _convert_nodes(freeze_attrs(graph), node)
    return SyntaxTree(graph, id)
end

function SyntaxTree(node::SyntaxNode)
    return SyntaxTree(SyntaxGraph(), node)
end

attrsummary(name, value) = string(name)
attrsummary(name, value::Number) = "$name=$value"

function _value_string(ex)
    k = kind(ex)
    str = k == K"Identifier" || k == K"MacroName" || is_operator(k) ? ex.name_val :
          k == K"Placeholder" ? ex.name_val :
          k == K"SSAValue"    ? "ssa"                 :
          k == K"core"        ? "core.$(ex.name_val)" :
          k == K"top"         ? "top.$(ex.name_val)"  :
          k == K"Symbol"      ? ":$(ex.name_val)" :
          k == K"globalref"   ? "$(ex.mod).$(ex.name_val)" :
          k == K"slot"        ? "slot" :
          repr(get(ex, :value, nothing))
    id = get(ex, :var_id, nothing)
    if !isnothing(id)
        idstr = replace(string(id),
                        "0"=>"₀", "1"=>"₁", "2"=>"₂", "3"=>"₃", "4"=>"₄",
                        "5"=>"₅", "6"=>"₆", "7"=>"₇", "8"=>"₈", "9"=>"₉")
        str = "$(str).$idstr"
    end
    if k == K"slot"
        # TODO: Ideally shouldn't need to rewrap the id here...
        srcex = SyntaxTree(ex.graph, ex.source)
        str = "$(str)/$(srcex.name_val)"
    end
    return str
end

function _show_syntax_tree(io, current_filename, node, indent, show_byte_offsets)
    sr = sourceref(node)
    if !isnothing(sr)
        fname = filename(sr)
        line, col = source_location(sr)
        posstr = "$(lpad(line, 4)):$(rpad(col,3))"
        if show_byte_offsets
            posstr *= "│$(lpad(first_byte(sr),6)):$(rpad(last_byte(sr),6))"
        end
    else
        fname = nothing
        posstr = "        "
        if show_byte_offsets
            posstr *= "│             "
        end
    end
    val = get(node, :value, nothing)
    nodestr = haschildren(node) ? "[$(untokenize(head(node)))]" : _value_string(node)

    treestr = string(indent, nodestr)

    std_attrs = Set([:name_val,:value,:kind,:syntax_flags,:source,:var_id])
    attrstr = join([attrsummary(n, getproperty(node, n)) for n in attrnames(node) if n ∉ std_attrs], ",")
    if !isempty(attrstr)
        treestr = string(rpad(treestr, 40), "│ $attrstr")
    end

    # Add filename if it's changed from the previous node
    if fname != current_filename[] && !isnothing(fname)
        #println(io, "# ", fname)
        treestr = string(rpad(treestr, 80), "│$fname")
        current_filename[] = fname
    end
    println(io, posstr, "│", treestr)
    if haschildren(node)
        new_indent = indent*"  "
        for n in children(node)
            _show_syntax_tree(io, current_filename, n, new_indent, show_byte_offsets)
        end
    end
end

function Base.show(io::IO, ::MIME"text/plain", tree::SyntaxTree; show_byte_offsets=false)
    println(io, "line:col│ tree                                   │ attributes                            | file_name")
    _show_syntax_tree(io, Ref{Union{Nothing,String}}(nothing), tree, "", show_byte_offsets)
end

function _show_syntax_tree_sexpr(io, ex)
    if !haschildren(ex)
        if is_error(ex)
            print(io, "(", untokenize(head(ex)), ")")
        else
            print(io, _value_string(ex))
        end
    else
        print(io, "(", untokenize(head(ex)))
        first = true
        for n in children(ex)
            print(io, ' ')
            _show_syntax_tree_sexpr(io, n)
            first = false
        end
        print(io, ')')
    end
end

function Base.show(io::IO, ::MIME"text/x.sexpression", node::SyntaxTree)
    _show_syntax_tree_sexpr(io, node)
end

function Base.show(io::IO, node::SyntaxTree)
    _show_syntax_tree_sexpr(io, node)
end

function reparent(ctx, ex::SyntaxTree)
    # Ensure `ex` has the same parent graph, in a somewhat loose sense.
    # Could relax by copying if necessary?
    # In that case, would we copy all the attributes? That would have slightly
    # different semantics.
    graph = syntax_graph(ctx)
    @assert graph.edge_ranges === ex.graph.edge_ranges
    SyntaxTree(graph, ex.id)
end

function ensure_attributes(ex::SyntaxTree; kws...)
    reparent(ensure_attributes(syntax_graph(ex); kws...), ex)
end

syntax_graph(ex::SyntaxTree) = ex.graph

function JuliaSyntax.build_tree(::Type{SyntaxTree}, stream::JuliaSyntax.ParseStream; kws...)
    SyntaxTree(JuliaSyntax.build_tree(SyntaxNode, stream; kws...))
end


#-------------------------------------------------------------------------------
# Lightweight vector of nodes ids with associated pointer to graph stored separately.
struct SyntaxList{GraphType, NodeIdVecType} <: AbstractVector{SyntaxTree}
    graph::GraphType
    ids::NodeIdVecType
end

function SyntaxList(graph::SyntaxGraph, ids::AbstractVector{NodeId})
    SyntaxList{typeof(graph), typeof(ids)}(graph, ids)
end

SyntaxList(graph::SyntaxGraph) = SyntaxList(graph, Vector{NodeId}())
SyntaxList(ctx) = SyntaxList(syntax_graph(ctx))

syntax_graph(lst::SyntaxList) = lst.graph

Base.size(v::SyntaxList) = size(v.ids)

Base.IndexStyle(::Type{<:SyntaxList}) = IndexLinear()

Base.getindex(v::SyntaxList, i::Int) = SyntaxTree(v.graph, v.ids[i])

function Base.getindex(v::SyntaxList, r::UnitRange)
    SyntaxList(v.graph, view(v.ids, r))
end

function Base.setindex!(v::SyntaxList, tree::SyntaxTree, i::Int)
    check_same_graph(v, tree)
    v.ids[i] = tree.id
end

function Base.setindex!(v::SyntaxList, id::NodeId, i::Int)
    v.ids[i] = id
end

function Base.push!(v::SyntaxList, tree::SyntaxTree)
    check_same_graph(v, tree)
    push!(v.ids, tree.id)
end

function Base.append!(v::SyntaxList, exs)
    for e in exs
        push!(v, e)
    end
    v
end

function Base.append!(v::SyntaxList, exs::SyntaxList)
    check_same_graph(v, exs)
    append!(v.ids, exs.ids)
    v
end

function Base.push!(v::SyntaxList, id::NodeId)
    push!(v.ids, id)
end

function Base.pop!(v::SyntaxList)
    SyntaxTree(v.graph, pop!(v.ids))
end


using Base.Iterators: Reverse

"""
    prev_sibling_assumed(cursor::GreenTreeCursor)::Union{Nothing, GreenTreeCursor}
    prev_sibling_assumed(cursor::RedTreeCursor)::Union{Nothing, RedTreeCursor}

Gives the previous sibling of the current node, but makes the assumption that
there is one or that we are at the top level.
Without knowing the parent, we cannot otherwise know which the last sibling is,
unless we are at the top level in which case `nothing` is returned.
"""
function prev_sibling_assumed end

"""
    GreenTreeCursor

Represents a cursors into a ParseStream output buffer that makes it easy to
work with the green tree representation.
"""
struct GreenTreeCursor
    parser_output::Vector{RawGreenNode}
    position::UInt32
end
GreenTreeCursor(stream::ParseStream) = GreenTreeCursor(stream.output, length(stream.output))
this(node::GreenTreeCursor) = node.parser_output[node.position]

const SENTINEL_INDEX = UInt32(1)
function prev_sibling_assumed(cursor::GreenTreeCursor)
    next_idx = cursor.position - this(cursor).node_span - UInt32(1)
    next_idx == SENTINEL_INDEX && return nothing
    GreenTreeCursor(cursor.parser_output, next_idx)
end

function Base.in(child::GreenTreeCursor, parent::GreenTreeCursor)
    @assert child.parser_output === parent.parser_output
    child.position < parent.position || return false
    return child.position >= parent.position - this(parent).node_span
end

# Debug printing
function Base.show(io::IO, node::GreenTreeCursor)
    print(io, Base.summary(this(node)), " @", node.position)
end

# Reverse iterator interface
Base.reverse(node::GreenTreeCursor) = Base.Iterators.Reverse(node)
Base.IteratorSize(::Type{Reverse{GreenTreeCursor}}) = Base.SizeUnknown()
@inline function Base.iterate(node::Reverse{GreenTreeCursor},
                              (next_idx, final)::NTuple{2, UInt32} =
                              (node.itr.position-UInt32(1), node.itr.position - this(node.itr).node_span - UInt32(1)))::Union{Nothing, Tuple{GreenTreeCursor, NTuple{2, UInt32}}}
    node = node.itr
    while true
        next_idx == final && return nothing
        next_node = GreenTreeCursor(node.parser_output, next_idx)
        nrgn = this(next_node)
        if getfield(nrgn, :head).kind == K"TOMBSTONE"
            # TOMBSTONED nodes are counted as part of the size of the tree, but
            # do not contribute either byte ranges or children.
            next_idx -= UInt32(1)
            continue
        end
        # Inlined prev_sibling_assumed
        new_next_idx = next_idx - nrgn.node_span - UInt32(1)
        return (next_node, (new_next_idx, final))
    end
end

# Accessors / predicates
is_leaf(node::GreenTreeCursor)     = !is_non_terminal(this(node))
head(node::GreenTreeCursor)        = this(node).head
treesize(node::GreenTreeCursor)    = this(node).node_span
is_non_terminal(node::GreenTreeCursor) = is_non_terminal(this(node))

"""
    span(node)

Get the number of bytes this node covers in the source text.
"""
span(node::GreenTreeCursor) = this(node).byte_span

"""
    RedTreeCursor

Wraps a `GreenTreeCursor` to keep track of the absolute position of the node
in the original source text.
"""
struct RedTreeCursor
    green::GreenTreeCursor
    # The last byte that is still part of the node
    byte_end::UInt32
end
RedTreeCursor(stream::ParseStream) = RedTreeCursor(
    GreenTreeCursor(stream), stream.next_byte - UInt32(1))

function prev_sibling_assumed(cursor::RedTreeCursor)
    prevgreen = prev_sibling_assumed(cursor.green)
    if prevgreen === nothing
        return nothing
    end
    return RedTreeCursor(prevgreen, cursor.byte_end - span(cursor))
end


Base.reverse(node::RedTreeCursor) = Base.Iterators.Reverse(node)
Base.IteratorSize(::Type{Reverse{RedTreeCursor}}) = Base.SizeUnknown()
@inline function Base.iterate(node::Reverse{RedTreeCursor})::Union{Nothing, Tuple{RedTreeCursor, NTuple{3, UInt32}}}
    r = iterate(Reverse(node.itr.green))
    return _iterate_red_cursor(r, node.itr.byte_end)
end

@inline function Base.iterate(node::Reverse{RedTreeCursor}, state::NTuple{3, UInt32})::Union{Nothing, Tuple{RedTreeCursor, NTuple{3, UInt32}}}
    r = iterate(Reverse(node.itr.green), Base.tail(state))
    return _iterate_red_cursor(r, first(state))
end

@inline function _iterate_red_cursor(r, byte_end)
    r === nothing && return nothing
    next_node, next_idx = r
    return RedTreeCursor(next_node, byte_end),
           (byte_end - span(next_node), next_idx...)
end

is_leaf(node::RedTreeCursor)     = is_leaf(node.green)
head(node::RedTreeCursor)        = head(node.green)
span(node::RedTreeCursor)        = span(node.green)
byte_range(node::RedTreeCursor)  = (node.byte_end - span(node.green) + UInt32(1)):node.byte_end
treesize(node::RedTreeCursor)    = treesize(node.green)
is_non_terminal(node::RedTreeCursor) = is_non_terminal(node.green)

function Base.show(io::IO, node::RedTreeCursor)
    print(io, node.green, " [", byte_range(node), "]")
end

has_toplevel_siblings(cursor::GreenTreeCursor) =
    treesize(cursor)+1 != length(cursor.parser_output)-1
has_toplevel_siblings(cursor::RedTreeCursor) =
    has_toplevel_siblings(cursor.green)
struct TopLevelSiblingIterator{C}
    cursor::C
end

function reverse_toplevel_siblings(cursor::RedTreeCursor)
    @assert cursor.green.position == length(cursor.green.parser_output)
    TopLevelSiblingIterator(cursor)
end

function reverse_toplevel_siblings(cursor::GreenTreeCursor)
    @assert cursor.position == length(cursor.parser_output)
    TopLevelSiblingIterator(cursor)
end

function Base.iterate(tsi::TopLevelSiblingIterator)
    return (tsi.cursor, tsi.cursor)
end
function Base.iterate(cursor::TopLevelSiblingIterator{C}, last::C) where {C}
    this = prev_sibling_assumed(last)
    this === nothing && return nothing
    return (this, this)
end

# HACK: Force inlining of `filter` for our cursors to avoid significant perf
# degradation.
@inline function Base.iterate(f::Iterators.Filter{<:Any, Iterators.Reverse{T}}, state...) where {T<:Union{RedTreeCursor, GreenTreeCursor}}
    y = iterate(f.itr, state...)
    while y !== nothing
        if f.flt(y[1])
            return y
        end
        y = iterate(f.itr, y[2])
    end
    nothing
end

Base.in(child::GreenTreeCursor, parent::RedTreeCursor) =
    in(child, parent.green)

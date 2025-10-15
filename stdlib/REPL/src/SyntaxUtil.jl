module SyntaxUtil

import Base.JuliaSyntax: build_tree
using Base.JuliaSyntax:
    AbstractSyntaxData, GreenNode, Kind, ParseStream, SourceFile, SyntaxHead, SyntaxNode, TreeNode,
    byte_range, children, first_byte, head, is_leaf, is_trivia, kind, parse_julia_literal, span,
    @K_str, _unsafe_wrap_substring

export CursorNode, char_range, char_last, children_nt, find_delim, seek_pos

# Like SyntaxNode, but keeps trivia, and tracks each child's index in its parent.
# Extracted from JuliaSyntax/src/syntax_tree.jl
# TODO: don't duplicate so much code?
struct CursorData <: AbstractSyntaxData
    source::SourceFile
    raw::GreenNode{SyntaxHead}
    position::Int
    index::Int
    index_nt::Int # nth non-trivia in parent
    val::Any
end

const CursorNode = TreeNode{CursorData}

function CursorNode(source::SourceFile, raw::GreenNode{SyntaxHead};
                    position::Integer=1)
    GC.@preserve source begin
        raw_offset, txtbuf = _unsafe_wrap_substring(source.code)
        offset = raw_offset - source.byte_offset
        _to_CursorNode(source, txtbuf, offset, raw, convert(Int, position))
    end
end

function _to_CursorNode(source::SourceFile, txtbuf::Vector{UInt8}, offset::Int,
                        raw::GreenNode{SyntaxHead},
                        position::Int, index::Int=-1, index_nt::Int=-1)
    if is_leaf(raw)
        valrange = position:position + span(raw) - 1
        val = parse_julia_literal(txtbuf, head(raw), valrange .+ offset)
        return CursorNode(nothing, nothing, CursorData(source, raw, position, index, index_nt, val))
    else
        cs = CursorNode[]
        pos = position
        i_nt = 1
        for (i,rawchild) in enumerate(children(raw))
            push!(cs, _to_CursorNode(source, txtbuf, offset, rawchild, pos, i, i_nt))
            pos += Int(rawchild.span)
            i_nt += !is_trivia(rawchild)
        end
        node = CursorNode(nothing, cs, CursorData(source, raw, position, index, index_nt, nothing))
        for c in cs
            c.parent = node
        end
        return node
    end
end

function build_tree(::Type{CursorNode}, stream::ParseStream;
                    filename=nothing, first_line=1, kws...)
    green_tree = build_tree(GreenNode, stream; kws...)
    source = SourceFile(stream, filename=filename, first_line=first_line)
    CursorNode(source, green_tree, position=first_byte(stream))
end

Base.show(io::IO, node::CursorNode) = show(io, MIME("text/plain"), node.raw)
Base.show(io::IO, mime::MIME{Symbol("text/plain")}, node::CursorNode) = show(io, mime, node.raw)

function Base.Expr(node::CursorNode)
    (; filename, first_line) = node.source
    src = SourceFile(node.source[byte_range(node)]; filename, first_line)
    Expr(SyntaxNode(src, node.raw))
end

char_range(node) = node.position:char_last(node)
char_last(node) = thisind(node.source, node.position + span(node) - 1)

children_nt(node) = [n for n in children(node) if !is_trivia(n)]

function seek_pos(node, pos)
    pos in byte_range(node) || return nothing
    (cs = children(node)) === nothing && return node
    for n in cs
        c = seek_pos(n, pos)
        c === nothing || return c
    end
    node
end

find_parent(node, k::Kind) = find_parent(node, n -> kind(n) == k)
function find_parent(node, f::Function)
    while node !== nothing && !f(node)
        node = node.parent
    end
    node
end

# Return the character range between left_kind and right_kind in node.  The left
# delimiter must be present, while the range will extend to the rest of the node
# if the right delimiter is missing.
function find_delim(node, left_kind, right_kind)
    cs = children(node)
    left = findfirst(c -> kind(c) == left_kind, cs)
    left !== nothing || return nothing, nothing
    right = findlast(c -> kind(c) == right_kind, cs)
    closed = right !== nothing && right != left
    right = closed ? thisind(node.source, cs[right].position - 1) : char_last(node)
    left = nextind(node.source, char_last(cs[left]))
    return left:right, closed
end

end

#-------------------------------------------------------------------------------
# AST interface, built on top of raw tree

"""
Design options:
* rust-analyzer treats their version of an untyped syntax node as a cursor into
  the green tree. They deallocate aggressively.
"""
mutable struct SyntaxNode
    source::SourceFile
    raw::GreenNode{SyntaxHead}
    position::Int
    parent::Union{Nothing,SyntaxNode}
    is_leaf::Bool
    val::Any
end

# Value of an error node with no children
struct ErrorVal
end

Base.show(io::IO, ::ErrorVal) = printstyled(io, "✘", color=:light_red)

function SyntaxNode(source::SourceFile, raw::GreenNode{SyntaxHead}, position::Integer=1)
    if !haschildren(raw) && !(is_syntax_kind(raw) || is_keyword(raw))
        # Leaf node
        k = kind(raw)
        val_range = position:position + span(raw) - 1
        val_str = view(source, val_range)
        # Here we parse the values eagerly rather than representing them as
        # strings. Maybe this is good. Maybe not.
        val = if k in KSet"Integer Float BinInt OctInt HexInt"
            julia_string_to_number(val_str, k)
        elseif k == K"true"
            true
        elseif k == K"false"
            false
        elseif k == K"Char"
            unescape_julia_string(val_str, false, false)[2]
        elseif k == K"Identifier"
            if has_flags(head(raw), RAW_STRING_FLAG)
                s = unescape_julia_string(val_str, false, true)
                Symbol(normalize_identifier(s))
            else
                Symbol(normalize_identifier(val_str))
            end
        elseif is_keyword(k)
            # This should only happen for tokens nested inside errors
            Symbol(val_str)
        elseif k in KSet"String CmdString"
            is_cmd = k == K"CmdString"
            is_raw = has_flags(head(raw), RAW_STRING_FLAG)
            unescape_julia_string(val_str, is_cmd, is_raw)
        elseif is_operator(k)
            isempty(val_range)  ?
                Symbol(untokenize(k)) : # synthetic invisible tokens
                Symbol(normalize_identifier(val_str))
        elseif k == K"nothing"
            nothing
        elseif k == K"error"
            ErrorVal()
        elseif k == K"@."
            :var"@__dot__"
        elseif k == K"MacroName"
            Symbol("@$(normalize_identifier(val_str))")
        elseif k == K"StringMacroName"
            Symbol("@$(normalize_identifier(val_str))_str")
        elseif k == K"CmdMacroName"
            Symbol("@$(normalize_identifier(val_str))_cmd")
        elseif k == K"core_@doc"
            GlobalRef(Core, :var"@doc")
        elseif k == K"core_@cmd"
            GlobalRef(Core, :var"@cmd")
        elseif is_syntax_kind(raw)
            nothing
        else
            error("Leaf node of kind $k unknown to SyntaxNode")
            val = nothing
        end
        return SyntaxNode(source, raw, position, nothing, true, val)
    else
        cs = SyntaxNode[]
        pos = position
        for (i,rawchild) in enumerate(children(raw))
            # FIXME: Allowing trivia is_error nodes here corrupts the tree layout.
            if !is_trivia(rawchild) || is_error(rawchild)
                push!(cs, SyntaxNode(source, rawchild, pos))
            end
            pos += rawchild.span
        end
        node = SyntaxNode(source, raw, position, nothing, false, cs)
        for c in cs
            c.parent = node
        end
        return node
    end
end

head(node::SyntaxNode) = head(node.raw)

haschildren(node::SyntaxNode) = !node.is_leaf
children(node::SyntaxNode) = haschildren(node) ? node.val::Vector{SyntaxNode} : ()

span(node::SyntaxNode) = span(node.raw)

"""
    sourcetext(node)

Get the full source text of a node.
"""
function sourcetext(node::SyntaxNode)
    val_range = (node.position-1) .+ (1:span(node))
    view(node.source, val_range)
end

function interpolate_literal(node::SyntaxNode, val)
    @assert kind(node) == K"$"
    SyntaxNode(node.source, node.raw, node.position, node.parent, true, val)
end

function _show_syntax_node(io, current_filename, node, indent)
    fname = node.source.filename
    line, col = source_location(node.source, node.position)
    posstr = "$(lpad(line, 4)):$(rpad(col,3))│$(lpad(node.position,6)):$(rpad(node.position+span(node)-1,6))│"
    nodestr = haschildren(node)   ?  "[$(untokenize(head(node)))]" :
              node.val isa Symbol ? string(node.val) :
              repr(node.val)
    treestr = string(indent, nodestr)
    # Add filename if it's changed from the previous node
    if fname != current_filename[]
        #println(io, "# ", fname)
        treestr = string(rpad(treestr, 40), "│$fname")
        current_filename[] = fname
    end
    println(io, posstr, treestr)
    if haschildren(node)
        new_indent = indent*"  "
        for n in children(node)
            _show_syntax_node(io, current_filename, n, new_indent)
        end
    end
end

function _show_syntax_node_sexpr(io, node)
    if !haschildren(node)
        if is_error(node)
            print(io, "(", untokenize(head(node)), ")")
        else
            print(io, node.val isa Symbol ? string(node.val) : repr(node.val))
        end
    else
        print(io, "(", untokenize(head(node)))
        first = true
        for n in children(node)
            print(io, ' ')
            _show_syntax_node_sexpr(io, n)
            first = false
        end
        print(io, ')')
    end
end

function Base.show(io::IO, ::MIME"text/plain", node::SyntaxNode)
    println(io, "line:col│ byte_range  │ tree                                   │ file_name")
    _show_syntax_node(io, Ref{Union{Nothing,String}}(nothing), node, "")
end

function Base.show(io::IO, ::MIME"text/x.sexpression", node::SyntaxNode)
    _show_syntax_node_sexpr(io, node)
end

function Base.show(io::IO, node::SyntaxNode)
    _show_syntax_node_sexpr(io, node)
end

function Base.push!(node::SyntaxNode, child::SyntaxNode)
    if !haschildren(node)
        error("Cannot add children")
    end
    args = node.val::Vector{SyntaxNode}
    push!(args, child)
end

function build_tree(::Type{SyntaxNode}, stream::ParseStream; filename=nothing, kws...)
    green_tree = build_tree(GreenNode, stream; kws...)
    source = SourceFile(sourcetext(stream), filename=filename)
    SyntaxNode(source, green_tree, first_byte(stream))
end

#-------------------------------------------------------------------------------
# Tree utilities

"""
    child(node, i1, i2, ...)

Get child at a tree path. If indexing accessed children, it would be
`node[i1][i2][...]`
"""
function child(node, path::Integer...)
    n = node
    for index in path
        n = children(n)[index]
    end
    return n
end

function setchild!(node::SyntaxNode, path, x)
    n1 = child(node, path[1:end-1]...)
    n1.val[path[end]] = x
end

# We can overload multidimensional Base.getindex / Base.setindex! for node
# types.
#
# The justification for this is to view a tree as a multidimensional ragged
# array, where descending depthwise into the tree corresponds to dimensions of
# the array.
#
# However... this analogy is only good for complete trees at a given depth (=
# dimension). But the syntax is oh-so-handy!
function Base.getindex(node::Union{SyntaxNode,GreenNode}, path::Int...)
    child(node, path...)
end
function Base.setindex!(node::SyntaxNode, x::SyntaxNode, path::Int...)
    setchild!(node, path, x)
end

"""
Get absolute position and span of the child of `node` at the given tree `path`.
"""
function child_position_span(node::GreenNode, path::Int...)
    n = node
    p = 1
    for index in path
        cs = children(n)
        for i = 1:index-1
            p += span(cs[i])
        end
        n = cs[index]
    end
    return n, p, n.span
end

function child_position_span(node::SyntaxNode, path::Int...)
    n = child(node, path...)
    n, n.position, span(n)
end

"""
Print the code, highlighting the part covered by `node` at tree `path`.
"""
function highlight(code::String, node, path::Int...; color=(40,40,70))
    node, p, span = child_position_span(node, path...)
    q = p + span
    print(stdout, code[1:p-1])
    _printstyled(stdout, code[p:q-1]; bgcolor=color)
    print(stdout, code[q:end])
end

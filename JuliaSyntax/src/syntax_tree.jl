#-------------------------------------------------------------------------------
# Syntax tree types

#-------------------------------------------------------------------------------
# Flags hold auxilary information about tokens/nonterminals which the Kind
# doesn't capture in a nice way.
const RawFlags = UInt32
const EMPTY_FLAGS = RawFlags(0)
const TRIVIA_FLAG = RawFlags(1<<0)
# Some of the following flags are head-specific and could probably be allowed
# to cover the same bits...
const INFIX_FLAG  = RawFlags(1<<1)
# Record whether syntactic operators were dotted
const DOTOP_FLAG = RawFlags(1<<2)
# Set when kind == K"String" was triple-delimited as with """ or ```
const TRIPLE_STRING_FLAG = RawFlags(1<<3)
# Set when the string is "raw" and needs minimal unescaping
const RAW_STRING_FLAG = RawFlags(1<<4)
# try-finally-catch
const TRY_CATCH_AFTER_FINALLY_FLAG = RawFlags(1<<5)
# Flags holding the dimension of an nrow or other UInt8 not held in the source
const NUMERIC_FLAGS = RawFlags(RawFlags(0xff)<<8)
# Todo ERROR_FLAG = 0x80000000 ?

function set_numeric_flags(n::Integer)
    f = RawFlags((n << 8) & NUMERIC_FLAGS)
    if numeric_flags(f) != n
        error("Numeric flags unable to hold large integer $n")
    end
    f
end

function numeric_flags(f::RawFlags)
    Int((f >> 8) % UInt8)
end

# Return true if any of `test_flags` are set
has_flags(flags::RawFlags, test_flags) = (flags & test_flags) != 0

# Function for combining flags. (Do we want this?)
function flags(; trivia::Bool=false,
               infix::Bool=false,
               dotop::Bool=false,
               try_catch_after_finally::Bool=false,
               numeric::Int=0)
    flags = RawFlags(0)
    trivia && (flags |= TRIVIA_FLAG)
    infix  && (flags |= INFIX_FLAG)
    dotop  && (flags |= DOTOP_FLAG)
    try_catch_after_finally && (flags |= TRY_CATCH_AFTER_FINALLY_FLAG)
    numeric != 0 && (flags |= set_numeric_flags(numeric))
    return flags::RawFlags
end

#-------------------------------------------------------------------------------
struct SyntaxHead
    kind::Kind
    flags::RawFlags
end

kind(head::SyntaxHead) = head.kind
flags(head::SyntaxHead) = head.flags
has_flags(head::SyntaxHead, test_flags) = has_flags(flags(head), test_flags)

is_trivia(head::SyntaxHead) = has_flags(head, TRIVIA_FLAG)
is_infix(head::SyntaxHead)  = has_flags(head, INFIX_FLAG)
is_dotted(head::SyntaxHead) = has_flags(head, DOTOP_FLAG)
numeric_flags(head::SyntaxHead) = numeric_flags(flags(head))
is_error(head::SyntaxHead)  = kind(head) == K"error"

function Base.summary(head::SyntaxHead)
    _kind_str(kind(head))
end

function untokenize(head::SyntaxHead; include_flag_suff=true)
    str = untokenize(kind(head))
    if is_dotted(head)
        str = "."*str
    end
    if include_flag_suff && flags(head) ∉ (EMPTY_FLAGS, DOTOP_FLAG)
        str = str*"-"
        is_trivia(head)  && (str = str*"t")
        is_infix(head)   && (str = str*"i")
        has_flags(head, TRIPLE_STRING_FLAG) && (str = str*"s")
        has_flags(head, RAW_STRING_FLAG) && (str = str*"r")
        has_flags(head, TRY_CATCH_AFTER_FINALLY_FLAG) && (str = str*"f")
        n = numeric_flags(head)
        n != 0 && (str = str*string(n))
    end
    str
end

kind(node::GreenNode{SyntaxHead})  = head(node).kind
flags(node::GreenNode{SyntaxHead}) = head(node).flags

is_infix(node) = is_infix(head(node))

# Value of an error node with no children
struct ErrorVal
end

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

Base.show(io::IO, ::ErrorVal) = printstyled(io, "✘", color=:light_red)

function SyntaxNode(source::SourceFile, raw::GreenNode{SyntaxHead}, position::Integer=1)
    if !haschildren(raw) && !(is_syntax_kind(raw) || is_keyword(raw))
        # Leaf node
        k = kind(raw)
        val_range = position:position + span(raw) - 1
        val_str = source[val_range]
        # Here we parse the values eagerly rather than representing them as
        # strings. Maybe this is good. Maybe not.
        val = if k in (K"Integer", K"BinInt", K"OctInt", K"HexInt")
            julia_string_to_number(Int, val_str, k)
        elseif k == K"Float"
            # FIXME: Other float types!
            julia_string_to_number(Float64, val_str, k)
        elseif k == K"true"
            true
        elseif k == K"false"
            false
        elseif k == K"Char"
            unescape_julia_string(val_str, false, false)[2]
        elseif k == K"Identifier"
            Symbol(val_str)
        elseif k == K"VarIdentifier"
            Symbol(val_str[5:end-1])
        elseif is_keyword(k)
            # This should only happen for tokens nested inside errors
            Symbol(val_str)
        elseif k in KSet`String CmdString`
            is_cmd = k == K"CmdString"
            is_raw = has_flags(head(raw), RAW_STRING_FLAG)
            has_flags(head(raw), TRIPLE_STRING_FLAG) ?
                process_triple_strings!([val_str], is_raw)[1] :
                unescape_julia_string(val_str, is_cmd, is_raw)
        elseif is_operator(k)
            isempty(val_range)  ?
                Symbol(untokenize(k)) : # synthetic invisible tokens
                Symbol(val_str)
        elseif k == K"NothingLiteral"
            nothing
        elseif k == K"error"
            ErrorVal()
        elseif k == K"@."
            :var"@__dot__"
        elseif k == K"MacroName"
            Symbol("@$val_str")
        elseif k == K"VarMacroName"
            Symbol("@$(val_str[5:end-1])")
        elseif k == K"StringMacroName"
            Symbol("@$(val_str)_str")
        elseif k == K"CmdMacroName"
            Symbol("@$(val_str)_cmd")
        elseif k == K"core_@doc"
            GlobalRef(Core, :var"@doc")
        elseif k == K"core_@cmd"
            GlobalRef(Core, :var"@cmd")
        elseif is_syntax_kind(raw)
            nothing
        else
            @error "Leaf node of kind $k unknown to SyntaxNode"
            val = nothing
        end
        return SyntaxNode(source, raw, position, nothing, true, val)
    else
        cs = SyntaxNode[]
        pos = position
        if kind(raw) == K"string" && has_flags(head(raw), TRIPLE_STRING_FLAG)
            # Triple quoted strings need special processing of sibling String literals
            strs = SubString[]
            str_nodes = SyntaxNode[]
            for (i,rawchild) in enumerate(children(raw))
                if !is_trivia(rawchild) || is_error(rawchild)
                    if kind(rawchild) == K"String"
                        val_range = pos:pos + span(rawchild) - 1
                        push!(strs, source[val_range])
                        n = SyntaxNode(source, rawchild, pos, nothing, true, nothing)
                        push!(cs, n)
                        push!(str_nodes, n)
                    else
                        push!(cs, SyntaxNode(source, rawchild, pos))
                    end
                end
                pos += rawchild.span
            end
            is_raw = has_flags(head(raw), RAW_STRING_FLAG)
            process_triple_strings!(strs, is_raw)
            for (s,n) in zip(strs, str_nodes)
                n.val = s
            end
        else
            for (i,rawchild) in enumerate(children(raw))
                # FIXME: Allowing trivia is_error nodes here corrupts the tree layout.
                if !is_trivia(rawchild) || is_error(rawchild)
                    push!(cs, SyntaxNode(source, rawchild, pos))
                end
                pos += rawchild.span
            end
        end
        node = SyntaxNode(source, raw, position, nothing, false, cs)
        for c in cs
            c.parent = node
        end
        return node
    end
end

is_error(node::SyntaxNode) = is_error(node.raw)
is_trivia(node::SyntaxNode) = is_trivia(node.raw)
has_flags(node::SyntaxNode, f) = has_flags(head(node), f)

head(node::SyntaxNode) = head(node.raw)
kind(node::SyntaxNode)  = kind(node.raw)
flags(node::SyntaxNode) = flags(node.raw)

haschildren(node::SyntaxNode) = !node.is_leaf
children(node::SyntaxNode) = haschildren(node) ? node.val::Vector{SyntaxNode} : ()

span(node::SyntaxNode) = span(node.raw)

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
            print(io, "(error)")
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
    _printstyled(stdout, code[p:q-1]; color=color)
    print(stdout, code[q:end])
end


#-------------------------------------------------------------------------------
# Conversion to Base.Expr

function is_eventually_call(ex)
    return Meta.isexpr(ex, :call) || (Meta.isexpr(ex, (:where, :(::))) &&
                                      is_eventually_call(ex.args[1]))
end

function _to_expr(node::SyntaxNode)
    if !haschildren(node)
        return node.val
    end
    args = Vector{Any}(undef, length(children(node)))
    args = map!(_to_expr, args, children(node))
    # Julia's standard `Expr` ASTs have children stored in a canonical
    # order which is often not always source order. We permute the children
    # here as necessary to get the canonical order.
    if is_infix(node.raw)
        args[2], args[1] = args[1], args[2]
    end
    loc = source_location(LineNumberNode, node.source, node.position)

    headstr = untokenize(head(node), include_flag_suff=false)
    headsym = !isnothing(headstr) ? Symbol(headstr) :
        error("Can't untokenize head of kind $(kind(node))")
    # Convert elements
    if headsym == :macrocall
        insert!(args, 2, loc)
    elseif headsym in (:call, :ref)
        # Move parameters block to args[2]
        if length(args) > 1 && Meta.isexpr(args[end], :parameters)
            insert!(args, 2, args[end])
            pop!(args)
        end
    elseif headsym in (:tuple, :parameters, :vect)
        # Move parameters blocks to args[1]
        if length(args) > 1 && Meta.isexpr(args[end], :parameters)
            pushfirst!(args, args[end])
            pop!(args)
        end
    elseif headsym == :try
        # Try children in source order:
        #   try_block catch_var catch_block else_block finally_block
        # Expr ordering:
        #   try_block catch_var catch_block [finally_block] [else_block]
        catch_ = nothing
        if has_flags(node, TRY_CATCH_AFTER_FINALLY_FLAG)
            catch_ = pop!(args)
            catch_var = pop!(args)
        end
        finally_ = pop!(args)
        else_ = pop!(args)
        if has_flags(node, TRY_CATCH_AFTER_FINALLY_FLAG)
            pop!(args)
            pop!(args)
            push!(args, catch_var)
            push!(args, catch_)
        end
        # At this point args is
        # [try_block catch_var catch_block]
        if finally_ !== false
            push!(args, finally_)
        end
        if else_ !== false
            push!(args, else_)
        end
    elseif headsym == :filter
        pushfirst!(args, last(args))
        pop!(args)
    elseif headsym == :flatten
        # The order of nodes inside the generators in Julia's flatten AST
        # is noncontiguous in the source text, so need to reconstruct
        # Julia's AST here from our alternative `flatten` expression.
        gen = Expr(:generator, args[1], args[end])
        for i in length(args)-1:-1:2
            gen = Expr(:generator, gen, args[i])
        end
        args = [gen]
    elseif headsym in (:nrow, :ncat)
        # For lack of a better place, the dimension argument to nrow/ncat
        # is stored in the flags
        pushfirst!(args, numeric_flags(flags(node)))
    elseif headsym == :typed_ncat
        insert!(args, 2, numeric_flags(flags(node)))
    elseif headsym == :(=)
        if is_eventually_call(args[1])
            if Meta.isexpr(args[2], :block)
                pushfirst!(args[2].args, loc)
            else
                # Add block for short form function locations
                args[2] = Expr(:block, loc, args[2])
            end
        end
    elseif headsym == :(->)
        if Meta.isexpr(args[2], :block)
            pushfirst!(args[2].args, loc)
        else
            # Add block for source locations
            args[2] = Expr(:block, loc, args[2])
        end
    end
    if headsym == :inert || (headsym == :quote &&
                                length(args) == 1 && !(only(args) isa Expr))
        return QuoteNode(only(args))
    else
        return Expr(headsym, args...)
    end
end

Base.Expr(node::SyntaxNode) = _to_expr(node)


#-------------------------------------------------------------------------------

function parse_all(::Type{SyntaxNode}, code::AbstractString; filename="none")
    source_file = SourceFile(code, filename=filename)

    stream = ParseStream(code)
    parse_all(stream)

    if !isempty(stream.diagnostics)
        buf = IOBuffer()
        show_diagnostics(IOContext(buf, stdout), stream, code)
        @error Text(String(take!(buf)))
    end

    green_tree = build_tree(GreenNode, stream, wrap_toplevel_as_kind=K"toplevel")

    SyntaxNode(source_file, green_tree)
end


"""
    parse_all(Expr, code::AbstractString; filename="none")

Parse the given code and convert to a standard Expr
"""
function parse_all(::Type{Expr}, code::AbstractString; filename="none")
    tree = parse_all(SyntaxNode, code; filename=filename)

    # convert to Julia expr
    ex = Expr(tree)

    # TODO: Don't remove line nums; try to get them consistent with Base.
    flisp_ex = remove_linenums!(flisp_parse_all(code))
    if remove_linenums!(deepcopy(ex)) != flisp_ex && !(!isempty(flisp_ex.args) &&
                           Meta.isexpr(flisp_ex.args[end], :error))
        @error "Mismatch with Meta.parse()" ex flisp_ex
    end
    ex
end

function remove_linenums!(ex)
    ex = Base.remove_linenums!(ex)
    if Meta.isexpr(ex, :toplevel)
        filter!(x->!(x isa LineNumberNode), ex.args)
    end
    ex
end

function flisp_parse_all(code)
    if VERSION >= v"1.6"
        Meta.parseall(code)
    else
        Base.parse_input_line(code)
    end
end

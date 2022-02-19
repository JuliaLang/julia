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
        val = if k in KSet`Integer Float BinInt OctInt HexInt`
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
        elseif k in KSet`String CmdString`
            is_cmd = k == K"CmdString"
            is_raw = has_flags(head(raw), RAW_STRING_FLAG)
            unescape_julia_string(val_str, is_cmd, is_raw)
        elseif is_operator(k)
            isempty(val_range)  ?
                Symbol(untokenize(k)) : # synthetic invisible tokens
                Symbol(normalize_identifier(val_str))
        elseif k == K"NothingLiteral"
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
            @error "Leaf node of kind $k unknown to SyntaxNode"
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

is_error(node::SyntaxNode) = is_error(node.raw)
is_trivia(node::SyntaxNode) = is_trivia(node.raw)
has_flags(node::SyntaxNode, f) = has_flags(head(node), f)

head(node::SyntaxNode) = head(node.raw)
kind(node::SyntaxNode)  = kind(node.raw)
flags(node::SyntaxNode) = flags(node.raw)

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

#-------------------------------------------------------------------------------
# Tree utilities

kind(node)  = kind(head(node))
flags(node) = flags(head(node))
is_infix(node) = is_infix(head(node))

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

function _to_expr(node::SyntaxNode, iteration_spec=false)
    if !haschildren(node)
        if node.val isa Union{Int128,UInt128,BigInt}
            # Ignore the values of large integers and convert them back to
            # symbolic/textural form for compatibility with the Expr
            # representation of these.
            str = replace(sourcetext(node), '_'=>"")
            headsym = :macrocall
            k = kind(node)
            macname = node.val isa Int128  ? Symbol("@int128_str")  :
                      node.val isa UInt128 ? Symbol("@uint128_str") :
                      Symbol("@big_str")
            return Expr(:macrocall, GlobalRef(Core, macname), nothing, str)
        else
            return node.val
        end
    end
    headstr = untokenize(head(node), include_flag_suff=false)
    headsym = !isnothing(headstr) ? Symbol(headstr) :
        error("Can't untokenize head of kind $(kind(node))")
    node_args = children(node)
    args = Vector{Any}(undef, length(node_args))
    if headsym == :for && length(node_args) == 2
        args[1] = _to_expr(node_args[1], true)
        args[2] = _to_expr(node_args[2], false)
    else
        map!(_to_expr, args, node_args)
    end
    # Julia's standard `Expr` ASTs have children stored in a canonical
    # order which is often not always source order. We permute the children
    # here as necessary to get the canonical order.
    if is_infix(node.raw)
        args[2], args[1] = args[1], args[2]
    end
    loc = source_location(LineNumberNode, node.source, node.position)
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
            gen = Expr(:flatten, Expr(:generator, gen, args[i]))
        end
        return gen
    elseif headsym in (:nrow, :ncat)
        # For lack of a better place, the dimension argument to nrow/ncat
        # is stored in the flags
        pushfirst!(args, numeric_flags(flags(node)))
    elseif headsym == :typed_ncat
        insert!(args, 2, numeric_flags(flags(node)))
    elseif headsym == :string && length(args) > 1
        # Julia string literals may be interspersed with trivia in two situations:
        # 1. Triple quoted string indentation is trivia
        # 2. An \ before newline removes the newline and any following indentation
        #
        # Such trivia is eagerly removed by the reference parser, so here we
        # concatenate adjacent string chunks together for compatibility.
        #
        # TODO: Manage the non-interpolation cases with String and CmdString
        # kinds instead?
        args2 = Vector{Any}()
        i = 1
        while i <= length(args)
            if args[i] isa String && i < length(args) && args[i+1] isa String
                buf = IOBuffer()
                while i <= length(args) && args[i] isa String
                    write(buf, args[i])
                    i += 1
                end
                push!(args2, String(take!(buf)))
            else
                push!(args2, args[i])
                i += 1
            end
        end
        args = args2
        if length(args2) == 1 && args2[1] isa String
            # If there's a single string remaining after joining we unwrap to
            # give a string literal.
            # """\n  a\n  b""" ==>  "a\nb"
            return args2[1]
        end
    # elseif headsym == :string && length(args) == 1 && version <= (1,5)
    #   Strip string from interpolations in 1.5 and lower to preserve
    #   "hi$("ho")" ==>  (string "hi" "ho")
    elseif headsym == :(=)
        if is_eventually_call(args[1]) && !iteration_spec
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
    elseif headsym == :function
        if length(args) > 1 && Meta.isexpr(args[1], :tuple)
            # Convert to weird Expr forms for long-form anonymous functions.
            #
            # (function (tuple (... xs)) body) ==> (function (... xs) body)
            if length(args[1].args) == 1 && Meta.isexpr(args[1].args[1], :...)
                # function (xs...) \n body end
                args[1] = args[1].args[1]
            end
        end
    end
    if headsym == :inert || (headsym == :quote && length(args) == 1 &&
                 !(a1 = only(args); a1 isa Expr || a1 isa QuoteNode ||
                   a1 isa Bool  # <- compat hack, Julia 1.4+
                  ))
        return QuoteNode(only(args))
    else
        return Expr(headsym, args...)
    end
end

Base.Expr(node::SyntaxNode) = _to_expr(node)


#-------------------------------------------------------------------------------

function build_tree(::Type{SyntaxNode}, stream::ParseStream; filename=nothing, kws...)
    green_tree = build_tree(GreenNode, stream; kws...)
    source = SourceFile(sourcetext(stream), filename=filename)
    SyntaxNode(source, green_tree, first_byte(stream))
end

function build_tree(::Type{Expr}, stream::ParseStream; kws...)
    Expr(build_tree(SyntaxNode, stream; kws...))
end


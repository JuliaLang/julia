#-------------------------------------------------------------------------------
# Conversion to Base.Expr

"""
    @isexpr(ex, head)
    @isexpr(ex, head, nargs)

Type inference friendly replacement for `Meta.isexpr`.

When using the pattern
```julia
if @isexpr(ex, headsym)
    body
end
```
Julia's type inference knows `ex isa Expr` inside `body`. But `Meta.isexpr`
hides this information from the compiler, for whatever reason.
"""
macro isexpr(ex, head)
    ex isa Symbol || error("First argument to `@isexpr` must be a variable name")
    :($(esc(ex)) isa Expr && $(esc(ex)).head == $(esc(head)))
end

macro isexpr(ex, head, nargs)
    ex isa Symbol || error("First argument to `@isexpr` must be a variable name")
    :($(esc(ex)) isa Expr &&
      $(esc(ex)).head == $(esc(head)) &&
      length($(esc(ex)).args) == $(esc(nargs)))
end

function _reorder_parameters!(args::Vector{Any}, params_pos::Int)
    p = 0
    for i = length(args):-1:1
        ai = args[i]
        if !@isexpr(ai, :parameters)
            break
        end
        p = i
    end
    if p == 0
        return
    end
    # nest frankentuples parameters sections
    for i = length(args)-1:-1:p
        pushfirst!((args[i]::Expr).args, pop!(args))
    end
    # Move parameters to args[params_pos]
    insert!(args, params_pos, pop!(args))
end

function _strip_parens(ex::Expr)
    while true
        if @isexpr(ex, :parens)
            if length(ex.args) == 1
                ex = ex.args[1]
            else
                # Only for error cases
                return Expr(:block, ex.args...)
            end
        else
            return ex
        end
    end
end


reverse_nontrivia_children(cursor::RedTreeCursor) = Iterators.filter(should_include_node, Iterators.reverse(cursor))
reverse_nontrivia_children(cursor) = Iterators.filter(should_include_node, Iterators.reverse(children(cursor)))

# Julia string literals in a `K"string"` node may be split into several chunks
# interspersed with trivia in two situations:
# 1. Triple quoted string indentation is trivia
# 2. An \ before newline removes the newline and any following indentation
#
# This function concatenating adjacent string chunks together as done in the
# reference parser.
function _string_to_Expr(cursor, source, txtbuf::Vector{UInt8}, txtbuf_offset::UInt32)
    ret = Expr(:string)
    args2 = Any[]
    i = 1
    it = reverse_nontrivia_children(cursor)
    r = iterate(it)
    while r !== nothing
        (child, state) = r
        ex = node_to_expr(child, source, txtbuf, txtbuf_offset)
        if isa(ex, String)
            # This branch combines consequent string chunks together.
            # It's unrolled once to avoid unnecessary allocations.
            r = iterate(it, state)
            if r === nothing
                pushfirst!(ret.args, ex)
                continue
            end
            (child, state) = r
            ex2 = node_to_expr(child, source, txtbuf, txtbuf_offset)
            if !isa(ex2, String)
                pushfirst!(ret.args, ex)
                ex = ex2
                # Fall through to process `ex` (!::String)
            else
                strings = String[ex2, ex]  # Note: reversed order since we're iterating backwards
                r = iterate(it, state)
                while r !== nothing
                    (child, state) = r
                    ex = node_to_expr(child, source, txtbuf, txtbuf_offset)
                    isa(ex, String) || break
                    pushfirst!(strings, ex)
                    r = iterate(it, state)
                end
                buf = IOBuffer()
                for s in strings
                    write(buf, s)
                end
                pushfirst!(ret.args, String(take!(buf)))
                r === nothing && break
                # Fall through to process `ex` (!::String)
            end
        end
        # ex not a string
        if @isexpr(ex, :parens, 1)
            ex = _strip_parens(ex)
            if ex isa String
                # Wrap interpolated literal strings in (string) so we can
                # distinguish them from the surrounding text (issue #38501)
                # Ie, "$("str")"  vs  "str"
                # https://github.com/JuliaLang/julia/pull/38692
                ex = Expr(:string, ex)
            end
        end
        @assert ex !== nothing
        pushfirst!(ret.args, ex)
        r = iterate(it, state)
    end

    if length(ret.args) == 1 && ret.args[1] isa String
        # If there's a single string remaining after joining, we unwrap
        # to give a string literal.
        #   """\n  a\n  b""" ==>  "a\nb"
        return only(ret.args)
    else
        # This only happens when the kind is K"string" or when an error has occurred.
        return ret
    end
end

# Shared fixups for Expr children in cases where the type of the parent node
# affects the child layout.
function fixup_Expr_child(::Type, head::SyntaxHead, @nospecialize(arg), first::Bool)
    isa(arg, Expr) || return arg
    k = kind(head)
    eq_to_kw_in_call = ((k == K"call" || k == K"dotcall") &&
                        is_prefix_call(head)) || k == K"ref"
    eq_to_kw_in_params = k != K"vect"   && k != K"curly" &&
                         k != K"braces" && k != K"ref"
    coalesce_dot = k in KSet"call dotcall curly" ||
                   (k == K"quote" && has_flags(head, COLON_QUOTE))
    was_parens = @isexpr(arg, :parens)
    arg = _strip_parens(arg)
    if @isexpr(arg, :(=)) && eq_to_kw_in_call && !first
        arg = Expr(:kw, arg.args...)
    elseif k != K"parens" && @isexpr(arg, :., 1) && arg.args[1] isa Tuple
        # This undoes the "Hack" below"
        h, a = arg.args[1]::Tuple{SyntaxHead,Any}
        arg = ((!was_parens && coalesce_dot && first) ||
                is_syntactic_operator(h)) ?
            Symbol(".", a) : Expr(:., a)
    elseif @isexpr(arg, :parameters) && eq_to_kw_in_params
        pargs = arg.args
        for j = 1:length(pargs)
            pj = pargs[j]
            if @isexpr(pj, :(=))
                pargs[j] = Expr(:kw, pj.args...)
            end
        end
    end
    return arg
end

# Remove the `do` block from the final position in a function/macro call arg list
function _extract_do_lambda!(args::Vector{Any})
    if length(args) > 1 && Meta.isexpr(args[end], :do_lambda)
        do_ex = pop!(args)::Expr
        return Expr(:->, do_ex.args...)
    else
        return nothing
    end
end

function _append_iterspec!(args::Vector{Any}, @nospecialize(ex))
    if @isexpr(ex, :iteration)
        for iter in ex.args::Vector{Any}
            push!(args, Expr(:(=), iter.args...))
        end
    else
        push!(args, ex)
    end
    return args
end

function parseargs!(retexpr::Expr, loc::LineNumberNode, cursor, source, txtbuf::Vector{UInt8}, txtbuf_offset::UInt32)
    args = retexpr.args
    firstchildhead = secondchildhead = head(cursor)
    firstchildrange::UnitRange{UInt32} = byte_range(cursor)
    itr = reverse_nontrivia_children(cursor)
    r = iterate(itr)
    while r !== nothing
        (child, state) = r
        r = iterate(itr, state)
        expr = node_to_expr(child, source, txtbuf, txtbuf_offset)
        @assert expr !== nothing
        secondchildhead = firstchildhead
        firstchildhead = head(child)
        firstchildrange = byte_range(child)
        pushfirst!(args, fixup_Expr_child(
            typeof(cursor), head(cursor), expr, r === nothing))
    end
    return (firstchildhead, secondchildhead, firstchildrange)
end

function version_to_expr(node)
    @assert kind(node) === K"VERSION"
    nv = numeric_flags(flags(node))
    return VersionNumber(1, nv ÷ 10, nv % 10)
end

_expr_leaf_val(node::SyntaxNode, _...) = node.val
_expr_leaf_val(cursor::RedTreeCursor, txtbuf::Vector{UInt8}, txtbuf_offset::UInt32) =
    parse_julia_literal(txtbuf, head(cursor), byte_range(cursor) .+ txtbuf_offset)
# Extended in JuliaLowering to support `node_to_expr(::SyntaxTree, ...)`

# Convert `cursor` (SyntaxNode or RedTreeCursor) to an Expr
# `source` is a SourceFile, or if node was an Expr originally, a LineNumberNode
function node_to_expr(cursor, source, txtbuf::Vector{UInt8}, txtbuf_offset::UInt32=UInt32(0))
    if !should_include_node(cursor)
        return nothing
    end

    nodehead = head(cursor)
    k = kind(cursor)
    srcrange::UnitRange{UInt32} = byte_range(cursor)
    if is_leaf(cursor)
        if is_error(k)
            return k == K"error" ?
                Expr(:error) :
                Expr(:error, "$(_token_error_descriptions[k]): `$(source[srcrange])`")
        elseif k == K"VERSION"
            return version_to_expr(nodehead)
        else
            scoped_val = _expr_leaf_val(cursor, txtbuf, txtbuf_offset)
            val = @isexpr(scoped_val, :scope_layer) ? scoped_val.args[1] : scoped_val
            if val isa Union{Int128,UInt128,BigInt}
                # Ignore the values of large integers and convert them back to
                # symbolic/textual form for compatibility with the Expr
                # representation of these.
                str = replace(source[srcrange], '_'=>"")
                macname = val isa Int128  ? Symbol("@int128_str")  :
                        val isa UInt128 ? Symbol("@uint128_str") :
                        Symbol("@big_str")
                return Expr(:macrocall, GlobalRef(Core, macname), nothing, str)
            elseif is_identifier(k)
                val2 = lower_identifier_name(val, k)
                return @isexpr(scoped_val, :scope_layer) ?
                    Expr(:scope_layer, val2, scoped_val.args[2]) : val2
            else
                return scoped_val
            end
        end
    end

    if k == K"string"
        return _string_to_Expr(cursor, source, txtbuf, txtbuf_offset)
    end

    loc = source_location(LineNumberNode, source, first(srcrange))

    if k == K"cmdstring"
        return Expr(:macrocall, GlobalRef(Core, Symbol("@cmd")), loc,
            _string_to_Expr(cursor, source, txtbuf, txtbuf_offset))
    end

    headstr = untokenize(nodehead, include_flag_suff=false)
    headsym = !isnothing(headstr) ?
              Symbol(headstr) :
              error("Can't untokenize head of kind $(k)")
    retexpr = Expr(headsym)

    # Block gets special handling for extra line number nodes
    if k == K"block" || (k == K"toplevel" && !has_flags(nodehead, TOPLEVEL_SEMICOLONS_FLAG))
        args = retexpr.args
        for child in reverse_nontrivia_children(cursor)
            expr = node_to_expr(child, source, txtbuf, txtbuf_offset)
            @assert expr !== nothing
            # K"block" does not have special first-child handling, so we do not need to keep track of that here
            pushfirst!(args, fixup_Expr_child(typeof(cursor), head(cursor), expr, false))
            pushfirst!(args, source_location(LineNumberNode, source, first(byte_range(child))))
        end
        isempty(args) && push!(args, loc)
        if k == K"block" && has_flags(nodehead, PARENS_FLAG)
            popfirst!(args)
        end
        return retexpr
    end

    # Now recurse to parse all arguments
    (firstchildhead, secondchildhead, firstchildrange) =
        parseargs!(retexpr, loc, cursor, source, txtbuf, txtbuf_offset)

    return _node_to_expr(retexpr, loc, srcrange,
                         firstchildhead, secondchildhead, firstchildrange,
                         nodehead, source)
end

function adjust_macro_name!(retexpr::Union{Expr, Symbol})
    if retexpr isa Symbol
        return lower_identifier_name(retexpr, K"macro_name")
    else
        retexpr::Expr
        if length(retexpr.args) == 2 && retexpr.head == :(.)
            arg2 = retexpr.args[2]
            if isa(arg2, QuoteNode) && arg2.value isa Symbol
                retexpr.args[2] = QuoteNode(lower_identifier_name(arg2.value, K"macro_name"))
            end
        end
        return retexpr
    end
end

# Split out from `node_to_expr` for codesize reasons, to avoid specialization on multiple
# tree types.
@noinline function _node_to_expr(retexpr::Expr, loc::LineNumberNode,
                                 srcrange::UnitRange{UInt32},
                                 firstchildhead::SyntaxHead, secondchildhead::SyntaxHead,
                                 firstchildrange::UnitRange{UInt32},
                                 nodehead::SyntaxHead,
                                 source)
    args = retexpr.args
    k = kind(nodehead)
    endloc = source_location(LineNumberNode, source, last(srcrange))
    if (k == K"var" || k == K"char") && length(retexpr.args) == 1
        # `var` and `char` nodes have a single argument which is the value.
        # However, errors can add additional errors tokens which we represent
        # as e.g. `Expr(:var, ..., Expr(:error))`.
        return retexpr.args[1]
    elseif k == K"macro_name"
        return adjust_macro_name!(retexpr.args[1])
    elseif k == K"?"
        retexpr.head = :if
    elseif k == K"op=" && length(args) == 3
        lhs = args[1]
        op = args[2]
        rhs = args[3]
        headstr = string(args[2], '=')
        retexpr.head = Symbol(headstr)
        retexpr.args = Any[lhs, rhs]
    elseif k == K".op=" && length(args) == 3
        lhs = args[1]
        op = args[2]
        rhs = args[3]
        headstr = '.' * string(args[2], '=')
        retexpr.head = Symbol(headstr)
        retexpr.args = Any[lhs, rhs]
    elseif k == K"macrocall"
        if length(args) >= 2
            a2 = args[2]
            if @isexpr(a2, :macrocall) && kind(firstchildhead) == K"CmdMacroName"
                # Fix up for custom cmd macros like foo`x`
                args[2] = a2.args[3]
            end
            if kind(secondchildhead) == K"VERSION"
                # Encode the syntax version into `loc` so that the argument order
                # matches what ordinary macros expect.
                loc = Core.MacroSource(loc, popat!(args, 2))
            end
        end
        do_lambda = _extract_do_lambda!(args)
        _reorder_parameters!(args, 2)
        insert!(args, 2, loc)
        if do_lambda isa Expr
            return Expr(:do, retexpr, do_lambda)
        end
    elseif k == K"doc"
        retexpr.head = :macrocall
        retexpr.args = [GlobalRef(Core, Symbol("@doc")), loc, args...]
    elseif k == K"dotcall" || k == K"call"
        # Julia's standard `Expr` ASTs have children stored in a canonical
        # order which is often not always source order. We permute the children
        # here as necessary to get the canonical order.
        if is_infix_op_call(nodehead) || is_postfix_op_call(nodehead)
            args[2], args[1] = args[1], args[2]
        end
        # Lower (call x ') to special ' head
        if is_postfix_op_call(nodehead) && args[1] == Symbol("'")
            popfirst!(args)
            retexpr.head = Symbol("'")
        end
        do_lambda = _extract_do_lambda!(args)
        # Move parameters blocks to args[2]
        _reorder_parameters!(args, 2)
        if retexpr.head === :dotcall
            funcname = args[1]
            if is_prefix_call(nodehead)
                retexpr.head = :.
                retexpr.args = Any[funcname, Expr(:tuple, args[2:end]...)]
            else
                # operator calls
                retexpr.head = :call
                if funcname isa Symbol
                    args[1] = Symbol(:., funcname)
                end # else funcname could be an Expr(:error), just propagate it
            end
        end
        if do_lambda isa Expr
            return Expr(:do, retexpr, do_lambda)
        end
    elseif k == K"."
        if length(args) == 2
            a2 = args[2]
            if !@isexpr(a2, :quote) && !(a2 isa QuoteNode)
                args[2] = QuoteNode(a2)
            end
        elseif length(args) == 1
            # Hack: Here we preserve the head of the operator to determine whether
            # we need to coalesce it with the dot into a single symbol later on.
            args[1] = (firstchildhead, args[1])
        end
    elseif k == K"ref" || k == K"curly"
        # Move parameters blocks to args[2]
        _reorder_parameters!(args, 2)
    elseif k == K"for"
        iters = _append_iterspec!([], args[1])
        args[1] = length(iters) == 1 ? only(iters) : Expr(:block, iters...)
        # Add extra line number node for the `end` of the block. This may seem
        # useless but it affects code coverage.
        push!(args[2].args, endloc)
    elseif k == K"while"
        # Line number node for the `end` of the block as in `for` loops.
        push!(args[2].args, endloc)
    elseif k in KSet"tuple vect braces"
        # Move parameters blocks to args[1]
        _reorder_parameters!(args, 1)
    elseif k == K"where"
        if length(args) == 2
            a2 = args[2]
            if @isexpr(a2, :braces)
                a2a = a2.args
                _reorder_parameters!(a2a, 2)
                retexpr.args = Any[args[1], a2a...]
            end
        end
    elseif k == K"catch"
        if kind(firstchildhead) == K"Placeholder"
            args[1] = false
        end
    elseif k == K"try"
        # Try children in source order:
        #   try_block catch_var catch_block else_block finally_block
        # Expr ordering:
        #   try_block catch_var catch_block [finally_block] [else_block]
        try_ = args[1]
        catch_var = false
        catch_ = false
        else_ = false
        finally_ = false
        for i in 2:length(args)
            a = args[i]
            if @isexpr(a, :catch)
                catch_var = a.args[1]
                catch_ = a.args[2]
            elseif @isexpr(a, :else)
                else_ = only(a.args)
            elseif @isexpr(a, :finally)
                finally_ = only(a.args)
            elseif @isexpr(a, :error)
                finally_ = Expr(:block, a) # Unclear where to put this but here will do?
            else
                @assert false "Illegal $a subclause in `try`"
            end
        end
        empty!(args)
        push!(args, try_, catch_var, catch_)
        if finally_ !== false || else_ !== false
            push!(args, finally_)
            if else_ !== false
                push!(args, else_)
            end
        end
    elseif k == K"generator"
        # Reconstruct the nested Expr form for generator from our flatter
        # source-ordered `generator` format.
        gen = args[1]
        for j = length(args):-1:2
            gen = Expr(:generator, gen)
            _append_iterspec!(gen.args, args[j])
            if j < length(args)
                # Additional `for`s flatten the inner generator
                gen = Expr(:flatten, gen)
            end
        end
        return gen
    elseif k == K"filter"
        @assert length(args) == 2
        retexpr.args = _append_iterspec!(Any[args[2]], args[1])
    elseif k == K"nrow" || k == K"ncat"
        # For lack of a better place, the dimension argument to nrow/ncat
        # is stored in the flags
        pushfirst!(args, numeric_flags(flags(nodehead)))
    elseif k == K"typed_ncat"
        insert!(args, 2, numeric_flags(flags(nodehead)))
    elseif k == K"elseif"
        # Block for conditional's source location
        args[1] = Expr(:block, loc, args[1])
    elseif k == K"->"
        a1 = args[1]
        if @isexpr(a1, :tuple)
            # TODO: This makes the Expr form objectively worse for the sake of
            # compatibility. We should consider deleting this special case in
            # the future as a minor change.
            if length(a1.args) == 1 &&
                    (!has_flags(firstchildhead, PARENS_FLAG) ||
                     !has_flags(firstchildhead, TRAILING_COMMA_FLAG)) &&
                    !Meta.isexpr(a1.args[1], :parameters)
                # `(a) -> c` is parsed without tuple on lhs in Expr form
                args[1] = a1.args[1]
            elseif length(a1.args) == 2 && (a11 = a1.args[1]; @isexpr(a11, :parameters) &&
                                            length(a11.args) <= 1 && !Meta.isexpr(a1.args[2], :(...)))
                # `(a; b=1) -> c`  parses args as `block` in Expr form :-(
                if length(a11.args) == 0
                    args[1] = Expr(:block, a1.args[2])
                else
                    a111 = only(a11.args)
                    assgn = @isexpr(a111, :kw) ? Expr(:(=), a111.args...) : a111
                    argloc = source_location(LineNumberNode, source, last(firstchildrange))
                    args[1] = Expr(:block, a1.args[2], argloc, assgn)
                end
            end
        end
        a2 = args[2]
        # Add function source location to rhs; add block if necessary
        if @isexpr(a2, :block)
            pushfirst!(a2.args, loc)
        else
            args[2] = Expr(:block, loc, args[2])
        end
    elseif k == K"function"
        if length(args) > 1
            if has_flags(nodehead, SHORT_FORM_FUNCTION_FLAG)
                a1 = args[1]
                a2 = args[2]
                if !@isexpr(a2, :block) && !@isexpr(a1, Symbol("'"))
                    args[2] = Expr(:block, a2)
                end
                retexpr.head = :(=)
            else
                a1 = args[1]
                if @isexpr(a1, :tuple) &&
                    !has_flags(firstchildhead, TRAILING_COMMA_FLAG)
                    # Convert to weird Expr forms for long-form anonymous functions.
                    #
                    # (function (tuple (... xs)) body) ==> (function (... xs) body)
                    if length(a1.args) == 1 && (a11 = a1.args[1]; @isexpr(a11, :...))
                        # function (xs...) \n body end
                        args[1] = a11
                    end
                end
            end
            arg2 = args[2]
            # Add location if not ErrorVal or unwrapped block
            @isexpr(arg2, :block) && pushfirst!(arg2.args, loc)
        end
    elseif k == K"macro"
        if length(args) > 1
            pushfirst!((args[2]::Expr).args, loc)
        end
    elseif k == K"module"
        insert!(args, kind(firstchildhead) == K"VERSION" ? 2 : 1, !has_flags(nodehead, BARE_MODULE_FLAG))
        pushfirst!((args[end]::Expr).args, loc)
    elseif k == K"quote"
        if length(args) == 1
            a1 = only(args)
            if !(a1 isa Expr || a1 isa QuoteNode || a1 isa Bool)
                # Flisp parser does an optimization here: simple values are stored
                # as inert QuoteNode rather than in `Expr(:quote)` quasiquote
                return QuoteNode(a1)
            end
        end
    elseif k == K"do"
        # Temporary head which is picked up by _extract_do_lambda
        retexpr.head = :do_lambda
    elseif k == K"let"
        a1 = args[1]
        if @isexpr(a1, :block)
            a1a = (args[1]::Expr).args
            filter!(a -> !(a isa LineNumberNode), a1a)
            # Ugly logic to strip the Expr(:block) in certain cases for compatibility
            if length(a1a) == 1
                a = a1a[1]
                if a isa Symbol || @isexpr(a, :(=)) || @isexpr(a, :(::))
                    args[1] = a
                end
            end
        end
    elseif k == K"local" || k === K"global"
        if length(args) == 1
            a1 = args[1]
            if @isexpr(a1, :const)
                # Normalize `local const` to `const local`
                args[1] = Expr(retexpr.head, (a1::Expr).args...)
                retexpr.head = :const
            elseif @isexpr(a1, :tuple)
                # Normalize `global (x, y)` to `global x, y`
                retexpr.args = a1.args
            end
        end
    elseif k == K"return" && isempty(args)
        push!(args, nothing)
    elseif k == K"juxtapose"
        retexpr.head = :call
        pushfirst!(args, :*)
    elseif k == K"struct"
        @assert args[2].head == :block
        orig_fields = args[2].args
        fields = Expr(:block)
        for field in orig_fields
            if @isexpr(field, :macrocall) && field.args[1] == GlobalRef(Core, Symbol("@doc"))
                # @doc macro calls don't occur within structs, in Expr form.
                push!(fields.args, field.args[3])
                push!(fields.args, field.args[4])
            else
                push!(fields.args, field)
            end
        end
        args[2] = fields
        pushfirst!(args, has_flags(nodehead, MUTABLE_FLAG))
    elseif k == K"importpath"
        retexpr.head = :.
        for i = 1:length(args)
            ai = args[i]
            if ai isa QuoteNode
                # Permit nonsense additional quoting such as
                # import A.(:b).:c
                args[i] = ai.value
            end
        end
    elseif k == K"wrapper"
        # This should only happen for errors wrapped next to what should have
        # been single statements or atoms - represent these as blocks.
        retexpr.head = :block
    elseif k == K"comparison"
        for i = 2:2:length(args)
            arg = args[i]
            if @isexpr(arg, :., 1)
                args[i] = Symbol(".", arg.args[1])
            end
        end
    elseif k == K"meta"
        # Expr uses plain identifiers, but JuliaSyntax uses quoted (Symbol) identifiers
        for (i, a) in enumerate(args)
            if a isa QuoteNode && a.value isa Symbol
                args[i] = a.value
            end
        end
    end

    return retexpr
end

function build_tree(::Type{Expr}, stream::ParseStream;
                    filename=nothing, first_line=1,
                    # unused, but required since `_parse` is written generic
                    keep_parens=false)
    source = SourceFile(stream, filename=filename, first_line=first_line)
    return build_tree(Expr, stream, source)
end

function build_tree(::Type{Expr}, stream::ParseStream, source::SourceFile)
    txtbuf = unsafe_textbuf(stream)
    cursor = RedTreeCursor(stream)
    wrapper_head = SyntaxHead(K"wrapper",EMPTY_FLAGS)
    if has_toplevel_siblings(cursor)
        entry = Expr(:block)
        for child in
                Iterators.filter(should_include_node, reverse_toplevel_siblings(cursor))
            pushfirst!(entry.args, fixup_Expr_child(
                RedTreeCursor, wrapper_head,
                node_to_expr(child, source, txtbuf), false))
        end
        length(entry.args) == 1 && (entry = only(entry.args))
    else
        entry = fixup_Expr_child(
            RedTreeCursor, wrapper_head,
            node_to_expr(cursor, source, txtbuf), false)
    end
    return entry
end

function to_expr(node)
    source = sourcefile(node)
    txtbuf_offset, txtbuf = _unsafe_wrap_substring(sourcetext(source))
    wrapper_head = SyntaxHead(K"wrapper",EMPTY_FLAGS)
    return fixup_Expr_child(
        typeof(node), wrapper_head,
        node_to_expr(node, source, txtbuf, UInt32(txtbuf_offset)), false)
end

Base.Expr(node::SyntaxNode) = to_expr(node)

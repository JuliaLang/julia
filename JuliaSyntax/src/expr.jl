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

function _reorder_parameters!(args::Vector{Any}, params_pos)
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

function _strip_parens(ex)
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

# Get Julia value of leaf node as it would be represented in `Expr` form
function _expr_leaf_val(node::SyntaxNode)
    node.val
end

function _leaf_to_Expr(source, txtbuf, txtbuf_offset, head, srcrange, node)
    k = kind(head)
    if k == K"MacroName" && view(source, srcrange) == "."
        return Symbol("@__dot__")
    elseif is_error(k)
        return k == K"error" ?
            Expr(:error) :
            Expr(:error, "$(_token_error_descriptions[k]): `$(source[srcrange])`")
    else
        val = isnothing(node) ?
            parse_julia_literal(txtbuf, head, srcrange .+ txtbuf_offset) :
            _expr_leaf_val(node)
        if val isa Union{Int128,UInt128,BigInt}
            # Ignore the values of large integers and convert them back to
            # symbolic/textural form for compatibility with the Expr
            # representation of these.
            str = replace(source[srcrange], '_'=>"")
            macname = val isa Int128  ? Symbol("@int128_str")  :
                      val isa UInt128 ? Symbol("@uint128_str") :
                      Symbol("@big_str")
            return Expr(:macrocall, GlobalRef(Core, macname), nothing, str)
        else
            return val
        end
    end
end

# Julia string literals in a `K"string"` node may be split into several chunks
# interspersed with trivia in two situations:
# 1. Triple quoted string indentation is trivia
# 2. An \ before newline removes the newline and any following indentation
#
# This function concatenating adjacent string chunks together as done in the
# reference parser.
function _string_to_Expr(args)
    args2 = Any[]
    i = 1
    while i <= length(args)
        if args[i] isa String
            if i < length(args) && args[i+1] isa String
                buf = IOBuffer()
                while i <= length(args) && args[i] isa String
                    write(buf, args[i]::String)
                    i += 1
                end
                push!(args2, String(take!(buf)))
            else
                push!(args2, args[i])
                i += 1
            end
        else
            ex = args[i]
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
            push!(args2, ex)
            i += 1
        end
    end
    if length(args2) == 1 && args2[1] isa String
        # If there's a single string remaining after joining, we unwrap
        # to give a string literal.
        #   """\n  a\n  b""" ==>  "a\nb"
        return only(args2)
    else
        # This only happens when the kind is K"string" or when an error has occurred. 
        return Expr(:string, args2...)
    end
end

# Shared fixups for Expr children in cases where the type of the parent node
# affects the child layout.
function _fixup_Expr_children!(head, loc, args)
    k = kind(head)
    eq_to_kw_in_call = ((k == K"call" || k == K"dotcall") &&
                        is_prefix_call(head)) || k == K"ref"
    eq_to_kw_in_params = k != K"vect"   && k != K"curly" &&
                         k != K"braces" && k != K"ref"
    coalesce_dot = k in KSet"call dotcall curly" ||
                   (k == K"quote" && flags(head) == COLON_QUOTE)
    for i in 1:length(args)
        arg = args[i]
        was_parens = @isexpr(arg, :parens)
        arg = _strip_parens(arg)
        if @isexpr(arg, :(=)) && eq_to_kw_in_call && i > 1 
            arg = Expr(:kw, arg.args...)
        elseif k != K"parens" && @isexpr(arg, :., 1) && arg.args[1] isa Tuple
            h, a = arg.args[1]::Tuple{SyntaxHead,Any}
            arg = ((!was_parens && coalesce_dot && i == 1) ||
                   (k == K"comparison" && iseven(i)) ||
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
        elseif k == K"let" && i == 1 && @isexpr(arg, :block)
            filter!(a -> !(a isa LineNumberNode), arg.args)
        end
        args[i] = arg
    end
    return args
end

# Remove the `do` block from the final position in a function/macro call arg list
function _extract_do_lambda!(args)
    if length(args) > 1 && Meta.isexpr(args[end], :do_lambda)
        do_ex = pop!(args)::Expr
        return Expr(:->, do_ex.args...)
    else
        return nothing
    end
end

function _append_iterspec!(args, ex)
    if @isexpr(ex, :iteration)
        for iter in ex.args::Vector{Any}
            push!(args, Expr(:(=), iter.args...))
        end
    else
        push!(args, ex)
    end
    return args
end

# Convert internal node of the JuliaSyntax parse tree to an Expr
function _internal_node_to_Expr(source, srcrange, head, childranges, childheads, args)
    k = kind(head)
    if (k == K"var" || k == K"char") && length(args) == 1
        # Ideally we'd like `@check length(args) == 1` as an invariant for all
        # K"var" and K"char" nodes, but this discounts having embedded error
        # nodes when ignore_errors=true is set.
        return args[1]
    elseif k == K"string"
        return _string_to_Expr(args)
    end

    loc = source_location(LineNumberNode, source, first(srcrange))
    endloc = source_location(LineNumberNode, source, last(srcrange))

    if k == K"cmdstring"
        return Expr(:macrocall, GlobalRef(Core, Symbol("@cmd")), loc, _string_to_Expr(args))
    end

    _fixup_Expr_children!(head, loc, args)

    headstr = untokenize(head, include_flag_suff=false)
    headsym = !isnothing(headstr) ?
              Symbol(headstr) :
              error("Can't untokenize head of kind $(k)")

    if k == K"?"
        headsym = :if
    elseif k == K"op=" && length(args) == 3
        lhs = args[1]
        op = args[2]
        rhs = args[3]
        headstr = string(args[2], '=')
        if is_dotted(head)
            headstr = '.'*headstr
        end
        headsym = Symbol(headstr)
        args = Any[lhs, rhs]
    elseif k == K"macrocall"
        if length(args) >= 2
            a2 = args[2]
            if @isexpr(a2, :macrocall) && kind(childheads[1]) == K"CmdMacroName"
                # Fix up for custom cmd macros like foo`x`
                args[2] = a2.args[3]
            end
        end
        do_lambda = _extract_do_lambda!(args)
        _reorder_parameters!(args, 2)
        insert!(args, 2, loc)
        if do_lambda isa Expr
            return Expr(:do, Expr(headsym, args...), do_lambda)
        end
    elseif k == K"block" || (k == K"toplevel" && !has_flags(head, TOPLEVEL_SEMICOLONS_FLAG))
        if isempty(args)
            push!(args, loc)
        else
            resize!(args, 2*length(args))
            for i = length(childranges):-1:1
                args[2*i] = args[i]
                args[2*i-1] = source_location(LineNumberNode, source, first(childranges[i]))
            end
        end
        if k == K"block" && has_flags(head, PARENS_FLAG)
            popfirst!(args)
        end
    elseif k == K"doc"
        headsym = :macrocall
        args = [GlobalRef(Core, Symbol("@doc")), loc, args...]
    elseif k == K"dotcall" || k == K"call"
        # Julia's standard `Expr` ASTs have children stored in a canonical
        # order which is often not always source order. We permute the children
        # here as necessary to get the canonical order.
        if is_infix_op_call(head) || is_postfix_op_call(head)
            args[2], args[1] = args[1], args[2]
        end
        # Lower (call x ') to special ' head
        if is_postfix_op_call(head) && args[1] == Symbol("'")
            popfirst!(args)
            headsym = Symbol("'")
        end
        do_lambda = _extract_do_lambda!(args)
        # Move parameters blocks to args[2]
        _reorder_parameters!(args, 2)
        if headsym === :dotcall
            funcname = args[1]
            if is_prefix_call(head)
                headsym = :.
                args = Any[funcname, Expr(:tuple, args[2:end]...)]
            else
                # operator calls
                headsym = :call
                if funcname isa Symbol
                    args[1] = Symbol(:., funcname)
                end # else funcname could be an Expr(:error), just propagate it
            end
        end
        if do_lambda isa Expr
            return Expr(:do, Expr(headsym, args...), do_lambda)
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
            args[1] = (childheads[1], args[1])
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
                args = Any[args[1], a2a...]
            end
        end
    elseif k == K"catch"
        if kind(childheads[1]) == K"Placeholder"
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
        args = Any[try_, catch_var, catch_]
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
        args = _append_iterspec!(Any[args[2]], args[1])
    elseif k == K"nrow" || k == K"ncat"
        # For lack of a better place, the dimension argument to nrow/ncat
        # is stored in the flags
        pushfirst!(args, numeric_flags(flags(head)))
    elseif k == K"typed_ncat"
        insert!(args, 2, numeric_flags(flags(head)))
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
                    (!has_flags(childheads[1], PARENS_FLAG) ||
                     !has_flags(childheads[1], TRAILING_COMMA_FLAG)) &&
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
                    argloc = source_location(LineNumberNode, source, last(childranges[1]))
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
            if has_flags(head, SHORT_FORM_FUNCTION_FLAG)
                a2 = args[2]
                if !@isexpr(a2, :block)
                    args[2] = Expr(:block, a2)
                end
                headsym = :(=)
            else
                a1 = args[1]
                if @isexpr(a1, :tuple)
                    # Convert to weird Expr forms for long-form anonymous functions.
                    #
                    # (function (tuple (... xs)) body) ==> (function (... xs) body)
                    if length(a1.args) == 1 && (a11 = a1.args[1]; @isexpr(a11, :...))
                        # function (xs...) \n body end
                        args[1] = a11
                    end
                end
            end
            pushfirst!((args[2]::Expr).args, loc)
        end
    elseif k == K"macro"
        if length(args) > 1
            pushfirst!((args[2]::Expr).args, loc)
        end
    elseif k == K"module"
        pushfirst!(args, !has_flags(head, BARE_MODULE_FLAG))
        pushfirst!((args[3]::Expr).args, loc)
    elseif k == K"inert"
        return QuoteNode(only(args))
    elseif k == K"quote" && length(args) == 1
        a1 = only(args)
        if !(a1 isa Expr || a1 isa QuoteNode || a1 isa Bool)
            # Flisp parser does an optimization here: simple values are stored
            # as inert QuoteNode rather than in `Expr(:quote)` quasiquote
            return QuoteNode(a1)
        end
    elseif k == K"do"
        # Temporary head which is picked up by _extract_do_lambda
        headsym = :do_lambda
    elseif k == K"let"
        a1 = args[1]
        if @isexpr(a1, :block)
            a1a = (args[1]::Expr).args
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
                args[1] = Expr(headsym, (a1::Expr).args...)
                headsym = :const
            elseif @isexpr(a1, :tuple)
                # Normalize `global (x, y)` to `global x, y`
                args = a1.args
            end
        end
    elseif k == K"return" && isempty(args)
        push!(args, nothing)
    elseif k == K"juxtapose"
        headsym = :call
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
        pushfirst!(args, has_flags(head, MUTABLE_FLAG))
    elseif k == K"importpath"
        headsym = :.
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
        headsym = :block
    end

    return Expr(headsym, args...)
end


# Stack entry for build_tree Expr conversion.
# We'd use `Tuple{UnitRange{Int},SyntaxHead,Any}` instead, but that's an
# abstract type due to the `Any` and tuple covariance which destroys
# performance.
struct _BuildExprStackEntry
    srcrange::UnitRange{Int}
    head::SyntaxHead
    ex::Any
end

function build_tree(::Type{Expr}, stream::ParseStream;
                    filename=nothing, first_line=1, kws...)
    source = SourceFile(stream, filename=filename, first_line=first_line)
    txtbuf = unsafe_textbuf(stream)
    args = Any[]
    childranges = UnitRange{Int}[]
    childheads = SyntaxHead[]
    entry = build_tree(_BuildExprStackEntry, stream; kws...) do head, srcrange, nodechildren
        if is_trivia(head) && !is_error(head)
            return nothing
        end
        k = kind(head)
        if isnothing(nodechildren)
            ex = _leaf_to_Expr(source, txtbuf, 0, head, srcrange, nothing)
        else
            resize!(childranges, length(nodechildren))
            resize!(childheads, length(nodechildren))
            resize!(args, length(nodechildren))
            for (i,c) in enumerate(nodechildren)
                childranges[i] = c.srcrange
                childheads[i] = c.head
                args[i] = c.ex
            end
            ex = _internal_node_to_Expr(source, srcrange, head, childranges, childheads, args)
        end
        return _BuildExprStackEntry(srcrange, head, ex)
    end
    loc = source_location(LineNumberNode, source, first(entry.srcrange))
    only(_fixup_Expr_children!(SyntaxHead(K"None",EMPTY_FLAGS), loc, Any[entry.ex]))
end

function _to_expr(node)
    file = sourcefile(node)
    if is_leaf(node)
        txtbuf_offset, txtbuf = _unsafe_wrap_substring(sourcetext(file))
        return _leaf_to_Expr(file, txtbuf, txtbuf_offset, head(node), byte_range(node), node)
    end
    cs = children(node)
    args = Any[_to_expr(c) for c in cs]
    _internal_node_to_Expr(file, byte_range(node), head(node), byte_range.(cs), head.(cs), args)
end

function to_expr(node)
    ex = _to_expr(node)
    loc = source_location(LineNumberNode, node)
    only(_fixup_Expr_children!(SyntaxHead(K"None",EMPTY_FLAGS), loc, Any[ex]))
end

function Base.Expr(node::SyntaxNode)
    to_expr(node)
end


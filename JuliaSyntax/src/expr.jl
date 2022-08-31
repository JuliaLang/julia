#-------------------------------------------------------------------------------
# Conversion to Base.Expr

function is_eventually_call(ex)
    return Meta.isexpr(ex, :call) || (Meta.isexpr(ex, (:where, :(::))) &&
                                      is_eventually_call(ex.args[1]))
end

function _to_expr(node::SyntaxNode, iteration_spec=false, need_linenodes=true)
    if !haschildren(node)
        val = node.val
        if val isa Union{Int128,UInt128,BigInt}
            # Ignore the values of large integers and convert them back to
            # symbolic/textural form for compatibility with the Expr
            # representation of these.
            str = replace(sourcetext(node), '_'=>"")
            headsym = :macrocall
            k = kind(node)
            macname = val isa Int128  ? Symbol("@int128_str")  :
                      val isa UInt128 ? Symbol("@uint128_str") :
                      Symbol("@big_str")
            return Expr(:macrocall, GlobalRef(Core, macname), nothing, str)
        else
            return val
        end
    end
    if kind(node) == K"?"
        headsym = :if
    else
        headstr = untokenize(head(node), include_flag_suff=false)
        headsym = !isnothing(headstr) ? Symbol(headstr) :
            error("Can't untokenize head of kind $(kind(node))")
    end
    node_args = children(node)
    insert_linenums = (headsym == :block || headsym == :toplevel) && need_linenodes
    args = Vector{Any}(undef, length(node_args)*(insert_linenums ? 2 : 1))
    if headsym == :for && length(node_args) == 2
        # No line numbers in for loop iteration spec
        args[1] = _to_expr(node_args[1], true, false)
        args[2] = _to_expr(node_args[2])
    elseif headsym == :let && length(node_args) == 2
        # No line numbers in let statement binding list
        args[1] = _to_expr(node_args[1], false, false)
        args[2] = _to_expr(node_args[2])
    else
        if insert_linenums
            if isempty(node_args)
                push!(args, source_location(LineNumberNode, node.source, node.position))
            else
                for i in 1:length(node_args)
                    n = node_args[i]
                    args[2*i-1] = source_location(LineNumberNode, n.source, n.position)
                    args[2*i] = _to_expr(n)
                end
            end
        else
            for i in 1:length(node_args)
                args[i] = _to_expr(node_args[i])
            end
        end
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
        if is_eventually_call(args[1]) && !iteration_spec && !Meta.isexpr(args[2], :block)
            # Add block for short form function locations
            args[2] = Expr(:block, loc, args[2])
        end
    elseif headsym == :elseif
        # Block for conditional's source location
        args[1] = Expr(:block, loc, args[1])
    elseif headsym == :(->)
        if Meta.isexpr(args[2], :block)
            parent = node.parent
            if parent isa SyntaxNode && kind(parent) != K"do"
                pushfirst!(args[2].args, loc)
            end
        else
            # Add block for source locations
            args[2] = Expr(:block, loc, args[2])
        end
    elseif headsym == :function
        if length(args) > 1
            if Meta.isexpr(args[1], :tuple)
                # Convert to weird Expr forms for long-form anonymous functions.
                #
                # (function (tuple (... xs)) body) ==> (function (... xs) body)
                if length(args[1].args) == 1 && Meta.isexpr(args[1].args[1], :...)
                    # function (xs...) \n body end
                    args[1] = args[1].args[1]
                end
            end
            pushfirst!(args[2].args, loc)
        end
    elseif headsym == :macro
        if length(args) > 1
            pushfirst!(args[2].args, loc)
        end
    elseif headsym == :module
        pushfirst!(args[3].args, loc)
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

function build_tree(::Type{Expr}, stream::ParseStream; kws...)
    Expr(build_tree(SyntaxNode, stream; kws...))
end

#-------------------------------------------------------------------------------
# Conversion to Base.Expr

function is_eventually_call(ex)
    return Meta.isexpr(ex, :call) || (Meta.isexpr(ex, (:where, :(::))) &&
                                      is_eventually_call(ex.args[1]))
end

function is_stringchunk(node)
    k = kind(node)
    return k == K"String" || k == K"CmdString"
end

function reorder_parameters!(args, params_pos)
    p = 0
    for i = length(args):-1:1
        if !Meta.isexpr(args[i], :parameters)
            break
        end
        p = i
    end
    if p == 0
        return
    end
    # nest frankentuples parameters sections
    for i = length(args)-1:-1:p
        pushfirst!(args[i].args, pop!(args))
    end
    # Move parameters to args[params_pos]
    insert!(args, params_pos, pop!(args))
end

function _to_expr(node::SyntaxNode; iteration_spec=false, need_linenodes=true,
                  eq_to_kw=false, inside_dot_expr=false, inside_vect_or_braces=false)
    if !haschildren(node)
        val = node.val
        if val isa Union{Int128,UInt128,BigInt}
            # Ignore the values of large integers and convert them back to
            # symbolic/textural form for compatibility with the Expr
            # representation of these.
            str = replace(sourcetext(node), '_'=>"")
            headsym = :macrocall
            macname = val isa Int128  ? Symbol("@int128_str")  :
                      val isa UInt128 ? Symbol("@uint128_str") :
                      Symbol("@big_str")
            return Expr(:macrocall, GlobalRef(Core, macname), nothing, str)
        else
            return val
        end
    end
    nodekind = kind(node)
    node_args = children(node)
    if nodekind == K"var"
        @check length(node_args) == 1
        return _to_expr(node_args[1])
    elseif nodekind == K"char"
        @check length(node_args) == 1
        return _to_expr(node_args[1])
    elseif nodekind == K"?"
        headsym = :if
    elseif nodekind == K"=" && !is_decorated(node) && eq_to_kw
        headsym = :kw
    else
        headstr = untokenize(head(node), include_flag_suff=false)
        headsym = !isnothing(headstr) ? Symbol(headstr) :
            error("Can't untokenize head of kind $(nodekind)")
    end
    if headsym == :string || headsym == :cmdstring
        # Julia string literals may be interspersed with trivia in two situations:
        # 1. Triple quoted string indentation is trivia
        # 2. An \ before newline removes the newline and any following indentation
        #
        # Such trivia is eagerly removed by the reference parser, so here we
        # concatenate adjacent string chunks together for compatibility.
        args = Vector{Any}()
        i = 1
        while i <= length(node_args)
            if is_stringchunk(node_args[i])
                if i < length(node_args) && is_stringchunk(node_args[i+1])
                    buf = IOBuffer()
                    while i <= length(node_args) && is_stringchunk(node_args[i])
                        write(buf, node_args[i].val)
                        i += 1
                    end
                    push!(args, String(take!(buf)))
                else
                    push!(args, node_args[i].val)
                    i += 1
                end
            else
                e = _to_expr(node_args[i])
                if e isa String && headsym == :string
                    # Wrap interpolated literal strings in (string) so we can
                    # distinguish them from the surrounding text (issue #38501)
                    # Ie, "$("str")"  vs  "str"
                    # https://github.com/JuliaLang/julia/pull/38692
                    e = Expr(:string, e)
                end
                push!(args, e)
                i += 1
            end
        end
        if length(args) == 1 && args[1] isa String
            # If there's a single string remaining after joining, we unwrap
            # to give a string literal.
            #   """\n  a\n  b""" ==>  "a\nb"
            # headsym === :cmdstring follows this branch
            return only(args)
        else
            @check headsym === :string
            return Expr(headsym, args...)
        end
    end

    # Convert children
    insert_linenums = (headsym == :block || headsym == :toplevel) && need_linenodes
    args = Vector{Any}(undef, length(node_args)*(insert_linenums ? 2 : 1))
    if headsym == :for && length(node_args) == 2
        # No line numbers in for loop iteration spec
        args[1] = _to_expr(node_args[1], iteration_spec=true, need_linenodes=false)
        args[2] = _to_expr(node_args[2])
    elseif headsym == :let && length(node_args) == 2
        # No line numbers in let statement binding list
        args[1] = _to_expr(node_args[1], need_linenodes=false)
        args[2] = _to_expr(node_args[2])
    else
        eq_to_kw_in_call =
            headsym == :call && is_prefix_call(node)          ||
            headsym == :ref
        eq_to_kw_all = headsym == :parameters && !inside_vect_or_braces ||
                      (headsym == :tuple && inside_dot_expr)
        in_dot = headsym == :.
        in_vb = headsym == :vect || headsym == :braces
        if insert_linenums && isempty(node_args)
            push!(args, source_location(LineNumberNode, node.source, node.position))
        else
            for i in 1:length(node_args)
                n = node_args[i]
                if insert_linenums
                    args[2*i-1] = source_location(LineNumberNode, n.source, n.position)
                end
                eq_to_kw = eq_to_kw_in_call && i > 1 || eq_to_kw_all
                args[insert_linenums ? 2*i : i] =
                    _to_expr(n, eq_to_kw=eq_to_kw,
                             inside_dot_expr=in_dot,
                             inside_vect_or_braces=in_vb)
            end
        end
    end

    # Special cases for various expression heads
    loc = source_location(LineNumberNode, node.source, node.position)
    if headsym == :macrocall
        insert!(args, 2, loc)
        if args[1] == Symbol("@.")
            args[1] = Symbol("@__dot__")
        end
    elseif headsym in (:call, :ref)
        # Julia's standard `Expr` ASTs have children stored in a canonical
        # order which is often not always source order. We permute the children
        # here as necessary to get the canonical order.
        if is_infix_op_call(node) || is_postfix_op_call(node)
            args[2], args[1] = args[1], args[2]
        end
        # Lower (call x ') to special ' head
        if is_postfix_op_call(node) && args[1] == Symbol("'")
            popfirst!(args)
            headsym = Symbol("'")
        end
        # Move parameters blocks to args[2]
        reorder_parameters!(args, 2)
    elseif headsym in (:tuple, :vect, :braces)
        # Move parameters blocks to args[1]
        reorder_parameters!(args, 1)
    elseif headsym in (:try, :try_finally_catch)
        # Try children in source order:
        #   try_block catch_var catch_block else_block finally_block
        # Expr ordering:
        #   try_block catch_var catch_block [finally_block] [else_block]
        catch_ = nothing
        if headsym === :try_finally_catch
            catch_ = pop!(args)
            catch_var = pop!(args)
        end
        finally_ = pop!(args)
        else_ = pop!(args)
        if headsym === :try_finally_catch
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
        headsym = :try
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
    # elseif headsym == :string && length(args) == 1 && version <= (1,5)
    #   Strip string from interpolations in 1.5 and lower to preserve
    #   "hi$("ho")" ==>  (string "hi" "ho")
    elseif headsym == :(=) && !is_decorated(node)
        if is_eventually_call(args[1]) && !iteration_spec && !Meta.isexpr(args[2], :block)
            # Add block for short form function locations
            args[2] = Expr(:block, loc, args[2])
        end
    elseif headsym == :elseif
        # Block for conditional's source location
        args[1] = Expr(:block, loc, args[1])
    elseif headsym == :(->)
        if Meta.isexpr(args[2], :block)
            pushfirst!(args[2].args, loc)
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
    elseif headsym == :inert || (headsym == :quote && length(args) == 1 &&
                 !(a1 = only(args); a1 isa Expr || a1 isa QuoteNode ||
                   a1 isa Bool  # <- compat hack, Julia 1.4+
                  ))
        return QuoteNode(only(args))
    elseif headsym == :do
        @check length(args) == 3
        return Expr(:do, args[1], Expr(:->, args[2], args[3]))
    elseif headsym == :let
        @check Meta.isexpr(args[1], :block)
        a1 = args[1].args
        # Ugly logic to strip the Expr(:block) in certian cases for compatibility
        if length(a1) == 1
            a = a1[1]
            if a isa Symbol || Meta.isexpr(a, (:(=), :(::)))
                args[1] = a
            end
        end
    elseif headsym == :local || headsym == :global
        if length(args) == 1 && Meta.isexpr(args[1], :const)
            # Normalize `local const` to `const local`
            args[1] = Expr(headsym, args[1].args...)
            headsym = :const
        end
    end
    return Expr(headsym, args...)
end

Base.Expr(node::SyntaxNode) = _to_expr(node)

function build_tree(::Type{Expr}, stream::ParseStream; kws...)
    Expr(build_tree(SyntaxNode, stream; kws...))
end

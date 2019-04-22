"""
parse_tuple(ps, ret)

`ret` is followed by a comma so tries to parse the rest of the
tuple.
"""
function parse_tuple end

@static if VERSION > v"1.1-"
    function parse_tuple(ps::ParseState, @nospecialize(ret))
        op = PUNCTUATION(next(ps))
        if (isassignment(ps.nt) && ps.nt.kind != Tokens.APPROX)
            ret = EXPR{TupleH}(Any[ret, op])
        elseif closer(ps)
            ret = ErrorToken(EXPR{TupleH}(Any[ret, op]))
        else
            nextarg = @closer ps tuple parse_expression(ps)
            ret = EXPR{TupleH}(Any[ret, op, nextarg])
        end
        return ret
    end
    
    function parse_tuple(ps::ParseState, ret::EXPR{TupleH})
        op = PUNCTUATION(next(ps))
        if (isassignment(ps.nt) && ps.nt.kind != Tokens.APPROX)
            push!(ret, op)
        elseif closer(ps)
            push!(ret, op)
            ret = ErrorToken(ret)
        else
            nextarg = @closer ps tuple parse_expression(ps)
            if !(is_lparen(first(ret.args)))
                push!(ret, op)
                push!(ret, nextarg)
            else
                ret = EXPR{TupleH}(Any[ret, op, nextarg])
            end
        end
        return ret
    end
else
    function parse_tuple(ps::ParseState, @nospecialize(ret))
        op = PUNCTUATION(next(ps))
        if closer(ps) || (isassignment(ps.nt) && ps.nt.kind != Tokens.APPROX)
            ret = EXPR{TupleH}(Any[ret, op])
        else
            nextarg = @closer ps tuple parse_expression(ps)
            ret = EXPR{TupleH}(Any[ret, op, nextarg])
        end
        return ret
    end
    
    function parse_tuple(ps::ParseState, ret::EXPR{TupleH})
        op = PUNCTUATION(next(ps))
        if closer(ps) || (isassignment(ps.nt) && ps.nt.kind != Tokens.APPROX)
            push!(ret, op)
        else
            nextarg = @closer ps tuple parse_expression(ps)
            if !(is_lparen(first(ret.args)))
                push!(ret, op)
                push!(ret, nextarg)
            else
                ret = EXPR{TupleH}(Any[ret, op, nextarg])
            end
        end
        return ret
    end
end

"""
    parse_array(ps)
Having hit '[' return either:
+ A vect
+ A vcat
+ A comprehension
+ An array (vcat of hcats)
"""
function parse_array(ps::ParseState)
    args = Any[PUNCTUATION(ps)]

    if ps.nt.kind == Tokens.RSQUARE
        accept_rsquare(ps, args)
        ret = EXPR{Vect}(args)
    else
        first_arg = @nocloser ps newline @closesquare ps  @closer ps insquare @closer ps ws @closer ps wsop @closer ps comma parse_expression(ps)

        if ps.nt.kind == Tokens.RSQUARE
            if first_arg isa EXPR{Generator} || first_arg isa EXPR{Flatten}
                accept_rsquare(ps, args)

                if first_arg.args[1] isa BinaryOpCall && is_pairarrow(first_arg.args[1].op)
                    return EXPR{DictComprehension}(Any[args[1], first_arg, INSTANCE(ps)])
                else
                    return EXPR{Comprehension}(Any[args[1], first_arg, INSTANCE(ps)])
                end
            elseif ps.ws.kind == SemiColonWS
                push!(args, first_arg)
                accept_rsquare(ps, args)
                return EXPR{Vcat}(args)
            else
                push!(args, first_arg)
                accept_rsquare(ps, args)
                ret = EXPR{Vect}(args)
            end
        elseif ps.nt.kind == Tokens.COMMA
            etype = Vect
            push!(args, first_arg)
            accept_comma(ps, args)
            @closesquare ps parse_comma_sep(ps, args, false)
            accept_rsquare(ps, args)
            return EXPR{etype}(args)
        elseif ps.ws.kind == NewLineWS
            ret = EXPR{Vcat}(args)
            push!(ret, first_arg)
            while ps.nt.kind != Tokens.RSQUARE
                if ps.nt.kind == Tokens.ENDMARKER
                    break
                end
                a = @closesquare ps  parse_expression(ps)
                push!(ret, a)
            end
            accept_rsquare(ps, ret)
            update_span!(ret)
            return ret
        elseif ps.ws.kind == WS || ps.ws.kind == SemiColonWS
            first_row = EXPR{Hcat}(Any[first_arg])
            while ps.nt.kind != Tokens.RSQUARE && ps.ws.kind != NewLineWS && ps.ws.kind != SemiColonWS
                if ps.nt.kind == Tokens.ENDMARKER
                    break
                end
                a = @closesquare ps @closer ps ws @closer ps wsop parse_expression(ps)
                push!(first_row, a)
            end
            if ps.nt.kind == Tokens.RSQUARE && ps.ws.kind != SemiColonWS
                if length(first_row.args) == 1
                    first_row = EXPR{Vcat}(first_row.args)
                end
                push!(first_row, INSTANCE(next(ps)))
                pushfirst!(first_row, args[1])
                update_span!(first_row)
                return first_row
            else
                if length(first_row.args) == 1
                    first_row = first_row.args[1]
                else
                    first_row = EXPR{Row}(first_row.args)
                end
                ret = EXPR{Vcat}(Any[args[1], first_row])
                while ps.nt.kind != Tokens.RSQUARE
                    if ps.nt.kind == Tokens.ENDMARKER
                        break
                    end
                    first_arg = @closesquare ps @closer ps ws @closer ps wsop parse_expression(ps)
                    push!(ret, EXPR{Row}(Any[first_arg]))
                    while ps.nt.kind != Tokens.RSQUARE && ps.ws.kind != NewLineWS && ps.ws.kind != SemiColonWS
                        if ps.nt.kind == Tokens.ENDMARKER
                            break
                        end
                        a = @closesquare ps @closer ps ws @closer ps wsop parse_expression(ps)
                        push!(last(ret.args), a)
                    end
                    # if only one entry dont use :row
                    if length(last(ret.args).args) == 1
                        ret.args[end] = ret.args[end].args[1]
                    end
                    update_span!(ret)
                end
                accept_rsquare(ps, ret)
                update_span!(ret)
                return ret
            end
        else
            ret = EXPR{Vect}(args)
            push!(ret, first_arg)
            push!(ret, accept_rsquare(ps))
        end
    end
    return ret
end

"""
    parse_ref(ps, ret)

Handles cases where an expression - `ret` - is followed by
`[`. Parses the following bracketed expression and modifies it's
`.head` appropriately.
"""
function parse_ref(ps::ParseState, ret)
    next(ps)
    ref = parse_array(ps)
    return _parse_ref(ret, ref)
end

function _parse_ref(ret, ref::EXPR{Vect})
    args = Any[ret]
    for a in ref.args
        push!(args, a)
    end
    return EXPR{Ref}(args)
end

function _parse_ref(ret, ref::EXPR{Hcat})
    args = Any[ret]
    for a in ref.args
        push!(args, a)
    end
    return EXPR{TypedHcat}(args)
end

function _parse_ref(ret, ref::EXPR{Vcat})
    args = Any[ret]
    for a in ref.args
        push!(args, a)
    end
    return EXPR{TypedVcat}(args)
end

function _parse_ref(ret, ref)
    args = Any[ret]
    for a in ref.args
        push!(args, a)
    end
    return EXPR{TypedComprehension}(args)
end

"""
parse_curly(ps, ret)

Parses the juxtaposition of `ret` with an opening brace. Parses a comma
seperated list.
"""
function parse_curly(ps::ParseState, ret)
    args = Any[ret, PUNCTUATION(next(ps))]
    parse_comma_sep(ps, args, true)
    accept_rbrace(ps, args)
    return EXPR{Curly}(args)
end

function parse_braces(ps::ParseState)
    args = Any[PUNCTUATION(ps)]
    parse_comma_sep(ps, args, true)
    accept_rbrace(ps, args)
    return EXPR{Braces}(args)
end



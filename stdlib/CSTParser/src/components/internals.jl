const term_c = (Tokens.RPAREN, Tokens.RSQUARE, Tokens.RBRACE, Tokens.END, Tokens.ELSE, Tokens.ELSEIF, Tokens.CATCH, Tokens.FINALLY, Tokens.ENDMARKER)

function parse_block(ps::ParseState, ret::Vector{Any} = Any[], closers = (Tokens.END,), docable = false)
    while ps.nt.kind ∉ closers && !ps.errored
        if ps.nt.kind ∈ term_c
            if ps.nt.kind == Tokens.ENDMARKER
                break
            elseif ps.nt.kind == Tokens.RPAREN
                if length(ps.closer.cc) > 1 && :paren == ps.closer.cc[end-1]
                    break
                else
                    push!(ps.errors, Error((ps.nt.startbyte:ps.nt.endbyte) .+ 1 , "Unexpected )."))
                    push!(ret, ErrorToken(INSTANCE(next(ps))))
                end
            elseif ps.nt.kind == Tokens.RBRACE
                if length(ps.closer.cc) > 1 && :brace == ps.closer.cc[end-1]
                    break
                else
                    push!(ps.errors, Error((ps.nt.startbyte:ps.nt.endbyte) .+ 1 , "Unexpected }."))
                    push!(ret, ErrorToken(INSTANCE(next(ps))))
                end
            elseif ps.nt.kind == Tokens.RSQUARE
                if length(ps.closer.cc) > 1 && :square == ps.closer.cc[end-1]
                    break
                else
                    push!(ps.errors, Error((ps.nt.startbyte:ps.nt.endbyte) .+ 1 , "Unexpected ]."))
                    push!(ret, ErrorToken(INSTANCE(next(ps))))
                end
            else
                push!(ps.errors, Error((ps.nt.startbyte:ps.nt.endbyte) .+ 1 , "Unexpected $(ps.nt.kind)."))
                push!(ret, ErrorToken(INSTANCE(next(ps))))
            end
        else
            if docable
                a = parse_doc(ps)
            else
                a = parse_expression(ps)
            end
            push!(ret, a)
        end
    end
    return ret
end


function parse_iter(ps::ParseState)
    startbyte = ps.nt.startbyte
    if ps.nt.kind == Tokens.OUTER && ps.nws.kind != EmptyWS && !Tokens.isoperator(ps.nnt.kind) 
        outer = INSTANCE(next(ps))
        arg = @closer ps range @closer ps ws parse_expression(ps)
        if is_range(arg)
            arg.arg1 = EXPR{Outer}([outer, arg.arg1])
            arg.fullspan += outer.fullspan
            arg.span = outer.fullspan + arg.span
        else
            arg = EXPR{ErrorToken}([outer, arg])
        end
    else
        arg = @closer ps range @closer ps ws parse_expression(ps)
    end
    return arg
end

function parse_ranges(ps::ParseState)
    startbyte = ps.nt.startbyte
    arg = parse_iter(ps)

    if (arg isa EXPR{Outer} && !is_range(arg.args[2])) || !is_range(arg)
        push!(ps.errors, Error(ps.nt.startbyte - arg.fullspan:ps.nt.startbyte , "Incorrect iteration specification."))
        arg = ErrorToken(arg)
    elseif ps.nt.kind == Tokens.COMMA
        arg = EXPR{Block}(Any[arg])
        while ps.nt.kind == Tokens.COMMA
            accept_comma(ps, arg)
            nextarg = parse_iter(ps)
            if (nextarg isa EXPR{Outer} && !is_range(nextarg.args[2])) || !is_range(nextarg)
                push!(ps.errors, Error(ps.nt.startbyte - nextarg.fullspan:ps.nt.startbyte , "Incorrect iteration specification."))
                arg = ErrorToken(arg)
            end
            push!(arg, nextarg)
        end
    end
    return arg
end


function is_range(x) false end
function is_range(x::BinarySyntaxOpCall) is_eq(x.op) end
function is_range(x::BinaryOpCall) is_in(x.op) || is_elof(x.op) end

function parse_end(ps::ParseState)
    if ps.closer.square
        ret = IDENTIFIER(ps)
    else
        push!(ps.errors, Error((ps.t.startbyte:ps.t.endbyte) .+ 1 , "Unexpected end."))
        ret = ErrorToken(IDENTIFIER(ps))
    end
    
    return ret
end

"""
    parse_call(ps, ret)

Parses a function call. Expects to start before the opening parentheses and is passed the expression declaring the function name, `ret`.
"""
function parse_call(ps::ParseState, ret, ismacro = false)
    sb = ps.nt.startbyte - ret.fullspan
    if is_minus(ret) || is_not(ret)
        arg = @closer ps unary @closer ps inwhere @precedence ps 13 parse_expression(ps)
        if arg isa EXPR{TupleH}
            pushfirst!(arg.args, ret)
            fullspan = ps.nt.startbyte - sb
            ret = EXPR{Call}(arg.args, fullspan, fullspan - (last(arg.args).fullspan - last(arg.args).span))
        elseif arg isa WhereOpCall && arg.arg1 isa EXPR{TupleH}
            ret = WhereOpCall(EXPR{Call}(Any[ret; arg.arg1.args]), arg.op, arg.args)
        else
            ret = UnaryOpCall(ret, arg)
        end
    elseif is_and(ret) || is_decl(ret) || is_exor(ret) 
        arg = @precedence ps 20 parse_expression(ps)
        if is_exor(ret) && arg isa EXPR{TupleH} && length(arg.args) == 3 && arg.args[2] isa UnarySyntaxOpCall && is_dddot(arg.args[2].arg2)
            arg = EXPR{InvisBrackets}(arg.args)
        end
        ret = UnarySyntaxOpCall(ret, arg)
    elseif is_issubt(ret) || is_issupt(ret)
        arg = @precedence ps PowerOp parse_expression(ps)
        ret = EXPR{Call}(Any[ret; arg.args])
    else
        !ismacro && ret isa EXPR{MacroName} && (ismacro = true)
        args = Any[ret, PUNCTUATION(next(ps))]
        @closeparen ps @default ps parse_comma_sep(ps, args, !ismacro)
        accept_rparen(ps, args)
        fullspan = ps.nt.startbyte - sb
        ret = EXPR{ismacro ? MacroCall : Call}(args, fullspan, fullspan - last(args).fullspan + last(args).span)
    end
    return ret
end


function parse_comma_sep(ps::ParseState, args::Vector{Any}, kw = true, block = false, istuple = false)
    @nocloser ps inwhere @nocloser ps newline @closer ps comma while !closer(ps)
        a = parse_expression(ps)

        if kw && !ps.closer.brace && a isa BinarySyntaxOpCall && is_eq(a.op)
            a = EXPR{Kw}(Any[a.arg1, a.op, a.arg2], a.fullspan, a.span)
        end
        push!(args, a)
        if ps.nt.kind == Tokens.COMMA
            accept_comma(ps, args)
        end
        if ps.ws.kind == SemiColonWS
            break
        end
    end

    if ps.ws.kind == SemiColonWS
        if block && !(istuple && length(args) > 2) && !(length(args) == 1 && args[1] isa PUNCTUATION) && !(last(args) isa UnarySyntaxOpCall && is_dddot(last(args).arg2))
            args1 = Any[pop!(args)]
            @nocloser ps newline @closer ps comma while @nocloser ps semicolon !closer(ps)
                a = parse_expression(ps)
                push!(args1, a)
            end
            body = EXPR{Block}(args1)
            push!(args, body)
            args = body
        else
            parse_parameters(ps, args)
        end
    end
    return args
end

function parse_parameters(ps, args::Vector{Any})
    sb = ps.nt.startbyte
    args1 = Any[]
    @nocloser ps inwhere @nocloser ps newline  @closer ps comma while @nocloser ps semicolon !closer(ps)
        a = parse_expression(ps)
        if !ps.closer.brace && a isa BinarySyntaxOpCall && is_eq(a.op)
            a = EXPR{Kw}(Any[a.arg1, a.op, a.arg2], a.fullspan, a.span)
        end
        push!(args1, a)
        if ps.nt.kind == Tokens.COMMA
            accept_comma(ps, args1)
        end
        if ps.ws.kind == SemiColonWS
            parse_parameters(ps, args1)
        end
    end
    if !isempty(args1)
        fullspan = ps.nt.startbyte - sb
        paras = EXPR{Parameters}(args1, fullspan, fullspan - last(args1).fullspan + last(args1).span)
        push!(args, paras)
    end
    return
end

"""
    parse_macrocall(ps)

Parses a macro call. Expects to start on the `@`.
"""
function parse_macrocall(ps::ParseState)
    sb = ps.t.startbyte
    at = PUNCTUATION(ps)
    if !isemptyws(ps.ws)
        push!(ps.errors, Error((ps.ws.startbyte + 1:ps.ws.endbyte) .+ 1 , "Unexpected whitespace in macrocall."))
        mname = ErrorToken(INSTANCE(next(ps)))
    else
        mname = EXPR{MacroName}(Any[at, IDENTIFIER(next(ps))])
    end

    # Handle cases with @ at start of dotted expressions
    if ps.nt.kind == Tokens.DOT && isemptyws(ps.ws)
        while ps.nt.kind == Tokens.DOT
            op = OPERATOR(next(ps))
            nextarg = IDENTIFIER(next(ps))
            mname = BinarySyntaxOpCall(mname, op, Quotenode(nextarg))
        end
    end

    if ps.nt.kind == Tokens.COMMA
        return EXPR{MacroCall}(Any[mname], mname.fullspan, mname.span)
    elseif isemptyws(ps.ws) && ps.nt.kind == Tokens.LPAREN
        return parse_call(ps, mname, true)
    else
        args = Any[mname]
        insquare = ps.closer.insquare
        @default ps while !closer(ps)
            a = @closer ps inmacro @closer ps ws @closer ps wsop parse_expression(ps)
            push!(args, a)
            if insquare && ps.nt.kind == Tokens.FOR
                break
            end
        end
        # return EXPR{MacroCall}(args)
        fullspan = ps.nt.startbyte - sb
        return EXPR{MacroCall}(args, fullspan, fullspan - last(args).fullspan + last(args).span)
    end
end




"""
parse_generator(ps)

Having hit `for` not at the beginning of an expression return a generator.
Comprehensions are parsed as SQUAREs containing a generator.
"""
function parse_generator(ps::ParseState, @nospecialize ret)
    kw = KEYWORD(next(ps))
    ret = EXPR{Generator}(Any[ret, kw])
    ranges = @closesquare ps parse_ranges(ps)

    if ps.nt.kind == Tokens.IF
        if ranges isa EXPR{Block}
            ranges = EXPR{Filter}(ranges.args)
        else
            ranges = EXPR{Filter}(Any[ranges])
        end
        pushfirst!(ranges, KEYWORD(next(ps)))
        cond = @closer ps range parse_expression(ps)
        pushfirst!(ranges, cond)
        push!(ret, ranges)
    elseif ranges isa EXPR{Block}
        append!(ret, ranges)
    else
        push!(ret, ranges)
    end
    

    if ret.args[1] isa EXPR{Generator} || ret.args[1] isa EXPR{Flatten}
        ret = EXPR{Flatten}(Any[ret])
    end

    return ret
end



function parse_dot_mod(ps::ParseState, is_colon = false)
    args = Any[]

    while ps.nt.kind == Tokens.DOT || ps.nt.kind == Tokens.DDOT || ps.nt.kind == Tokens.DDDOT
        d = OPERATOR(next(ps))
        if is_dot(d)
            push!(args, OPERATOR(1, 1, Tokens.DOT, false))
        elseif is_ddot(d)
            push!(args, OPERATOR(1, 1, Tokens.DOT, false))
            push!(args, OPERATOR(1, 1, Tokens.DOT, false))
        elseif is_dddot(d)
            push!(args, OPERATOR(1, 1, Tokens.DOT, false))
            push!(args, OPERATOR(1, 1, Tokens.DOT, false))
            push!(args, OPERATOR(1, 1, Tokens.DOT, false))
        end
    end

    # import/export ..
    if ps.nt.kind == Tokens.COMMA || ps.ws.kind == NewLineWS || ps.nt.kind == Tokens.ENDMARKER
        if length(args) == 2
            return Any[INSTANCE(ps)]
        end
    end

    while true
        if ps.nt.kind == Tokens.AT_SIGN
            at = PUNCTUATION(next(ps))
            a = INSTANCE(next(ps))
            push!(args, EXPR{MacroName}(Any[at, a]))
        elseif ps.nt.kind == Tokens.LPAREN
            a = EXPR{InvisBrackets}(Any[PUNCTUATION(next(ps))])
            push!(a, @closeparen ps parse_expression(ps))
            accept_rparen(ps, a)
            push!(args, a)
        elseif ps.nt.kind == Tokens.EX_OR
            a = @closer ps comma parse_expression(ps)
            push!(args, a)
        elseif !is_colon && isoperator(ps.nt)
            next(ps)
            push!(args, OPERATOR(ps.nt.startbyte - ps.t.startbyte,  1 + ps.t.endbyte - ps.t.startbyte, ps.t.kind, false))
        else
            push!(args, INSTANCE(next(ps)))
        end

        if ps.nt.kind == Tokens.DOT
            push!(args, PUNCTUATION(next(ps)))
        elseif isoperator(ps.nt) && (ps.nt.dotop || ps.nt.kind == Tokens.DOT)
            push!(args, PUNCTUATION(Tokens.DOT, 1, 1))
            ps.nt = RawToken(ps.nt.kind, ps.nt.startpos, ps.nt.endpos, ps.nt.startbyte + 1, ps.nt.endbyte, ps.nt.token_error, false)
        else
            break
        end
    end
    args
end

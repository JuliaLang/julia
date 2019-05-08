function longest_common_prefix(prefixa, prefixb)
    maxplength = min(sizeof(prefixa), sizeof(prefixb))
    maxplength == 0 && return ""
    idx = findfirst(i -> (prefixa[i] != prefixb[i]), 1:maxplength)
    idx = idx == nothing ? maxplength : idx - 1
    prefixa[1:idx]
end

function skip_to_nl(str, idxend)
    while (idxend < sizeof(str)) && str[idxend] != '\n'
        idxend = nextind(str, idxend)
    end
    idxend > sizeof(str) ? prevind(str, idxend) : idxend
end

tostr(buf::IOBuffer) = _unescape_string(String(take!(buf)))

"""
parse_string_or_cmd(ps)

When trying to make an `INSTANCE` from a string token we must check for
interpolating opoerators.
"""
function parse_string_or_cmd(ps::ParseState, prefixed = false)
    sfullspan = ps.nt.startbyte - ps.t.startbyte
    sspan = 1 + ps.t.endbyte - ps.t.startbyte

    istrip = (ps.t.kind == Tokens.TRIPLE_STRING) || (ps.t.kind == Tokens.TRIPLE_CMD)
    iscmd = ps.t.kind == Tokens.CMD || ps.t.kind == Tokens.TRIPLE_CMD

    if ps.errored
        return ErrorToken()
    end

    lcp = nothing
    exprs_to_adjust = []
    function adjust_lcp(expr, last = false)
    end
    function adjust_lcp(expr::LITERAL, last = false)
        push!(exprs_to_adjust, expr)
        str = expr.val
        (isempty(str) || (lcp != nothing && isempty(lcp))) && return
        (last && str[end] == '\n') && return (lcp = "")
        idxstart, idxend = 2, 1
        while nextind(str, idxend) - 1 < sizeof(str) && (lcp == nothing || !isempty(lcp))
            idxend = skip_to_nl(str, idxend)
            idxstart = nextind(str, idxend)
            while nextind(str, idxend) - 1 < sizeof(str)
                c = str[nextind(str, idxend)]
                if c == ' ' || c == '\t'
                    idxend += 1
                elseif c == '\n'
                    # All whitespace lines in the middle are ignored
                    idxend += 1
                    idxstart = idxend + 1
                else
                    prefix = str[idxstart:idxend]
                    lcp = lcp === nothing ? prefix : longest_common_prefix(lcp, prefix)
                    break
                end
            end
        end
        if idxstart != nextind(str, idxend)
            prefix = str[idxstart:idxend]
            lcp = lcp === nothing ? prefix : longest_common_prefix(lcp, prefix)
        end
    end

    # there are interpolations in the string
    if prefixed != false || iscmd
        t_str = val(ps.t, ps)
        _val = istrip ? t_str[4:prevind(t_str, sizeof(t_str), 3)] : t_str[2:prevind(t_str, sizeof(t_str))]
        expr = LITERAL(sfullspan, sspan,
            iscmd ? replace(_val, "\\`" => "`") :
                    replace(_val, "\\\"" => "\""), ps.t.kind)
        if istrip
            adjust_lcp(expr)
            ret = EXPR{StringH}(Any[expr], sfullspan, sspan)
        else
            return expr
        end
    else
        ret = EXPR{StringH}(Any[], sfullspan, sspan)
        input = IOBuffer(val(ps.t, ps))
        startbytes = istrip ? 3 : 1
        seek(input, startbytes)
        b = IOBuffer()
        while true
            if eof(input)
                lspan = position(b)
                # str = tostr(b)
                if b.size == 0
                # if sizeof(str) == 0
                    ex = ErrorToken()
                elseif istrip
                    str = tostr(b)
                    str = str[1:prevind(str, prevind(str, sizeof(str), 2))]
                    ex = LITERAL(lspan + ps.nt.startbyte - ps.t.endbyte - 1 + startbytes, lspan + startbytes, str, Tokens.STRING)
                else
                    str = tostr(b)
                    str =  str[1:prevind(str, sizeof(str))]
                    ex = LITERAL(lspan + ps.nt.startbyte - ps.t.endbyte - 1 + startbytes, lspan + startbytes, str, Tokens.STRING)
                end
                push!(ret.args, ex)
                istrip && adjust_lcp(ex, true)
                break
            end
            c = read(input, Char)
            if c == '\\'
                write(b, c)
                write(b, read(input, Char))
            elseif c == '$'
                lspan = position(b)
                str = tostr(b)
                ex = LITERAL(lspan + startbytes, lspan + startbytes, str, Tokens.STRING)
                push!(ret.args, ex); istrip && adjust_lcp(ex)
                startbytes = 0
                op = OPERATOR(1, 1, Tokens.EX_OR, false)
                if peekchar(input) == '('
                    lparen = PUNCTUATION(Tokens.LPAREN, 1, 1)
                    rparen = PUNCTUATION(Tokens.RPAREN, 1, 1)
                    skip(input, 1)
                    ps1 = ParseState(input)

                    if ps1.nt.kind == Tokens.RPAREN
                        call = UnarySyntaxOpCall(op, EXPR{InvisBrackets}(Any[lparen, rparen]))
                        push!(ret.args, call)
                        skip(input, 1)
                    else
                        interp = @closer ps1 paren parse_expression(ps1)
                        call = UnarySyntaxOpCall(op, EXPR{InvisBrackets}(Any[lparen, interp, rparen]))
                        push!(ret.args, call)
                        seek(input, ps1.nt.startbyte + 1)
                    end
                    # Compared to flisp/JuliaParser, we have an extra lookahead token,
                    # so we need to back up one here
                else
                    pos = position(input)
                    ps1 = ParseState(input)
                    next(ps1)
                    if ps1.t.kind == Tokens.WHITESPACE
                        t = EXPR{ErrorToken}([], ps.t.startbyte, ps.t.endbyte - ps.t.startbyte + 1)
                    else
                        t = INSTANCE(ps1)
                    end
                    # Attribute trailing whitespace to the string
                    t = adjustspan(t)
                    call = UnarySyntaxOpCall(op, t)
                    push!(ret.args, call)
                    seek(input, pos + t.fullspan)
                end
            else
                write(b, c)
            end
        end
    end

    single_string_T = (Tokens.STRING,ps.t.kind)
    if istrip
        if lcp != nothing && !isempty(lcp)
            for expr in exprs_to_adjust
                for (i, a) in enumerate(ret.args)
                    if expr == a
                        ret.args[i] = typeof(a)(expr.fullspan, expr.span, replace(expr.val, "\n$lcp" => "\n"), expr.kind)
                    end
                end
            end
        end
        # Drop leading newline
        if ret.args[1] isa LITERAL && ret.args[1].kind in single_string_T &&
                !isempty(ret.args[1].val) && ret.args[1].val[1] == '\n'
            ret.args[1] = dropleadlingnewline(ret.args[1])
        end
    end

    if (length(ret.args) == 1 && ret.args[1] isa LITERAL && ret.args[1].kind in single_string_T)
        ret = ret.args[1]::LITERAL
    end
    update_span!(ret)

    return ret
end


adjustspan(x::IDENTIFIER) = IDENTIFIER(x.span, x.span, x.val)
adjustspan(x::KEYWORD)= KEYWORD(x.kind, x.span, x.span)
adjustspan(x::OPERATOR) = OPERATOR(x.span, x.span, x.kind, x.dot)
adjustspan(x::LITERAL) = LITERAL(x.span, x.span, x.val, x.kind)
adjustspan(x::PUNCTUATION) = PUNCTUATION(x.kind, x.span, x.span)
function adjustspan(x::EXPR)
    x.fullspan = x.span
    return x
end

dropleadlingnewline(x::LITERAL) = LITERAL(x.fullspan, x.span, x.val[2:end], x.kind)

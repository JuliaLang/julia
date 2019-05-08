macro addctx(t, func)
    n = length(func.args[2].args)
    insert!(func.args[2].args, n, :(pop!(ps.closer.cc)))
    pushfirst!(func.args[2].args, :(push!(ps.closer.cc, $(t))))
    return esc(func)
end

macro enterctx(ps, t)
    :(push!($(esc(ps)).closer.cc, $t))
end

macro exitctx(ps, ret)
    quote
        pop!($(esc(ps)).closer.cc)
        $(esc(ret))
    end
end

function accept_rparen(ps)
    if ps.nt.kind == Tokens.RPAREN
        return PUNCTUATION(next(ps))
    else
        push!(ps.errors, Error((ps.ws.startbyte:ps.ws.endbyte) .+ 1 , "Expected )."))
        return ErrorToken(PUNCTUATION(Tokens.RPAREN, 0, 0))
    end
end
accept_rparen(ps::ParseState, args) = push!(args, accept_rparen(ps))

function accept_rsquare(ps)
    if ps.nt.kind == Tokens.RSQUARE
        return PUNCTUATION(next(ps))
    else
        push!(ps.errors, Error((ps.t.startbyte:ps.t.endbyte) .+ 1 , "Expected ]."))
        return ErrorToken(PUNCTUATION(Tokens.RSQUARE, 0, 0))
    end
end
accept_rsquare(ps::ParseState, args) = push!(args, accept_rsquare(ps))

function accept_rbrace(ps)
    if ps.nt.kind == Tokens.RBRACE
        return PUNCTUATION(next(ps))
    else
        push!(ps.errors, Error((ps.t.startbyte:ps.t.endbyte) .+ 1 , "Expected }."))
        return ErrorToken(PUNCTUATION(Tokens.RBRACE, 0, 0))
    end
end
accept_rbrace(ps::ParseState, args) = push!(args, accept_rbrace(ps))

function accept_end(ps::ParseState)
    if ps.nt.kind == Tokens.END
        return KEYWORD(next(ps))
    else
        push!(ps.errors, Error((ps.t.startbyte:ps.t.endbyte) .+ 1 , "Expected end."))
        return ErrorToken(KEYWORD(Tokens.END, 0, 0))
    end
end
accept_end(ps::ParseState, args) = push!(args, accept_end(ps))

function accept_comma(ps)
    if ps.nt.kind == Tokens.COMMA
        return PUNCTUATION(next(ps))
    else
        return PUNCTUATION(Tokens.RPAREN, 0, 0)
    end
end
accept_comma(ps::ParseState, args) = push!(args, accept_comma(ps))

function recover_endmarker(ps)
    if ps.nt.kind == Tokens.ENDMARKER
        if !isempty(ps.closer.cc)
            closert = last(ps.closer.cc)
            if closert == :block
                return ErrorToken(KEYWORD(Tokens.END, 0, 0))
            elseif closert == :paren
                return ErrorToken(PUNCTUATION(Tokens.RPAREN, 0, 0))
            elseif closert == :square
                return ErrorToken(PUNCTUATION(Tokens.RSQUARE, 0, 0))
            elseif closert == :brace
                return ErrorToken(PUNCTUATION(Tokens.RBRACE, 0, 0))
            end
        end
    end
end
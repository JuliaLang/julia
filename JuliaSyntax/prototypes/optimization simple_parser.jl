using JuliaSyntax: @K_str, is_literal, is_keyword, is_operator

# ---------- Helpers ----------

@inline function peek_is(st, kinds...)
    k = peek(st)
    return any(x -> x == k, kinds)
end

@inline function expect_or_eof(st, kind, msg)
    if peek(st) == kind
        bump(st, TRIVIA_FLAG)
        return true
    elseif peek(st) == K"EndMarker"
        emit_diagnostic(st, error="Unexpected end of input")
        return false
    end
    emit_diagnostic(st, error=msg)
    return false
end

# ---------- Top Level ----------

function parse_toplevel(st)
    mark = position(st)

    while peek(st) != K"EndMarker"
        bump_trivia(st, skip_newlines=true)
        peek(st) == K"EndMarker" && break
        parse_statement(st)
    end

    emit(st, mark, K"toplevel")
end

function parse_statement(st)
    peek(st) == K"function" ? parse_function_def(st) : parse_assignment(st)
end

# ---------- Functions & Blocks ----------

function parse_function_def(st)
    mark = position(st)
    bump(st, TRIVIA_FLAG)        # function keyword
    parse_call(st)               # function signature
    parse_block(st, K"end")
    emit(st, mark, K"function")
end

function parse_block(st, closing_kind, mark=position(st))
    while true
        bump_trivia(st, skip_newlines=true)

        if peek(st) == closing_kind
            bump(st, TRIVIA_FLAG)
            break
        elseif peek(st) == K"EndMarker"
            emit_diagnostic(st, error="Unexpected end of input")
            break
        end

        parse_assignment(st)
    end

    emit(st, mark, K"block")
end

# ---------- Assignments ----------

function parse_assignment(st)
    mark = position(st)
    parse_expression(st)

    if peek(st) == K"="
        bump(st, TRIVIA_FLAG)
        parse_expression(st)
        emit(st, mark, K"=")
    end
end

# ---------- Expressions (operator precedence) ----------

function parse_expression(st)
    parse_binary_chain(st, parse_term, (K"+", K"-"))
end

function parse_term(st)
    parse_binary_chain(st, parse_call, (K"*", K"/"))
end

function parse_binary_chain(st, subparser, ops)
    mark = position(st)
    subparser(st)

    while peek_is(st, ops...)
        bump(st)
        subparser(st)
        emit(st, mark, K"call", INFIX_FLAG)
    end
end

# ---------- Function Calls ----------

function parse_call(st)
    mark = position(st)
    parse_atom(st)

    peek(st) == K"(" || return

    bump(st, TRIVIA_FLAG)
    need_comma = false

    while true
        k = peek(st)

        if k == K")"
            bump(st, TRIVIA_FLAG)
            break
        elseif k == K"EndMarker"
            emit_diagnostic(st, error="Unexpected end of input")
            break
        elseif need_comma
            expect_or_eof(st, K",", "Expected a `,`")
        end

        parse_expression(st)
        need_comma = true
    end

    emit(st, mark, K"call")
end

# ---------- Atoms ----------

function parse_atom(st)
    bump_trivia(st, skip_newlines=true)
    mark = position(st)
    k = peek(st)

    if k == K"Identifier" || is_literal(k)
        bump(st)

    elseif peek_is(st, K"-", K"+")
        bump(st)
        parse_atom(st)
        emit(st, mark, K"call")

    elseif k == K"("
        bump(st, TRIVIA_FLAG)
        parse_expression(st)
        expect_or_eof(st, K")", "Expected `)` following expression")

    elseif k == K"begin"
        bump(st, TRIVIA_FLAG)
        parse_block(st, K"end", mark)

    else
        bump(st)
        emit(st, mark, K"error",
             error="Expected literal, identifier or opening parenthesis")
    end
end

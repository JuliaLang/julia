# Example parser for a very basic grammar
#
# This is simple but has some problems, most notably that expressions and terms
# aren't recursive so things like `a + b + c` can't be parsed!
#
# expression ::=
#   term | term "+" term | term "-" term
#
# term ::=
#   atom | atom "*" atom | atom "/" atom
#
# atom ::=
#   literal | identifier | "(" expression ")" | "-" atom | "+" atom
#

function parse_atom(st)
    p = position(st)
    k = peek(st)
    if k == K"Identifier" || isliteral(k)
        bump(st)
    elseif k in (K"-", K"+")
        bump(st)
        parse_atom(st)
        emit(st, p, K"call")
    elseif k == K"("
        bump(st, TRIVIA_FLAG)
        parse_expression(st)
        if peek(st) == K")"
            bump(st, TRIVIA_FLAG)
            # emit(st, p, K"(")
        else
            emit(st, p, K"(",
                 error="Expected `)` following expression")
        end
    else
        bump(st)
        emit(st, p, K"Error",
             error="Expected literal, identifier or opening parenthesis")
    end
end

function parse_term(st)
    p = position(st)
    parse_atom(st)
    k = peek(st)
    if k in (K"*", K"/")
        bump(st)
        parse_atom(st)
        emit(st, p, K"call", INFIX_FLAG)
    end
end

function parse_expression(st)
    p = position(st)
    parse_term(st)
    k = peek(st)
    if k in (K"+", K"-")
        bump(st)
        parse_term(st)
        emit(st, p, K"call", INFIX_FLAG)
    end
end

function parse_and_show(production::Function, code)
    st = ParseStream(code)
    production(st)
    t = JuliaSyntax.to_raw_tree(st)
    show(stdout, MIME"text/plain"(), t, code, show_trivia=true)
    if !isempty(st.diagnostics)
        println()
        for d in st.diagnostics
            JuliaSyntax.show_diagnostic(stdout, d, code)
        end
    end
    t
end

println()
println("Example diagnostics:")
parse_and_show(parse_expression, "(x + a*y) * (b")

println()
println("Example good parse:")
parse_and_show(parse_expression, "(x + a*y) * b")
nothing

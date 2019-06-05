module SExprs

export Unquote, @sexpr_str

struct Unquote
    name::Symbol
end

function skipws(io::IO)
    while !eof(io)
        c = read(io, Char)
        isspace(c) || return c
    end
    error("S-Expression terminated early by EOF")
end

putback(io::IO) = skip(io, -1) # assumes seekable :-/

function _parse(io::IO, c::Char)
    if c == '('
        lst = Any[]
        while true
            c = skipws(io)
            c == ')' && return lst
            push!(lst, _parse(io, c))
        end
    elseif c == ','
        c = skipws(io)
        c != ')' || error("No symbol found for unquote ','.  Got unexpected ')' instead")
        x = _parse(io, c)
        x isa Symbol || error("Unquote only supports symbols as arguments. Got $x")
        return Unquote(x)
    elseif isdigit(c)
        n = c-'0'
        while !eof(io)
            c = read(io, Char)
            if !isdigit(c)
                putback(io)
                break
            end
            n = 10*n + (c-'0')
        end
        return n
    elseif c == '"'
        buf = IOBuffer(sizehint=10)
        while true
            !eof(io) || error("Unterminated string")
            c = read(io, Char)
            c == '"' && break
            write(buf, c)
        end
        return String(take!(buf))
    elseif c == '#'
        !eof(io) || error("Unterminated #")
        c = read(io, Char)
        return c == 't' ? true :
               c == 'f' ? false :
               error("Invalid boolean")
    else
        # Anything else is a word...
        buf = IOBuffer(sizehint=10)
        write(buf, c)
        while !eof(io)
            c = read(io, Char)
            isspace(c) && break
            if c == ')' || c == ','
                putback(io)
                break
            end
            write(buf, c)
        end
        return Symbol(take!(buf))
    end
end

function parse(io::IO)
    c = skipws(io)
    c != ')' || error("Closing ')' found before '('")
    _parse(io, c)
end

parse(content::String) = parse(IOBuffer(content))

deparse(io::IO, sx::String)  = print(io, '"', sx, '"')
deparse(io::IO, sx::Bool)    = print(io, sx ? "#t" : "#f")
deparse(io::IO, sx) = print(io, sx)
deparse(io::IO, sx::Unquote) = print(io, ',', string(sx.name))
function deparse(io::IO, sx::Vector)
    write(io, '(')
    for (i,e) in enumerate(sx)
        deparse(io, e)
        i != length(sx) && write(io, ' ')
    end
    write(io, ')')
end

function deparse(sx)
    buf = IOBuffer()
    deparse(buf, sx)
    String(take!(buf))
end

fill_unquotes_expr(sx) = sx
fill_unquotes_expr(sx::Symbol) = QuoteNode(sx)
fill_unquotes_expr(sx::Unquote) = esc(sx.name)
fill_unquotes_expr(sx::Vector) = Expr(:vect, map(fill_unquotes_expr, sx)...)

macro sexpr_str(str)
    sx = parse(IOBuffer(str))
    fill_unquotes_expr(sx)
end

end


using Test

@testset "S-Expressions" begin
    parse = SExprs.parse

    # atoms
    @test parse("#t") == true
    @test parse("#f") == false
    @test parse("aa") == :aa
    @test parse("+-*") == Symbol("+-*")
    @test parse("12") == 12
    @test parse("\"some str 123\"") == "some str 123"
    # lists
    @test parse("(aa bb cc)") == [:aa, :bb, :cc]
    @test parse("(+ b (* c d))") == [:+, :b, [:*, :c, :d]]
    # unquote
    @test parse(",aa") == SExprs.Unquote(:aa)
    # whitespace
    @test parse("   ( a\nb\n\r(c   d))   ") == [:a, :b, [:c, :d]]

    # interpolation
    x = 10
    y = "str"
    @test SExprs.sexpr"(x ,y ,Int)" == [:x, "str", Int]

    # parse errors
    @test_throws ErrorException parse(")")
    @test_throws ErrorException parse("(")
    @test_throws ErrorException parse("(,)")
    @test_throws ErrorException parse(",")
    @test_throws ErrorException parse(",(")
    @test_throws ErrorException parse(",(a)")
    @test_throws ErrorException parse("\"asdf")
    @test_throws ErrorException parse("#")

    # printing
    @test SExprs.deparse([1,2, [:aa, :bb], SExprs.Unquote(:cc)]) == "(1 2 (aa bb) ,cc)"
end

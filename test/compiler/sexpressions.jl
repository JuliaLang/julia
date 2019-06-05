module SExprs

struct Unquote
    name::Symbol
end

function skipws(io::IO)
    while !eof(io)
        c = read(io, Char)
        if !isspace(c)
            return c
        end
    end
    error("S-Expression terminated early by EOF")
end

function _parse(io::IO, c::Char)
    if c == '('
        lst = Any[] # ok ok, this is not a real man's list.
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
    else
        # Anything else is a word...
        buf = IOBuffer(sizehint=10)
        write(buf, c)
        while !eof(io)
            c = read(io, Char)
            isspace(c) && break
            if c == ')' || c == ','
                skip(io, -1) # Assumes seekable :-/
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

macro sexpr_str(str)
    parse(IOBuffer(str))
end

end


using Test

@testset "S-Expressions" begin

    parse = SExprs.parse
    Unquote = SExprs.Unquote

    @test parse("aa") == :aa
    @test parse(",aa") == Unquote(:aa)
    @test parse("(aa bb cc)") == [:aa, :bb, :cc]
    @test parse("(+ b (* c d))") == [:+, :b, [:*, :c, :d]]
    # Whitespace
    @test parse("   ( a\nb\n\r(c   d))   ") == [:a, :b, [:c, :d]]

    # Errors
    @test_throws ErrorException parse(")")
    @test_throws ErrorException parse("(")
    @test_throws ErrorException parse("(,)")
    @test_throws ErrorException parse(",")
    @test_throws ErrorException parse(",(")
    @test_throws ErrorException parse(",(a)")
end

@testset "parser API" begin
    @testset "String and buffer input" begin
        # String
        let
            ex,diag,pos = parse(Expr, "x+y\nz")
            @test JuliaSyntax.remove_linenums!(ex) == Expr(:toplevel, :(x+y), :z)
            @test diag == []
            @test pos == 6
        end
        @test parse(Expr, "x+y\nz", rule=:statement) == (:(x+y), [], 4)
        @test parse(Expr, "x+y\nz", rule=:atom) == (:x, [], 2)
        @test parse(Expr, "x+y\nz", 5, rule=:atom) == (:z, [], 6)

        # Vector{UInt8}
        @test parse(Expr, Vector{UInt8}("x+y"), rule=:statement) == (:(x+y), [], 4)
        @test parse(Expr, Vector{UInt8}("x+y"), 3, rule=:statement) == (:y, [], 4)
        # Ptr{UInt8}, len
        code = "x+y"
        GC.@preserve code begin
            stream = ParseStream(pointer(code), 3)
            parse(stream, rule=:statement)
            @test JuliaSyntax.build_tree(Expr, stream) == :(x+y)
            @test JuliaSyntax.last_byte(stream) == 3
        end

        # SubString
        @test parse(Expr, SubString("x+y"), rule=:statement) == (:(x+y), [], 4)
        @test parse(Expr, SubString("x+y"), 1, rule=:atom) == (:x, [], 2)
        @test parse(Expr, SubString("x+y"), 3, rule=:atom) == (:y, [], 4)
        @test parse(Expr, SubString("x+y",3,3), 1, rule=:atom) == (:y, [], 2)
        @test parse(Expr, SubString("α+x"), rule=:statement) == (:(α+x), [], 5)
    end

    @testset "IO input" begin
        # IOBuffer
        io = IOBuffer("x+y")
        @test parse(Expr, io, rule=:statement) == (:(x+y), [])
        @test position(io) == 3
        io = IOBuffer("x+y")
        seek(io, 2)
        @test parse(Expr, io, rule=:atom) == (:y, [])
        @test position(io) == 3
        # A GenericIOBuffer, not actually IOBuffer
        io = IOBuffer(SubString("x+y"))
        @test parse(Expr, io, rule=:statement) == (:(x+y), [])
        @test position(io) == 3
        # Another type of GenericIOBuffer
        io = IOBuffer(codeunits("x+y"))
        @test parse(Expr, io, rule=:statement) == (:(x+y), [])
        @test position(io) == 3
        # IOStream
        mktemp() do path, io
            write(io, "x+y")
            close(io)

            open(path, "r") do io
                @test parse(Expr, io, rule=:statement) == (:(x+y), [])
                @test position(io) == 3
            end
        end
    end

    @testset "parseall" begin
        @test JuliaSyntax.remove_linenums!(parseall(Expr, " x ")) == Expr(:toplevel, :x)
        @test parseall(Expr, " x ", rule=:statement) == :x
        @test parseall(Expr, " x ", rule=:atom) == :x
        # TODO: Fix this situation with trivia here; the brackets are trivia, but
        # must be parsed to discover the atom inside. But in GreenTree we only
        # place trivia as siblings of the leaf node with identifier `x`, not as
        # children.
        @test_broken parseall(Expr, "(x)", rule=:atom) == :x

        @test_throws JuliaSyntax.ParseError parseall(Expr, "x+y", rule=:atom)
        @test_throws JuliaSyntax.ParseError parseall(Expr, "x+y\nz", rule=:statement)
    end
end

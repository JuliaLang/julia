@testset "parser API" begin
    @testset "parse with String input" begin
        @test parsestmt(Expr, " x ") == :x
        @test JuliaSyntax.remove_linenums!(parseall(Expr, " x ")) == Expr(:toplevel, :x)
        @test parseatom(Expr, " x ") == :x
        @test parseatom(Expr, "(x)") == :x

        # SubString
        @test parsestmt(Expr, SubString("x+y")) == :(x+y)
        @test parsestmt(Expr, SubString("α+x")) == :(α+x)
        @test parseatom(Expr, SubString("x+y",3,3)) == :y

        # Exceptions due to extra trailing syntax
        @test_throws JuliaSyntax.ParseError parseatom(Expr, "x+y")
        @test_throws JuliaSyntax.ParseError parsestmt(Expr, "x+y\nz")

        # ignore_warnings flag
        @test_throws JuliaSyntax.ParseError parsestmt(Expr, "import . .A")
        @test parsestmt(Expr, "import . .A", ignore_warnings=true) == :(import ..A)

        # version selection
        @test_throws JuliaSyntax.ParseError parsestmt(Expr, "[a ;; b]", version=v"1.6")
        @test parsestmt(Expr, "[a ;; b]", version=v"1.7") == Expr(:ncat, 2, :a, :b)

        # filename
        @test parsestmt(Expr, "begin\na\nend", filename="foo.jl", first_line=55) ==
            Expr(:block, LineNumberNode(56, Symbol("foo.jl")), :a)

        # ignore_trivia
        @test parseatom(Expr, " x ", ignore_trivia=true) == :x
        @test_throws JuliaSyntax.ParseError parseatom(Expr, " x ", ignore_trivia=false)

        # Top level parsing
        @test parseall(Expr, "a\nb") ==
            Expr(:toplevel, LineNumberNode(1), :a, LineNumberNode(2), :b)
        @test parseall(Expr, "a\nb #==#") ==
            Expr(:toplevel, LineNumberNode(1), :a, LineNumberNode(2), :b)
        @test parseall(Expr, "#==#\na\nb") ==
            Expr(:toplevel, LineNumberNode(2), :a, LineNumberNode(3), :b)
        @test parseall(Expr, "a\nb\n#==#") ==
            Expr(:toplevel, LineNumberNode(1), :a, LineNumberNode(2), :b)
    end

    @testset "IO input" begin
        # IOBuffer
        io = IOBuffer("x+y")
        @test parse!(Expr, io, rule=:statement) == (:(x+y), [])
        @test position(io) == 3
        io = IOBuffer("x+y")
        seek(io, 2)
        @test parse!(Expr, io, rule=:atom) == (:y, [])
        @test position(io) == 3
        # A GenericIOBuffer, not actually IOBuffer
        io = IOBuffer(SubString("x+y"))
        @test parse!(Expr, io, rule=:statement) == (:(x+y), [])
        @test position(io) == 3
        # Another type of GenericIOBuffer
        io = IOBuffer(codeunits("x+y"))
        @test parse!(Expr, io, rule=:statement) == (:(x+y), [])
        @test position(io) == 3
        # IOStream
        mktemp() do path, io
            write(io, "x+y")
            close(io)

            open(path, "r") do io
                @test parse!(Expr, io, rule=:statement) == (:(x+y), [])
                @test position(io) == 3
            end
        end
    end

    @testset "parse with String and index input" begin
        # String
        let
            ex,pos = parseall(Expr, "x+y\nz", 1)
            @test JuliaSyntax.remove_linenums!(ex) == Expr(:toplevel, :(x+y), :z)
            @test pos == 6
        end
        @test parsestmt(Expr, "x+y\nz", 1)     == (:(x+y), 4)
        @test parseatom(Expr, "x+y\nz", 1) == (:x, 2)
        @test parseatom(Expr, "x+y\nz", 5) == (:z, 6)

        # SubString
        @test parsestmt(Expr, SubString("α+x\ny"), 1)  == (:(α+x), 5)
        @test parseatom(Expr, SubString("x+y"), 1) == (:x, 2)
        @test parseatom(Expr, SubString("x+y"), 3) == (:y, 4)

        @test parseatom(Expr, SubString("x+1.0"), 3) == (1.0, 6)
        @test parseatom(Expr, SubString("x+\"\n\""), 3) == ("\n", 6)

        # Line numbers are relative to the start of the string we're currently
        # parsing
        @test JuliaSyntax.parsestmt(Expr, "begin\na\nend\nbegin\nb\nend", 1) ==
            (Expr(:block, LineNumberNode(2), :a), 12)
        @test JuliaSyntax.parsestmt(Expr, "begin\na\nend\nbegin\nb\nend", 12) ==
            (Expr(:block, LineNumberNode(3), :b), 24)
    end

    @testset "error/warning handling" begin
        parseshow(s;kws...) = sprint(show, MIME("text/x.sexpression"), parsestmt(SyntaxNode, s; kws...))
        @test_throws JuliaSyntax.ParseError parseshow("try finally catch ex end")
        @test parseshow("try finally catch ex end", ignore_warnings=true) ==
            "(try (block) (finally (block)) (catch ex (block)))"
        # ignore_errors
        @test_throws JuliaSyntax.ParseError parseshow("[a; b, c]")
        @test_throws JuliaSyntax.ParseError parseshow("[a; b, c]", ignore_warnings=true)
        @test parseshow("[a; b, c]", ignore_errors=true) == "(vcat a b (error-t) c)"
        # errors in literals
        @test parseshow("\"\\z\"", ignore_errors=true) == "(string (ErrorInvalidEscapeSequence))"
        @test parseshow("'\\z'", ignore_errors=true) == "(char (ErrorInvalidEscapeSequence))"
        @test parseshow("'abc'", ignore_errors=true) == "(char (ErrorOverLongCharacter))"
        @test parseshow("1e1000", ignore_errors=true) == "(ErrorNumericOverflow)"
        @test parseshow("1f1000", ignore_errors=true) == "(ErrorNumericOverflow)"
    end
end

@testset "ParseError printing" begin
    try
        parsestmt(SyntaxNode, "a -- b -- c", filename="somefile.jl")
        @assert false "error should be thrown"
    catch exc
        @test exc isa JuliaSyntax.ParseError
        @test sprint(showerror, exc) == """
            ParseError:
            # Error @ somefile.jl:1:3
            a -- b -- c
            # └┘ ── invalid operator"""
        @test occursin("Stacktrace:\n", sprint(showerror, exc, catch_backtrace()))
        file_url = JuliaSyntax._file_url("somefile.jl")
        @test sprint(showerror, exc, context=:color=>true) == """
            ParseError:
            \e[90m# Error @ \e[0;0m\e]8;;$file_url#1:3\e\\\e[90msomefile.jl:1:3\e[0;0m\e]8;;\e\\
            a \e[48;2;120;70;70m--\e[0;0m b -- c
            \e[90m# └┘ ── \e[0;0m\e[91minvalid operator\e[0;0m"""
    end

    try
        # Test that warnings are printed first followed by only the first error
        parsestmt(SyntaxNode, """
           @(a)
           x -- y
           z -- y""", filename="somefile.jl")
        @assert false "error should be thrown"
    catch exc
        @test exc isa JuliaSyntax.ParseError
        @test sprint(showerror, exc) == """
            ParseError:
            # Warning @ somefile.jl:1:2
            @(a)
            #└─┘ ── parenthesizing macro names is unnecessary
            # Error @ somefile.jl:2:1
            @(a)
            x
            ╙ ── unexpected text after parsing statement"""
    end

    try
        # Test that initial warnings are always printed
        parsestmt(SyntaxNode, """
           @(a)""", filename="somefile.jl")
        @assert false "error should be thrown"
    catch exc
        @test exc isa JuliaSyntax.ParseError
        @test sprint(showerror, exc) == """
            ParseError:
            # Warning @ somefile.jl:1:2
            @(a)
            #└─┘ ── parenthesizing macro names is unnecessary"""
    end
end

tokensplit(str) = [kind(tok) => untokenize(tok, str) for tok in tokenize(str)]

@testset "tokenize() API" begin
    # tokenize() is eager
    @test tokenize("aba") isa Vector{JuliaSyntax.Token}

    # . is a separate token from + in `.+`
    @test tokensplit("a .+ β") == [
        K"Identifier" => "a",
        K"Whitespace" => " ",
        K"." => ".",
        K"+" => "+",
        K"Whitespace" => " ",
        K"Identifier" => "β",
    ]

    # Contextual keywords become identifiers where necessary
    @test tokensplit("outer = 1") == [
        K"Identifier" => "outer",
        K"Whitespace" => " ",
        K"=" => "=",
        K"Whitespace" => " ",
        K"Integer" => "1",
    ]

    # A predicate based on flags()
    @test JuliaSyntax.is_suffixed(tokenize("+₁")[1])

    # Buffer interface
    @test tokenize(Vector{UInt8}("a + b")) == tokenize("a + b")

    buf = Vector{UInt8}("a-β")
    @test untokenize.(tokenize(buf), Ref(buf,)) == [
        Vector{UInt8}("a"),
        Vector{UInt8}("-"),
        Vector{UInt8}("β")
    ]

    @test kind(JuliaSyntax.Token()) == K"None"

    @test tokensplit("'\\") == [
        K"'" => "'",
        K"ErrorInvalidEscapeSequence" => "\\",
        K"error" => ""
    ]
end

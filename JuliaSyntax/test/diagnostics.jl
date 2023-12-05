function diagnostic(str; only_first=false, allow_multiple=false, rule=:all, version=v"1.6")
    stream = ParseStream(str; version=version)
    parse!(stream, rule=rule)
    if allow_multiple
        stream.diagnostics
    else
        if !only_first
            @test length(stream.diagnostics) == 1
        end
        return isempty(stream.diagnostics) ? nothing : stream.diagnostics[1]
    end
end

@testset "token errors" begin
    @test diagnostic("a\xf5b") == Diagnostic(2, 2, :error, "invalid UTF-8 sequence \"\\xf5\"")
    @test diagnostic("# a\xf5b") == Diagnostic(1, 5, :error, "invalid UTF-8 sequence \"# a\\xf5b\"")
    for c in ['\u00ad', '\u200b', '\u200c', '\u200d',
              '\u200e', '\u200f', '\u2060', '\u2061']
        @test diagnostic("a$(c)b") ==
            Diagnostic(2, 1+sizeof(string(c)), :error, "invisible character $(repr(c))")
    end
    @test diagnostic(":⥻") == Diagnostic(2, 4, :error, "unknown unicode character '⥻'")

    @test diagnostic("\"X \u202a X\"") == Diagnostic(2, 8, :error, "unbalanced bidirectional unicode formatting \"X \\u202a X\"")
    @test diagnostic("#= \u202a =#") == Diagnostic(1, 9, :error, "unbalanced bidirectional unicode formatting \"#= \\u202a =#\"")
    @test diagnostic("\"X \u202a \$xx\u202c\"", allow_multiple=true) == [
        Diagnostic(2, 7, :error, "unbalanced bidirectional unicode formatting \"X \\u202a \"")
        Diagnostic(11, 13, :error, "unbalanced bidirectional unicode formatting \"\\u202c\"")
    ]

    @test diagnostic("0x") == Diagnostic(1, 2, :error, "invalid numeric constant")
    @test diagnostic("0x0.1") == Diagnostic(1, 5, :error, "hex float literal must contain `p` or `P`")
end

@testset "parser errors" begin
	@test diagnostic("+ #==# (a,b)") ==
        Diagnostic(2, 7, :error, "whitespace not allowed between prefix function call and argument list")
    @test diagnostic("1 -+ (a=1, b=2)") ==
        Diagnostic(5, 5, :error, "whitespace not allowed between prefix function call and argument list")
    @test diagnostic("\n+ (x, y)") ==
        Diagnostic(3, 3, :error, "whitespace not allowed between prefix function call and argument list")

	@test diagnostic("A.@B.x", only_first=true) ==
        Diagnostic(3, 4, :error, "`@` must appear on first or last macro name component")
	@test diagnostic("@M.(x)") ==
        Diagnostic(1, 3, :error, "dot call syntax not supported for macros")

	@test diagnostic("try x end") ==
        Diagnostic(1, 9, :error, "try without catch or finally")
    # TODO: better range
	@test diagnostic("@A.\$x a") ==
        Diagnostic(6, 5, :error, "invalid macro name")

	@test diagnostic("a, , b") ==
        Diagnostic(4, 4, :error, "unexpected `,`")
    @test diagnostic(")", allow_multiple=true) == [
        Diagnostic(1, 1, :error, "unexpected `)`")
        Diagnostic(1, 1, :error, "extra tokens after end of expression")
    ]

    @test diagnostic("if\nfalse\nend") ==
        Diagnostic(3, 3, :error, "missing condition in `if`")
    @test diagnostic("if false\nelseif\nend") ==
        Diagnostic(16, 16, :error, "missing condition in `elseif`")

    @test diagnostic("f(x::V) where {V) = x", allow_multiple=true) == [
        Diagnostic(17, 16, :error, "Expected `}` or `,`")
        Diagnostic(17, 21, :error, "extra tokens after end of expression")
    ]
    @test diagnostic("[1)", allow_multiple=true) == [
        Diagnostic(3, 2, :error, "Expected `]` or `,`")
        Diagnostic(3, 3, :error, "extra tokens after end of expression")
    ]
    @test diagnostic("f(x, y #=hi=#\ng(z)") == Diagnostic(7, 6, :error, "Expected `)` or `,`")
    @test diagnostic("(x, y \nz") == Diagnostic(6, 5, :error, "Expected `)` or `,`")
    @test diagnostic("function f(x, y \nz end") == Diagnostic(16, 15, :error, "Expected `)` or `,`")
 
    @test diagnostic("sin. (1)") ==
        Diagnostic(5, 5, :error, "whitespace is not allowed here")
    @test diagnostic("x [i]") ==
        Diagnostic(2, 2, :error, "whitespace is not allowed here")
    @test diagnostic("\nf() [i]") ==
        Diagnostic(5, 5, :error, "whitespace is not allowed here")
    @test diagnostic("\nf() (i)") ==
        Diagnostic(5, 5, :error, "whitespace is not allowed here")
    @test diagnostic("\nf() .i") ==
        Diagnostic(5, 5, :error, "whitespace is not allowed here")
    @test diagnostic("\nf() {i}") ==
        Diagnostic(5, 5, :error, "whitespace is not allowed here")
    @test diagnostic("\n@ m") ==
        Diagnostic(3, 3, :error, "whitespace is not allowed here")
    @test diagnostic("\nusing a .b") ==
        Diagnostic(9, 9, :error, "whitespace is not allowed here")

    @test diagnostic("const x") ==
        Diagnostic(1, 7, :error, "expected assignment after `const`")
    @test diagnostic("global const x") ==
        Diagnostic(1, 14, :error, "expected assignment after `const`")

    @test diagnostic("(for i=1; println())") ==
        Diagnostic(20, 19, :error, "Expected `end`")
    @test diagnostic("(try i=1; println())", allow_multiple=true) == [
        Diagnostic(2, 19, :error, "try without catch or finally")
        Diagnostic(20, 19, :error, "Expected `end`")
    ]

    @test diagnostic("\"\$(x,y)\"") ==
        Diagnostic(3, 7, :error, "invalid interpolation syntax")

    @test diagnostic("", rule=:statement) ==
        Diagnostic(1, 0, :error, "premature end of input")
    @test diagnostic("", rule=:atom) ==
        Diagnostic(1, 0, :error, "premature end of input")
end

@testset "parser warnings" begin
	@test diagnostic("@(A)", only_first=true) ==
        Diagnostic(2, 4, :warning, "parenthesizing macro names is unnecessary")
	@test diagnostic("try finally catch a ; b end") ==
        Diagnostic(13, 23, :warning, "`catch` after `finally` will execute out of order")
	@test diagnostic("import .  .A") ==
        Diagnostic(9, 10, :warning, "space between dots in import path")
	@test diagnostic("import A .==") ==
        Diagnostic(9, 9, :warning, "space between dots in import path")
	@test diagnostic("import A.:+") ==
        Diagnostic(10, 10, :warning, "quoting with `:` is not required here")
    # No warning for import `:` symbol
    @test diagnostic("import A.:, :", allow_multiple=true) == []
    @test diagnostic("import A.(:+)") ==
        Diagnostic(10, 13, :warning, "parentheses are not required here")
    @test diagnostic("export (x)") ==
        Diagnostic(8, 10, :warning, "parentheses are not required here")
    @test diagnostic("export :x") ==
        Diagnostic(8, 9, :error, "expected identifier")
    @test diagnostic("public = 4", version=v"1.11") ==
        diagnostic("public[7] = 5", version=v"1.11") ==
        diagnostic("public() = 6", version=v"1.11") ==
        Diagnostic(1, 6, :warning, "using public as an identifier is deprecated")
end

@testset "diagnostics for literal parsing" begin
    # Float overflow/underflow
    @test diagnostic("x = 10.0e1000;") ==
        Diagnostic(5, 13, :error, "overflow in floating point literal")
    @test diagnostic("x = 10.0f1000;") ==
        Diagnostic(5, 13, :error, "overflow in floating point literal")
    @test diagnostic("x = 10.0e-1000;") ==
        Diagnostic(5, 14, :warning, "underflow to zero in floating point literal")
    @test diagnostic("x = 10.0f-1000;") ==
        Diagnostic(5, 14, :warning, "underflow to zero in floating point literal")
    # Underflow boundary
    @test diagnostic("5e-324", allow_multiple=true) == []
    @test diagnostic("2e-324") ==
        Diagnostic(1, 6, :warning, "underflow to zero in floating point literal")

    # Char
    @test diagnostic("x = ''") ==
        Diagnostic(6, 5, :error, "empty character literal")
    @test diagnostic("x = 'abc'") ==
        Diagnostic(6, 8, :error, "character literal contains multiple characters")
    @test diagnostic("x = '\\xq'") ==
        Diagnostic(6, 7, :error, "invalid hex escape sequence")
    @test diagnostic("x = '\\uq'") ==
        Diagnostic(6, 7, :error, "invalid unicode escape sequence")
    @test diagnostic("x = '\\Uq'") ==
        Diagnostic(6, 7, :error, "invalid unicode escape sequence")
    @test diagnostic("x = '\\777'") ==
        Diagnostic(6, 9, :error, "invalid octal escape sequence")
    @test diagnostic("x = '\\k'") ==
        Diagnostic(6, 7, :error, "invalid escape sequence")
    @test diagnostic("'\\", allow_multiple=true) == [
        Diagnostic(2, 2, :error, "invalid escape sequence"),
        Diagnostic(3, 2, :error, "unterminated character literal")
    ]
    # Various cases from Base
    @test diagnostic("'\\xff\\xff\\xff\\xff'") ==
        Diagnostic(2, 17, :error, "character literal contains multiple characters")
    @test diagnostic("'\\100\\42'") ==
        Diagnostic(2, 8, :error, "character literal contains multiple characters")
    @test diagnostic("'\\xff\\xff\\xff\\xff\\xff'") ==
        Diagnostic(2, 21, :error, "character literal contains multiple characters")
    @test diagnostic("'abcd'") ==
        Diagnostic(2, 5, :error, "character literal contains multiple characters")
    @test diagnostic("'\\uff\\xff'") ==
        Diagnostic(2, 9, :error, "character literal contains multiple characters")
    @test diagnostic("'\\xffa'") ==
        Diagnostic(2, 6, :error, "character literal contains multiple characters")
    @test diagnostic("'\\uffffa'") ==
        Diagnostic(2, 8, :error, "character literal contains multiple characters")
    @test diagnostic("'\\U00002014a'") ==
        Diagnostic(2, 12, :error, "character literal contains multiple characters")
    @test diagnostic("'\\1000'") ==
        Diagnostic(2, 6, :error, "character literal contains multiple characters")

    # String
    @test diagnostic("x = \"abc\\xq\"") ==
        Diagnostic(9, 10, :error, "invalid hex escape sequence")
    @test diagnostic("x = \"abc\\uq\"") ==
        Diagnostic(9, 10, :error, "invalid unicode escape sequence")
    @test diagnostic("x = \"abc\\Uq\"") ==
        Diagnostic(9, 10, :error, "invalid unicode escape sequence")
    @test diagnostic("x = \"abc\\777\"") ==
        Diagnostic(9, 12, :error, "invalid octal escape sequence")
    @test diagnostic("x = \"abc\\k\"") ==
        Diagnostic(9, 10, :error, "invalid escape sequence")
    @test diagnostic("x = \"abc\\k \\k\"", allow_multiple=true) == [
        Diagnostic(9, 10, :error, "invalid escape sequence"),
        Diagnostic(12, 13, :error, "invalid escape sequence")
    ]
    @test diagnostic("\"\$x෴  \"") ==
        Diagnostic(4, 6, :error, "interpolated variable ends with invalid character; use `\$(...)` instead")
end

@testset "diagnostic printing" begin
    stream = JuliaSyntax.ParseStream("a -- b -- c")
    JuliaSyntax.parse!(stream)
    @test sprint(JuliaSyntax.show_diagnostics, stream) == """
        # Error @ line 1:3
        a -- b -- c
        # └┘ ── invalid operator
        # Error @ line 1:8
        a -- b -- c
        #      └┘ ── invalid operator"""

    stream = JuliaSyntax.ParseStream("a -- b")
    JuliaSyntax.parse!(stream)
    fname = "test.jl"
    sf = SourceFile(stream, filename=fname)
    url = JuliaSyntax._file_url(fname)
    @test sprint(JuliaSyntax.show_diagnostics, stream.diagnostics, sf,
                 context=:color=>true) == """
        \e[90m# Error @ \e[0;0m\e]8;;$url#1:3\e\\\e[90mtest.jl:1:3\e[0;0m\e]8;;\e\\
        a \e[48;2;120;70;70m--\e[0;0m b
        \e[90m# └┘ ── \e[0;0m\e[91minvalid operator\e[0;0m"""

    if Sys.isunix()
        tempdirname = mktempdir()
        cd(tempdirname) do
            rm(tempdirname)
            # Test _file_url doesn't fail with nonexistant directories
            @test isnothing(JuliaSyntax._file_url(joinpath("__nonexistant__", "test.jl")))
        end
    end
end

function diagnostic(str; only_first=false, allow_multiple=false)
    stream = ParseStream(str)
    parse!(stream)
    if allow_multiple
        stream.diagnostics
    else
        if !only_first
            @test length(stream.diagnostics) == 1
        end
        return stream.diagnostics[1]
    end
end

@testset "parser errors" begin
	@test diagnostic("+ #==# (a,b)") ==
        Diagnostic(2, 7, :error, "whitespace not allowed between prefix function call and argument list")
	@test diagnostic("A.@B.x", only_first=true) ==
        Diagnostic(3, 4, :error, "`@` must appear on first or last macro name component")
	@test diagnostic("@M.(x)") ==
        Diagnostic(1, 3, :error, "dot call syntax not supported for macros")

	@test diagnostic("try x end") ==
        Diagnostic(1, 9, :error, "try without catch or finally")
    # TODO: better range
	@test diagnostic("@A.\$x a") ==
        Diagnostic(6, 5, :error, "invalid macro name")
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
    @test diagnostic("import A.(:+)") ==
        Diagnostic(10, 13, :warning, "parentheses are not required here")
    @test diagnostic("export (x)") ==
        Diagnostic(8, 10, :warning, "parentheses are not required here")
    @test diagnostic("export :x") == 
        Diagnostic(8, 9, :error, "expected identifier")
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
end

function diagnostic(str; allow_multiple=false)
    stream = ParseStream(str)
    parse!(stream)
    if allow_multiple
        stream.diagnostics
    else
        @test length(stream.diagnostics) == 1
        only(stream.diagnostics)
    end
end

@testset "diagnostics for literal parsing" begin
    # Float overflow/underflow
    @test diagnostic("x = 10.0e1000;") ==
        Diagnostic(5, 13, :error, "overflow in floating point literal")
    @test diagnostic("x = 10.0f1000;") ==
        Diagnostic(5, 13, :error, "overflow in floating point literal")
    @test diagnostic("x = 10.0e-1000;") ==
        Diagnostic(5, 14, :warning, "underflow in floating point literal")
    @test diagnostic("x = 10.0f-1000;") ==
        Diagnostic(5, 14, :warning, "underflow in floating point literal")

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

@testset "try/catch" begin

test_mod = Module()

@test isempty(current_exceptions())

@testset "tail position" begin

    @test JuliaLowering.include_string(test_mod, """
    try
        1
    catch
        2
    end
    """) == 1

    @test JuliaLowering.include_string(test_mod, """
    try
        error("hi")
        1
    catch
        2
    end
    """) == 2

    @test JuliaLowering.include_string(test_mod, """
    try
        error("hi")
    catch exc
        exc
    end
    """) == ErrorException("hi")


    @test JuliaLowering.include_string(test_mod, """
    try
        1
    catch
        2
    else
        3
    end
    """) == 3

    @test JuliaLowering.include_string(test_mod, """
    try
        error("hi")
        1
    catch
        2
    else
        3
    end
    """) == 2

    @test JuliaLowering.include_string(test_mod, """
    begin
        function f()
            try
                return 1
            catch
            end
            return 2
        end
        f()
    end
    """) == 1

    @test JuliaLowering.include_string(test_mod, """
    begin
        function f()
            try
                return 1
            catch
            end
        end
        f()
    end
    """) == 1

end

@testset "value position" begin

    @test JuliaLowering.include_string(test_mod, """
    let
        x = try
            1
        catch
            2
        end
        x
    end
    """) == 1

    @test JuliaLowering.include_string(test_mod, """
    let
        x = try
            error("hi")
            1
        catch
            2
        end
        x
    end
    """) == 2

    @test JuliaLowering.include_string(test_mod, """
    let
        x = try
            error("hi")
        catch exc
            exc
        end
        x
    end
    """) == ErrorException("hi")


    @test JuliaLowering.include_string(test_mod, """
    let
        x = try
            1
        catch
            2
        else
            3
        end
        x
    end
    """) == 3

    @test JuliaLowering.include_string(test_mod, """
    let
        x = try
            error("hi")
            1
        catch
            2
        else
            3
        end
        x
    end
    """) == 2

end

@testset "not value/tail position" begin

    @test JuliaLowering.include_string(test_mod, """
    let x = -1
        try
            x = 1
        catch
            x = 2
        end
        x
    end
    """) == 1

    @test JuliaLowering.include_string(test_mod, """
    let x = -1
        try
            error("hi")
            x = 1
        catch
            x = 2
        end
        x
    end
    """) == 2

    @test JuliaLowering.include_string(test_mod, """
    let x = -1
        try
            x = error("hi")
        catch exc
            x = exc
        end
        x
    end
    """) == ErrorException("hi")


    @test JuliaLowering.include_string(test_mod, """
    let x = -1
        try
            x = 1
        catch
            x = 2
        else
            x = 3
        end
        x
    end
    """) == 3

    @test JuliaLowering.include_string(test_mod, """
    let x = -1
        try
            error("hi")
            x = 1
        catch
            x = 2
        else
            x = 3
        end
        x
    end
    """) == 2

end

@testset "exception stack" begin

    @test JuliaLowering.include_string(test_mod, """
    try
        try
            error("hi")
        catch
            error("ho")
        end
    catch
        a = []
        for x in current_exceptions()
            push!(a, x.exception)
        end
        a
    end
    """) == [ErrorException("hi"), ErrorException("ho")]

end

@test isempty(current_exceptions())

test_ir_cases(joinpath(@__DIR__, "exceptions_ir.jl"))

end

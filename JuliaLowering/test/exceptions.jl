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
        function g()
            try
                return 1
            catch
            end
        end
        g()
    end
    """) == 1

    @test JuliaLowering.include_string(test_mod, """
    let x = -1
        while true
            try
                error("hi")
            catch
                x = 2
                break
            end
        end
        x
    end
    """) == 2

    @test JuliaLowering.include_string(test_mod, """
    let x = -1
        while true
            try
                x = 2
                break
            catch
            end
        end
        x
    end
    """) == 2
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

end

#-------------------------------------------------------------------------------
@testset "try/finally" begin

test_mod = Module()

@test JuliaLowering.include_string(test_mod, """
let x = -1
    try
        x = 1
    finally
        x = 2
    end
    x
end
""") == 2

@test JuliaLowering.include_string(test_mod, """
let x = -1
    try
        try
            error("hi")
            x = 1
        finally
            x = 2
        end
    catch
    end
    x
end
""") == 2

JuliaLowering.include_string(test_mod, """
begin
    function nested_finally(a, x, b, c)
        try
            try
                if x
                    return b
                end
                c
            finally
                push!(a, 1)
            end
        finally
            push!(a, 2)
        end
    end
end
""")
@test (a = []; res = test_mod.nested_finally(a, true, 100, 200); (a, res)) == ([1,2], 100)
@test (a = []; res = test_mod.nested_finally(a, false, 100, 200); (a, res)) == ([1,2], 200)

@test JuliaLowering.include_string(test_mod, """
try
    1
catch
    2
finally
    3
end
""") == 1

@test JuliaLowering.include_string(test_mod, """
try
    error("hi")
    1
catch
    2
finally
    3
end
""") == 2

end

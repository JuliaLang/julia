# This file is a part of Julia. License is MIT: https://julialang.org/license

nested_error_expr = quote
    try
        __not_a_binding__
    catch
        1 ÷ 0  # Generate error while handling error
    end
end

nested_error_pattern = r"""
    ERROR: DivideError: integer division error
    Stacktrace:.*

    caused by:
    UndefVarError: __not_a_binding__ not defined
    Stacktrace:.*
    """s

@testset "display_error" begin
    # Display of errors which cause more than one entry on the exception stack
    err_str = try
        eval(nested_error_expr)
    catch
        excs = Base.catch_stack()
        @test typeof.(first.(excs)) == [UndefVarError, DivideError]
        sprint(Base.display_error, excs)
    end
    @test occursin(nested_error_pattern, err_str)
end

@testset "Fallback REPL" begin
    # Fallback REPL should show errors with display_error
    errio = IOBuffer()
    Base.eval_user_input(errio, nested_error_expr, true)
    err_str = String(take!(errio))
    @test occursin(nested_error_pattern, err_str)
end

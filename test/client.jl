# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base.StackTraces: StackFrame

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

    caused by: UndefVarError: `__not_a_binding__` not defined in `Main`
    Stacktrace:.*
    """s

@testset "display_error" begin
    # Display of errors which cause more than one entry on the exception stack
    excs = try
        Core.eval(Main, nested_error_expr)
    catch
        Base.current_exceptions()
    end
    @test typeof.(first.(excs)) == [UndefVarError, DivideError]
    @test occursin(nested_error_pattern, sprint(Base.display_error, excs))

    @test occursin(r"""
        2-element ExceptionStack:
        DivideError: integer division error
        Stacktrace:.*

        caused by: UndefVarError: `__not_a_binding__` not defined in `Main`
        Stacktrace:.*
        """s, sprint(show, excs))
end

@testset "Fallback REPL" begin
    # Fallback REPL should show errors with display_error
    errio = IOBuffer()
    Base.eval_user_input(errio, nested_error_expr, true)
    err_str = String(take!(errio))
    @test occursin(nested_error_pattern, err_str)
end

@testset "display_error(io, er, bt) works" begin
    errio = IOBuffer()
    Base.display_error(errio, ErrorException, [])
    err_str = String(take!(errio))
    @test occursin(r"""
        ERROR: ErrorException
        """s, err_str)
end

@testset "defining `ans` and `err`" begin
    @test eval(:(ans = 1)) == 1
    @test eval(:(err = 1)) == 1
end

@testset "scrub REPL-related frames" begin
    repl_bt = [StackFrame(:foo, "foo.jl", 1),
          StackFrame(:__repl_entry_anysuffix, "client.jl", 2),
          StackFrame(:bar, "bar.jl", 3)]
    scrubbed_repl_bt = Base.scrub_repl_backtrace(repl_bt)

    nonrepl_bt = [StackFrame(:foo, "foo.jl", 1),
          StackFrame(:baz, "baz.jl", 2),
          StackFrame(:bar, "bar.jl", 3)]
    scrubbed_nonrepl_bt = Base.scrub_repl_backtrace(nonrepl_bt)

    @test length(scrubbed_repl_bt) == 1
    @test scrubbed_repl_bt[1].func == :foo
    @test length(scrubbed_nonrepl_bt) == 3

    errio = IOBuffer()
    lower_errexpr = :(@bad)
    Base.eval_user_input(errio, lower_errexpr, false)
    outstr = String(take!(errio))
    @test occursin("ERROR: LoadError: UndefVarError: `@bad`", outstr)
    @test !occursin("_repl_entry", outstr)
    @test !occursin(r"\.[/\\]client.jl", outstr)

    errexpr = :(error("fail"))
    Base.eval_user_input(errio, errexpr, false)
    outstr = String(take!(errio))
    @test occursin("ERROR: fail", outstr)
    @test !occursin("_repl_entry", outstr)
    @test !occursin(r"\.[/\\]client.jl", outstr)
end

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

    # driver entries (script/-e execution) cut like REPL entries, including
    # the eval/include machinery they run user code through
    script_bt = [StackFrame(:foo, "foo.jl", 1),
          StackFrame(Symbol("top-level scope"), "script.jl", 2),
          StackFrame(:include_string, "loading.jl", 3),
          StackFrame(:_include, "loading.jl", 4),
          StackFrame(:__script_entry_include, "client.jl", 5),
          StackFrame(:exec_options, "client.jl", 6),
          StackFrame(:_start, "client.jl", 7)]
    @test [f.func for f in Base.scrub_repl_backtrace(script_bt)] ==
        [:foo, Symbol("top-level scope")]

    # machinery frames not adjacent to the driver cut belong to user code
    user_eval_bt = [StackFrame(:foo, "foo.jl", 1),
          StackFrame(:eval, "boot.jl", 2),
          StackFrame(Symbol("top-level scope"), "script.jl", 3),
          StackFrame(:eval, "boot.jl", 4),
          StackFrame(:__script_entry_eval, "client.jl", 5)]
    @test [f.func for f in Base.scrub_repl_backtrace(user_eval_bt)] ==
        [:foo, :eval, Symbol("top-level scope")]

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

function capture_stderr(f)
    mktemp() do path, io
        redirect_stderr(f, io)
        flush(io)
        read(path, String)
    end
end

@testset "shell mode commands" begin
    @test_throws "no cmd to execute" Base.repl_cmd(``, devnull)
    @test_throws "cd method only takes one argument" Base.repl_cmd(`cd a b`, devnull)
    withenv("OLDPWD" => nothing) do
        @test_throws "cd: OLDPWD not set" Base.repl_cmd(`cd -`, devnull)
    end
    @test_throws "repl_cmd: expected an `AbstractCmd`, got $Int" Base.repl_cmd(1, devnull)

    # a command that cannot be started shows the error instead of throwing it
    missing_cmd = "/nonexistent/julia_test_cmd"
    @test occursin("ERROR: IOError: could not spawn `$missing_cmd`",
                   capture_stderr(() -> Base.repl_cmd(pipeline(`$missing_cmd`, stdout=devnull), devnull)))
    if !Sys.iswindows()
        @test occursin("ERROR: IOError: could not spawn `$missing_cmd -c",
                       capture_stderr(() -> withenv(() -> Base.repl_cmd(`echo hi`, devnull), "JULIA_SHELL" => missing_cmd)))
    end
    @test capture_stderr(() -> Base.display_error(ErrorException("boom"))) == "ERROR: boom\n"
    @test Base.parse_input_line(SubString("1 + 1")) == Base.parse_input_line("1 + 1")
    withenv("JULIA_STACKFRAME_LINEINFO_COLOR" => nothing, "JULIA_STACKFRAME_FUNCTION_COLOR" => nothing) do
        @test Base.stackframe_lineinfo_color() === Base.stackframe_function_color() === :bold
    end
end

@testset "exec_options" begin
    exe = Base.julia_cmd()
    # CI debug logging turns on verbose tests and evaluation tracing
    @test read(addenv(`$exe --startup-file=no -e 'print(ENV["JULIA_TEST_VERBOSE"], Base.TRACE_EVAL)'`,
                      "RUNNER_DEBUG" => "true"), String) == "trueloc"
    mktempdir() do dir
        sep = Sys.iswindows() ? ";" : ":"
        depot = dir * sep
        mkpath(joinpath(dir, "config"))
        write(joinpath(dir, "config", "startup.jl"), "error(\"startup failed\")")
        err = IOBuffer()
        p = run(pipeline(ignorestatus(addenv(`$exe --startup-file=yes -e 'print(1)'`, "JULIA_DEPOT_PATH" => depot));
                         stdout=devnull, stderr=err))
        @test p.exitcode == 1
        @test occursin("startup failed", String(take!(err)))
    end
end

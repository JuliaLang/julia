# This file is a part of Julia. License is MIT: https://julialang.org/license

# Make a copy of the original environment
original_env = copy(ENV)

using Test, Base.CoreLogging
import Base.CoreLogging: BelowMinLevel, Debug, Info, Warn, Error,
    handle_message, shouldlog, min_enabled_level, catch_exceptions

import Test: collect_test_logs, TestLogger
using Printf: @sprintf

isdefined(Main, :MacroCalls) || @eval Main include("testhelpers/MacroCalls.jl")
using Main.MacroCalls

#-------------------------------------------------------------------------------
@testset "Logging" begin

@testset "Basic logging" begin
    @test_logs (Debug, "a") min_level=Debug @debug "a"
    @test_logs (Info,  "a") @info  "a"
    @test_logs (Warn,  "a") @warn  "a"
    @test_logs (Error, "a") @error "a"
end

#-------------------------------------------------------------------------------
# Front end

@testset "Log message formatting" begin
    @test_logs (Info, "sum(A) = 16.0") @info begin
        A = fill(1.0, 4, 4)
        "sum(A) = $(sum(A))"
    end
    x = 10.50
    @test_logs (Info, "10.5") @info "$x"
    @test_logs (Info, "10.500") @info @sprintf("%.3f", x)
end

@testset "Programmatically defined levels" begin
    level = Info
    @test_logs (Info, "X") @logmsg level "X"
    level = Warn
    @test_logs (Warn, "X") @logmsg level "X"
end

@testset "Structured logging with key value pairs" begin
    foo_val = 10
    bar_val = 100
    logs,_ = collect_test_logs() do
        @info "test"  bar_val  progress=0.1  foo=foo_val  2*3  bar(x)=1.2  real_line=(@__LINE__)
        @info begin
            value_in_msg_block = 1000.0
            "test2"
        end value_in_msg_block
        test_splatting(;kws...) = @info "test3" kws...
        test_splatting(a=1,b=2.0)
    end
    @test length(logs) == 3

    record = logs[1]
    kwargs = record.kwargs

    # Builtin metadata
    @test record._module == @__MODULE__
    @test record.file == Base.source_path()
    @test record.line == kwargs[:real_line]
    @test record.id isa Symbol
    @test occursin(r"^.*logging_[[:xdigit:]]{8}$", String(record.id))

    # User-defined metadata
    @test kwargs[:bar_val] === bar_val
    @test kwargs[:progress] == 0.1
    @test kwargs[:foo] === foo_val
    @test kwargs[Symbol(:(2*3))] === 6
    @test kwargs[Symbol(:(bar(x)))] === 1.2

    # Keyword values accessible from message block
    record2 = logs[2]
    @test occursin((Info, "test2"), record2)
    kwargs = record2.kwargs
    @test kwargs[:value_in_msg_block] === 1000.0

    # Splatting of keywords
    record3 = logs[3]
    @test occursin((Info, "test3"), record3)
    kwargs = record3.kwargs
    @test sort(collect(keys(kwargs))) == [:a, :b]
    @test kwargs[:a] === 1
    @test kwargs[:b] === 2.0
end

@testset "Log message exception handling" begin
    # Exceptions in message creation are caught by default
    @test_logs (Error, Test.Ignored(), Test.Ignored(), :logevent_error) catch_exceptions=true @info "foo $(1÷0)"
    # Exceptions propagate if explicitly disabled for the logger (by default
    # for the test logger)
    @test_throws DivideError collect_test_logs() do
        @info  "foo $(1÷0)"
    end
    # trivial expressions create the errors explicitly instead of throwing them (to avoid try/catch)
    for i in 1:2
        local msg, x, y
        logmsg = (function() @info msg x=y end,
                  function() @info msg x=y z=1+1 end)[i]
        @test_logs (Error, Test.Ignored(), Test.Ignored(), :logevent_error) catch_exceptions=true logmsg()
        @test_throws UndefVarError(:msg, :local) collect_test_logs(logmsg)
        @test (only(collect_test_logs(logmsg, catch_exceptions=true)[1]).kwargs[:exception]::Tuple{UndefVarError, Vector})[1] === UndefVarError(:msg, :local)
        msg = "the msg"
        @test_logs (Error, Test.Ignored(), Test.Ignored(), :logevent_error) catch_exceptions=true logmsg()
        @test_throws UndefVarError(:y, :local) collect_test_logs(logmsg)
        @test (only(collect_test_logs(logmsg, catch_exceptions=true)[1]).kwargs[:exception]::Tuple{UndefVarError, Vector})[1] === UndefVarError(:y, :local)
        y = "the y"
        @test_logs (Info,"the msg") logmsg()
        @test only(collect_test_logs(logmsg)[1]).kwargs[:x] === "the y"
    end
end
@testset "Log message handle_message exception handling" begin
    # Exceptions in log handling (printing) of msg are caught by default.
    struct Foo end
    Base.show(::IO, ::Foo) = 1 ÷ 0

    # We cannot use `@test_logs` here, since test_logs does not actually _print_ the message
    # (i.e. it does not invoke handle_message). To test exception handling during printing,
    # we have to use `@test_warn` to see what was printed.
    @test_warn r"Error: Exception while generating log record in module .*DivideError: integer division error"s @info Foo()

    # Exceptions in log handling (printing) of attributes are caught by default
    @test_warn r"Error: Exception while generating log record in module .*DivideError: integer division error"s @info "foo" x=Foo()
end

@testset "Special keywords" begin
    logger = TestLogger()
    with_logger(logger) do
        @info "foo" _module=Base.Core _id=:asdf _group=:somegroup _file="/a/file" _line=-10
    end
    @test length(logger.logs) == 1
    record = logger.logs[1]
    @test record._module == Base.Core
    @test record.group === :somegroup
    @test record.id === :asdf
    @test record.file == "/a/file"
    @test record.line == -10
    # Test consistency with shouldlog() function arguments
    @test record.level   == logger.shouldlog_args[1]
    @test record._module == logger.shouldlog_args[2]
    @test record.group   == logger.shouldlog_args[3]
    @test record.id      == logger.shouldlog_args[4]

    # handling of nothing
    logger = TestLogger()
    with_logger(logger) do
        @info "foo" _module = nothing _file = nothing _line = nothing
    end
    @test length(logger.logs) == 1
    record = logger.logs[1]
    @test record._module === nothing
    @test record.file === nothing
    @test record.line === nothing
end

# PR #28209
@testset "0-arg MethodErrors" begin
    @test_throws MethodError @macrocall(@logmsg :Notice)
    @test_throws MethodError @macrocall(@debug)
    @test_throws MethodError @macrocall(@info)
    @test_throws MethodError @macrocall(@warn)
    @test_throws MethodError @macrocall(@error)
end

@testset "Any type" begin
    @test_logs (:info, sum) @info sum
    # TODO: make this work (here we want `@test_logs` to fail)
    # @test_fails @test_logs (:info, "sum") @info sum   # `sum` works, `"sum"` does not

    # check that the message delivered to the user works
    mktempdir() do dir
        path_stdout = joinpath(dir, "stdout.txt")
        path_stderr = joinpath(dir, "stderr.txt")
        redirect_stdio(stdout=path_stdout, stderr=path_stderr) do
            @info sum
        end
        @test occursin("Info: sum", read(path_stderr, String))
    end
end

#-------------------------------------------------------------------------------
# Early log level filtering

@testset "Early log filtering" begin
    @testset "Log filtering, per task logger" begin
        @test_logs (Warn, "c") min_level=Warn begin
            @info "b"
            @warn "c"
        end
    end

    @testset "Log level filtering - global flag" begin
        # Test utility: Log once at each standard level
        function log_each_level()
            @debug "a"
            @info  "b"
            @warn  "c"
            @error "d"
        end

        disable_logging(BelowMinLevel)
        @test_logs (Debug, "a") (Info, "b") (Warn, "c") (Error, "d") min_level=Debug  log_each_level()

        disable_logging(Debug)
        @test_logs (Info, "b") (Warn, "c") (Error, "d") min_level=Debug  log_each_level()

        disable_logging(Info)
        @test_logs (Warn, "c") (Error, "d") min_level=Debug  log_each_level()

        disable_logging(Warn)
        @test_logs (Error, "d") min_level=Debug  log_each_level()

        disable_logging(Error)
        @test_logs log_each_level()

        # Reset to default
        disable_logging(BelowMinLevel)
    end

    @testset "Log level filtering - ENV" begin
        logger = TestLogger()
        with_logger(logger) do
            for (e, r) in (("", false),
                            (",,,,", false),
                            ("al", false),
                            ("all", true),
                            ("a,b,all,c", true),
                            ("a,b,,c", false),
                            ("Mainb", false),
                            ("aMain", false),
                            ("Main", true),
                            ("a,b,Main,c", true),
                            ("Base", true),
                            ("a,b,Base,c", true),
                            ("Filesystem", true),
                            ("a,b,Filesystem,c", true),
                            ("a,b,Base.Filesystem,c", false),
                            ("!al", true),
                            ("all,!al", true),
                            ("all,!al,!all", false),
                            ("!all,Main", true),
                            ("!all,!Main", false),
                            ("!all,a,b,!Main,c", false),
                            ("!all,Filesystem", true),
                            ("!all,Base.Filesystem", false),
                            ("a,b,all,!all,c", false),
                            ("!Main", false),
                            ("a,b,!Main,c", false),
                            ("!Base", false),
                            ("all,!Base", false),
                            ("!all,Base", true),
                            ("!all,!Base", false),
                            ("a,b,!Base,c", false),
                            ("all,a,b,!Base,c", false),
                            ("!all,a,b,Base,c", true),
                            ("!all,a,b,!Base,c", false),
                            ("!Filesystem", false),
                            ("all,!Filesystem", false),
                            ("!all,Filesystem", true),
                            ("!all,!Filesystem", false),
                            ("a,b,!Filesystem,c", false),
                            ("all,a,b,!Filesystem,c", false),
                            ("!all,a,b,Filesystem,c", true),
                            ("!all,a,b,!Filesystem,c", false),
                            ("a,b,!Base.Filesystem,c", true),
                            ("all,a,b,!Base.Filesystem,c", true),
                            ("!all,a,b,Base.Filesystem,c", false),
                            ("!all,a,b,!Base.Filesystem,c", false),
                           )
                ENV["JULIA_DEBUG"] = e
                @test CoreLogging.env_override_minlevel(:Main, Base.Filesystem) === r
                @test CoreLogging.current_logger_for_env(BelowMinLevel, :Main, Base.Filesystem) === (r ? logger : nothing)
                @test CoreLogging.current_logger_for_env(Info, :Main, Base.Filesystem) === logger
            end
        end
    end
    ENV["JULIA_DEBUG"] = ""
end

#-------------------------------------------------------------------------------

@eval module LogModuleTest
    function a()
        @info  "a"
    end

    module Submodule
        function b()
            @info  "b"
        end
    end
end

@testset "Capture of module information" begin
    @test_logs(
        (Info, "a", LogModuleTest),
        (Info, "b", LogModuleTest.Submodule),
        begin
            LogModuleTest.a()
            LogModuleTest.Submodule.b()
        end
    )
end


#-------------------------------------------------------------------------------
@testset "Logger installation and access" begin
    @testset "Global logger" begin
        logger1 = global_logger()
        logger2 = TestLogger()
        # global_logger() returns the previously installed logger
        @test logger1 === global_logger(logger2)
        # current logger looks up global logger by default.
        @test current_logger() === logger2
        global_logger(logger1) # Restore global logger
    end
end

#-------------------------------------------------------------------------------

# Custom log levels

@eval module LogLevelTest
    using Base.CoreLogging

    struct MyLevel
        level::Int
    end

    Base.convert(::Type{LogLevel}, l::MyLevel) = LogLevel(l.level)

    const critical = MyLevel(10000)
    const debug_verbose = MyLevel(-10000)
end

@testset "Custom log levels" begin
    @test_logs (LogLevelTest.critical, "blah") @logmsg LogLevelTest.critical "blah"
    logs,_ = collect_test_logs(min_level=Debug) do
        @logmsg LogLevelTest.debug_verbose "blah"
    end
    @test length(logs) == 0
end


#-------------------------------------------------------------------------------

@testset "SimpleLogger" begin
    # Log level limiting
    @test min_enabled_level(SimpleLogger(devnull, Debug)) == Debug
    @test min_enabled_level(SimpleLogger(devnull, Error)) == Error

    # Log limiting
    logger = SimpleLogger(devnull)
    @test shouldlog(logger, Info, Base, :group, :asdf) === true
    handle_message(logger, Info, "msg", Base, :group, :asdf, "somefile", 1, maxlog=2)
    @test shouldlog(logger, Info, Base, :group, :asdf) === true
    handle_message(logger, Info, "msg", Base, :group, :asdf, "somefile", 1, maxlog=2)
    @test shouldlog(logger, Info, Base, :group, :asdf) === false
    @test catch_exceptions(logger) === false

    # Log formatting
    function genmsg(level, message, _module, filepath, line; kws...)
        io = IOBuffer()
        logger = SimpleLogger(io, Debug)
        handle_message(logger, level, message, _module, :group, :id,
                       filepath, line; kws...)
        String(take!(io))
    end

    function genmsg_err(level, message, _module, filepath, line; kws...)
        fname = tempname()
        f = open(fname, "w")
        logger = SimpleLogger()
        redirect_stderr(f) do
            handle_message(logger, level, message, _module, :group, :id,
                           filepath, line; kws...)
        end
        close(f)
        buf = read(fname)
        rm(fname)
        String(buf)
    end

    # Simple
    @test genmsg_err(Info, "msg", Main, "some/path.jl", 101) ==
    """
    ┌ Info: msg
    └ @ Main some/path.jl:101
    """

    # Multiline message
    @test genmsg_err(Warn, "line1\nline2", Main, "some/path.jl", 101) ==
    """
    ┌ Warning: line1
    │ line2
    └ @ Main some/path.jl:101
    """

    # Keywords
    @test genmsg(Error, "msg", Base, "other.jl", 101, a=1, b="asdf") ==
    """
    ┌ Error: msg
    │   a = 1
    │   b = asdf
    └ @ Base other.jl:101
    """

    # nothing values
    @test genmsg(Warn, "msg", nothing, nothing, nothing) ==
    """
    ┌ Warning: msg
    └ @ nothing nothing:nothing
    """
end

# Issue #26273
let m = Module(:Bare26273i, false)
    Core.eval(m, :(import Base: @error))
    @test_logs (:error, "Hello") Core.eval(m, quote
        @error "Hello"
    end)
end

@testset "#26335: _module and _file kwargs" begin
    ignored = Test.Ignored()
    @test_logs (:warn, "a", ignored, ignored, ignored, "foo.jl") (@warn "a" _file="foo.jl")
    @test_logs (:warn, "a", Base) (@warn "a" _module=Base)
end

# Issue #28786
@testset "ID generation" begin
    logs,_ = collect_test_logs() do
        for i in 1:2
            @info "test"
            @info "test"
        end
    end
    @test length(logs) == 4
    @test logs[1].id == logs[3].id
    @test logs[2].id == logs[4].id
    @test logs[1].id != logs[2].id
end

# Issue #34485
@testset "`_group` must be a `Symbol`" begin
    (record,), _ = collect_test_logs() do
        @info "test"
    end
    @test record.group === :corelogging  # name of this file
end

@testset "complicated kwargs logging macro" begin
    @test_logs (:warn, "foo")  @warn "foo" argvals=:((DoNotCare{$(Expr(:escape, :Any))}(),))
end

@testset "stdlib path" begin
    logger = TestLogger()
    with_logger(logger) do
        @info "foo" _file=joinpath(Sys.BUILD_STDLIB_PATH, "InteractiveUtils", "src", "InteractiveUtils.jl")
    end
    logs = logger.logs
    @test length(logs) == 1
    record = logs[1]
    @test isfile(record.file)
end

end

# Restore the original environment
for k in keys(ENV)
    if !haskey(original_env, k)
        delete!(ENV, k)
    end
end
for (k, v) in pairs(original_env)
    ENV[k] = v
end

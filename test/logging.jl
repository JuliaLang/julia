# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.CoreLogging
import Base.CoreLogging: BelowMinLevel, Debug, Info, Warn, Error,
    handle_message, shouldlog, min_enabled_level, catch_exceptions

import Test: collect_test_logs, TestLogger
using Base.Printf: @sprintf

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
        @info "test"  bar_val  progress=0.1  foo=foo_val  2*3  real_line=(@__LINE__)
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
    @test_logs (Error,Test.Ignored(),Test.Ignored(),:logevent_error) catch_exceptions=true  @info "foo $(1÷0)"
    # Exceptions propagate if explicitly disabled for the logger (by default
    # for the test logger)
    @test_throws DivideError collect_test_logs() do
        @info  "foo $(1÷0)"
    end
end

@testset "Special keywords" begin
    logger = TestLogger()
    with_logger(logger) do
        @info "foo" _module=Base.Core _id=:asdf _group=:somegroup _file="/a/file" _line=-10
    end
    @test length(logger.logs) == 1
    record = logger.logs[1]
    @test record._module == Base.Core
    @test record.group == :somegroup
    @test record.id == :asdf
    @test record.file == "/a/file"
    @test record.line == -10
    # Test consistency with shouldlog() function arguments
    @test record.level   == logger.shouldlog_args[1]
    @test record._module == logger.shouldlog_args[2]
    @test record.group   == logger.shouldlog_args[3]
    @test record.id      == logger.shouldlog_args[4]
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

    # Simple
    @test genmsg(Info, "msg", Main, "some/path.jl", 101) ==
    """
    ┌ Info: msg
    └ @ Main some/path.jl:101
    """

    # Multiline message
    @test genmsg(Warn, "line1\nline2", Main, "some/path.jl", 101) ==
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
end

# Issue #26273
let m = Module(:Bare26273i, false)
    eval(m, :(import Base: @error))
    @test_logs (:error, "Hello") eval(m, quote
        @error "Hello"
    end)
end

end

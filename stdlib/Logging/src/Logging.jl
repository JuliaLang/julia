# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Logging

Capturing, filtering and presenting streams of log events. The logging macros such as
`@info` are exported by `Base`, so this module is only needed to change where log events
go, with loggers such as [`ConsoleLogger`](@ref) and functions such as
[`with_logger`](@ref) and [`global_logger`](@ref).
"""
module Logging

import Base.CoreLogging:
    LogLevel,
    AbstractLogger,
    NullLogger,
    handle_message, shouldlog, min_enabled_level, catch_exceptions,
    var"@debug",
    var"@info",
    var"@warn",
    var"@error",
    var"@logmsg",
    with_logger,
    current_logger,
    global_logger,
    disable_logging,
    SimpleLogger,
    Debug,
    Info,
    Warn,
    Error,
    BelowMinLevel,
    AboveMaxLevel,
    default_logcolor,
    closed_stream,
    ConsoleLogger,
    default_metafmt,
    # Some packages use `Logging.default_logcolor`
    default_logcolor

export
    AbstractLogger,
    LogLevel,
    NullLogger,
    @debug,
    @info,
    @warn,
    @error,
    @logmsg,
    with_logger,
    current_logger,
    global_logger,
    disable_logging,
    SimpleLogger,
    ConsoleLogger,
    BelowMinLevel,
    Debug,
    Info,
    Warn,
    Error,
    AboveMaxLevel

public handle_message, shouldlog, min_enabled_level, catch_exceptions

end

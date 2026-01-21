# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Utilities for capturing, filtering and presenting streams of log events.
Normally you don't need to import `Logging` to create log events; for this
the standard logging macros such as `@info` are already exported by `Base`
and available by default.
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

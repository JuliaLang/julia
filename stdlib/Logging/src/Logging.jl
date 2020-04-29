# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Utilities for capturing, filtering and presenting streams of log events.
Normally you don't need to import `Logging` to create log events; for this
the standard logging macros such as `@info` are already exported by `Base`
and available by default.
"""
module Logging

# Import the CoreLogging implementation into Logging as new const bindings.
# Doing it this way (rather than with import) makes these symbols accessible to
# tab completion.
for sym in [
    :AbstractLogLevel, :LogLevel, :BelowMinLevel, :Debug, :Info, :Warn, :Error, :AboveMaxLevel,
    :AbstractLogger,
    :NullLogger,
    :handle_message, :severity, :shouldlog, :min_enabled_level, :catch_exceptions,
    Symbol("@debug"),
    Symbol("@info"),
    Symbol("@warn"),
    Symbol("@error"),
    Symbol("@logmsg"),
    :with_logger,
    :current_logger,
    :global_logger,
    :disable_logging,
    :SimpleLogger]
    @eval const $sym = Base.CoreLogging.$sym
end

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
    ConsoleLogger

include("ConsoleLogger.jl")

# The following are also part of the public API, but not exported:
#
# 1. Log levels:
#   BelowMinLevel, Debug, Info, Warn, Error, AboveMaxLevel,
#
# 2. AbstractLogger message related functions:
#  handle_message, shouldlog, min_enabled_level, catch_exceptions,

function __init__()
    global_logger(ConsoleLogger(stderr))
end


#--------------------------------------------------
# DEPRECATIONS
#
# In julia 1.4 the `severity` function was added to clarify the ordinal vs
# categorical nature of log levels. The following functions are deprecated,
# but left here until stdlibs can be versioned independently from Base.
Base.isless(a::LogLevel, b::LogLevel) = isless(severity(a), severity(b))
Base.:+(level::LogLevel, inc::Integer) = LogLevel(severity(level)+inc)
Base.:-(level::LogLevel, inc::Integer) = LogLevel(severity(level)-inc)
Base.convert(::Type{LogLevel}, level::Integer) = LogLevel(level)

# Backward compatible fallbacks for levels.
# `convert` used to be called by the macro-based lowering to support custom levels.
severity(level) = severity(convert(LogLevel, level))
# New lowering for global level override - fallback to use of convert
shouldlog(level) = shouldlog(convert(LogLevel, level))

end

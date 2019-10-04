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
    :handle_message, :default_importance, :shouldlog, :min_enabled_level, :catch_exceptions,
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

# In julia 1.4 the `importance` field was added. The following declarations are
# soft deprecations for backward compatibility with pre-1.4 logging backends.
handle_message(logger, importance, level, msg, _module, group, id, file, line; kws...) =
    handle_message(logger, level, msg, _module, group, id, file, line; importance=importance, kws...)
shouldlog(logger, importance, level, _module, group, id) =
    shouldlog(logger, level, _module, group, id)

#=
function Base.isless(a::LogLevel, b::LogLevel)
    Base.depwarn("isless(a::LogLevel, b::LogLevel) is deprecated", :isless)
    isless(a.level, b.level)
end

function Base.:+(level::LogLevel, inc::Integer)
    Base.depwarn("+(level::LogLevel, inc::Integer) is deprecated", :+)
    LogLevel(level.level+inc)
end

function Base.:-(level::LogLevel, inc::Integer)
    Base.depwarn("-(level::LogLevel, inc::Integer) is deprecated", :-)
    LogLevel(level.level-inc)
end

function Base.convert(::Type{LogLevel}, level::Integer)
    Base.depwarn("convert(::Type{LogLevel}, level::Integer) is deprecated", :convert)
    LogLevel(level)
end
=#

Base.isless(a::LogLevel, b::LogLevel) = isless(a.level, b.level)
Base.:+(level::LogLevel, inc::Integer) = LogLevel(level.level+inc)
Base.:-(level::LogLevel, inc::Integer) = LogLevel(level.level-inc)
Base.convert(::Type{LogLevel}, level::Integer) = LogLevel(level)

end

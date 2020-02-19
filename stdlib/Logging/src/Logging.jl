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
    :LogLevel, :BelowMinLevel, :Debug, :Info, :Warn, :Error, :AboveMaxLevel,
    :AbstractLogger,
    :NullLogger,
    :handle_message, :shouldlog, :min_enabled_level, :catch_exceptions,
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

end

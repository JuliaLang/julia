# This file is a part of Julia. License is MIT: https://julialang.org/license

__precompile__(true)

module Logging

# For now, simply import most names from Base - we don't want to fully
# stabilize this API for 1.0 so it should officially live here in a stdlib
# package.
#
# See #24490

import Base.CoreLogging:
    LogLevel, BelowMinLevel, Debug, Info, Warn, Error, AboveMaxLevel,
    AbstractLogger,
    NullLogger,
    handle_message, shouldlog, min_enabled_level, catch_exceptions,
    @debug,
    @info,
    @warn,
    @error,
    @logmsg,
    with_logger,
    current_logger,
    global_logger,
    disable_logging,
    SimpleLogger

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

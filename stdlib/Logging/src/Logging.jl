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
    :LogLevel,
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

# LogLevel aliases (re-)documented here (JuliaLang/julia#40978)
"""
    Debug

Alias for [`LogLevel(-1000)`](@ref LogLevel).
"""
const Debug = Base.CoreLogging.Debug
"""
    Info

Alias for [`LogLevel(0)`](@ref LogLevel).
"""
const Info = Base.CoreLogging.Info
"""
    Warn

Alias for [`LogLevel(1000)`](@ref LogLevel).
"""
const Warn = Base.CoreLogging.Warn
"""
    Error

Alias for [`LogLevel(2000)`](@ref LogLevel).
"""
const Error = Base.CoreLogging.Error
"""
    BelowMinLevel

Alias for [`LogLevel(-1_000_001)`](@ref LogLevel).
"""
const BelowMinLevel = Base.CoreLogging.BelowMinLevel
"""
    AboveMaxLevel

Alias for [`LogLevel(1_000_001)`](@ref LogLevel).
"""
const AboveMaxLevel = Base.CoreLogging.AboveMaxLevel

using Base.CoreLogging:
    closed_stream, ConsoleLogger, default_metafmt

# Some packages use `Logging.default_logcolor`
const default_logcolor = Base.CoreLogging.default_logcolor

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

# The following are also part of the public API, but not exported:
#
# 1. Log levels:
#   BelowMinLevel, Debug, Info, Warn, Error, AboveMaxLevel,
#
# 2. AbstractLogger message related functions:
#  handle_message, shouldlog, min_enabled_level, catch_exceptions,

end

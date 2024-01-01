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
    :LogLevel, :BelowMinLevel, :AboveMaxLevel,
    :AbstractLogger,
    :NullLogger,
    :handle_message, :shouldlog, :min_enabled_level, :catch_exceptions,
    Symbol("@debug"),
    Symbol("@info"),
    Symbol("@warn"),
    Symbol("@error"),
    Symbol("@logmsg"),
    :custom_log_levels,
    :with_logger,
    :current_logger,
    :global_logger,
    :disable_logging,
    :SimpleLogger]
    @eval const $sym = Base.CoreLogging.$sym
end

"""
    @create_log_macro(name::Symbol, level::Int, color::Union{Int,Symbol})

Creates a custom log macro like `@info`, `@warn` etc. with a given `name`, `level` and
`color`. The macro created is named with the lowercase form of `name` but the given form
is used for the printing.

The available color keys can be seen by typing `Base.text_colors` in the help mode of the REPL

```julia-repl
julia> @create_log_macro(:MyLog, 200, :magenta)
@mylog (macro with 1 method)

julia> @mylog "hello"
[ MyLog: hello
```
"""
macro create_log_macro(name, level, color)
    macro_name = Symbol(lowercase(string(name)))
    macro_string = QuoteNode(name)
    loglevel = LogLevel(level)
    if loglevel in (BelowMinLevel, Debug, Info, Warn, Error, AboveMaxLevel)
        throw(ArgumentError("Cannot use the same log level as a built in log macro"))
    end
    if haskey(custom_log_levels, loglevel)
        throw(ArgumentError("Custom log macro already exists for given log level"))
    end
    quote
        $(custom_log_levels)[$(esc(loglevel))] = ($(macro_string), $(esc(color)))
        macro $(esc(macro_name))(exs...)
            $(Base.CoreLogging.logmsg_code)(($(Base.CoreLogging.@_sourceinfo))..., $(esc(loglevel)), exs...)
        end
    end
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

using Base.CoreLogging:
    closed_stream

export
    AbstractLogger,
    LogLevel,
    NullLogger,
    @debug,
    @info,
    @warn,
    @error,
    @logmsg,
    @create_log_macro,
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

include("ConsoleLogger.jl")

# The following are also part of the public API, but not exported:
#
# 1. Log levels:
#   BelowMinLevel, Debug, Info, Warn, Error, AboveMaxLevel,
#
# 2. AbstractLogger message related functions:
#  handle_message, shouldlog, min_enabled_level, catch_exceptions,

function __init__()
    global_logger(ConsoleLogger())
end

end

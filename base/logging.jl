# This file is a part of Julia. License is MIT: https://julialang.org/license

module CoreLogging

import Base: print, show

export
    AbstractLogger,
    AbstractLogLevel,
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
    SimpleLogger

#-------------------------------------------------------------------------------
# The AbstractLogger interface
"""
A logger controls how log records are filtered and dispatched.  When a log
record is generated, the logger is the first piece of user configurable code
which gets to inspect the record and decide what to do with it.
"""
abstract type AbstractLogger ; end

"""
    handle_message(logger, level, message, _module, group, id, file, line; key1=val1, ...)

Log a message to `logger` at `level`.  The logical location at which the
message was generated is given by module `_module` and `group`; the source
location by `file` and `line`. `id` is an arbitrary unique value (typically a
[`Symbol`](@ref)) to be used as a key to identify the log statement when
filtering.
"""
function handle_message end

"""
    shouldlog(logger, level, _module, group, id)

Return true when `logger` accepts a message at `level`, generated for
`_module`, `group` and with unique log identifier `id`.

For very early filtering of custom log levels, users may override
`shouldlog(level)`.
"""
function shouldlog end

"""
    min_enabled_level(logger)

Return the minimum enabled severity (or log level) for `logger`. Log messages
with a severity less than this will be filtered out early in the log event
processing pipeline before `shouldlog` or `handle_message` is called.
"""
function min_enabled_level end

"""
    catch_exceptions(logger)

Return true if the logger should catch exceptions which happen during log
record construction.  By default, messages are caught

By default all exceptions are caught to prevent log message generation from
crashing the program.  This lets users confidently toggle little-used
functionality - such as debug logging - in a production system.

If you want to use logging as an audit trail you should disable this for your
logger type.
"""
catch_exceptions(logger) = true



"""
    NullLogger()

Logger which disables all messages and produces no output - the logger
equivalent of /dev/null.
"""
struct NullLogger <: AbstractLogger; end

min_enabled_level(::NullLogger) = severity(AboveMaxLevel)
shouldlog(::NullLogger, args...) = false
handle_message(::NullLogger, args...; kwargs...) =
    error("Null logger handle_message() should not be called")


#-------------------------------------------------------------------------------
# Standard log levels
"""
    AbstractLogLevel

A parent type for all log levels, including the standard `LogLevel` and user
defined levels.

User defined levels must implement the `severity` function to map the level to
an integer. Implementing `print` is also encouraged for producing a
human-readable textural form of the level.
"""
abstract type AbstractLogLevel ; end

"""
    severity(level)

Return an `Int` defining the severity of a log `level` to be used as the
primary log filtering mechanism.

Severity of user defined log levels should be relative to the standard log
levels which are defined to have `severity.([Debug, Info, Warn, Error]) ==
[-1000, 0, 1000, 2000]`.
"""
function severity
end

severity(sev::Integer) = convert(Int, sev)

"""
    LogLevel(level)

Severity/verbosity of a log record.

The log level provides a key against which potential log records may be
filtered, before any other work is done to construct the log record data
structure itself.
"""
struct LogLevel <: AbstractLogLevel
    level::Int32
end

LogLevel(level::LogLevel) = level

severity(level::LogLevel) = Int(level.level)

const BelowMinLevel = LogLevel(-1000001)
const Debug         = LogLevel(   -1000)
const Info          = LogLevel(       0)
const Warn          = LogLevel(    1000)
const Error         = LogLevel(    2000)
const AboveMaxLevel = LogLevel( 1000001)

function print(io::IO, level::LogLevel)
    if     level == BelowMinLevel  print(io, "BelowMinLevel")
    elseif level == Debug          print(io, "Debug")
    elseif level == Info           print(io, "Info")
    elseif level == Warn           print(io, "Warning")
    elseif level == Error          print(io, "Error")
    elseif level == AboveMaxLevel  print(io, "AboveMaxLevel")
    else                           print(io, "LogLevel($(level.level))")
    end
end

show(io::IO, level::LogLevel) = print(io, level == Warn ? "Warn" : level)

#-------------------------------------------------------------------------------
# Logging macros

_logmsg_docs = """
    @debug message  [key=value | value ...]
    @info  message  [key=value | value ...]
    @warn  message  [key=value | value ...]
    @error message  [key=value | value ...]

    @logmsg level message [key=value | value ...]

Create a log record with an informational `message`.  For convenience, four
logging macros `@debug`, `@info`, `@warn` and `@error` are defined which log at
the standard log levels `Debug`, `Info`, `Warn` and `Error`.  `@logmsg`
allows `level` to be set programmatically to any `LogLevel` or custom log level
types.

`message` should be an expression which evaluates to a string which is a human
readable description of the log event.  By convention, this string will be
formatted as markdown when presented.

The optional list of `key=value` pairs supports arbitrary user defined
metadata which will be passed through to the logging backend as part of the
log record.  If only a `value` expression is supplied, a key representing the
expression will be generated using [`Symbol`](@ref). For example, `x` becomes `x=x`,
and `foo(10)` becomes `Symbol("foo(10)")=foo(10)`.  For splatting a list of
key value pairs, use the normal splatting syntax, `@info "blah" kws...`.

There are some keys which allow automatically generated log data to be
overridden:

  * `_module=mod` can be used to specify a different originating module from
    the source location of the message.
  * `_group=symbol` can be used to override the message group (this is
    normally derived from the base name of the source file).
  * `_id=symbol` can be used to override the automatically generated unique
    message identifier.  This is useful if you need to very closely associate
    messages generated on different source lines.
  * `_file=string` and `_line=integer` can be used to override the apparent
    source location of a log message.

There's also some key value pairs which have conventional meaning:

  * `maxlog=integer` should be used as a hint to the backend that the message
    should be displayed no more than `maxlog` times.
  * `exception=ex` should be used to transport an exception with a log message,
    often used with `@error`. An associated backtrace `bt` may be attached
    using the tuple `exception=(ex,bt)`.

# Examples

```
@debug "Verbose debugging information.  Invisible by default"
@info  "An informational message"
@warn  "Something was odd.  You should pay attention"
@error "A non fatal error occurred"

x = 10
@info "Some variables attached to the message" x a=42.0

@debug begin
    sA = sum(A)
    "sum(A) = \$sA is an expensive operation, evaluated only when `shouldlog` returns true"
end

for i=1:10000
    @info "With the default backend, you will only see (i = \$i) ten times"  maxlog=10
    @debug "Algorithm1" i progress=i/10000
end
```
"""

# Get (module,filepath,line) for the location of the caller of a macro.
# Designed to be used from within the body of a macro.
macro _sourceinfo()
    esc(quote
        (__module__,
         __source__.file === nothing ? "?" : String(__source__.file::Symbol),
         __source__.line)
    end)
end

macro logmsg(level, exs...) logmsg_code((@_sourceinfo)..., esc(level), exs...) end
macro debug(exs...) logmsg_code((@_sourceinfo)..., :Debug, exs...) end
macro  info(exs...) logmsg_code((@_sourceinfo)..., :Info,  exs...) end
macro  warn(exs...) logmsg_code((@_sourceinfo)..., :Warn,  exs...) end
macro error(exs...) logmsg_code((@_sourceinfo)..., :Error, exs...) end

# Logging macros share documentation
@eval @doc $_logmsg_docs :(@logmsg)
@eval @doc $_logmsg_docs :(@debug)
@eval @doc $_logmsg_docs :(@info)
@eval @doc $_logmsg_docs :(@warn)
@eval @doc $_logmsg_docs :(@error)

_log_record_ids = Set{Symbol}()
# Generate a unique, stable, short, somewhat human readable identifier for a
# logging *statement*. The idea here is to have a key against which log events
# can be filtered and otherwise manipulated. The key should uniquely identify
# the source location in the originating module, but ideally should be stable
# across versions of the originating module, provided the log generating
# statement itself doesn't change.
function log_record_id(_module, level, message, log_kws)
    modname = _module === nothing ?  "" : join(fullname(_module), "_")
    # Use an arbitrarily chosen eight hex digits here. TODO: Figure out how to
    # make the id exactly the same on 32 and 64 bit systems.
    h = UInt32(hash(string(modname, level, message, log_kws)) & 0xFFFFFFFF)
    while true
        id = Symbol(modname, '_', string(h, base = 16, pad = 8))
        # _log_record_ids is a registry of log record ids for use during
        # compilation, to ensure uniqueness of ids.  Note that this state will
        # only persist during module compilation so it will be empty when a
        # precompiled module is loaded.
        if !(id in _log_record_ids)
            push!(_log_record_ids, id)
            return id
        end
        h += 1
    end
end

default_group(file) = Symbol(splitext(basename(file))[1])

# Generate code for logging macros
function logmsg_code(_module, file, line, level, message, exs...)
    id = Expr(:quote, log_record_id(_module, level, message, exs))
    group = nothing
    kwargs = Any[]
    for ex in exs
        if ex isa Expr && ex.head === :(=) && ex.args[1] isa Symbol
            k,v = ex.args
            if !(k isa Symbol)
                throw(ArgumentError("Expected symbol for key in key value pair `$ex`"))
            end
            k = ex.args[1]
            # Recognize several special keyword arguments
            if k === :_id
                # id may be overridden if you really want several log
                # statements to share the same id (eg, several pertaining to
                # the same progress step).  In those cases it may be wise to
                # manually call log_record_id to get a unique id in the same
                # format.
                id = esc(v)
            elseif k === :_module
                _module = esc(v)
            elseif k === :_line
                line = esc(v)
            elseif k === :_file
                file = esc(v)
            elseif k === :_group
                group = esc(v)
            else
                # Copy across key value pairs for structured log records
                push!(kwargs, Expr(:kw, k, esc(v)))
            end
        elseif ex isa Expr && ex.head === :...
            # Keyword splatting
            push!(kwargs, esc(ex))
        else
            # Positional arguments - will be converted to key value pairs
            # automatically.
            push!(kwargs, Expr(:kw, Symbol(ex), esc(ex)))
        end
    end

    if group === nothing
        group = if isdefined(Base, :basename) && isa(file, String)
            # precompute if we can
            QuoteNode(default_group(file))
        else
            # memoized run-time execution
            ref = Ref{Symbol}()
            :(isassigned($ref) ? $ref[]
                               : $ref[] = default_group(something($file, "")))
        end
    end

    quote
        level = $level
        if shouldlog(level)
            group = $group
            _module = $_module
            sev = severity(level)
            logger = current_logger_for_env(sev, group, _module)
            if !(logger === nothing)
                id = $id
                # Second chance at an early bail-out (before computing the message),
                # based on arbitrary logger-specific logic.
                if shouldlog(logger, level, _module, group, id)
                    file = $file
                    line = $line
                    try
                        msg = $(esc(message))
                        handle_message(logger, level, msg, _module, group, id, file, line; $(kwargs...))
                    catch err
                        logging_error(logger, level, _module, group, id, file, line, err)
                    end
                end
            end
        end
        nothing
    end
end

# Report an error in log message creation (or in the logger itself).
@noinline function logging_error(logger, level, _module, group, id,
                                 filepath, line, @nospecialize(err))
    if !catch_exceptions(logger)
        rethrow()
    end
    try
        msg = "Exception while generating log record in module $_module at $filepath:$line"
        handle_message(logger, Error, msg, _module, :logevent_error, id, filepath, line; exception=(err,catch_backtrace()))
    catch err2
        try
            # Give up and write to stderr, in three independent calls to
            # increase the odds of it getting through.
            print(stderr, "Exception handling log message: ")
            println(stderr, err)
            println(stderr, "  module=$_module  file=$filepath  line=$line")
            println(stderr, "  Second exception: ", err2)
        catch
        end
    end
    nothing
end

# Log a message. Called from the julia C code; kwargs is in the format
# Any[key1,val1, ...] for simplicity in construction on the C side.
function logmsg_shim(level, message, _module, group, id, file, line, kwargs)
    real_kws = Any[(kwargs[i],kwargs[i+1]) for i in 1:2:length(kwargs)]
    @logmsg(convert(LogLevel, level), message,
            _module=_module, _id=id, _group=group,
            _file=String(file), _line=line, real_kws...)
end

# Global log limiting mechanism for super fast but inflexible global log
# limiting.
const _min_enabled_severity = Ref{Int}(severity(Debug))

function shouldlog(level::AbstractLogLevel)
    severity(level) >= getindex(_min_enabled_severity)
end

# LogState - a concretely typed cache of data extracted from the logger, plus
# the logger itself.
struct LogState
    min_severity::Int
    logger::AbstractLogger
end

LogState(logger) = LogState(severity(min_enabled_level(logger)), logger)

function current_logstate()
    logstate = current_task().logstate
    return (logstate !== nothing ? logstate : _global_logstate)::LogState
end

# helper function to get the current logger, if enabled for the specified message type
@noinline function current_logger_for_env(sev::Int, group, _module)
    logstate = current_logstate()
    if sev >= logstate.min_severity || env_override_minlevel(group, _module)
        return logstate.logger
    end
    return nothing
end

function with_logstate(f::Function, logstate)
    t = current_task()
    old = t.logstate
    try
        t.logstate = logstate
        f()
    finally
        t.logstate = old
    end
end


#-------------------------------------------------------------------------------
# Control of the current logger and early log filtering

"""
    disable_logging(level)

Disable all log messages at log level severity equal to or less than
`severity(level)`. This is a *global* setting, intended to make debug logging
extremely cheap when disabled.
"""
disable_logging(level::AbstractLogLevel) = disable_logging(severity(level))
function disable_logging(level::Int)
    _min_enabled_severity[] = level + 1
end

let _debug_groups_include::Vector{Symbol} = Symbol[],
    _debug_groups_exclude::Vector{Symbol} = Symbol[],
    _debug_str::String = ""
global function env_override_minlevel(group, _module)
    debug = get(ENV, "JULIA_DEBUG", "")
    if !(debug === _debug_str)
        _debug_str = debug
        empty!(_debug_groups_include)
        empty!(_debug_groups_exclude)
        for g in split(debug, ',')
            if !isempty(g)
                if startswith(g, "!")
                    if !isempty(g[2:end])
                        push!(_debug_groups_exclude, Symbol(g[2:end]))
                    end
                else
                    push!(_debug_groups_include, Symbol(g))
                end
            end
        end
        unique!(_debug_groups_include)
        unique!(_debug_groups_exclude)
    end

    if !(:all in _debug_groups_exclude) && (:all in _debug_groups_include || !isempty(_debug_groups_exclude))
        if isempty(_debug_groups_exclude)
            return true
        elseif isa(group, Symbol) && group in _debug_groups_exclude
            return false
        elseif isa(_module, Module) && (nameof(_module) in _debug_groups_exclude || nameof(Base.moduleroot(_module)) in _debug_groups_exclude)
            return false
        else
            return true
        end
    else
        if isempty(_debug_groups_include)
            return false
        elseif isa(group, Symbol) && group in _debug_groups_include
            return true
        elseif isa(_module, Module) && (nameof(_module) in _debug_groups_include || nameof(Base.moduleroot(_module)) in _debug_groups_include)
            return true
        else
            return false
        end
    end
    return false
end
end


"""
    global_logger()

Return the global logger, used to receive messages when no specific logger
exists for the current task.

    global_logger(logger)

Set the global logger to `logger`, and return the previous global logger.
"""
global_logger() = _global_logstate.logger

function global_logger(logger::AbstractLogger)
    prev = _global_logstate.logger
    global _global_logstate = LogState(logger)
    prev
end

"""
    with_logger(function, logger)

Execute `function`, directing all log messages to `logger`.

# Example

```julia
function test(x)
    @info "x = \$x"
end

with_logger(logger) do
    test(1)
    test([1,2])
end
```
"""
with_logger(f::Function, logger::AbstractLogger) = with_logstate(f, LogState(logger))

"""
    current_logger()

Return the logger for the current task, or the global logger if none is
attached to the task.
"""
current_logger() = current_logstate().logger


#-------------------------------------------------------------------------------
# SimpleLogger
"""
    SimpleLogger(stream=stderr, min_severity=Info)

Simplistic logger for logging all messages with level greater than or equal to
`min_severity` to `stream`.
"""
struct SimpleLogger <: AbstractLogger
    stream::IO
    min_severity::Int
    message_limits::Dict{Any,Int}
end
function SimpleLogger(stream::IO=stderr, min_severity=Info)
    SimpleLogger(stream, severity(min_severity), Dict{Any,Int}())
end

shouldlog(logger::SimpleLogger, level, _module, group, id) =
    get(logger.message_limits, id, 1) > 0

min_enabled_level(logger::SimpleLogger) = logger.min_severity

catch_exceptions(logger::SimpleLogger) = false

function handle_message(logger::SimpleLogger, level, message, _module, group, id,
                        filepath, line; maxlog=nothing, kwargs...)
    if maxlog !== nothing && maxlog isa Integer
        remaining = get!(logger.message_limits, id, maxlog)
        logger.message_limits[id] = remaining - 1
        remaining > 0 || return
    end
    buf = IOBuffer()
    iob = IOContext(buf, logger.stream)
    msglines = split(chomp(string(message)), '\n')
    println(iob, "┌ ", level, ": ", msglines[1])
    for i in 2:length(msglines)
        println(iob, "│ ", msglines[i])
    end
    for (key, val) in kwargs
        println(iob, "│   ", key, " = ", val)
    end
    println(iob, "└ @ ", something(_module, "nothing"), " ",
            something(filepath, "nothing"), ":", something(line, "nothing"))
    write(logger.stream, take!(buf))
    nothing
end

_global_logstate = LogState(SimpleLogger(Core.stderr, CoreLogging.Info))

end # CoreLogging

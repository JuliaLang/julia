# This file is a part of Julia. License is MIT: https://julialang.org/license

module CoreLogging

import Base: isless, +, -, convert, show
import Base.ScopedValues: ScopedValue, with, @with

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

Return `true` when `logger` accepts a message at `level`, generated for
`_module`, `group` and with unique log identifier `id`.
"""
function shouldlog end

"""
    min_enabled_level(logger)

Return the minimum enabled level for `logger` for early filtering.  That is,
the log level below or equal to which all messages are filtered.
"""
function min_enabled_level end

"""
    catch_exceptions(logger)

Return `true` if the logger should catch exceptions which happen during log
record construction.  By default, messages are caught.

By default all exceptions are caught to prevent log message generation from
crashing the program.  This lets users confidently toggle little-used
functionality - such as debug logging - in a production system.

If you want to use logging as an audit trail you should disable this for your
logger type.
"""
catch_exceptions(logger) = true


# Prevent invalidation when packages define custom loggers
# Using invoke in combination with @nospecialize eliminates backedges to these methods
Base.@constprop :none function _invoked_shouldlog(logger, level, _module, group, id)
    @nospecialize
    return invoke(
        shouldlog,
        Tuple{typeof(logger), typeof(level), typeof(_module), typeof(group), typeof(id)},
        logger, level, _module, group, id
    )::Bool
end

function _invoked_min_enabled_level(@nospecialize(logger))
    return invoke(min_enabled_level, Tuple{typeof(logger)}, logger)::LogLevel
end

function _invoked_catch_exceptions(@nospecialize(logger))
    return invoke(catch_exceptions, Tuple{typeof(logger)}, logger)::Bool
end

"""
    NullLogger()

Logger which disables all messages and produces no output - the logger
equivalent of /dev/null.
"""
struct NullLogger <: AbstractLogger; end

min_enabled_level(::NullLogger) = AboveMaxLevel
shouldlog(::NullLogger, args...) = false
handle_message(::NullLogger, args...; kwargs...) =
    (@nospecialize; error("Null logger handle_message() should not be called"))


#-------------------------------------------------------------------------------
# Standard log levels
"""
    LogLevel(level)

Severity/verbosity of a log record.

The log level provides a key against which potential log records may be
filtered, before any other work is done to construct the log record data
structure itself.

# Examples
```jldoctest; setup = :(import Logging)
julia> Logging.LogLevel(0) == Logging.Info
true
```
"""
struct LogLevel
    level::Int32
end

LogLevel(level::LogLevel) = level

isless(a::LogLevel, b::LogLevel) = isless(a.level, b.level)
+(level::LogLevel, inc::Integer) = LogLevel(level.level+inc)
-(level::LogLevel, inc::Integer) = LogLevel(level.level-inc)
convert(::Type{LogLevel}, level::Integer) = LogLevel(level)
convert(::Type{Int32}, level::LogLevel) = level.level

"""
    BelowMinLevel

Alias for [`LogLevel(-1_000_001)`](@ref LogLevel).
"""
const BelowMinLevel = LogLevel(-1000001)
"""
    Debug

Alias for [`LogLevel(-1000)`](@ref LogLevel).
"""
const Debug         = LogLevel(   -1000)
"""
    Info

Alias for [`LogLevel(0)`](@ref LogLevel).
"""
const Info          = LogLevel(       0)
"""
    Warn

Alias for [`LogLevel(1000)`](@ref LogLevel).
"""
const Warn          = LogLevel(    1000)
"""
    Error

Alias for [`LogLevel(2000)`](@ref LogLevel).
"""
const Error         = LogLevel(    2000)
"""
    AboveMaxLevel

Alias for [`LogLevel(1_000_001)`](@ref LogLevel).
"""
const AboveMaxLevel = LogLevel( 1000001)

# Global log limiting mechanism for super fast but inflexible global log limiting.
# Atomic ensures that the value is always consistent across threads.
const _min_enabled_level = Threads.Atomic{Int32}(Debug)

function show(io::IO, level::LogLevel)
    if     level == BelowMinLevel  print(io, "BelowMinLevel")
    elseif level == Debug          print(io, "Debug")
    elseif level == Info           print(io, "Info")
    elseif level == Warn           print(io, "Warn")
    elseif level == Error          print(io, "Error")
    elseif level == AboveMaxLevel  print(io, "AboveMaxLevel")
    else                           print(io, "LogLevel($(level.level))")
    end
end


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
the standard severity levels `Debug`, `Info`, `Warn` and `Error`.  `@logmsg`
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

```julia
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
    @nospecialize
    modname = _module === nothing ?  "" : join(fullname(_module), "_")
    # Use an arbitrarily chosen eight hex digits here. TODO: Figure out how to
    # make the id exactly the same on 32 and 64 bit systems.
    h = UInt32(hash(string(modname, level, message, log_kws)::String) & 0xFFFFFFFF)
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

function issimple(@nospecialize val)
    val isa String && return true
    val isa Symbol && return true
    val isa QuoteNode && return true
    val isa Number && return true
    val isa Char && return true
    if val isa Expr
        val.head === :quote && issimple(val.args[1]) && return true
        val.head === :inert && return true
    end
    return false
end
function issimplekw(@nospecialize val)
    if val isa Expr
        if val.head === :kw
            val = val.args[2]
            if val isa Expr && val.head === :escape
                issimple(val.args[1]) && return true
            end
        end
    end
    return false
end

# helper function to get the current logger, if enabled for the specified message type
@noinline Base.@constprop :none function current_logger_for_env(std_level::LogLevel, group, _module)
    logstate = @inline current_logstate()
    if std_level >= logstate.min_enabled_level || env_override_minlevel(group, _module)
        return logstate.logger
    end
    return nothing
end

# Generate code for logging macros
function logmsg_code(_module, file, line, level, message, exs...)
    @nospecialize
    log_data = process_logmsg_exs(_module, file, line, level, message, exs...)
    if !isa(message, Symbol) && issimple(message) && isempty(log_data.kwargs)
        logrecord = quote
            msg = $(message)
            kwargs = (;)
            true
        end
    elseif issimple(message) && all(issimplekw, log_data.kwargs)
        # if message and kwargs are just values and variables, we can avoid try/catch
        # complexity by adding the code for testing the UndefVarError by hand
        checkerrors = nothing
        for kwarg in reverse(log_data.kwargs)
            if isa(kwarg.args[2].args[1], Symbol)
                checkerrors = Expr(:if, Expr(:isdefined, kwarg.args[2]), checkerrors, Expr(:call, Expr(:core, :UndefVarError), QuoteNode(kwarg.args[2].args[1]), QuoteNode(:local)))
            end
        end
        if isa(message, Symbol)
            message = esc(message)
            checkerrors = Expr(:if, Expr(:isdefined, message), checkerrors, Expr(:call, Expr(:core, :UndefVarError), QuoteNode(message.args[1]), QuoteNode(:local)))
        end
        logrecord = quote
            let err = $checkerrors
                if err === nothing
                    msg = $(message)
                    kwargs = (;$(log_data.kwargs...))
                    true
                else
                    @invokelatest $(logging_error)(logger, level, _module, group, id, file, line, err, false)
                    false
                end
            end
        end
    else
        logrecord = quote
            try
                msg = $(esc(message))
                kwargs = (;$(log_data.kwargs...))
                true
            catch err
                @invokelatest $(logging_error)(logger, level, _module, group, id, file, line, err, true)
                false
            end
        end
    end
    return quote
        let
            level = $level
            # simplify std_level code emitted, if we know it is one of our global constants
            std_level = $(level isa Symbol ? :level : :(level isa $LogLevel ? level : convert($LogLevel, level)::$LogLevel))
            if std_level.level >= $(_min_enabled_level)[]
                group = $(log_data._group)
                _module = $(log_data._module)
                logger = $(current_logger_for_env)(std_level, group, _module)
                if !(logger === nothing)
                    id = $(log_data._id)
                    # Second chance at an early bail-out (before computing the message),
                    # based on arbitrary logger-specific logic.
                    if invokelatest($shouldlog, logger, level, _module, group, id)
                        file = $(log_data._file)
                        if file isa String
                            file = Base.fixup_stdlib_path(file)
                        end
                        line = $(log_data._line)
                        local msg, kwargs
                        $(logrecord) && $handle_message_nothrow(
                            logger, level, msg, _module, group, id, file, line;
                            kwargs...)
                    end
                end
            end
            nothing
        end
    end
end

@noinline function handle_message_nothrow(logger, level, msg, _module, group, id, file, line; kwargs...)
    @nospecialize
    try
        @invokelatest handle_message(
            logger, level, msg, _module, group, id, file, line;
            kwargs...)

    catch err
        @invokelatest logging_error(logger, level, _module, group, id, file, line, err, true)
    end
end

function process_logmsg_exs(_orig_module, _file, _line, level, message, exs...)
    @nospecialize
    local _group, _id
    _module = _orig_module
    kwargs = Any[]
    for ex in exs
        if ex isa Expr && ex.head === :(=)
            k, v = ex.args
            if !(k isa Symbol)
                k = Symbol(k)
            end

            # Recognize several special keyword arguments
            if k === :_group
                _group = esc(v)
            elseif k === :_id
                _id = esc(v)
            elseif k === :_module
                _module = esc(v)
            elseif k === :_file
                _file = esc(v)
            elseif k === :_line
                _line = esc(v)
            else
                # Copy across key value pairs for structured log records
                push!(kwargs, Expr(:kw, k, esc(v)))
            end
        elseif ex isa Expr && ex.head === :... # Keyword splatting
            push!(kwargs, esc(ex))
        else # Positional arguments - will be converted to key value pairs automatically.
            push!(kwargs, Expr(:kw, Symbol(ex), esc(ex)))
        end
    end

    if !@isdefined(_group)
        _group = default_group_code(_file)
    end
    if !@isdefined(_id)
        _id = Expr(:quote, log_record_id(_orig_module, level, message, exs))
    end
    return (;_module, _group, _id, _file, _line, kwargs)
end

function default_group_code(file)
    @nospecialize
    if file isa String && isdefined(Base, :basename)
        QuoteNode(default_group(file))  # precompute if we can
    else
        ref = Ref{Symbol}()  # memoized run-time execution
        :(isassigned($ref) ? $ref[] : $ref[] = default_group(something($file, ""))::Symbol)
    end
end


# Report an error in log message creation
@noinline function logging_error(logger, level, _module, group, id,
                                 filepath, line, @nospecialize(err), real::Bool)
    @nospecialize
    if !_invoked_catch_exceptions(logger)
        real ? rethrow(err) : throw(err)
    end
    msg = try
              "Exception while generating log record in module $_module at $filepath:$line"
          catch ex
              LazyString("Exception handling log message: ", ex)
          end
    bt = real ? catch_backtrace() : backtrace()
    handle_message(
        logger, Error, msg, _module, :logevent_error, id, filepath, line;
        exception=(err,bt))
    nothing
end

# Log a message. Called from the julia C code; kwargs is in the format
# Any[key1,val1, ...] for simplicity in construction on the C side.
function logmsg_shim(level, message, _module, group, id, file, line, kwargs)
    @nospecialize
    real_kws = Any[(kwargs[i], kwargs[i+1]) for i in 1:2:length(kwargs)]
    @logmsg(convert(LogLevel, level), message,
            _module=_module, _id=id, _group=group,
            _file=String(file), _line=line, real_kws...)
    nothing
end

# LogState - a cache of data extracted from the logger, plus the logger itself.
struct LogState
    min_enabled_level::LogLevel
    logger::AbstractLogger
end

LogState(logger) = LogState(LogLevel(_invoked_min_enabled_level(logger)), logger)

const CURRENT_LOGSTATE = ScopedValue{LogState}()

function current_logstate()
    maybe = @inline Base.ScopedValues.get(CURRENT_LOGSTATE)
    return something(maybe, _global_logstate)::LogState
end

with_logstate(f::Function, logstate) = @with(CURRENT_LOGSTATE => logstate, f())

#-------------------------------------------------------------------------------
# Control of the current logger and early log filtering

"""
    disable_logging(level)

Disable all log messages at log levels equal to or less than `level`.  This is
a *global* setting, intended to make debug logging extremely cheap when
disabled. Note that this cannot be used to enable logging that is currently disabled
by other mechanisms.

# Examples
```julia
Logging.disable_logging(Logging.Info) # Disable debug and info
```
"""
function disable_logging(level::LogLevel)
    _min_enabled_level[] = level + 1
end

let _debug_groups_include::Vector{Symbol} = Symbol[],
    _debug_groups_exclude::Vector{Symbol} = Symbol[],
    _debug_str::String = ""
global Base.@constprop :none function env_override_minlevel(group, _module)
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


global _global_logstate::LogState

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

# Examples

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
function with_logger(@nospecialize(f::Function), logger::AbstractLogger)
    with_logstate(f, LogState(logger))
end

"""
    current_logger()

Return the logger for the current task, or the global logger if none is
attached to the task.
"""
current_logger() = current_logstate().logger

const closed_stream = IOBuffer(UInt8[])
close(closed_stream)

#-------------------------------------------------------------------------------
# SimpleLogger
"""
    SimpleLogger([stream,] min_level=Info)

Simplistic logger for logging all messages with level greater than or equal to
`min_level` to `stream`. If stream is closed then messages with log level
greater or equal to `Warn` will be logged to `stderr` and below to `stdout`.

This Logger is thread-safe, with a lock taken around orchestration of message
limits i.e. `maxlog`, and writes to the stream.
"""
struct SimpleLogger <: AbstractLogger
    stream::IO
    lock::ReentrantLock
    min_level::LogLevel
    message_limits::Dict{Any,Int}
end
SimpleLogger(stream::IO, level=Info) = SimpleLogger(stream, ReentrantLock(), level, Dict{Any,Int}())
SimpleLogger(level=Info) = SimpleLogger(closed_stream, level)

shouldlog(logger::SimpleLogger, level, _module, group, id) =
    @lock logger.lock get(logger.message_limits, id, 1) > 0

min_enabled_level(logger::SimpleLogger) = logger.min_level

catch_exceptions(logger::SimpleLogger) = false

function handle_message(logger::SimpleLogger, level::LogLevel, message, _module, group, id,
                        filepath, line; kwargs...)
    @nospecialize
    maxlog = get(kwargs, :maxlog, nothing)
    if maxlog isa Core.BuiltinInts
        @lock logger.lock begin
            remaining = get!(logger.message_limits, id, Int(maxlog)::Int)
            remaining == 0 && return
            logger.message_limits[id] = remaining - 1
        end
    end
    buf = IOBuffer()
    stream::IO = logger.stream
    iob = IOContext(buf, stream)
    levelstr = level == Warn ? "Warning" : string(level)
    msglines = eachsplit(chomp(convert(String, string(message))::String), '\n')
    msg1, rest = Iterators.peel(msglines)
    println(iob, "┌ ", levelstr, ": ", msg1)
    for msg in rest
        println(iob, "│ ", msg)
    end
    for (key, val) in kwargs
        key === :maxlog && continue
        println(iob, "│   ", key, " = ", val)
    end
    println(iob, "└ @ ", _module, " ", filepath, ":", line)
    b = take!(buf)
    @lock logger.lock begin
        if !(isopen(stream)::Bool)
            stream = stderr
        end
        write(stream, b)
    end
    nothing
end

_global_logstate = LogState(SimpleLogger())

include("logging/ConsoleLogger.jl")

end # CoreLogging

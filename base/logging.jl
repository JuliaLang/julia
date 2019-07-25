# This file is a part of Julia. License is MIT: https://julialang.org/license

module CoreLogging

import Base: isless, +, -, convert, show

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
location by `file` and `line`. `id` is an arbitrary unique [`Symbol`](@ref) to be used
as a key to identify the log statement when filtering.
"""
function handle_message end

"""
    shouldlog(logger, level, _module, group, id)

Return true when `logger` accepts a message at `level`, generated for
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

min_enabled_level(::NullLogger) = AboveMaxLevel
shouldlog(::NullLogger, args...) = false
handle_message(::NullLogger, args...; kwargs...) =
    error("Null logger handle_message() should not be called")


#-------------------------------------------------------------------------------
# Standard log levels
"""
    LogLevel(level)

Severity/verbosity of a log record.

The log level provides a key against which potential log records may be
filtered, before any other work is done to construct the log record data
structure itself.
"""
struct LogLevel
    level::Int32
end

LogLevel(level::LogLevel) = level

isless(a::LogLevel, b::LogLevel) = isless(a.level, b.level)
+(level::LogLevel, inc::Integer) = LogLevel(level.level+inc)
-(level::LogLevel, inc::Integer) = LogLevel(level.level-inc)
convert(::Type{LogLevel}, level::Integer) = LogLevel(level)

const BelowMinLevel = LogLevel(-1000001)
const Debug         = LogLevel(   -1000)
const Info          = LogLevel(       0)
const Warn          = LogLevel(    1000)
const Error         = LogLevel(    2000)
const AboveMaxLevel = LogLevel( 1000001)

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
         __source__.file === nothing ? "?" : String(__source__.file),
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
    # Use an arbitriraly chosen eight hex digits here. TODO: Figure out how to
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
            if k == :_id
                # id may be overridden if you really want several log
                # statements to share the same id (eg, several pertaining to
                # the same progress step).  In those cases it may be wise to
                # manually call log_record_id to get a unique id in the same
                # format.
                id = esc(v)
            elseif k == :_module
                _module = esc(v)
            elseif k == :_line
                line = esc(v)
            elseif k == :_file
                file = esc(v)
            elseif k == :_group
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
            QuoteNode(splitext(basename(file))[1])
        else
            # memoized run-time execution
            ref = Ref{Symbol}()
            :(isassigned($ref) ? $ref[]
                               : $ref[] = Symbol(splitext(basename(something($file, "")))[1]))
        end
    end

    quote
        level = $level
        std_level = convert(LogLevel, level)
        if std_level >= getindex(_min_enabled_level)
            group = $group
            _module = $_module
            logger = current_logger_for_env(std_level, group, _module)
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
        rethrow(err)
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
const _min_enabled_level = Ref(Debug)

# LogState - a concretely typed cache of data extracted from the logger, plus
# the logger itself.
struct LogState
    min_enabled_level::LogLevel
    logger::AbstractLogger
end

LogState(logger) = LogState(LogLevel(min_enabled_level(logger)), logger)

function current_logstate()
    logstate = current_task().logstate
    return (logstate !== nothing ? logstate : _global_logstate)::LogState
end

# helper function to get the current logger, if enabled for the specified message type
@noinline function current_logger_for_env(std_level::LogLevel, group, _module)
    logstate = current_logstate()
    if std_level >= logstate.min_enabled_level || env_override_minlevel(group, _module)
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

Disable all log messages at log levels equal to or less than `level`.  This is
a *global* setting, intended to make debug logging extremely cheap when
disabled.
"""
function disable_logging(level::LogLevel)
    _min_enabled_level[] = level + 1
end

let _debug_groups = Symbol[],
    _debug_str::String = ""
global function env_override_minlevel(group, _module)
    debug = get(ENV, "JULIA_DEBUG", "")
    if !(debug === _debug_str)
        _debug_str = debug
        empty!(_debug_groups)
        for g in split(debug, ',')
            isempty(g) && continue
            if g == "all"
                empty!(_debug_groups)
                push!(_debug_groups, :all)
                break
            end
            push!(_debug_groups, Symbol(g))
        end
    end
    if isempty(_debug_groups)
        return false
    end
    if _debug_groups[1] == :all
        return true
    end
    if isa(group, Symbol) && group in _debug_groups
        return true
    end
    if isa(_module, Module)
        if nameof(_module) in _debug_groups
            return true
        end
        if nameof(Base.moduleroot(_module)) in _debug_groups
            return true
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
    SimpleLogger(stream=stderr, min_level=Info)

Simplistic logger for logging all messages with level greater than or equal to
`min_level` to `stream`.
"""
struct SimpleLogger <: AbstractLogger
    stream::IO
    min_level::LogLevel
    message_limits::Dict{Any,Int}
end
SimpleLogger(stream::IO=stderr, level=Info) = SimpleLogger(stream, level, Dict{Any,Int}())

shouldlog(logger::SimpleLogger, level, _module, group, id) =
    get(logger.message_limits, id, 1) > 0

min_enabled_level(logger::SimpleLogger) = logger.min_level

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
    levelstr = level == Warn ? "Warning" : string(level)
    msglines = split(chomp(string(message)), '\n')
    println(iob, "┌ ", levelstr, ": ", msglines[1])
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

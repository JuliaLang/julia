module Logging

import Base.Meta: isexpr

export
    # Frontend
    @debug, @info, @warn, @error, @logmsg,
    # Log control
    with_logger, current_logger, global_logger,
    disable_logging, configure_logging,
    # Logger type
    AbstractLogger,
    # Logger methods
    handle_message, shouldlog, min_enabled_level,
    # Loggers
    NullLogger, SimpleLogger

"""
A logger controls how log records are filtered and dispatched.  When a log
record is generated, the logger is the first piece of user configurable code
which gets to inspect the record and decide what to do with it.
"""
abstract type AbstractLogger ; end


"""
Severity/verbosity of a log record.

The log level provides a key against which potential log records may be
filtered, before any other work is done to construct the log record data
structure itself.
"""
struct LogLevel
    level::Int32
end

LogLevel(level::LogLevel) = level

Base.isless(a::LogLevel, b::LogLevel) = isless(a.level, b.level)
Base.:+(level::LogLevel, inc) = LogLevel(level.level+inc)
Base.:-(level::LogLevel, inc) = LogLevel(level.level-inc)

const BelowMinLevel = LogLevel(typemin(Int32))
const Debug         = LogLevel(-1000)
const Info          = LogLevel(0)
const Warn          = LogLevel(1000)
const Error         = LogLevel(2000)
const AboveMaxLevel = LogLevel(typemax(Int32))

function Base.show(io::IO, level::LogLevel)
    if     level == BelowMinLevel  print(io, "BelowMinLevel")
    elseif level == Debug          print(io, "Debug")
    elseif level == Info           print(io, "Info")
    elseif level == Warn           print(io, "Warn")
    elseif level == Error          print(io, "Error")
    elseif level == AboveMaxLevel  print(io, "AboveMaxLevel")
    else                           print(io, "LogLevel($(level.level))")
    end
end

parse_level(level) = level
parse_level(level::String) = parse_level(Symbol(lowercase(level)))
function parse_level(level::Symbol)
    if      level == :belowminlevel  return  BelowMinLevel
    elseif  level == :debug          return  Debug
    elseif  level == :info           return  Info
    elseif  level == :warn           return  Warn
    elseif  level == :error          return  Error
    elseif  level == :abovemaxlevel  return  AboveMaxLevel
    else
        throw(ArgumentError("Unknown log level $level"))
    end
end


# Global log limiting mechanism for super fast but inflexible global log
# limiting.
const _min_enabled_level = Ref(Debug)


# A concretely typed cache of data extracted from the logger, plus the logger
# itself.
struct LogState
    min_enabled_level::LogLevel
    logger::AbstractLogger
end

LogState(logger) = LogState(LogLevel(min_enabled_level(logger)), logger)


#-------------------------------------------------------------------------------
# Logging macros and frontend

# Registry of log record ids for use during compilation, to ensure uniqueness
# of ids.  Note that this state will only persist during module compilation
# so it will be empty when a precompiled module is loaded.
_log_record_ids = Set{Symbol}()

# Generate a unique, stable, short, human readable identifier for a logging
# statement.  The idea here is to have a key against which log records can be
# filtered and otherwise manipulated. The key should uniquely identify the
# source location in the originating module, but should be stable across
# versions of the originating module, provided the log generating statement
# itself doesn't change.
function log_record_id(_module, level, message_ex)
    modname = join(fullname(_module), "_")
    # Use (1<<31) to fit well within an (arbitriraly chosen) eight hex digits,
    # as we increment h to resolve any collisions.
    h = hash(string(modname, level, message_ex)) % (1<<31)
    while true
        id = Symbol(string(modname, '_', hex(h, 8)))
        if !(id in _log_record_ids)
            push!(_log_record_ids, id)
            return id
        end
        h += 1
    end
end

# Generate code for @logmsg
function logmsg_code(_module, file, line, level, message, exs...)
    # Generate a unique message id by default
    messagetemplate = string(message)
    id = Expr(:quote, log_record_id(_module, level, messagetemplate))
    group = Expr(:quote, Symbol(splitext(basename(file))[1]))
    kwargs = Any[]
    for ex in exs
        if isexpr(ex, :(=)) && isa(ex.args[1], Symbol)
            k,v = ex.args
            if !(k isa Symbol)
                throw(ArgumentError("Expected symbol for key in key value pair `$ex`"))
            end
            k = ex.args[1]
            # Recognize several special keyword arguments
            if k == :_id
                # id may be overridden if you really want several log
                # statements to share the same id (eg, several pertaining to
                # the same progress step).
                #
                # TODO: Refine this - doing it as is, is probably a bad idea
                # for consistency, and is hard to make unique between modules.
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
                v = esc(v)
                # Copy across key value pairs for structured log records
                push!(kwargs, Expr(:kw, k, v))
            end
        #=
        # FIXME - decide whether this special syntax is a good idea.
        # It's probably only a good idea if we decide to pass these as keyword
        # arguments to shouldlog()
        elseif ex.head == :vect
            # Match "log control" keyword argument syntax, eg
            # @info "Foo" [once=true]
            for keyval in ex.args
                if !isa(keyval, Expr) || keyval.head != :(=) || !isa(keyval.args[1], Symbol)
                    throw(ArgumentError("Expected key value pair inside log control, got $keyval"))
                end
                k,v = keyval.args
                if k == :max_log
                    max_log = v
                elseif k == :id
                    if !isa(v, Symbol)
                        throw(ArgumentError("id should be a symbol"))
                    end
                    id = Expr(:quote, v)
                elseif k == :progress
                    progress = v
                    push!(kwargs, Expr(:kw, k, v))
                else
                    throw(ArgumentError("Unknown log control $k"))
                end
            end
        =#
        else
            # Positional arguments - will be converted to key value pairs
            # automatically.
            push!(kwargs, Expr(:kw, Symbol(ex), esc(ex)))
        end
    end
    quote
        level = $level
        std_level = convert(LogLevel, level)
        if std_level >= _min_enabled_level[]
            logstate = current_logstate()
            if std_level >= logstate.min_enabled_level
                logger = logstate.logger
                _module = $_module
                id = $id
                group = $group
                # Second chance at an early bail-out, based on arbitrary
                # logger-specific logic.
                if shouldlog(logger, level, _module, group, id)
                    # Bind log record generation into a closure, allowing us to
                    # defer creation of the records until after filtering.
                    create_msg = function cm(logger, level, _module, group, id, file, line)
                        msg = $(esc(message))
                        handle_message(logger, level, msg, _module, group, id, file, line; $(kwargs...))
                    end
                    file = $file
                    line = $line
                    dispatch_message(logger, level, _module, group, id, file, line, create_msg)
                end
            end
        end
        nothing
    end
end

# Get (module,filepath,line) for the location of the caller of a macro.
# Designed to be used from within the body of a macro.
macro sourceinfo()
    esc(quote
        (__module__,
         __source__.file == nothing ? "?" : String(__source__.file),
         __source__.line)
    end)
end


_macro_docs = """
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

`message` can be any type.  You should generally ensure that `string(message)`
gives useful information, but custom logger backends may serialize the message
in a more useful way in general.

The optional list of `key=value` pairs supports arbitrary user defined
metadata which will be passed through to the logging backend as part of the
log record.  If only a `value` expression is supplied, a key will be generated
using `Symbol`. For example, `x` becomes `x=x`, and `foo(10)` becomes
`Symbol("foo(10)")=foo(10)`.

By convention, there are some keys and values which will be interpreted in a
special way:

  * `progress=fraction` should be used to indicate progress through an
    algorithmic step named by `message`, it should be a value in the interval
    [0,1], and would generally be used to drive a progress bar or meter.
  * `max_log=integer` should be used as a hint to the backend that the message
    should be displayed no more than `max_log` times.
  * `id=:symbol` can be used to override the unique message identifier.  This
    is useful if you need to very closely associate messages generated in
    different invocations of `@logmsg`.
  * `file=string` and `line=integer` can be used to override the apparent
    source location of a log message.  Generally, this is not encouraged.


# Examples

```
@debug "Verbose degging information.  Invisible by default"
@info  "An informational message"
@warn  "Something was odd.  You should pay attention"
@error "A non fatal error occurred"

@debug begin
    sA = sum(A)
    "sum(A) = \$sA is an expensive operation, evaluated only when `shouldlog` returns true"
end

for i=1:10000
    @info "With the default backend, you will only see (i = \$i) ten times"  max_log=10
    @debug "Algorithm1" i progress=i/10000
end

level = Info
a = 100
@logmsg level "Some message with attached values" a foo(2)
```
"""

macro logmsg(level, message, exs...) logmsg_code((@sourceinfo)..., esc(level), message, exs...) end
macro debug(message, exs...) logmsg_code((@sourceinfo)..., :Debug, message, exs...) end
macro  info(message, exs...) logmsg_code((@sourceinfo)..., :Info,  message, exs...) end
macro  warn(message, exs...) logmsg_code((@sourceinfo)..., :Warn,  message, exs...) end
macro error(message, exs...) logmsg_code((@sourceinfo)..., :Error, message, exs...) end

# Logging macros share the same documentation
@eval @doc $_macro_docs :(@logmsg)
@eval @doc $_macro_docs :(@debug)
@eval @doc $_macro_docs :(@info)
@eval @doc $_macro_docs :(@warn)
@eval @doc $_macro_docs :(@error)


"""
    handle_message(logger, level, message, _module, group, id, file, line; key1=val1, ...)

Log a message to `logger` at `level`.  The logical location at which the
message was generated is given by module `_module` and `group`; the source
location by `file` and `line`. `id` is an arbitrary unique `Symbol` to be used
as a key to identify the log statement when filtering.
"""
function handle_message end


"""
    shouldlog(logger, level, _module, group, id)

Return true when `logger` accepts a message at `level`, generated for
`_module`, `group` and with unique log identifier `id`.
"""
shouldlog(::AbstractLogger, args...) = true


"""
    min_enabled_level(logger)

Return the maximum disabled level for `logger` for early filtering.  That is,
the log level below or equal to which all messages are filtered.
"""
min_enabled_level(logger::AbstractLogger) = Info


function dispatch_message(logger, level, _module, group, id, filepath, line, create_msg)
    # Catch all exceptions, to prevent log message generation from crashing
    # the program.  This lets users confidently toggle little-used
    # functionality - such as debug logging - in a production system.
    #
    # Users need to override and disable this if they want to use logging
    # as an audit trail.
    try
        create_msg(logger, level, _module, group, id, filepath, line)
    catch err
        # Try really hard to get the message to the logger, with
        # progressively less information.
        try
            msg = "Exception while generating log record in module $_module at $filepath:$line"
            handle_message(logger, Error, msg, _module, group, id, filepath, line; exception=err)
        catch err2
            try
                # Give up and write to STDERR, in three independent calls to
                # increase the odds of it getting through.
                print(STDERR, "Exception handling log message: ")
                println(STDERR, err)
                println(STDERR, "  module=$_module  file=$filepath  line=$line")
                println(STDERR, "  Second exception: ", err2)
            catch
            end
        end
    end
    nothing
end


#-------------------------------------------------------------------------------
# Logger implementations
"""
    NullLogger()

Logger which disables all messages and produces no output
"""
struct NullLogger <: AbstractLogger; end

min_enabled_level(::NullLogger) = AboveMaxLevel
shouldlog(::NullLogger, args...) = false
handle_message(::NullLogger, args...; kwargs...) = error("Null logger handle_message() should not be called")


"""
    SimpleLogger(stream=STDERR, min_level=Info)

Simplistic logger for logging all messages with level not less than `min_level`
to `stream`.
"""
struct SimpleLogger <: AbstractLogger
    stream::IO
    min_level::LogLevel
end
SimpleLogger(stream::IO=STDERR, level=Info) = SimpleLogger(stream, parse_level(level))

function configure_logging(logger::SimpleLogger; min_level=Info)
    SimpleLogger(logger.stream, parse_level(min_level))
end

shouldlog(logger::SimpleLogger, level, args...) = !(level < logger.min_level)

min_enabled_level(logger::SimpleLogger) = logger.min_level

function handle_message(logger::SimpleLogger, level, msg, _module, group, id,
                        filepath, line; kwargs...)
    println(logger.stream, "$level [$(basename(String(filepath))):$line]: $msg")
end


#-------------------------------------------------------------------------------
# Logger control and lookup

_global_logstate = LogState(NullLogger()) # See __init__

"""
    global_logger()

Return the global logger, used to receive messages when no specific logger
exists for the current task.

    global_logger(logger)

Set the global logger to `logger`.
"""
global_logger() = _global_logstate.logger

function global_logger(logger::AbstractLogger)
    global _global_logstate = LogState(logger)
    logger
end

function current_logstate()
    get(task_local_storage(), :LOGGER_STATE, _global_logstate)::LogState
end

function with_logstate(f::Function, logstate)
    task_local_storage(f, :LOGGER_STATE, logstate)
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
with_logger(f::Function, logger::AbstractLogger) =
    with_logstate(f, LogState(logger))


"""
    current_logger()

Return the logger for the current task, or the global logger if none is
is attached to the task.
"""
current_logger() = current_logstate().logger


#-------------------------------------------------------------------------------

"""
    configure_logging(args...; kwargs...)

Call `configure_logging` with the current logger, and update cached log
filtering information.
"""
function configure_logging(args...; kwargs...)
    logger = configure_logging(current_logger(), args...; kwargs...)::AbstractLogger
    if haskey(task_local_storage(), :LOGGER_STATE)
        task_local_storage()[:LOGGER_STATE] = LogState(logger)
    else
        global _global_logstate = LogState(logger)
    end
    logger
end

configure_logging(::AbstractLogger, args...; kwargs...) = throw(ArgumentError("No configure_logging method matches the provided arguments."))

"""
    disable_logging(level)

Disable all log messages at log levels equal to or less than `level`.  This is
a *global* setting, intended to make debug logging extremely cheap when
disabled.
"""
function disable_logging(level::LogLevel)
    _min_enabled_level[] = level + 1
end
disable_logging(level) = disable_logging(parse_level(level))


function __init__()
    # Need to set this in __init__, as it refers to STDERR
    global_logger(SimpleLogger(STDERR))
end

end

# Logging

The [`Logging`](@ref Logging.Logging) module provides a way to record the history and progress of a
computation as a log of events.  Events are created by inserting a logging
statement into the source code, for example:

```julia
@warn "Abandon printf debugging, all ye who enter here!"
┌ Warning: Abandon printf debugging, all ye who enter here!
└ @ Main REPL[1]:1
```

The system provides several advantages over peppering your source code with
calls to `println()`.  First, it allows you to control the visibility and
presentation of messages without editing the source code.  For example, in
contrast to the `@warn` above

```julia
@debug "The sum of some values $(sum(rand(100)))"
```

will produce no output by default.  Furthermore, it's very cheap to leave debug
statements like this in the source code because the system avoids evaluating
the message if it would later be ignored.  In this case `sum(rand(100))` and
the associated string processing will never be executed unless debug logging is
enabled.

Second, the logging tools allow you to attach arbitrary data to each event as a
set of key--value pairs. This allows you to capture local variables and other
program state for later analysis. For example, to attach the local array
variable `A` and the sum of a vector `v` as the key `s` you can use

```jldoctest
A = ones(Int, 4, 4)
v = ones(100)
@info "Some variables"  A  s=sum(v)

# output
┌ Info: Some variables
│   A =
│    4×4 Array{Int64,2}:
│     1  1  1  1
│     1  1  1  1
│     1  1  1  1
│     1  1  1  1
└   s = 100.0
```

All of the logging macros `@debug`, `@info`, `@warn` and `@error` share common
features that are described in detail in the documentation for the more
general macro [`@logmsg`](@ref).

## Log event structure

Each event generates several pieces of data, some provided by the user and some
automatically extracted. Let's examine the user-defined data first:

* The *log level* is a broad category for the message that is used for early
  filtering. There are several standard levels of type [`LogLevel`](@ref);
  user-defined levels are also possible.
  Each is distinct in purpose:
  - `Debug` is information intended for the developer of the program.
  These events are disabled by default.
  - `Info` is for general information to the user.
  Think of it as an alternative to using `println` directly.
  - `Warn` means something is wrong and action is likely required
  but that for now the program is still working.
  - `Error` means something is wrong and it is unlikely to be recovered,
  at least by this part of the code.
  Often this log-level is unneeded as throwing an exception can convey
  all the required information.

* The *message*  is an object describing the event. By convention
  `AbstractString`s passed as messages are assumed to be in markdown format.
  Other types will be displayed using `print(io, obj)` or `string(obj)` for
  text-based output and possibly `show(io,mime,obj)` for other multimedia
  displays used in the installed logger.
* Optional *key--value pairs* allow arbitrary data to be attached to each event.
  Some keys have conventional meaning that can affect the way an event is
  interpreted (see [`@logmsg`](@ref)).

The system also generates some standard information for each event:

* The `module` in which the logging macro was expanded.
* The `file` and `line` where the logging macro occurs in the source code.
* A message `id` that is a unique, fixed identifier for the *source code
  statement* where the logging macro appears. This identifier is designed to be
  fairly stable even if the source code of the file changes, as long as the
  logging statement itself remains the same.
* A `group` for the event, which is set to the base name of the file by default,
  without extension.  This can be used to group messages into categories more
  finely than the log level (for example, all deprecation warnings have group
  `:depwarn`), or into logical groupings across or within modules.

Notice that some useful information such as the event time is not included by
default. This is because such information can be expensive to extract and is
also *dynamically* available to the current logger. It's simple to define a
[custom logger](@ref AbstractLogger-interface) to augment event data with the
time, backtrace, values of global variables and other useful information as
required.


## Processing log events

As you can see in the examples, logging statements make no mention of
where log events go or how they are processed. This is a key design feature
that makes the system composable and natural for concurrent use. It does this
by separating two different concerns:

* *Creating* log events is the concern of the module author who needs to
  decide where events are triggered and which information to include.
* *Processing* of log events — that is, display, filtering, aggregation and
  recording — is the concern of the application author who needs to bring
  multiple modules together into a cooperating application.

### Loggers

Processing of events is performed by a *logger*, which is the first piece of
user configurable code to see the event. All loggers must be subtypes of
[`AbstractLogger`](@ref).

When an event is triggered, the appropriate logger is found by looking for a
task-local logger with the global logger as fallback.  The idea here is that
the application code knows how log events should be processed and exists
somewhere at the top of the call stack. So we should look up through the call
stack to discover the logger — that is, the logger should be *dynamically
scoped*. (This is a point of contrast with logging frameworks where the
logger is *lexically scoped*; provided explicitly by the module author or as a
simple global variable. In such a system it's awkward to control logging while
composing functionality from multiple modules.)

The global logger may be set with [`global_logger`](@ref), and task-local
loggers controlled using [`with_logger`](@ref).  Newly spawned tasks inherit
the logger of the parent task.

There are three logger types provided by the library.  [`ConsoleLogger`](@ref)
is the default logger you see when starting the REPL.  It displays events in a
readable text format and tries to give simple but user friendly control over
formatting and filtering.  [`NullLogger`](@ref) is a convenient way to drop all
messages where necessary; it is the logging equivalent of the [`devnull`](@ref)
stream.  [`SimpleLogger`](@ref) is a very simplistic text formatting logger,
mainly useful for debugging the logging system itself.

Custom loggers should come with overloads for the functions described in the
[reference section](@ref AbstractLogger-interface).

### Early filtering and message handling

When an event occurs, a few steps of early filtering occur to avoid generating
messages that will be discarded:

1. The message log level is checked against a global minimum level (set via
   [`disable_logging`](@ref)).  This is a crude but extremely cheap global
   setting.
2. The current logger state is looked up and the message level checked against the
   logger's cached minimum level, as found by calling [`Logging.min_enabled_level`](@ref).
   This behavior can be overridden via environment variables (more on this later).
3. The [`Logging.shouldlog`](@ref) function is called with the current logger, taking
   some minimal information (level, module, group, id) which can be computed
   statically.  Most usefully, `shouldlog` is passed an event `id` which can be
   used to discard events early based on a cached predicate.

If all these checks pass, the message and key--value pairs are evaluated in full
and passed to the current logger via the [`Logging.handle_message`](@ref) function.
`handle_message()` may perform additional filtering as required and display the
event to the screen, save it to a file, etc.

Exceptions that occur while generating the log event are captured and logged
by default.  This prevents individual broken events from crashing the
application, which is helpful when enabling little-used debug events in a
production system.  This behavior can be customized per logger type by
extending [`Logging.catch_exceptions`](@ref).

## Testing log events

Log events are a side effect of running normal code, but you might find
yourself wanting to test particular informational messages and warnings. The
`Test` module provides a [`@test_logs`](@ref) macro that can be used to
pattern match against the log event stream.

## Environment variables

Message filtering can be influenced through the `JULIA_DEBUG` environment
variable, and serves as an easy way to enable debug logging for a file or
module. For example, loading julia with `JULIA_DEBUG=loading` will activate
`@debug` log messages in `loading.jl`:

```
$ JULIA_DEBUG=loading julia -e 'using OhMyREPL'
┌ Debug: Rejecting cache file /home/user/.julia/compiled/v0.7/OhMyREPL.ji due to it containing an invalid cache header
└ @ Base loading.jl:1328
[ Info: Recompiling stale cache file /home/user/.julia/compiled/v0.7/OhMyREPL.ji for module OhMyREPL
┌ Debug: Rejecting cache file /home/user/.julia/compiled/v0.7/Tokenize.ji due to it containing an invalid cache header
└ @ Base loading.jl:1328
...
```

Similarly, the environment variable can be used to enable debug logging of
modules, such as `Pkg`, or module roots (see [`Base.moduleroot`](@ref)). To
enable all debug logging, use the special value `all`.

To turn debug logging on from the REPL, set `ENV["JULIA_DEBUG"]` to the
name of the module of interest. Functions defined in the REPL belong to
module `Main`; logging for them can be enabled like this:
```julia-repl
julia> foo() = @debug "foo"
foo (generic function with 1 method)

julia> foo()

julia> ENV["JULIA_DEBUG"] = Main
Main

julia> foo()
┌ Debug: foo
└ @ Main REPL[1]:1

```

## Writing log events to a file

Sometimes it can be useful to write log events to a file. Here is an example
of how to use a task-local and global logger to write information to a text
file:

```julia-repl
# Load the logging module
julia> using Logging

# Open a textfile for writing
julia> io = open("log.txt", "w+")
IOStream(<file log.txt>)

# Create a simple logger
julia> logger = SimpleLogger(io)
SimpleLogger(IOStream(<file log.txt>), Info, Dict{Any,Int64}())

# Log a task-specific message
julia> with_logger(logger) do
           @info("a context specific log message")
       end

# Write all buffered messages to the file
julia> flush(io)

# Set the global logger to logger
julia> global_logger(logger)
SimpleLogger(IOStream(<file log.txt>), Info, Dict{Any,Int64}())

# This message will now also be written to the file
julia> @info("a global log message")

# Close the file
julia> close(io)
```


## Reference

### Logging module
```@docs
Logging.Logging
```

### Creating events

```@docs
Logging.@logmsg
Logging.LogLevel
```

### [Processing events with AbstractLogger](@id AbstractLogger-interface)

Event processing is controlled by overriding functions associated with
`AbstractLogger`:

| Methods to implement                |                        | Brief description                        |
|:----------------------------------- |:---------------------- |:---------------------------------------- |
| [`Logging.handle_message`](@ref)    |                        | Handle a log event                       |
| [`Logging.shouldlog`](@ref)         |                        | Early filtering of events                |
| [`Logging.min_enabled_level`](@ref) |                        | Lower bound for log level of accepted events |
| **Optional methods**                | **Default definition** | **Brief description**                    |
| [`Logging.catch_exceptions`](@ref)  | `true`                 | Catch exceptions during event evaluation |


```@docs
Logging.AbstractLogger
Logging.handle_message
Logging.shouldlog
Logging.min_enabled_level
Logging.catch_exceptions
Logging.disable_logging
```

### Using Loggers

Logger installation and inspection:

```@docs
Logging.global_logger
Logging.with_logger
Logging.current_logger
```

Loggers that are supplied with the system:

```@docs
Logging.NullLogger
Logging.ConsoleLogger
Logging.SimpleLogger
```

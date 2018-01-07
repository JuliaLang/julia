# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    ConsoleLogger(stream=STDERR, min_level=Info; meta_formatter=default_metafmt, show_limited=true)

Logger with formatting optimized for readability in a text console, for example
interactive work with the Julia REPL.

Log levels less than `min_level` are filtered out.

Message formatting can be controlled by setting keyword arguments:

* `meta_formatter` is a function which takes the log event metadata
  `(level, _module, group, id, file, line)` and produces a prefix and suffix
  for the log message.  The default is to prefix with the log level and add a
  suffix containing the module, file and line location.
* `show_limited` limits the printing of large data structures to something
  which can fit on the screen by setting the `:limit` `IOContext` key during
  formatting.
"""
struct ConsoleLogger <: AbstractLogger
    stream::IO
    min_level::LogLevel
    meta_formatter
    show_limited::Bool
    message_limits::Dict{Any,Int}
end
function ConsoleLogger(stream::IO=STDERR, min_level=Info;
                       meta_formatter=default_metafmt, show_limited=true)
    ConsoleLogger(stream, min_level, meta_formatter,
                  show_limited, Dict{Any,Int}())
end

shouldlog(logger::ConsoleLogger, level, _module, group, id) =
    get(logger.message_limits, id, 1) > 0

min_enabled_level(logger::ConsoleLogger) = logger.min_level

# Formatting of values in key value pairs
showvalue(io, msg) = show(io, "text/plain", msg)
function showvalue(io, e::Tuple{Exception,Any})
    ex,bt = e
    showerror(io, ex, bt; backtrace = bt!=nothing)
end
showvalue(io, ex::Exception) = showvalue(io, (ex,catch_backtrace()))

function default_metafmt(level, _module, group, id, file, line)
    ((level == Warn ? "Warning" : string(level))*':',
     "@ $_module $(basename(file)):$line")
end

function handle_message(logger::ConsoleLogger, level, message, _module, group, id,
                        filepath, line; maxlog=nothing, kwargs...)
    if maxlog != nothing && maxlog isa Integer
        remaining = get!(logger.message_limits, id, maxlog)
        logger.message_limits[id] = remaining - 1
        remaining > 0 || return
    end
    color = level < Info  ? Base.debug_color() :
            level < Warn  ? Base.info_color() :
            level < Error ? Base.warn_color() :
                            Base.error_color()
    buf = IOBuffer()
    iob = IOContext(buf, logger.stream)
    if logger.show_limited
        iob = IOContext(iob, :limit=>true)
    end
    msglines = split(chomp(string(message)), '\n')
    dsize = displaysize(logger.stream)
    width = dsize[2]
    prefix,suffix = logger.meta_formatter(level, _module, group, id, filepath, line)
    length(prefix) == 0 || (prefix = prefix*" ")
    length(suffix) == 0 || (suffix = " "*suffix)
    singlelinewidth = 2 + length(msglines[1]) + length(prefix) + length(suffix)
    issingleline = length(msglines) + length(kwargs) == 1 && singlelinewidth <= width
    print_with_color(color, iob, issingleline ? "[ " : "┌ ", bold=true)
    if length(prefix) > 0
        print_with_color(color, iob, prefix, bold=true)
    end
    print(iob, msglines[1])
    if issingleline
        npad = (width - singlelinewidth)
    else
        println(iob)
        for i in 2:length(msglines)
            print_with_color(color, iob, "│ ", bold=true)
            println(iob, msglines[i])
        end
        valbuf = IOBuffer()
        rows_per_value = max(1, dsize[1]÷(length(kwargs)+1))
        valio = IOContext(IOContext(valbuf, iob),
                          :displaysize=>(rows_per_value,dsize[2]-5))
        for (key,val) in pairs(kwargs)
            print_with_color(color, iob, "│ ", bold=true)
            print(iob, "  ", key, " =")
            showvalue(valio, val)
            vallines = split(String(take!(valbuf)), '\n')
            if length(vallines) == 1
                println(iob, " ", vallines[1])
            else
                println(iob)
                for line in vallines
                    print_with_color(color, iob, "│    ", bold=true)
                    println(iob, line)
                end
            end
        end
        print_with_color(color, iob, "└ ", bold=true)
        npad = width - 2 - length(suffix)
    end
    if length(suffix) > 0
        print(iob, " "^npad)
        print_with_color(:light_black, iob, suffix, bold=false)
    end
    print(iob, "\n")
    write(logger.stream, take!(buf))
    nothing
end


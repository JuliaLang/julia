# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    ConsoleLogger(stream=stderr, min_level=Info; meta_formatter=default_metafmt,
                  show_limited=true, right_justify=0)

Logger with formatting optimized for readability in a text console, for example
interactive work with the Julia REPL.

Log levels less than `min_level` are filtered out.

Message formatting can be controlled by setting keyword arguments:

* `meta_formatter` is a function which takes the log event metadata
  `(level, _module, group, id, file, line)` and returns a color (as would be
  passed to printstyled), prefix and suffix for the log message.  The
  default is to prefix with the log level and a suffix containing the module,
  file and line location.
* `show_limited` limits the printing of large data structures to something
  which can fit on the screen by setting the `:limit` `IOContext` key during
  formatting.
* `right_justify` is the integer column which log metadata is right justified
  at. The default is zero (metadata goes on its own line).
"""
struct ConsoleLogger <: AbstractLogger
    stream::IO
    min_level::LogLevel
    meta_formatter
    show_limited::Bool
    right_justify::Int
    message_limits::Dict{Any,Int}
end
function ConsoleLogger(stream::IO=stderr, min_level=Info;
                       meta_formatter=default_metafmt, show_limited=true,
                       right_justify=0)
    ConsoleLogger(stream, min_level, meta_formatter,
                  show_limited, right_justify, Dict{Any,Int}())
end

shouldlog(logger::ConsoleLogger, level, _module, group, id) =
    get(logger.message_limits, id, 1) > 0

min_enabled_level(logger::ConsoleLogger) = logger.min_level

# Formatting of values in key value pairs
showvalue(io, msg) = show(io, "text/plain", msg)
function showvalue(io, e::Tuple{Exception,Any})
    ex,bt = e
    showerror(io, ex, bt; backtrace = bt!==nothing)
end
showvalue(io, ex::Exception) = showerror(io, ex)

function default_logcolor(level)
    level < Info  ? Base.debug_color() :
    level < Warn  ? Base.info_color()  :
    level < Error ? Base.warn_color()  :
                    Base.error_color()
end

function default_metafmt(level, _module, group, id, file, line)
    color = default_logcolor(level)
    prefix = (level == Warn ? "Warning" : string(level))*':'
    suffix = ""
    Info <= level < Warn && return color, prefix, suffix
    _module !== nothing && (suffix *= "$(_module)")
    if file !== nothing
        _module !== nothing && (suffix *= " ")
        suffix *= Base.contractuser(file)
        if line !== nothing
            suffix *= ":$(isa(line, UnitRange) ? "$(first(line))-$(last(line))" : line)"
        end
    end
    !isempty(suffix) && (suffix = "@ " * suffix)
    return color, prefix, suffix
end

# Length of a string as it will appear in the terminal (after ANSI color codes
# are removed)
function termlength(str)
    N = 0
    in_esc = false
    for c in str
        if in_esc
            if c == 'm'
                in_esc = false
            end
        else
            if c == '\e'
                in_esc = true
            else
                N += 1
            end
        end
    end
    return N
end

function handle_message(logger::ConsoleLogger, level, message, _module, group, id,
                        filepath, line; maxlog=nothing, kwargs...)
    if maxlog !== nothing && maxlog isa Integer
        remaining = get!(logger.message_limits, id, maxlog)
        logger.message_limits[id] = remaining - 1
        remaining > 0 || return
    end

    # Generate a text representation of the message and all key value pairs,
    # split into lines.
    msglines = [(indent=0,msg=l) for l in split(chomp(string(message)), '\n')]
    dsize = displaysize(logger.stream)
    if !isempty(kwargs)
        valbuf = IOBuffer()
        rows_per_value = max(1, dsize[1]÷(length(kwargs)+1))
        valio = IOContext(IOContext(valbuf, logger.stream),
                          :displaysize => (rows_per_value,dsize[2]-5),
                          :limit => logger.show_limited)
        for (key,val) in pairs(kwargs)
            showvalue(valio, val)
            vallines = split(String(take!(valbuf)), '\n')
            if length(vallines) == 1
                push!(msglines, (indent=2,msg=SubString("$key = $(vallines[1])")))
            else
                push!(msglines, (indent=2,msg=SubString("$key =")))
                append!(msglines, ((indent=3,msg=line) for line in vallines))
            end
        end
    end

    # Format lines as text with appropriate indentation and with a box
    # decoration on the left.
    color,prefix,suffix = logger.meta_formatter(level, _module, group, id, filepath, line)
    minsuffixpad = 2
    buf = IOBuffer()
    iob = IOContext(buf, logger.stream)
    nonpadwidth = 2 + (isempty(prefix) || length(msglines) > 1 ? 0 : length(prefix)+1) +
                  msglines[end].indent + termlength(msglines[end].msg) +
                  (isempty(suffix) ? 0 : length(suffix)+minsuffixpad)
    justify_width = min(logger.right_justify, dsize[2])
    if nonpadwidth > justify_width && !isempty(suffix)
        push!(msglines, (indent=0,msg=SubString("")))
        minsuffixpad = 0
        nonpadwidth = 2 + length(suffix)
    end
    for (i,(indent,msg)) in enumerate(msglines)
        boxstr = length(msglines) == 1 ? "[ " :
                 i == 1                ? "┌ " :
                 i < length(msglines)  ? "│ " :
                                         "└ "
        printstyled(iob, boxstr, bold=true, color=color)
        if i == 1 && !isempty(prefix)
            printstyled(iob, prefix, " ", bold=true, color=color)
        end
        print(iob, " "^indent, msg)
        if i == length(msglines) && !isempty(suffix)
            npad = max(0, justify_width - nonpadwidth) + minsuffixpad
            print(iob, " "^npad)
            printstyled(iob, suffix, color=:light_black)
        end
        println(iob)
    end

    write(logger.stream, take!(buf))
    nothing
end


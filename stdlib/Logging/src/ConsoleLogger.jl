"""
    ConsoleLogger(stream::IO; min_level=Info, catch_exceptions=true)

A logger designed for readable log formatting at the julia REPL. `min_level` is
the default log level for all modules.

If `catch_exceptions` is set to `true`, the logger will log any exceptions
generated during log message processing at `Error` level.  Setting this to
`false` can be a helpful way to debug errors in your log statements.
"""
mutable struct ConsoleLogger <: AbstractLogger
    stream::IO
    default_min_level::LogLevel
    catch_exceptions::Bool
    prev_progress_key
    message_counts::Dict{Any,Int}
    module_limits::Dict{Module,LogLevel}
    blacklisted_ids::Set{Any}
end

function ConsoleLogger(stream::IO; min_level=Info, catch_exceptions=true)
    ConsoleLogger(stream, parse_level(min_level), catch_exceptions, nothing,
             Dict{Any,Int}(), Dict{Module,LogLevel}(), Set{Any}())
end

"""
    configure_logging(logger::ConsoleLogger, [_module]; min_level=Info)

Set minimum log level `min_level` for module `_module`.  If `_module` is not
provided, reset the log level for all modules.
"""
function configure_logging(logger::ConsoleLogger, _module=nothing;
                           min_level=Info)
    min_level = parse_level(min_level)
    if _module == nothing
        empty!(logger.module_limits)
        logger.default_min_level = min_level
    else
        # Per-module log limiting
        logger.module_limits[_module] = min_level
    end
    logger
end

#-------------------------------------------------------------------------------
# Standard AbstractLogger functions
function shouldlog(logger::ConsoleLogger, level, _module, group, id)
    if level < get(logger.module_limits, _module, logger.default_min_level)
        return false
    end
    if id in logger.blacklisted_ids
        return false
    end
    return true
end

function min_enabled_level(logger::ConsoleLogger)
    min_level = logger.default_min_level
    for (_,level) ∈ logger.module_limits
        if level < min_level
            min_level = level
        end
    end
    return min_level
end

catch_exceptions(logger::ConsoleLogger) = logger.catch_exceptions


#-------------------------------------------------------------------------------
# Console message formatting
formatmsg(msg) = string(msg)

formatmsg(e::Tuple{Exception,Any}) = formatmsg(e[1], e[2])
function formatmsg(ex::Exception)
    bt = catch_backtrace()
    formatmsg(ex, isempty(bt) ? nothing : bt)
end
function formatmsg(ex_msg::Exception, bt)
    io = IOBuffer()
    showerror(io, ex_msg, bt; backtrace = bt!=nothing)
    String(take!(io))
end

# Length of a string as it will appear in the terminal (after ANSI color codes
# are removed).  TODO: Make this less crude.
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

function levelstyle(level::LogLevel)
    if     level < Info  return ((:blue,   true), "D-")
    elseif level < Warn  return ((:cyan,   true), "I-")
    elseif level < Error return ((:yellow, true), "W-")
    else                 return ((:red,    true), "E-")
    end
end

function handle_message(logger::ConsoleLogger, level, msg, _module, group,
                        id, file, line; maxlog=nothing, kwargs...)
    if maxlog !== nothing
        count = get!(logger.message_counts, id, 0)
        count += 1
        logger.message_counts[id] = count
        if count > maxlog
            push!(logger.blacklisted_ids, id)
            return
        end
    end
    display_message(logger, level, formatmsg(msg), _module, group, id,
                    file, line; kwargs...)
end

# Print a string with prefixes on each line, and a suffix on the last line
function print_with_decorations(io, prefixes, decoration_color, str, suffix)
    @assert !isempty(prefixes)
    p = 1
    splitter = r"(\n|\e\[[0-9;]*m)"
    i = start(str)
    n = endof(str)
    colorcode = ""
    stylecode = ""
    print_with_color(decoration_color[1], io, prefixes[p], bold=decoration_color[2])
    linelen = length(prefixes[p])
    p == length(prefixes) || (p += 1)
    print(io, colorcode, stylecode)
    while i <= n
        r = search(str,splitter,i)
        j = isempty(r) ? n : last(r)
        linelen += length(SubString(str, i, (isempty(r) ? n : prevind(str,first(r)))))
        print(io, SubString(str, i, j))
        i = nextind(str,j)
        if !isempty(r)
            if str[r[1]] == '\n'
                !Base.have_color || print(io, "\e[0m")
                print_with_color(decoration_color[1], io, prefixes[p], bold=decoration_color[2])
                linelen = length(prefixes[p])
                p == length(prefixes) || (p += 1)
                !Base.have_color || print(io, "\e[0m")
                print(io, colorcode, stylecode)
            else
                code = str[r]
                if code == "\e[m"
                elseif code == "\e[22m"
                    stylecode = ""
                elseif code == "\e[39m"
                    colorcode = ""
                else
                    icode = parse(Int, code[3:end-1])
                    if 1 <= icode <= 29
                        stylecode = code
                    elseif 30 <= icode <= 38
                        colorcode = code
                    end
                end
            end
        end
    end
    if !isempty(suffix)
        width = displaysize(io)[2]
        if length(suffix) > width - linelen
            print_with_color(decoration_color[1], io, "\n", prefixes[p], bold=decoration_color[2])
            linelen = length(prefixes[p])
        end
        print(io, " "^max(0, (width - linelen - length(suffix))))
        print_with_color(decoration_color[1], io, suffix, bold=decoration_color[2])
    end
end

function display_message(logger::ConsoleLogger, level, msg::AbstractString,
                         _module, group, id, filepath, line;
                         progress=nothing, once=nothing, kwargs...)
    # TODO: progress throttling?
    # Log printing
    filename = basename(String(filepath))
    color, prefix = levelstyle(convert(LogLevel, level))
    # Attempt at avoiding the problem of distracting metadata in info log
    # messages - print metadata to the right hand side.
    metastr = " $level $filename:$line"
    msg = rstrip(msg, '\n')

    # Hack: render markdown to temporary buffer, and munge it to remove
    # trailing newlines, even with confounding color codes.
    buf = IOBuffer()
    dsize = displaysize(logger.stream)
    width = dsize[2]
    Markdown.term(IOContext(buf, :displaysize=>(dsize[1],width-2)), Markdown.parse(msg))
    msg = String(take!(buf))
    msg = replace(msg, r"\n(\e\[[0-9]+m)$", s"\1")
    msg = rstrip(msg)

    if progress === nothing
        if logger.prev_progress_key !== nothing
            print(logger.stream, "\n")
        end
        logger.prev_progress_key = nothing

        print_with_decorations(logger.stream, [prefix, "| "], color, msg, metastr)
        print(logger.stream, "\n")

        for (k,v) in pairs(kwargs)
            valstr = formatmsg(v)
            kvstr = '\n' in valstr ? string(k, " =\n", formatmsg(v)) :
                                     string(k, " = ", formatmsg(v))
            print_with_decorations(logger.stream, ["|   ", "|    "], color, kvstr, "")
            print(logger.stream, "\n")
        end
    else
        progress_key = msg
        if logger.prev_progress_key !== nothing && logger.prev_progress_key != progress_key
            print(logger.stream, "\n")
        end
        nbar = max(1, width - (termlength(msg) + length(prefix) + length(metastr)) - 3)
        nfilledbar = round(Int, clamp(progress, 0, 1)*nbar)
        msgbar = string(msg, " [", "-"^nfilledbar, " "^(nbar - nfilledbar), "]")
        print(logger.stream, "\r")
        print_with_decorations(logger.stream, [prefix], color, msgbar, metastr)
        logger.prev_progress_key = progress_key
    end
end



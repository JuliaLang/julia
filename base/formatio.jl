# This file is a part of Julia. License is MIT: https://julialang.org/license

## printing with color ##

const ansi_reset = "\033[0m"
const ansi_formats = (:bold, :underline, :blink, :reverse, :hidden)

const text_colors = AnyDict(
    # colors
    :black         => "\033[30m",
    :red           => "\033[31m",
    :green         => "\033[32m",
    :yellow        => "\033[33m",
    :blue          => "\033[34m",
    :magenta       => "\033[35m",
    :cyan          => "\033[36m",
    :white         => "\033[37m",
    :light_black   => "\033[90m", # gray
    :light_red     => "\033[91m",
    :light_green   => "\033[92m",
    :light_yellow  => "\033[93m",
    :light_blue    => "\033[94m",
    :light_magenta => "\033[95m",
    :light_cyan    => "\033[96m",
    # special
    :normal        => ansi_reset,
    :default       => "\033[39m",
    # formats
    :bold          => "\033[1m",
    :underline     => "\033[4m",
    :blink         => "\033[5m",
    :reverse       => "\033[7m",
    :hidden        => "\033[8m",
)

for i in 0:255
    text_colors[i] = "\033[38;5;$(i)m"
end

const disable_text_style = AnyDict(
    :bold      => "\033[22m",
    :underline => "\033[24m",
    :blink     => "\033[25m",
    :reverse   => "\033[27m",
    :hidden    => "\033[28m",
)

# Create a docstring with an automatically generated list
# of colors.
let possible_formatting_symbols = [:normal, :bold, :default]
    available_text_colors = collect(Iterators.filter(x -> !isa(x, Integer), keys(text_colors)))
    available_text_colors = cat(
        sort!(intersect(available_text_colors, possible_formatting_symbols), rev=true),
        sort!(setdiff(  available_text_colors, possible_formatting_symbols));
        dims=1)
global const available_text_colors_docstring =
    string(join([string("`:", key, "`")
                 for key in available_text_colors],
                ",\n", ", or \n"))
end

"""Dictionary of color codes for the terminal.

Available colors are: $available_text_colors_docstring as well as the integers 0 to 255 inclusive.

The color `:default` will print text in the default color while the color `:normal`
will print text with all text properties (like boldness) reset.
Printing with the color `:nothing` will print the string without modifications.
"""
text_colors

"""
    printstyled([io], xs...; bold::Bool=false, color::Union{Symbol,Int}=:normal)

Print `xs` in a color specified as a symbol or integer, optionally in bold.

`color` may take any of the values $available_text_colors_docstring
or an integer between 0 and 255 inclusive. Note that not all terminals support 256 colors.
If the keyword `bold` is given as `true`, the result will be printed in bold.
"""
printstyled(io::IO, msg...; bold::Bool=false, color::Union{Int,Symbol}=:normal) =
    with_output_color(print, color, io, msg...; bold=bold)
printstyled(msg...; bold::Bool=false, color::Union{Int,Symbol}=:normal) =
    printstyled(stdout, msg...; bold=bold, color=color)


"""
    IOFormatBuffer

`IOFormatBuffer` provides a mechanism for recording annotation regions of an IOBuffer.
These regions can be used to colorize, or otherwise structure or describe, each byte in the output.
"""
struct IOFormatBuffer <: AbstractPipe
    buf::IOBuffer
    annotation::Vector{Any} # covers bytes of buf from [start to end]
    starts::Vector{Int}
    ends::Vector{Int}
    function IOFormatBuffer()
        buf = IOBuffer(read=false, write=true, append=true)
        buf.seekable = false
        return new(buf, Symbol[], Int[], Int[])
    end
end

pipe_writer(io::IOFormatBuffer) = io.buf
pipe_reader(io::IOFormatBuffer) = error("IOFormatBuffer not readable")

"""
    bytesavailable(io::IOFormatBuffer)

Return the amount of data (number of bytes) in the IOFormatBuffer.
"""
bytesavailable(io::IOFormatBuffer) = io.buf.size

# While IOFormatBuffer doesn't define `pipe_reader`,
# it is valid to try to ask some questions about the read state.
# It is just not valid to try to actually change that state!
mark(io::IOFormatBuffer) = mark(io.buf)
reset(io::IOFormatBuffer) = reset(io.buf)
unmark(io::IOFormatBuffer) = unmark(io.buf)
ismarked(io::IOFormatBuffer) = ismarked(io.buf)
seekend(io::IOFormatBuffer) = io

function bytesavailable_until_text(io::IOFormatBuffer, width::Integer, chars="", truncmark="…")
    io.buf.readable = true
    nb = bytesavailable_until_text(io.buf, width, chars, truncmark)
    io.buf.readable = false
    return nb
end

"""
    truncate(io::IOFormatBuffer, n)

Truncate the content in the buffer to at most `n` bytes,
before applying any formatting.

See also [`bytesavailable(io::IOFormatBuffer)`](@ref).
"""
function truncate(io::IOFormatBuffer, n::Integer)
    ismarked(io.buf) && io.buf.mark > n && unmark(io.buf)
    if n <= 0
        empty!(io.annotation)
        empty!(io.starts)
        empty!(io.ends)
        seekend(io.buf)
    elseif n < bytesavailable(io)
        io.buf.size = n
        io.buf.ptr = 1
        for id in 1:length(io.annotation)
            io.starts[id] > n && (io.starts[id] = n)
            io.ends[id] >= n && (io.ends[id] = n - 1)
        end
    end
    return io
end

function with_output_color(f::Function, color::Union{Int, Symbol}, io::IO, args...; bold::Bool = false)
    buf = IOContext(IOFormatBuffer(), io)
    try
        with_output_color(f, color, buf, args...; bold = bold)
    finally
        write(io, buf.io)
    end
    nothing
end

function with_output_color(f::Function, color::Union{Int, Symbol}, buf::IOContext{IOFormatBuffer}, args...; bold::Bool = false)
    let id = push!(buf.io, color)
        bold && push!(buf.io, :bold)
        f(buf, args...)
        pop!(buf.io, id)
    end
end

# record the start position for the region `fmt`
# return a token for `pop!`
@noinline function push!(io::IOFormatBuffer, fmt::Symbol)
    ismarked(io.buf) && unmark(io.buf)
    push!(io.annotation, fmt)
    push!(io.starts, bytesavailable(io.buf) + 1)
    push!(io.ends, -1)
    return length(io.annotation)
end

# record the end position for the region `fmt` (and all nested regions)
@noinline function pop!(io::IOFormatBuffer, start::Int)
    ismarked(io.buf) && unmark(io.buf)
    p = bytesavailable(io.buf)
    while start <= length(io.ends)
        if io.ends[start] == -1
            io.ends[start] = p
        end
        start += 1
    end
    nothing
end

# render `mark_io` and return the annotated buffer
function apply_ansi_format(mark_io::IOFormatBuffer)
    pop!(mark_io, 1)
    out = mark_io.buf.data
    copy = UInt8[]
    sizehint!(copy, length(out) + length(mark_io.annotation) * length(ansi_reset) * 2)
    nextid = 1
    stack = Int[]
    color = []
    format = zeros(length(ansi_formats))
    emptyline = true
    for i in 1:length(out)
        # record all of the spans that start on this byte
        while nextid <= length(mark_io.starts) && mark_io.starts[nextid] == i
            fmt = mark_io.annotation[nextid]
            if mark_io.ends[nextid] >= i && haskey(text_colors, fmt)
                # determine what effect this formatting command will have
                is_fmt = findfirst(isequal(fmt), ansi_formats)
                if is_fmt === nothing
                    if fmt === :normal
                        fill!(format, 0)
                    end
                    push!(color, fmt)
                else
                    format[is_fmt] += 1
                end
                emptyline || append!(copy, codeunits(text_colors[fmt]))
                push!(stack, nextid)
            end
            nextid += 1
        end
        # around newlines, reset then reapply the current format
        byte = out[i]
        newline = (byte == UInt8('\n'))
        if emptyline && !newline
            # re-apply formatting on the new empty line
            # before the first byte
            if !isempty(color)
                append!(copy, codeunits(text_colors[color[end]]))
            end
            for fmt in 1:length(ansi_formats)
                if format[fmt] > 0
                    append!(copy, codeunits(text_colors[ansi_formats[fmt]]))
                end
            end
        end
        if !emptyline && newline
            # disable formatting formatting before the newline
            # if we printed anything
            for fmt in 1:length(ansi_formats)
                if format[fmt] > 0
                    append!(copy, codeunits(disable_text_style[ansi_formats[fmt]]))
                end
            end
            if !isempty(color) && color[end] ∉ (:default, :normal)
                append!(copy, codeunits(text_colors[:default]))
            end
            # alternatively: append!(copy, codeunits(ansi_reset))
        end
        push!(copy, byte)
        emptyline = newline
        # terminate all of the spans that end on this byte
        while !isempty(stack) && (mark_io.ends[stack[end]] == i)
            fmt = mark_io.annotation[pop!(stack)]
            # determine the reverse operation
            is_fmt = findfirst(isequal(fmt), ansi_formats)
            if is_fmt === nothing
                # for colors: re-apply the previous color from the stack
                pop!(color) # === fmt
                newcolor = (isempty(color) ? :default : color[end])
                if newcolor !== fmt
                    if !(newcolor === :default && fmt === :normal)
                        emptyline || append!(copy, codeunits(text_colors[newcolor]))
                    end
                    if fmt === :normal
                        # when clearing `normal` need to recompute the `format` list
                        # since we cleared it above
                        for active in stack
                            fmt = mark_io.annotation[active]
                            is_fmt = findfirst(isequal(fmt), ansi_formats)
                            if fmt === :normal
                                fill!(format, 0)
                            elseif is_fmt !== nothing
                                format[is_fmt] += 1
                            end
                        end
                    end
                end
            else
                if (format[is_fmt] -= 1) == 0
                    emptyline || append!(copy, codeunits(disable_text_style[fmt]))
                end
            end
        end
    end
    return copy
end

# render and move the data in `mark_io` into `io`
function write(io::IO, mark_io::IOFormatBuffer)
    fmt = mark_io.buf.data
    resize!(fmt, mark_io.buf.size)
    if get(io, :color, false)
        fmt = apply_ansi_format(mark_io)
    end
    truncate(mark_io, 0)
    return write(io, fmt)
end

#function write(io::IOFormatBuffer, mark_io::IOFormatBuffer)
#    TODO: preserve `annotation` information, rather than eagerly rendering it
#end
#write(io::IOContext{IOFormatBuffer}, mark_io::IOFormatBuffer) = write(io.io, mark_io)

# NOTE: I recommend wrapping this html output in:
#   <div style="white-space: pre-wrap; font-family: monospace; unicode-bidi: embed">
# since `print` functions typically expect white-space (and mono-spacing) to be significant
#
#const default_html_tags = Dict{Symbol, Tuple{String, String}}([
#    :black         => ("<span style='color:black'>", "</span>"),
#    :red           => ("<span style='color:darkred'>", "</span>"),
#    :green         => ("<span style='color:lightgreen'>", "</span>"),
#    :yellow        => ("<span style='color:gold'>", "</span>"),
#    :blue          => ("<span style='color:lightblue'>", "</span>"),
#    :magenta       => ("<span style='color:purple'>", "</span>"),
#    :cyan          => ("<span style='color:lightcyan'>", "</span>"),
#    :white         => ("<span style='color:white'>", "</span>"),
#    # "bright" colors
#    :light_black   => ("<span style='color:gray'>", "</span>"), # gray
#    :light_red     => ("<span style='color:red'>", "</span>"),
#    :light_green   => ("<span style='color:green'>", "</span>"),
#    :light_yellow  => ("<span style='color:yellow'>", "</span>"),
#    :light_blue    => ("<span style='color:blue'>", "</span>"),
#    :light_magenta => ("<span style='color:magenta'>", "</span>"),
#    :light_cyan    => ("<span style='color:cyan'>", "</span>"),
#    # special
#    :default       => ("<span style='color:initial'>", "</span>"),
#    :normal        => ("<span style='color:initial;font-weight:initial;text-decoration:initial;font-style:initial'>", "</span>"),
#    # formats
#    :bold          => ("<b>", "</b>"),
#    :italic        => ("<i>", "</i>"),
#    :underline     => ("<u>", "</u>"),
#    :blink         => ("<blink>", "</blink>"),
#    # :reverse       => ("<span style='color:inherit(background-color);background-color:inherit(color)'>", "</span>"), # TODO: is this possible?
#    :hidden        => ("<span style='display:none'>", "</span>"), # not exactly right: it should still take render space but just not be visible
#    ])
#function apply_html_format(mark_io::IOFormatBuffer, html_tags::typeof(default_html_tags)=default_html_tags)
#    pop!(mark_io, 1)
#    out = mark_io.buf.data
#    copy = UInt8[]
#    sizehint!(copy, length(out))
#    nextid = 1
#    end_pos = Int[]
#    end_tag = String[]
#    for i in 1:length(out)
#        # record all of the spans that start on this byte
#        while nextid <= length(mark_io.starts) && (mark_io.starts[nextid] == i)
#            fmt = mark_io.annotation[nextid]
#            # determine what effect this formatting command will have
#            tag = get(html_tags, fmt, nothing)
#            if tag !== nothing
#                append!(copy, codeunits(tag[1]))
#                push!(end_pos, mark_io.ends[nextid])
#                push!(end_tag, tag[2])
#            end
#            nextid += 1
#        end
#        byte = out[i]
#        if byte == UInt8('&')
#            append!(copy, codeunits("&amp;"))
#        elseif byte == UInt8('>')
#            append!(copy, codeunits("&gt;"))
#        elseif byte == UInt8('<')
#            append!(copy, codeunits("&lt;"))
#        else
#            push!(copy, byte)
#        end
#        # terminate all of the spans that end on this byte
#        while !isempty(end_pos) && (end_pos[end] == i)
#            pop!(end_pos)
#            fmt = pop!(end_tag)
#            append!(copy, fmt)
#        end
#    end
#    return copy
#end

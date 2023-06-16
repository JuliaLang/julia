# Compatibility hacks for older Julia versions
if VERSION < v"1.1"
    isnothing(x) = x === nothing
end
if VERSION < v"1.4"
    function only(x::Union{AbstractVector,AbstractString})
        if length(x) != 1
            error("Collection must contain exactly 1 element")
        end
        return x[1]
    end
end
if VERSION < v"1.5"
    import Base.peek
end

_unsafe_wrap_substring(s) = (s.offset, unsafe_wrap(Vector{UInt8}, s.string))

#--------------------------------------------------
#
# Internal error, used as assertion failure for cases we expect can't happen.
@noinline function internal_error(strs...)
    error("Internal error: ", strs...)
end

# Like @assert, but always enabled and calls internal_error()
macro check(ex, msgs...)
    msg = isempty(msgs) ? ex : msgs[1]
    if isa(msg, AbstractString)
        msg = msg
    elseif !isempty(msgs) && (isa(msg, Expr) || isa(msg, Symbol))
        msg = :(string($(esc(msg))))
    else
        msg = string(msg)
    end
    return :($(esc(ex)) ? nothing : internal_error($msg))
end

# Really remove line numbers, even from Expr(:toplevel)
remove_linenums!(ex) = ex
function remove_linenums!(ex::Expr)
    if ex.head === :block || ex.head === :quote || ex.head === :toplevel
        filter!(ex.args) do x
            !(isa(x, Expr) && x.head === :line || isa(x, LineNumberNode))
        end
    end
    for subex in ex.args
        subex isa Expr && remove_linenums!(subex)
    end
    return ex
end

# String macro to get the UInt8 code of an ascii character
macro u8_str(str)
    c = str == "\\" ? '\\' : only(unescape_string(str))
    isascii(c) || error("Non-ascii character in u8_str")
    codepoint(c) % UInt8
end

#-------------------------------------------------------------------------------
# Text printing/display utils

const _fg_color_codes = Dict(
    :black         => 30,
    :red           => 31,
    :green         => 32,
    :yellow        => 33,
    :blue          => 34,
    :magenta       => 35,
    :cyan          => 36,
    :white         => 37,
    :light_black   => 90, # gray
    :light_red     => 91,
    :light_green   => 92,
    :light_yellow  => 93,
    :light_blue    => 94,
    :light_magenta => 95,
    :light_cyan    => 96,
    :light_white   => 97,
)

"""
    _printstyled(io::IO, text;
                 fgcolor=nothing, bgcolor=nothing, href=nothing)

Like Base.printstyled, but allows providing RGB colors for true color
terminals, both foreground and background colors, and hyperlinks. Colors may be
given as one of the standard color names as in `Base.printstyled`, an integer
for 256 color terms, or an (r,g,b) triple with `0 <= r <= 255` etc for true
color terminals.

* `fgcolor` - set foreground color
* `bgcolor` - set background color
* `href`    - set hyperlink reference
"""
function _printstyled(io::IO, text; fgcolor=nothing, bgcolor=nothing, href=nothing)
    if (isnothing(fgcolor) && isnothing(bgcolor) && isnothing(href)) || !get(io, :color, false)
        print(io, text)
        return
    end
    colcode = ""
    if !isnothing(fgcolor)
        if fgcolor isa Symbol && haskey(_fg_color_codes, fgcolor)
            colcode *= "\e[$(_fg_color_codes[fgcolor])m"
        elseif fgcolor isa Integer && 0 <= fgcolor <= 255
            colcode *= "\e[38;5;$(fgcolor)m"
        elseif fgcolor isa Tuple && length(fgcolor) == 3 && all(0 .<= fgcolor .<= 255)
            colcode *= "\e[38;2;$(fgcolor[1]);$(fgcolor[2]);$(fgcolor[3])m"
        else
            error("Invalid ansi color $fgcolor")
        end
    end
    if !isnothing(bgcolor)
        if bgcolor isa Symbol && haskey(_fg_color_codes, bgcolor)
            colcode *= "\e[$(10 + _fg_color_codes[bgcolor])m"
        elseif bgcolor isa Integer && 0 <= bgcolor <= 255
            colcode *= "\e[48;5;$(bgcolor)m"
        elseif bgcolor isa Tuple && length(bgcolor) == 3 && all(0 .<= bgcolor .<= 255)
            colcode *= "\e[48;2;$(bgcolor[1]);$(bgcolor[2]);$(bgcolor[3])m"
        else
            error("Invalid ansi color $bgcolor")
        end
    end
    colreset = "\e[0;0m"
    first = true
    for linepart in split(text, '\n')
        first || print(io, '\n')
        line = string(colcode, linepart, colreset)
        if !isnothing(href)
            line = "\e]8;;$href\e\\$line\e]8;;\e\\"
        end
        print(io, line)
        first = false
    end
end


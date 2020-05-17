# This file is a part of Julia. License is MIT: https://julialang.org/license

"""global menu configuration parameters"""
const CONFIG = Dict{Symbol,Union{Char,String,Bool}}()

struct Indicators
    cursor::Char
    checked::String
    unchecked::String
end
Indicators() = Indicators(CONFIG)
Indicators(settings) = Indicators(settings[:cursor], settings[:checked], settings[:unchecked])

"""
    config( <see arguments> )

Keyword-only function to configure global menu parameters

# Arguments
 - `charset::Symbol=:na`: ui characters to use (`:ascii` or `:unicode`); overridden by other arguments
 - `cursor::Char='>'|'→'`: character to use for cursor
 - `up_arrow::Char='^'|'↑'`: character to use for up arrow
 - `down_arrow::Char='v'|'↓'`: character to use for down arrow
 - `checked::String="[X]"|"✓"`: string to use for checked
 - `unchecked::String="[ ]"|"⬚")`: string to use for unchecked
 - `scroll::Symbol=:nowrap`: If `:wrap` wrap cursor around top and bottom, if :`nowrap` do not wrap cursor
 - `suppress_output::Bool=false`: For testing. If true, menu will not be printed to console.
 - `ctrl_c_interrupt::Bool=true`: If `false`, return empty on ^C, if `true` throw InterruptException() on ^C
"""
function config(;charset::Symbol = :na,
                scroll::Symbol = :na,
                cursor::Char = '\0',
                up_arrow::Char = '\0',
                down_arrow::Char = '\0',
                checked::String = "",
                unchecked::String = "",
                suppress_output::Union{Nothing, Bool}=nothing,
                supress_output::Union{Nothing, Bool}=nothing,   # TODO: remove Julia 2.0
                ctrl_c_interrupt::Union{Nothing, Bool}=nothing)

    if supress_output !== nothing
        # TODO: remove this whole block for Julia 2.0.
        # This was a documented option with a typo, so we need to support it throughout Julia 1.x.
        # During Julia 1.x, the typo `supress_output` takes precedence over `suppress_output`.
        suppress_output === nothing || @warn "`supress_output` is deprecated, use `suppress_output`"
        suppress_output = supress_output
    end

    if charset === :ascii
        cursor     = '>'
        up_arrow   = '^'
        down_arrow = 'v'
        checked    = "[X]"
        unchecked  = "[ ]"
    elseif charset === :unicode
        cursor     = '→'
        up_arrow   = '↑'
        down_arrow = '↓'
        checked    = "✓"
        unchecked  = "⬚"
    elseif charset === :na
    else
        throw(ArgumentError("charset should be :ascii or :unicode, received $charset"))
    end

    scroll ∉ [:na, :wrap, :nowrap] && throw(ArgumentError("scroll must be :wrap or :nowrap, received $scroll"))
    scroll === :wrap   && (CONFIG[:scroll_wrap] = true)
    scroll === :nowrap && (CONFIG[:scroll_wrap] = false)

    cursor     != '\0' && (CONFIG[:cursor]     = cursor)
    up_arrow   != '\0' && (CONFIG[:up_arrow]   = up_arrow)
    down_arrow != '\0' && (CONFIG[:down_arrow] = down_arrow)
    checked    != ""   && (CONFIG[:checked]    = checked)
    unchecked  != ""   && (CONFIG[:unchecked]  = unchecked)
    suppress_output isa Bool   && (CONFIG[:suppress_output] = suppress_output)
    ctrl_c_interrupt isa Bool && (CONFIG[:ctrl_c_interrupt] = ctrl_c_interrupt)

    return nothing
end

# Set up defaults
config(charset=:ascii, scroll=:nowrap, suppress_output=false, ctrl_c_interrupt=true)

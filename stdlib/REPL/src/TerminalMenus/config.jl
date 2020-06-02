# This file is a part of Julia. License is MIT: https://julialang.org/license

abstract type AbstractConfig end

struct Config <: AbstractConfig
    cursor::Char
    up_arrow::Char
    down_arrow::Char
    scroll_wrap::Bool
    ctrl_c_interrupt::Bool
end

struct MultiSelectConfig <: AbstractConfig
    config::Config
    checked::String
    unchecked::String
end

"""
    Config(; scroll_wrap=false, ctrl_c_interrupt=true, charset=:ascii, cursor::Char, up_arrow::Char, down_arrow::Char)

Configure behavior for selection menus via keyword arguments:

- `scroll_wrap`, if `true`, causes the menu to wrap around when scrolling above the first
  or below the last entry
- `ctrl_c_interrupt`, if `true`, throws an `InterruptException` if the user hits Ctrl-C
  during menu selection. If `false`, [`TerminalMenus.request`](@ref) will return the
  default result from [`TerminalMenus.selected`](@ref).
- `charset` affects the default values for `cursor`, `up_arrow`, and `down_arrow`,
  and can be `:ascii` or `:unicode`
- `cursor` is the character printed to indicate the option that will be chosen by
  hitting "Enter." Defaults are '>' or '→', depending on `charset`.
- `up_arrow` is the character printed when the display does not include the first entry.
  Defaults are '^' or '↑', depending on `charset`.
- `down_arrow` is the character printed when the display does not include the last entry.
  Defaults are 'v' or '↓', depending on `charset`.

Subtypes of `ConfiguredMenu` will print `cursor`, `up_arrow`, and `down_arrow` automatically
as needed, your `writeline` method should not print them.

!!! compat Julia 1.6
    `Config` is available as of Julia 1.6. On older releases use the global `CONFIG`.
"""
function Config(;
                charset::Symbol = :ascii,
                cursor::Char = '\0',
                up_arrow::Char = '\0',
                down_arrow::Char = '\0',
                scroll_wrap::Bool = false,
                ctrl_c_interrupt::Bool = true)
    charset === :ascii || charset === :unicode ||
        throw(ArgumentError("charset should be :ascii or :unicode, received $charset"))
    if cursor == '\0'
        cursor = charset === :ascii ? '>' : '→'
    end
    if up_arrow == '\0'
        up_arrow = charset === :ascii ? '^' : '↑'
    end
    if down_arrow == '\0'
        down_arrow = charset === :ascii ? 'v' : '↓'
    end
    return Config(cursor, up_arrow, down_arrow, scroll_wrap, ctrl_c_interrupt)
end

"""
    MultiSelectConfig(; charset=:ascii, checked::String, unchecked::String, kwargs...)

Configure behavior for a multiple-selection menu via keyword arguments:

- `checked` is the string to print when an option has been selected.
  Defaults are "[X]" or "✓", depending on `charset`.
- `unchecked` is the string to print when an option has not been selected.
  Defaults are "[ ]" or "⬚", depending on `charset`.

All other keyword arguments are as described for [`TerminalMenus.Config`](@ref).
`checked` and `unchecked` are not printed automatically, and should be printed by
your `writeline` method.

!!! compat Julia 1.6
    `MultiSelectConfig` is available as of Julia 1.6. On older releases use the global `CONFIG`.
"""
function MultiSelectConfig(;
                           charset::Symbol = :ascii,
                           checked::String = "",
                           unchecked::String = "",
                           kwargs...)
    charset === :ascii || charset === :unicode || throw(ArgumentError("charset should be :ascii or :unicode, received $charset"))
    if isempty(checked)
        checked = charset === :ascii ? "[X]" : "✓"
    end
    if isempty(unchecked)
        unchecked = charset === :ascii ? "[ ]" : "⬚"
    end
    return MultiSelectConfig(Config(; charset=charset, kwargs...), checked, unchecked)
end



## Below is the old-style CONFIG interface, kept for backwards compatibility.
## Not recommended for any new menu types.

"""global menu configuration parameters"""
const CONFIG = Dict{Symbol,Union{Char,String,Bool}}()

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
                supress_output::Union{Nothing, Bool}=nothing,   # typo was documented, unfortunately
                ctrl_c_interrupt::Union{Nothing, Bool}=nothing)

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
    supress_output isa Bool   && (CONFIG[:supress_output] = supress_output)
    ctrl_c_interrupt isa Bool && (CONFIG[:ctrl_c_interrupt] = ctrl_c_interrupt)

    return nothing
end

# Set up defaults
config(charset=:ascii, scroll=:nowrap, supress_output=false, ctrl_c_interrupt=true)

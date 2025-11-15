# This file is a part of Julia. License is MIT: https://julialang.org/license

"""

    RadioMenu

A menu that allows a user to select a single option from a list.

# Sample Output

```julia-repl
julia> request(RadioMenu(options, pagesize=4))
Choose your favorite fruit:
^  grape
   strawberry
 > blueberry
v  peach
Your favorite fruit is blueberry!
```

"""
mutable struct RadioMenu{C} <: _ConfiguredMenu{C}
    options::Array{String,1}
    keybindings::Vector{Char}
    pagesize::Int
    pageoffset::Int
    selected::Union{Nothing, Int}
    on_cancel::Union{Nothing, Int}
    header::String
    config::C
end

RadioMenu(options, keybindings, pagesize, pageoffset, selected, config) =
    RadioMenu(options, keybindings, pagesize, pageoffset, selected, -1, "", config)

const default_radio_header = "[press: Enter=select, q=abort]"

"""

    RadioMenu(options::Vector{String}; pagesize::Int=10,
                                       on_cancel::Union{Nothing, Int}=-1,
                                       header::Union{String, Bool}=false,
                                       keybindings::Vector{Char}=Char[],
                                       kwargs...)

Create a RadioMenu object. Use `request(menu::RadioMenu)` to get user input.
`request()` returns an `Int` which is the index of the option selected by the
user.

# Arguments

  - `options::Vector{String}`: Options to be displayed
  - `pagesize::Int=10`: The number of options to be displayed at one time, the menu will scroll if length(options) > pagesize
  - `keybindings::Vector{Char}=Char[]`: Shortcuts to pick corresponding entry from `options`
  - `on_cancel::Union{Nothing, Int}=-1`: Value returned if aborted. Default is `-1` for backward compat. It is recommended to set `on_cancel=nothing` for consistency.
  - `header::Union{String, Bool}=false`: Header displayed above the menu. If `true`, the default header "[press: Enter=select, q=abort]" is used. If `false`, no header is displayed. A custom header can be provided as a `String`.

Any additional keyword arguments will be passed to [`TerminalMenus.Config`](@ref).

!!! compat "Julia 1.8"
    The `keybindings` argument requires Julia 1.8 or later.
"""
function RadioMenu(options::Array{String,1};
    on_cancel=-1, header=false, pagesize::Int=10, warn::Bool=true, keybindings::Vector{Char}=Char[], kwargs...)

    length(options) < 1 && error("RadioMenu must have at least one option")
    length(keybindings) in [0, length(options)] || error("RadioMenu must have either no keybindings, or one per option")

    # if pagesize is -1, use automatic paging
    pagesize = pagesize == -1 ? length(options) : pagesize
    # pagesize shouldn't be bigger than options
    pagesize = min(length(options), pagesize)
    # after other checks, pagesize must be greater than 1
    pagesize < 1 && error("pagesize must be >= 1")

    pageoffset = 0
    selected = -1 # none

    is_not_legacy = isnothing(on_cancel) || (header != false) || !isempty(kwargs)

    if header == true
         header = default_radio_header
    elseif header == false
        header = ""
    end

    if is_not_legacy
        RadioMenu(options, keybindings, pagesize, pageoffset, selected, on_cancel, header, Config(; kwargs...))
    else
        warn && Base.depwarn("Legacy `RadioMenu` interface is deprecated, set a configuration option such as `RadioMenu(options; charset=:ascii)` to trigger the new interface.", :RadioMenu)
        RadioMenu(options, keybindings, pagesize, pageoffset, selected, on_cancel, header, CONFIG)
    end
end



# AbstractMenu implementation functions
# See AbstractMenu.jl
#######################################

options(m::RadioMenu) = m.options

function pick(menu::RadioMenu, cursor::Int)
    menu.selected = cursor
    return true #break out of the menu
end

function writeline(buf::IOBuffer, menu::RadioMenu{Config}, idx::Int, iscursor::Bool)
    print(buf, replace(menu.options[idx], "\n" => "\\n"))
end

function keypress(m::RadioMenu, i::UInt32)
    isempty(m.keybindings) && return false
    i = findfirst(isequal(i), Int.(m.keybindings))
    isnothing(i) && return false
    m.selected = i
    return true
end

# Legacy interface
function writeLine(buf::IOBuffer, menu::RadioMenu{<:Dict}, idx::Int, cursor::Bool)
    # print a ">" on the selected entry
    cursor ? print(buf, menu.config[:cursor] ," ") : print(buf, "  ")

    print(buf, replace(menu.options[idx], "\n" => "\\n"))
end

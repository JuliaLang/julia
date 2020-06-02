# This file is a part of Julia. License is MIT: https://julialang.org/license
# This file tests the Julia 1.0-1.5 extension interface of TerminalMenus

using REPL: TerminalMenus

mutable struct OldRadioMenu <: TerminalMenus.AbstractMenu
    options::Array{String,1}
    pagesize::Int
    pageoffset::Int
    selected::Int
end


"""

    OldRadioMenu(options::Array{String,1}; pagesize::Int=10)

Create a OldRadioMenu object. Use `request(menu::OldRadioMenu)` to get user input.
`request()` returns an `Int` which is the index of the option selected by the
user.

# Arguments

  - `options::Array{String, 1}`: Options to be displayed
  - `pagesize::Int=10`: The number of options to be displayed at one time, the menu will scroll if length(options) > pagesize
"""
function OldRadioMenu(options::Array{String,1}; pagesize::Int=10)
    length(options) < 2 && error("OldRadioMenu must have at least two options")

    # if pagesize is -1, use automatic paging
    pagesize = pagesize == -1 ? length(options) : pagesize
    # pagesize shouldn't be bigger than options
    pagesize = min(length(options), pagesize)
    # after other checks, pagesize must be greater than 2
    pagesize < 2 && error("pagesize must be >= 2")

    pageoffset = 0
    selected = -1 # none

    OldRadioMenu(options, pagesize, pageoffset, selected)
end



# AbstractMenu implementation functions
# See AbstractMenu.jl
#######################################

TerminalMenus.options(m::OldRadioMenu) = m.options

TerminalMenus.cancel(m::OldRadioMenu) = m.selected = -1

function TerminalMenus.pick(menu::OldRadioMenu, cursor::Int)
    menu.selected = cursor
    return true #break out of the menu
end

function TerminalMenus.writeLine(buf::IOBuffer, menu::OldRadioMenu, idx::Int, cursor::Bool)
    # print a ">" on the selected entry
    cursor ? print(buf, TerminalMenus.CONFIG[:cursor] ," ") : print(buf, "  ")

    print(buf, replace(menu.options[idx], "\n" => "\\n"))
end

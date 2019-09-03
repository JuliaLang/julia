# This file is a part of Julia. License is MIT: https://julialang.org/license

"""

    RadioMenu

A menu that allows a user to select a single option from a list.

# Sample Output

```julia
julia> request(RadioMenu(options, pagesize=4))
Choose your favorite fruit:
^  grape
   strawberry
 > blueberry
v  peach
Your favorite fruit is blueberry!
```

"""
mutable struct RadioMenu <: AbstractMenu
    options::Array{String,1}
    pagesize::Int
    pageoffset::Int
    selected::Int
end


"""

    RadioMenu(options::Array{String,1}; pagesize::Int=10)

Create a RadioMenu object. Use `request(menu::RadioMenu)` to get user input.
`request()` returns an `Int` which is the index of the option selected by the
user.

# Arguments

  - `options::Array{String, 1}`: Options to be displayed
  - `pagesize::Int=10`: The number of options to be displayed at one time, the menu will scroll if length(options) > pagesize
"""
function RadioMenu(options::Array{String,1}; pagesize::Int=10)
    length(options) < 2 && error("RadioMenu must have at least two options")

    # if pagesize is -1, use automatic paging
    pagesize = pagesize == -1 ? length(options) : pagesize
    # pagesize shouldn't be bigger than options
    pagesize = min(length(options), pagesize)
    # after other checks, pagesize must be greater than 2
    pagesize < 2 && error("pagesize must be >= 2")

    pageoffset = 0
    selected = -1 # none

    RadioMenu(options, pagesize, pageoffset, selected)
end



# AbstractMenu implementation functions
# See AbstractMenu.jl
#######################################

options(m::RadioMenu) = m.options

cancel(m::RadioMenu) = m.selected = -1

function pick(menu::RadioMenu, cursor::Int)
    menu.selected = cursor
    return true #break out of the menu
end

function writeLine(buf::IOBuffer, menu::RadioMenu, idx::Int, cursor::Bool)
    # print a ">" on the selected entry
    cursor ? print(buf, CONFIG[:cursor] ," ") : print(buf, "  ")

    print(buf, replace(menu.options[idx], "\n" => "\\n"))
end

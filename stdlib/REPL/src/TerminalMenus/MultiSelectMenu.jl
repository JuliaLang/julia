# This file is a part of Julia. License is MIT: https://julialang.org/license

"""

    MultiSelectMenu

A menu that allows a user to select a multiple options from a list.

# Sample Output

```julia
julia> request(MultiSelectMenu(options))
Select the fruits you like:
[press: d=done, a=all, n=none]
   [ ] apple
 > [X] orange
   [X] grape
   [ ] strawberry
   [ ] blueberry
   [X] peach
   [ ] lemon
   [ ] lime
You like the following fruits:
  - orange
  - grape
  - peach
```

"""
mutable struct MultiSelectMenu <: AbstractMenu
    options::Array{String,1}
    pagesize::Int
    pageoffset::Int
    selected::Set{Int}
end


"""

    MultiSelectMenu(options::Array{String,1}; pagesize::Int=10)

Create a MultiSelectMenu object. Use `request(menu::MultiSelectMenu)` to get
user input. `request()` returns a `Set` containing the indices of options that
were selected by the user.

# Arguments

  - `options::Array{String, 1}`: Options to be displayed
  - `pagesize::Int=10`: The number of options to be displayed at one time, the menu will scroll if length(options) > pagesize
"""
function MultiSelectMenu(options::Array{String,1}; pagesize::Int=10)
    length(options) < 2 && error("MultiSelectMenu must have at least two options")

    # if pagesize is -1, use automatic paging
    pagesize = pagesize == -1 ? length(options) : pagesize
    # pagesize shouldn't be bigger than options
    pagesize = min(length(options), pagesize)
    # after other checks, pagesize must be greater than 2
    pagesize < 2 && error("pagesize must be >= 2")

    pageoffset = 0
    selected = Set{Int}() # none

    MultiSelectMenu(options, pagesize, pageoffset, selected)
end



# AbstractMenu implementation functions
# See AbstractMenu.jl
#######################################

header(m::MultiSelectMenu) = "[press: d=done, a=all, n=none]"

options(m::MultiSelectMenu) = m.options

cancel(m::MultiSelectMenu) = m.selected = Set{Int}()

# Do not exit menu when a user selects one of the options
function pick(menu::MultiSelectMenu, cursor::Int)
    if cursor in menu.selected
        delete!(menu.selected, cursor)
    else
        push!(menu.selected, cursor)
    end

    return false #break out of the menu
end

function writeLine(buf::IOBuffer, menu::MultiSelectMenu, idx::Int, cursor::Bool)
    # print a ">" on the selected entry
    cursor ? print(buf, CONFIG[:cursor]," ") : print(buf, "  ")
    if idx in menu.selected
        print(buf, CONFIG[:checked], " ")
    else
        print(buf, CONFIG[:unchecked], " ")
    end

    print(buf, replace(menu.options[idx], "\n" => "\\n"))
end

# d: Done, return from request
# a: Select all
# n: Deselect all
function keypress(menu::MultiSelectMenu, key::UInt32)
    if key == UInt32('d') || key == UInt32('D')
        return true # break
    elseif key == UInt32('a') || key == UInt32('A')
        menu.selected = Set(1:length(menu.options))
    elseif key == UInt32('n') || key == UInt32('N')
        menu.selected = Set{Int}()
    end
    false # don't break
end

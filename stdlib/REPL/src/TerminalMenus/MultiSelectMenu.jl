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
mutable struct MultiSelectMenu{C} <: _ConfiguredMenu{C}
    options::Array{String,1}
    pagesize::Int
    pageoffset::Int
    selected::Set{Int}
    config::C
end


"""

    MultiSelectMenu(options::Array{String,1}; pagesize::Int=10, selected=[], kwargs...)

Create a MultiSelectMenu object. Use `request(menu::MultiSelectMenu)` to get
user input. It returns a `Set` containing the indices of options that
were selected by the user.

# Arguments

  - `options::Array{String, 1}`: Options to be displayed
  - `pagesize::Int=10`: The number of options to be displayed at one time, the menu will scroll if length(options) > pagesize
  - `selected=[]`: pre-selected items. `i ∈ selected` means that `options[i]` is preselected.

Any additional keyword arguments will be passed to [`TerminalMenus.MultiSelectConfig`](@ref).
"""
function MultiSelectMenu(options::Array{String,1}; pagesize::Int=10, selected=Int[], warn::Bool=true, kwargs...)
    length(options) < 2 && error("MultiSelectMenu must have at least two options")

    # if pagesize is -1, use automatic paging
    pagesize = pagesize == -1 ? length(options) : pagesize
    # pagesize shouldn't be bigger than options
    pagesize = min(length(options), pagesize)
    # after other checks, pagesize must be greater than 2
    pagesize < 2 && error("pagesize must be >= 2")

    pageoffset = 0
    _selected = Set{Int}()
    for item in selected
        push!(_selected, item)
    end

    if !isempty(kwargs)
        MultiSelectMenu(options, pagesize, pageoffset, _selected, MultiSelectConfig(; kwargs...))
    else
        warn && Base.depwarn("Legacy `MultiSelectMenu` interface is deprecated, set a configuration option such as `MultiSelectMenu(options; charset=:ascii)` to trigger the new interface.", :MultiSelectMenu)
        MultiSelectMenu(options, pagesize, pageoffset, _selected, CONFIG)
    end

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

function writeline(buf::IOBuffer, menu::MultiSelectMenu{MultiSelectConfig}, idx::Int, iscursor::Bool)
    if idx in menu.selected
        print(buf, menu.config.checked, " ")
    else
        print(buf, menu.config.unchecked, " ")
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


## Legacy interface
function TerminalMenus.writeLine(buf::IOBuffer, menu::MultiSelectMenu{<:Dict}, idx::Int, cursor::Bool)
    # print a ">" on the selected entry
    cursor ? print(buf, menu.config[:cursor]," ") : print(buf, "  ")
    if idx in menu.selected
        print(buf, menu.config[:checked], " ")
    else
        print(buf, menu.config[:unchecked], " ")
    end

    print(buf, replace(menu.options[idx], "\n" => "\\n"))
end

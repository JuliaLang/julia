# This file is a part of Julia. License is MIT: https://julialang.org/license

"""

    MultiSelectMenu

A menu that allows a user to select a multiple options from a list.

# Sample Output

```julia-repl
julia> request(MultiSelectMenu(options))
Select the fruits you like:
[press: Enter=toggle, a=all, n=none, d=done, q=abort]
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
    selected::Union{Nothing, Set{Int}}
    on_cancel::Union{Nothing, Set{Int}}
    header::String
    config::C
end

const default_msm_header = "[press: Enter=toggle, a=all, n=none, d=done, q=abort]"

MultiSelectMenu(options, pagesize, pageoffset, selected, config) =
    MultiSelectMenu(options, pagesize, pageoffset, selected, Set{Int}(), default_msm_header, config)

"""

    MultiSelectMenu(options::Vector{String}; on_cancel=Set{Int}(), header=true, pagesize::Int=10, selected=[], kwargs...)

Create a MultiSelectMenu object. Use `request(menu::MultiSelectMenu)` to get
user input. It returns a `Set` containing the indices of options that
were selected by the user.

# Arguments

  - `options::Vector{String}`: Options to be displayed
  - `pagesize::Int=10`: The number of options to be displayed at one time, the menu will scroll if length(options) > pagesize
  - `selected=Set{Int}()`: pre-selected items. `i ∈ selected` means that `options[i]` is preselected.
  - `on_cancel::Union{Nothing, Set{Int}}=Set{Int}()`: Value returned if aborted. Default is empty set for backward compat. It is recommended to set `on_cancel=nothing` to be able to discriminate between "nothing selected" vs. "aborted".
  - `header::Union{String, Bool}`: Header displayed above menu. Default is `true`, producing "[press: Enter=toggle, a=all, n=none, d=done, q=abort]". `false`
results in no header. You can provide your own string.

Any additional keyword arguments will be passed to [`TerminalMenus.MultiSelectConfig`](@ref).

!!! compat "Julia 1.6"
    The `selected` argument requires Julia 1.6 or later.
"""
function MultiSelectMenu(options::Array{String,1};
    on_cancel=Set{Int}(), header=true, pagesize::Int=10, selected=Int[], warn::Bool=true, kwargs...)

    length(options) < 1 && error("MultiSelectMenu must have at least one option")

    # if pagesize is -1, use automatic paging
    pagesize = pagesize == -1 ? length(options) : pagesize
    # pagesize shouldn't be bigger than options
    pagesize = min(length(options), pagesize)
    # after other checks, pagesize must be at least 1
    pagesize < 1 && error("pagesize must be >= 1")

    pageoffset = 0
    _selected = Set{Int}()
    for item in selected
        push!(_selected, item)
    end

    is_not_legacy = isnothing(on_cancel) || (header != true)  || !isempty(kwargs)

    if header == true
        header = default_msm_header
    elseif header == false
        header = ""
    end

    if is_not_legacy
        MultiSelectMenu(options, pagesize, pageoffset, _selected, on_cancel, header, MultiSelectConfig(; kwargs...))
    else
        warn && Base.depwarn("Legacy `MultiSelectMenu` interface is deprecated, set a configuration option such as `MultiSelectMenu(options; charset=:ascii)` to trigger the new interface.", :MultiSelectMenu)
        MultiSelectMenu(options, pagesize, pageoffset, _selected, on_cancel, header, CONFIG)
    end

end



# AbstractMenu implementation functions
# See AbstractMenu.jl
#######################################

options(m::MultiSelectMenu) = m.options

cancel(m::MultiSelectMenu) = m.selected = m.on_cancel

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

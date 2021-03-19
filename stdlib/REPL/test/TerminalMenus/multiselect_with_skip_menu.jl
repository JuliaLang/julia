# This file is a part of Julia. License is MIT: https://julialang.org/license

# Like MultiSelect but adds `n`/`p` to move to next/previous
# unselected item and `N`/`P` to move to next/previous selected item.
mutable struct MultiSelectWithSkipMenu <: TerminalMenus._ConfiguredMenu{TerminalMenus.Config}
    options::Array{String,1}
    pagesize::Int
    pageoffset::Int
    selected::Set{Int}
    cursor::Base.RefValue{Int}
    config::TerminalMenus.MultiSelectConfig
end

function MultiSelectWithSkipMenu(options::Array{String,1}; pagesize::Int=10,
                                 selected=Int[], kwargs...)
    length(options) < 1 && error("MultiSelectWithSkipMenu must have at least one option")

    pagesize = pagesize == -1 ? length(options) : pagesize
    pagesize = min(length(options), pagesize)
    pagesize < 1 && error("pagesize must be >= 1")

    pageoffset = 0
    _selected = Set{Int}()
    for item in selected
        push!(_selected, item)
    end

    MultiSelectWithSkipMenu(options, pagesize, pageoffset, _selected,
                            Ref{Int}(1),
                            TerminalMenus.MultiSelectConfig(; kwargs...))
end

TerminalMenus.header(m::MultiSelectWithSkipMenu) = "[press: d=done, a=all, c=none, npNP=move with skip, $(length(m.selected)) items selected]"

TerminalMenus.options(m::MultiSelectWithSkipMenu) = m.options

TerminalMenus.cancel(m::MultiSelectWithSkipMenu) = m.selected = Set{Int}()

# Do not exit menu when a user selects one of the options
function TerminalMenus.pick(menu::MultiSelectWithSkipMenu, cursor::Int)
    if cursor in menu.selected
        delete!(menu.selected, cursor)
    else
        push!(menu.selected, cursor)
    end

    return false
end

function TerminalMenus.writeline(buf::IOBuffer,
                                 menu::MultiSelectWithSkipMenu,
                                 idx::Int, iscursor::Bool)
    if idx in menu.selected
        print(buf, menu.config.checked, " ")
    else
        print(buf, menu.config.unchecked, " ")
    end

    print(buf, replace(menu.options[idx], "\n" => "\\n"))
end

# d: Done, return from request
# a: Select all
# c: Deselect all
# n: Move to next unselected
# p: Move to previous unselected
# N: Move to next selected
# P: Move to previous selected
function TerminalMenus.keypress(menu::MultiSelectWithSkipMenu, key::UInt32)
    if key == UInt32('d') || key == UInt32('D')
        return true # break
    elseif key == UInt32('a') || key == UInt32('A')
        menu.selected = Set(1:length(menu.options))
    elseif key == UInt32('c') || key == UInt32('C')
        menu.selected = Set{Int}()
    elseif key == UInt32('n')
        move_cursor!(menu, 1, false)
    elseif key == UInt32('p')
        move_cursor!(menu, -1, false)
    elseif key == UInt32('N')
        move_cursor!(menu, 1, true)
    elseif key == UInt32('P')
        move_cursor!(menu, -1, true)
    end
    false # don't break
end

function move_cursor!(menu, direction, selected)
    c = menu.cursor[]
    while true
        c += direction
        if !(1 <= c <= length(menu.options))
            return
        end
        if (c in menu.selected) == selected
            break
        end
    end
    menu.cursor[] = c
    if menu.pageoffset >= c - 1
        menu.pageoffset = max(c - 2, 0)
    end
    if menu.pageoffset + menu.pagesize <= c
        menu.pageoffset = min(c + 1, length(menu.options)) - menu.pagesize
    end
end

# Intercept the `request` call to insert the cursor field.
function TerminalMenus.request(term::REPL.Terminals.TTYTerminal,
                               m::MultiSelectWithSkipMenu;
                               cursor::Int=1, kwargs...)
    m.cursor[] = cursor
    invoke(TerminalMenus.request, Tuple{REPL.Terminals.TTYTerminal,
                                        TerminalMenus.AbstractMenu},
           term, m; cursor=m.cursor, kwargs...)
end

# These tests are specifically designed to verify that a `RefValue`
# input to the AbstractMenu `request` function works as intended.
menu = MultiSelectWithSkipMenu(string.(1:5), selected=[2, 3])
buf = IOBuffer()
TerminalMenus.printmenu(buf, menu, 1; init=true)
@test occursin("2 items selected", String(take!(buf)))
@test simulate_input(Set([2, 3, 4]), menu, 'n', :enter, 'd')
buf = IOBuffer()
TerminalMenus.printmenu(buf, menu, 1; init=true)
@test occursin("3 items selected", String(take!(buf)))

menu = MultiSelectWithSkipMenu(string.(1:5), selected=[2, 3])
@test simulate_input(Set([2]), menu, 'P', :enter, 'd', cursor=5)

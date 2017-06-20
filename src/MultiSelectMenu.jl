type MultiSelectMenu <: AbstractMenu
    options::Array{String,1}
    pagesize::Int
    pageoffset::Int
    selected::Set{Int}
end

function MultiSelectMenu(options::Array{String,1}; pagesize=10)
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

header(m::MultiSelectMenu) = "[press: d=done, a=all, n=none]"
options(m::MultiSelectMenu) = m.options
cancel(m::MultiSelectMenu) = m.selected = Set{Int}()

"return true if done"
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
    cursor ? print(buf, "> ") : print(buf, "  ")
    idx in menu.selected ? print(buf, "[X] ") : print(buf, "[ ] ")

    print(buf, replace(menu.options[idx], "\n", "\\n"))
end

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

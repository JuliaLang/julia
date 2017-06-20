include("util.jl")

type RadioMenu <: AbstractMenu
    options::Array{String,1}
    pagesize::Int
    pageoffset::Int
    selected::Int
end

function RadioMenu(options::Array{String,1}; pagesize=10)
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

options(m::RadioMenu) = m.options
cancel(m::RadioMenu) = m.selected = -1

"return true if done"
function pick(menu::RadioMenu, cursor::Int)
    menu.selected = cursor
    return true #break out of the menu
end

function writeLine(buf::IOBuffer, menu::RadioMenu, idx::Int, cursor::Bool)
    # print a ">" on the selected entry
    cursor ? print(buf, "> ") : print(buf, "  ")

    print(buf, replace(menu.options[idx], "\n", "\\n"))
end

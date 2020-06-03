# This file is a part of Julia. License is MIT: https://julialang.org/license

using REPL.TerminalMenus, Test

mutable struct DynamicMenu <: TerminalMenus._ConfiguredMenu{TerminalMenus.Config}
    pagesize::Int
    pageoffset::Int
    selected::Int
    numopts::Int
    config::TerminalMenus.Config
end

TerminalMenus.numoptions(m::DynamicMenu) = m.numopts
function TerminalMenus.writeline(buf::IO, m::DynamicMenu, idx::Int, iscursor::Bool)
    print(buf, idx)
    iscursor && print(buf, '*')
end

matchback(str) = match(r"\e\[(\d+)A", str)

function linesplitter(str)
    strs = split(str, '\n')
    s1 = strs[1]
    m = matchback(s1)
    if m === nothing
        nback, startidx = 0, 1
    else
        nback = parse(Int, m.captures[1])
        startidx = m.offset+length(m.match)
    end
    strs[1] = s1[startidx:end]  # discard the portion that moves the terminal
    @test all(s->startswith(s, "\e[2K"), strs)
    return nback, replace.(map(s->s[5:end], strs), ('\r'=>"",))
end

io = IOBuffer()

menu = DynamicMenu(4, 0, -1, 8, TerminalMenus.Config())

cursor = 1
state = TerminalMenus.printmenu(io, menu, cursor; init=true)
str = String(take!(io))
@test count(isequal('\n'), str) == state
nback, strs = linesplitter(str)
@test nback == 0
@test strs == [" > 1*", "   2", "   3", "v  4"]

cursor = TerminalMenus.move_down!(menu, cursor)
@test cursor == 2
state = TerminalMenus.printmenu(io, menu, cursor; oldstate=state)
str = String(take!(io))
@test count(isequal('\n'), str) == state
nback, strs = linesplitter(str)
@test nback == 3
@test strs == ["   1", " > 2*", "   3", "v  4"]

menu.numopts = 3   # dynamically changing the number of options
state = TerminalMenus.printmenu(io, menu, cursor; oldstate=state)
str = String(take!(io))
nback, strs = linesplitter(str)
@test nback == 3
@test count(isequal('\n'), str) - parse(Int, matchback(strs[end]).captures[1]) == state
@test strs[1:end-1] == ["   1", " > 2*", "   3"]

menu.numopts = 6
state = TerminalMenus.printmenu(io, menu, cursor; oldstate=state)
str = String(take!(io))
nback, strs = linesplitter(str)
@test nback == 2
@test strs == ["   1", " > 2*", "   3", "v  4"]

cursor = TerminalMenus.move_down!(menu, cursor)
cursor = TerminalMenus.move_down!(menu, cursor)
state = TerminalMenus.printmenu(io, menu, cursor; oldstate=state)
str = String(take!(io))
nback, strs = linesplitter(str)
@test nback == 3
@test strs == ["^  2", "   3", " > 4*", "v  5"]

cursor = TerminalMenus.move_down!(menu, cursor)
state = TerminalMenus.printmenu(io, menu, cursor; oldstate=state)
str = String(take!(io))
nback, strs = linesplitter(str)
@test nback == 3
@test strs == ["^  3", "   4", " > 5*", "   6"]

cursor = TerminalMenus.move_down!(menu, cursor)
state = TerminalMenus.printmenu(io, menu, cursor; oldstate=state)
str = String(take!(io))
nback, strs = linesplitter(str)
@test nback == 3
@test strs == ["^  3", "   4", "   5", " > 6*"]

cursor = TerminalMenus.move_up!(menu, cursor)
state = TerminalMenus.printmenu(io, menu, cursor; oldstate=state)
str = String(take!(io))
nback, strs = linesplitter(str)
@test nback == 3
@test strs == ["^  3", "   4", " > 5*", "   6"]

cursor = TerminalMenus.page_up!(menu, cursor)
@test cursor == 1
state = TerminalMenus.printmenu(io, menu, cursor; oldstate=state)
str = String(take!(io))
nback, strs = linesplitter(str)
@test nback == 3
@test strs == [" > 1*", "   2", "   3", "v  4"]

cursor = TerminalMenus.move_down!(menu, cursor)
cursor = TerminalMenus.move_down!(menu, cursor)
cursor = TerminalMenus.move_down!(menu, cursor)
cursor = TerminalMenus.page_down!(menu, cursor)
@test cursor == menu.numopts
state = TerminalMenus.printmenu(io, menu, cursor; oldstate=state)
str = String(take!(io))
nback, strs = linesplitter(str)
@test nback == 3
@test strs == ["^  3", "   4", "   5", " > 6*"]

# This file is a part of Julia. License is MIT: https://julialang.org/license

# Check to make sure types are imported properly
@test RadioMenu <: TerminalMenus.AbstractMenu

# Invalid Menu Params
@test_throws ErrorException RadioMenu(["one"])
@test_throws ErrorException RadioMenu(["one", "two", "three"], pagesize=1)

# Constructor
@test RadioMenu(["one", "two", "three"]).pagesize == 3
@test RadioMenu(string.(1:30), pagesize=-1).pagesize == 30
@test RadioMenu(string.(1:4), pagesize=10).pagesize == 4
@test RadioMenu(string.(1:100)).pagesize == 10

radio_menu = RadioMenu(string.(1:20))
@test TerminalMenus.options(radio_menu) == string.(1:20)
radio_menu.selected = 2
TerminalMenus.cancel(radio_menu)
@test radio_menu.selected == -1
@test TerminalMenus.header(radio_menu) == ""

# Output
for kws in ((charset=:ascii,),
            (charset=:unicode,),
            (cursor='@',))
    local radio_menu
    radio_menu = RadioMenu(string.(1:10); kws...)
    c = isdefined(kws, :cursor) ? kws.cursor : (kws.charset === :ascii ? '>' : '→')
    buf = IOBuffer()
    TerminalMenus.writeline(buf, radio_menu, 1, true)
    @test String(take!(buf)) == "1"
    TerminalMenus.printmenu(buf, radio_menu, 1; init=true)
    @test startswith(String(take!(buf)), "\e[2K $c 1")
    TerminalMenus.printmenu(buf, radio_menu, 2; init=true)
    @test startswith(String(take!(buf)), string("\e[2K   1\r\n\e[2K $c 2"))
end

# Test using stdin
radio_menu = RadioMenu(string.(1:10))
@test simulate_input(3, radio_menu, :down, :down, :enter)

# This file is a part of Julia. License is MIT: https://julialang.org/license

# Check to make sure types are imported properly
@test MultiSelectMenu <: TerminalMenus.AbstractMenu

# Invalid Menu Params
@test_throws ErrorException MultiSelectMenu(["one"])
@test_throws ErrorException MultiSelectMenu(["one", "two", "three"], pagesize=1)

# Constructor
@test MultiSelectMenu(["one", "two", "three"]).pagesize == 3
@test MultiSelectMenu(string.(1:30), pagesize=-1).pagesize == 30
@test MultiSelectMenu(string.(1:4), pagesize=10).pagesize == 4
@test MultiSelectMenu(string.(1:100)).pagesize == 10

multi_menu = MultiSelectMenu(string.(1:20))
@test TerminalMenus.options(multi_menu) == string.(1:20)
@test TerminalMenus.header(multi_menu) == "[press: d=done, a=all, n=none]"

# Output
TerminalMenus.config() # Use default chars
CONFIG = TerminalMenus.CONFIG

multi_menu = MultiSelectMenu(string.(1:10))
buf = IOBuffer()
TerminalMenus.writeline(buf, multi_menu, 1, true, TerminalMenus.Indicators('@',"c","u"))
@test String(take!(buf)) == "@ u 1"
TerminalMenus.config(cursor='+')
TerminalMenus.printmenu(buf, multi_menu, 1; init=true)
@test startswith(String(take!(buf)), string("\e[2K + ", CONFIG[:unchecked], " 1"))
TerminalMenus.config(charset=:unicode)
push!(multi_menu.selected, 1)
TerminalMenus.printmenu(buf, multi_menu, 2; init=true)
@test startswith(String(take!(buf)), string("\e[2K   ", CONFIG[:checked], " 1\r\n\e[2K ", CONFIG[:cursor], " ", CONFIG[:unchecked], " 2"))

# Test SDTIN
multi_menu = MultiSelectMenu(string.(1:10))
CONFIG[:suppress_output] = true
@test simulate_input(Set([1,2]), multi_menu, :enter, :down, :enter, 'd')
CONFIG[:suppress_output] = false

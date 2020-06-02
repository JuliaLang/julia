# This file is a part of Julia. License is MIT: https://julialang.org/license
# This file tests the Julia 1.0-1.5 extension interface of TerminalMenus

include("OldMultiSelectMenu.jl")

# Check to make sure types are imported properly
@test OldMultiSelectMenu <: TerminalMenus.AbstractMenu

# Invalid Menu Params
@test_throws ErrorException OldMultiSelectMenu(["one"])
@test_throws ErrorException OldMultiSelectMenu(["one", "two", "three"], pagesize=1)

# Constructor
@test OldMultiSelectMenu(["one", "two", "three"]).pagesize == 3
@test OldMultiSelectMenu(string.(1:30), pagesize=-1).pagesize == 30
@test OldMultiSelectMenu(string.(1:4), pagesize=10).pagesize == 4
@test OldMultiSelectMenu(string.(1:100)).pagesize == 10

multi_menu = OldMultiSelectMenu(string.(1:20))
@test TerminalMenus.options(multi_menu) == string.(1:20)
@test TerminalMenus.header(multi_menu) == "[press: d=done, a=all, n=none]"

# Output
TerminalMenus.config() # Use default chars
CONFIG = TerminalMenus.CONFIG

multi_menu = OldMultiSelectMenu(string.(1:10))
buf = IOBuffer()
TerminalMenus.writeLine(buf, multi_menu, 1, true)
@test String(take!(buf)) == string(CONFIG[:cursor], " ", CONFIG[:unchecked], " 1")
TerminalMenus.config(cursor='+')
TerminalMenus.writeLine(buf, multi_menu, 1, true)
@test String(take!(buf)) == string("+ ", CONFIG[:unchecked], " 1")
TerminalMenus.config(charset=:unicode)
TerminalMenus.writeLine(buf, multi_menu, 1, true)
@test String(take!(buf)) == string(CONFIG[:cursor], " ", CONFIG[:unchecked], " 1")

# Test SDTIN
multi_menu = OldMultiSelectMenu(string.(1:10))
@test simulate_input(Set([1,2]), multi_menu, :enter, :down, :enter, 'd')

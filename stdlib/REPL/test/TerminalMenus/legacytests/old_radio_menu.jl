# This file is a part of Julia. License is MIT: https://julialang.org/license
# This file tests the Julia 1.0-1.5 extension interface of TerminalMenus

include("OldRadioMenu.jl")

# Check to make sure types are imported properly
@test OldRadioMenu <: TerminalMenus.AbstractMenu

# Invalid Menu Params
@test_throws ErrorException OldRadioMenu(["one"])
@test_throws ErrorException OldRadioMenu(["one", "two", "three"], pagesize=1)

# Constructor
@test OldRadioMenu(["one", "two", "three"]).pagesize == 3
@test OldRadioMenu(string.(1:30), pagesize=-1).pagesize == 30
@test OldRadioMenu(string.(1:4), pagesize=10).pagesize == 4
@test OldRadioMenu(string.(1:100)).pagesize == 10

radio_menu = OldRadioMenu(string.(1:20))
@test TerminalMenus.options(radio_menu) == string.(1:20)
radio_menu.selected = 2
TerminalMenus.cancel(radio_menu)
@test radio_menu.selected == -1
@test TerminalMenus.header(radio_menu) == ""

# Output
TerminalMenus.config() # Use default chars
CONFIG = TerminalMenus.CONFIG

radio_menu = OldRadioMenu(string.(1:10))
buf = IOBuffer()
TerminalMenus.writeLine(buf, radio_menu, 1, true)
@test String(take!(buf)) == string(CONFIG[:cursor], " 1")
TerminalMenus.config(cursor='+')
TerminalMenus.writeLine(buf, radio_menu, 1, true)
@test String(take!(buf)) == "+ 1"
TerminalMenus.config(charset=:unicode)
TerminalMenus.writeLine(buf, radio_menu, 1, true)
@test String(take!(buf)) == string(CONFIG[:cursor], " 1")

# Test using stdin
radio_menu = OldRadioMenu(string.(1:10))
@test simulate_input(3, radio_menu, :down, :down, :enter)

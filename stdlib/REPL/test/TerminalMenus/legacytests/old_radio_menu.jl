# This file is a part of Julia. License is MIT: https://julialang.org/license
# This file tests the legacy Julia 1.0-1.5 extension interface of TerminalMenus
# They are run with `warn=false` to avoid triggering test failures.

# Check to make sure types are imported properly
@test RadioMenu <: TerminalMenus.AbstractMenu

# Constructor
@test RadioMenu(["one", "two", "three"], warn=false).pagesize == 3
@test RadioMenu(string.(1:30), pagesize=-1, warn=false).pagesize == 30
@test RadioMenu(string.(1:4), pagesize=10, warn=false).pagesize == 4
@test RadioMenu(string.(1:100), warn=false).pagesize == 10

radio_menu = RadioMenu(string.(1:20), warn=false)
@test TerminalMenus.options(radio_menu) == string.(1:20)
radio_menu.selected = 2
TerminalMenus.cancel(radio_menu)
@test radio_menu.selected == -1
@test TerminalMenus.header(radio_menu) == ""

# Output
TerminalMenus.config() # Use default chars
CONFIG = TerminalMenus.CONFIG

radio_menu = RadioMenu(string.(1:10), warn=false)
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
radio_menu = RadioMenu(string.(1:10), warn=false)
@test simulate_input(3, radio_menu, :down, :down, :enter)
radio_menu = RadioMenu(["single option"], warn=false)
@test simulate_input(1, radio_menu, :up, :up, :down, :up, :enter)
radio_menu = RadioMenu(string.(1:3), pagesize=1, warn=false)
@test simulate_input(3, radio_menu, :down, :down, :down, :down, :enter)

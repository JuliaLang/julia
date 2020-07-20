# This file is a part of Julia. License is MIT: https://julialang.org/license
# This file tests the legacy Julia 1.0-1.5 extension interface of TerminalMenus
# They are run with `warn=false` to avoid triggering test failures.

# Check to make sure types are imported properly
@test MultiSelectMenu <: TerminalMenus.AbstractMenu

# Constructor
@test MultiSelectMenu(["one", "two", "three"], warn=false).pagesize == 3
@test MultiSelectMenu(string.(1:30), pagesize=-1, warn=false).pagesize == 30
@test MultiSelectMenu(string.(1:4), pagesize=10, warn=false).pagesize == 4
@test MultiSelectMenu(string.(1:100), warn=false).pagesize == 10

multi_menu = MultiSelectMenu(string.(1:20), warn=false)
@test TerminalMenus.options(multi_menu) == string.(1:20)
@test TerminalMenus.header(multi_menu) == "[press: d=done, a=all, n=none]"

# Output
TerminalMenus.config() # Use default chars
CONFIG = TerminalMenus.CONFIG

multi_menu = MultiSelectMenu(string.(1:10), warn=false)
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
multi_menu = MultiSelectMenu(string.(1:10), warn=false)
@test simulate_input(Set([1,2]), multi_menu, :enter, :down, :enter, 'd')
multi_menu = MultiSelectMenu(["single option"], warn=false)
@test simulate_input(Set([1]), multi_menu, :up, :up, :down, :enter, 'd')

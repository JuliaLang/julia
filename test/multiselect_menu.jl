# Check to make sure types are imported properly
@test MultiSelectMenu <: TerminalMenus.AbstractMenu

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

term_width = 100
multi_menu = MultiSelectMenu(string.(1:10))
buf = IOBuffer()
TerminalMenus.writeLine(buf, multi_menu, 1, true, term_width)
@test String(take!(buf)) == string(CONFIG[:cursor], " ", CONFIG[:unchecked], " 1")
TerminalMenus.config(cursor='+')
TerminalMenus.writeLine(buf, multi_menu, 1, true, term_width)
@test String(take!(buf)) == string("+ ", CONFIG[:unchecked], " 1")
TerminalMenus.config(charset=:unicode)
TerminalMenus.writeLine(buf, multi_menu, 1, true, term_width)
@test String(take!(buf)) == string(CONFIG[:cursor], " ", CONFIG[:unchecked], " 1")

# Test SDTIN
multi_menu = MultiSelectMenu(string.(1:10))
@test simulateInput(Set([1,2]), multi_menu, :enter, :enter, 'd')
multi_menu = MultiSelectMenu(["single option"])
@test simulateInput(Set([1]), multi_menu, :up, :up, :down, :enter, 'd')

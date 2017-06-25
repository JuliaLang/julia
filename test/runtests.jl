using TerminalMenus
using Base.Test

include("radio_menu.jl")
include("multiselect_menu.jl")

# Other test

# scroll must only accept symbols
@test_throws TerminalMenus.config(scroll=true)
# :foo is not a valid scroll option
@test_throws TerminalMenus.config(scroll=:foo)
# Test scroll wrap
TerminalMenus.config(scroll=:wrap)
@test TerminalMenus.CONFIG[:scroll_wrap] == true
# Updating some params shouldn't change other ones
TerminalMenus.config(charset=:ascii)
@test TerminalMenus.CONFIG[:scroll_wrap] == true
TerminalMenus.config(scroll=:nowrap)
@test TerminalMenus.CONFIG[:scroll_wrap] == false

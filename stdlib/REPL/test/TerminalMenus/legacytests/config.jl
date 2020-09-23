# This file is a part of Julia. License is MIT: https://julialang.org/license
# This file tests the Julia 1.0-1.5 extension interface of TerminalMenus

# scroll must only accept symbols
@test_throws TypeError TerminalMenus.config(scroll=true)
# :foo is not a valid scroll option
@test_throws ArgumentError TerminalMenus.config(scroll=:foo)
# Test scroll wrap
TerminalMenus.config(scroll=:wrap)
@test TerminalMenus.CONFIG[:scroll_wrap] == true
# Updating some params shouldn't change other ones
TerminalMenus.config(charset=:ascii)
@test TerminalMenus.CONFIG[:scroll_wrap] == true
TerminalMenus.config(scroll=:nowrap)
@test TerminalMenus.CONFIG[:scroll_wrap] == false

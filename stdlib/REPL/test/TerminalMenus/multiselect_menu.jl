# This file is a part of Julia. License is MIT: https://julialang.org/license

# This file tests the new Julia 1.6+ extension interface of TerminalMenus
# To trigger the new interface, at least one configuration keyword argument must be supplied.

# Check to make sure types are imported properly
@test MultiSelectMenu{TerminalMenus.MultiSelectConfig} <: TerminalMenus.ConfiguredMenu  # TODO Julia 2.0: delete parameter

# Invalid Menu Params
@test_throws ErrorException MultiSelectMenu(["one"], charset=:ascii)
@test_throws ErrorException MultiSelectMenu(["one", "two", "three"], pagesize=1, charset=:ascii)

# Constructor
@test MultiSelectMenu(["one", "two", "three"], charset=:ascii).pagesize == 3
@test MultiSelectMenu(string.(1:30), pagesize=-1, charset=:ascii).pagesize == 30
@test MultiSelectMenu(string.(1:4), pagesize=10, charset=:ascii).pagesize == 4
@test MultiSelectMenu(string.(1:100), charset=:ascii).pagesize == 10

multi_menu = MultiSelectMenu(string.(1:20), charset=:ascii)
@test TerminalMenus.options(multi_menu) == string.(1:20)
@test TerminalMenus.header(multi_menu) == "[press: d=done, a=all, n=none]"

# Output
for kws in ((charset=:ascii,),
            (charset=:unicode,),
            (cursor='@', checked="c", unchecked="u",))
    local multi_menu
    multi_menu = MultiSelectMenu(string.(1:10); kws...)
    cur = isdefined(kws, :cursor) ? kws.cursor : (kws.charset === :ascii ? '>' : '→')
    chk = isdefined(kws, :checked) ? kws.checked : (kws.charset === :ascii ? "[X]" : "✓")
    uck = isdefined(kws, :unchecked) ? kws.unchecked : (kws.charset === :ascii ? "[ ]" : "⬚")

    buf = IOBuffer()
    TerminalMenus.writeline(buf, multi_menu, 1, true)
    @test String(take!(buf)) == "$uck 1"
    TerminalMenus.printmenu(buf, multi_menu, 1; init=true)
    @test startswith(String(take!(buf)), string("\e[2K $cur $uck 1"))
    push!(multi_menu.selected, 1)
    TerminalMenus.printmenu(buf, multi_menu, 2; init=true)
    @test startswith(String(take!(buf)), string("\e[2K   $chk 1\r\n\e[2K $cur $uck 2"))
end

# Test SDTIN
multi_menu = MultiSelectMenu(string.(1:10), charset=:ascii)
@test simulate_input(Set([1,2]), multi_menu, :enter, :down, :enter, 'd')

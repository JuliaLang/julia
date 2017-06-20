using TerminalMenus
using Base.Test

# For now, just check to make sure types are imported properly
@test RadioMenu <: TerminalMenus.AbstractMenu

# Invalid Menu Params
@test_throws Exception RadioMenu(["one"])
@test_throws Exception RadioMenu(["one", "two", "three"], pagesize=1)

# Constructor
@test RadioMenu(["one", "two", "three"]).pagesize == 3
@test RadioMenu(string.(1:30), pagesize=-1).pagesize == 30
@test RadioMenu(string.(1:4), pagesize=10).pagesize == 4
@test RadioMenu(string.(1:100)).pagesize == 10
@test RadioMenu(string.(1:100), default=3).pageoffset == 2
@test RadioMenu(string.(1:15), default=20).pageoffset == 5

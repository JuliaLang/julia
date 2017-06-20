using TerminalMenus
using Base.Test

# For now, just check to make sure types are imported properly
@test RadioMenu <: TerminalMenus.AbstractMenu

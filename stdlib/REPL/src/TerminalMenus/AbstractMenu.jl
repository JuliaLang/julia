# This file is a part of Julia. License is MIT: https://julialang.org/license

"""

    AbstractMenu

The supertype for all Menu types.


# Functions

The following functions can be called on all <:AbstractMenu types.
Details can be found in

## Exported

  - `request(m::AbstractMenu)`
  - `request(msg::AbstractString, m::AbstractMenu)`

## Hidden

  - `printmenu(m::AbstractMenu, cursor::Int; init::Bool=false, oldstate=nothing)`


# Subtypes

All subtypes must be mutable, and must contain the fields `pagesize::Int` and
`pageoffset::Int`. They must also implement the following functions.

## Necessary Functions

These functions must be implemented for all subtypes of AbstractMenu.

  - `pick(m::AbstractMenu, cursor::Int)`
  - `cancel(m::AbstractMenu)`
  - `options(m::AbstractMenu)`
  - `writeline(buf::IOBuffer, m::AbstractMenu, idx::Int, cursor)`

If `m` does not have a field called `selected`, then you must also implement `selected(m)`.

## Optional Functions

These functions do not need to be implemented for all AbstractMenu
subtypes.

  - `header(m::AbstractMenu)`
  - `keypress(m::AbstractMenu, i::UInt32)`
  - `numoptions(m::AbstractMenu)`
  - `selected(m::AbstractMenu)`

"""
abstract type AbstractMenu end



# NECESSARY FUNCTIONS
# These functions must be implemented for all subtypes of AbstractMenu
######################################################################

"""
    pick(m::AbstractMenu, cursor::Int)

Defines what happens when a user presses the Enter key while the menu is open.
If `true` is returned, `request()` will exit.
`cursor` indexes the position of the selection.
"""
pick(m::AbstractMenu, cursor::Int) = error("unimplemented")

"""
    cancel(m::AbstractMenu)

Define what happens when a user cancels ('q' or ctrl-c) a menu.
`request()` will always exit after calling this function.
"""
cancel(m::AbstractMenu) = error("unimplemented")

"""
    options(m::AbstractMenu)

Return a list of strings to be displayed as options in the current page.

Alternatively, implement `numoptions`, in which case `options` is not needed.
"""
options(m::AbstractMenu) = error("unimplemented")

"""
    writeline(buf::IOBuffer, m::AbstractMenu, idx::Int, cursor::Bool, indicators::Indicators)

Write the option at index `idx` to the buffer. If `indicators !== nothing`, the current line
corresponds to the cursor position. The method is responsible for displaying visual indicator(s)
about the state of this menu item; the configured characters are returned in `indicators`
in fields with the following names:
    `cursor::Char`: the character used to indicate the cursor position
    `checked::String`: a string used to indicate this option has been marked
    `unchecked::String`: a string used to indicate this option has not been marked
The latter two are relevant only for menus that support multiple selection.

!!! compat "Julia 1.6"
    `writeline` requires Julia 1.6 or higher.

    On older versions of Julia, this was
        `writeLine(buf::IOBuffer, m::AbstractMenu, idx, cursor::Bool)`
    and the indicators can be obtained from `TerminalMenus.CONFIG`, a Dict indexed by `Symbol`
    keys with the same names as the fields of `indicators`.

    This older function is supported on all Julia 1.x versions but will be dropped in Julia 2.0.
"""
function writeline(buf::IOBuffer, m::AbstractMenu, idx::Int, cursor::Bool, indicators)
    # error("unimplemented")    # TODO: use this in Julia 2.0
    writeLine(buf, m, idx, cursor)
end


# OPTIONAL FUNCTIONS
# These functions do not need to be implemented for all menu types
##################################################################

"""
    header(m::AbstractMenu)

Displays the header above the menu when it is rendered to the screen.
Defaults to "".
"""
header(m::AbstractMenu) = ""

"""
    keypress(m::AbstractMenu, i::UInt32)

Send any non-standard keypress event to this function.
If `true` is returned, `request()` will exit.
Defaults to `false`.
"""
keypress(m::AbstractMenu, i::UInt32) = false

"""
    numoptions(m::AbstractMenu)

Return the number of options in menu `m`. Defaults to `length(options(m))`.
"""
numoptions(m::AbstractMenu) = length(options(m))

"""
    selected(m::AbstractMenu)

Return information about the user-selected option. Defaults to `m.selected`.
"""
selected(m::AbstractMenu) = m.selected

"""
    request(m::AbstractMenu; cursor=1)

Display the menu and enter interactive mode. `cursor` indicates the initial item
for the cursor.

Returns `selected(m)` which varies based on menu type.
"""
request(m::AbstractMenu; kwargs...) = request(terminal, m; kwargs...)

function request(term::REPL.Terminals.TTYTerminal, m::AbstractMenu; cursor::Int=1)
    menu_header = header(m)
    !CONFIG[:suppress_output] && menu_header != "" && println(term.out_stream, menu_header)

    state = printmenu(term.out_stream, m, cursor, init=true)

    raw_mode_enabled = REPL.Terminals.raw!(term, true)
    raw_mode_enabled && print(term.out_stream, "\x1b[?25l") # hide the cursor

    try
        while true
            lastoption = numoptions(m)
            c = readkey(term.in_stream)

            if c == Int(ARROW_UP)
                if cursor > 1
                    cursor -= 1 # move selection up
                    if cursor < (2+m.pageoffset) && m.pageoffset > 0
                        m.pageoffset -= 1 # scroll page up
                    end
                elseif CONFIG[:scroll_wrap] # wrap to bottom
                    cursor = lastoption
                    m.pageoffset = lastoption - m.pagesize
                end

            elseif c == Int(ARROW_DOWN)
                if cursor < lastoption
                    cursor += 1 # move selection down
                    if m.pagesize + m.pageoffset <= cursor < lastoption
                        m.pageoffset += 1 # scroll page down
                    end
                elseif CONFIG[:scroll_wrap] # wrap to top
                    cursor = 1
                    m.pageoffset = 0
                end

            elseif c == Int(PAGE_UP)
                # If we're at the bottom, move the page 1 less to move the cursor up from
                # the bottom entry, since we try to avoid putting the cursor at bounds.
                m.pageoffset -= m.pagesize - (cursor == lastoption ? 1 : 0)
                m.pageoffset = max(m.pageoffset, 0)
                cursor -= m.pagesize
                cursor = max(cursor, 1)

            elseif c == Int(PAGE_DOWN)
                m.pageoffset += m.pagesize - (cursor == 1 ? 1 : 0)
                m.pageoffset = min(m.pageoffset, lastoption - m.pagesize)
                cursor += m.pagesize
                cursor = min(cursor, lastoption)

            elseif c == Int(HOME_KEY)
                cursor = 1
                m.pageoffset = 0

            elseif c == Int(END_KEY)
                cursor = lastoption
                m.pageoffset = lastoption - m.pagesize

            elseif c == 13 # <enter>
                # will break if pick returns true
                pick(m, cursor) && break

            elseif c == UInt32('q')
                cancel(m)
                break

            elseif c == 3 # ctrl-c
                cancel(m)
                CONFIG[:ctrl_c_interrupt] ? throw(InterruptException()) : break

            else
                # will break if keypress returns true
                keypress(m, c) && break
            end

            state = printmenu(term.out_stream, m, cursor, oldstate=state)
        end
    finally # always disable raw mode
        if raw_mode_enabled
            print(term.out_stream, "\x1b[?25h") # unhide cursor
            REPL.Terminals.raw!(term, false)
        end
    end
    println(term.out_stream)

    return selected(m)
end


"""
    request([term,] msg::AbstractString, m::AbstractMenu)

Shorthand for `println(msg); request(m)`.
"""
request(msg::AbstractString, m::AbstractMenu) = request(terminal, msg, m)

function request(term::REPL.Terminals.TTYTerminal, msg::AbstractString, m::AbstractMenu)
    println(term.out_stream, msg)
    request(term, m)
end


"""
    printmenu(out, m::AbstractMenu, cursor::Int; init::Bool=false, oldstate=nothing) -> newstate

Display the state of a menu. `init=true` causes `m.pageoffset` to be initialized to zero,
and starts printing at the current cursor location; when `init` is false, the terminal will
preserve the current setting of `m.pageoffset` and overwrite the previous display.
Returns `newstate`, which can be passed in as `oldstate` on the next call to allow accurate
overwriting of the previous display.

!!! compat "Julia 1.6"
    `printmenu` requires Julia 1.6 or higher.

    On older versions of Julia, this was called `printMenu` and it lacked the `state` argument/return value.
    This older function is supported on all Julia 1.x versions but will be dropped in Julia 2.0.
"""
function printmenu(out, m::AbstractMenu, cursor::Int; oldstate=nothing, init::Bool=false)
    # TODO Julia 2.0?: get rid of `init` and just use `oldstate`
    CONFIG[:suppress_output] && return

    buf = IOBuffer()
    indicators = Indicators()
    lastoption = numoptions(m)
    ncleared = oldstate === nothing ? m.pagesize-1 : oldstate

    if init
        m.pageoffset = 0
    else
        print(buf, "\x1b[999D\x1b[$(ncleared)A")   # move left 999 spaces and up `ncleared` lines
    end

    firstline = m.pageoffset+1
    lastline = min(m.pagesize+m.pageoffset, lastoption)

    for i in firstline:lastline
        # clearline
        print(buf, "\x1b[2K")

        if i == firstline && m.pageoffset > 0
            print(buf, CONFIG[:up_arrow])
        elseif i == lastline && i != lastoption
            print(buf, CONFIG[:down_arrow])
        else
            print(buf, " ")
        end

        writeline(buf, m, i, i == cursor, indicators)

        i != lastline && print(buf, "\r\n")
    end

    newstate = lastline-firstline  # final line doesn't have `\n`
    if newstate < ncleared && oldstate !== nothing
        # we printed fewer lines than last time. Erase the leftovers.
        for i = newstate+1:ncleared
            print(buf, "\r\n\x1b[2K")
        end
        print(buf, "\x1b[$(ncleared-newstate)A")
    end

    print(out, String(take!(buf)))

    return newstate
end

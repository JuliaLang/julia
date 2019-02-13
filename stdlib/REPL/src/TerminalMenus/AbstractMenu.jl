# This file is a part of Julia. License is MIT: https://julialang.org/license

"""

    AbstractMenu

The supertype for all Menu types.
See AbstractMenu.jl for descriptions of functions mentioned in this
doc string.


# Functions

The following functions can be called on all <:AbstractMenu types.
Details can be found in

## Exported

  - `request(m::AbstractMenu)`
  - `request(msg::AbstractString, m::AbstractMenu)`

## Hidden

  - `printMenu(m::AbstractMenu, cursor::Int; init::Bool=false)`


# Subtypes

All subtypes must contain the fields `pagesize::Int` and
`pageoffset::Int`. They must also implement the following functions.

## Necessary Functions

These functions must be implemented for all subtypes of AbstractMenu.

  - `pick(m::AbstractMenu, cursor::Int)`
  - `cancel(m::AbstractMenu)`
  - `options(m::AbstractMenu)`
  - `writeLine(buf::IOBuffer, m::AbstractMenu, idx::Int, cur::Bool)`

## Optional Functions

These functions do not need to be implemented for all AbstractMenu
subtypes.

  - `header(m::AbstractMenu)`
  - `keypress(m::AbstractMenu, i::UInt32)`

"""
abstract type AbstractMenu end



# NECESSARY FUNCTIONS
# These functions must be implemented for all subtypes of AbstractMenu
######################################################################

"""
    pick(m::AbstractMenu, cursor::Int)

Defines what happens when a user presses the Enter key while the menu is open.
If `true` is returned, `request()` will exit.
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
"""
options(m::AbstractMenu) = error("unimplemented")

"""
    writeLine(buf::IOBuffer, m::AbstractMenu, idx::Int, cur::Bool)

Write the option at index `idx` to the buffer. If cursor is `true` display the cursor.
"""
function writeLine(buf::IOBuffer, m::AbstractMenu, idx::Int, cur::Bool)
    error("unimplemented")
end


# OPTIONAL FUNCTIONS
# These functions do not need to be implemented for all menu types
##################################################################

"""
    header(m::AbstractMenu)

Displays the header above the menu when it is rendered to the screen.
"""
header(m::AbstractMenu) = ""

"""
    keypress(m::AbstractMenu, i::UInt32)

Send any non-standard keypress event to this function.
If `true` is returned, `request()` will exit.
"""
keypress(m::AbstractMenu, i::UInt32) = false




"""
    request(m::AbstractMenu)

Display the menu and enter interactive mode. Returns `m.selected` which
varies based on menu type.
"""
request(m::AbstractMenu) = request(terminal, m)

function request(term::REPL.Terminals.TTYTerminal, m::AbstractMenu)
    cursor = 1

    menu_header = header(m)
    !CONFIG[:supress_output] && menu_header != "" && println(term.out_stream, menu_header)

    printMenu(term.out_stream, m, cursor, init=true)

    raw_mode_enabled = enableRawMode(term)
    raw_mode_enabled && print(term.out_stream, "\x1b[?25l") # hide the cursor

    lastoption = length(options(m))
    try
        while true
            c = readKey(term.in_stream)

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

            printMenu(term.out_stream, m, cursor)
        end
    finally # always disable raw mode
        if raw_mode_enabled
            print(term.out_stream, "\x1b[?25h") # unhide cursor
            disableRawMode(term)
        end
    end
    println(term.out_stream)

    return m.selected
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
    printMenu(out, m::AbstractMenu, cursor::Int; init::Bool=false)

Display the state of a menu.
"""
function printMenu(out, m::AbstractMenu, cursor::Int; init::Bool=false)
    CONFIG[:supress_output] && return

    buf = IOBuffer()

    lines = m.pagesize-1

    if init
        m.pageoffset = 0
    else
        # Move the cursor to the beginning of where it should print
        print(buf, "\x1b[999D\x1b[$(lines)A")
    end

    firstline = m.pageoffset+1
    lastline = m.pagesize+m.pageoffset

    for i in firstline:lastline
        # clearline
        print(buf, "\x1b[2K")

        if i == firstline && m.pageoffset > 0
            print(buf, CONFIG[:up_arrow])
        elseif i == lastline && i != length(options(m))
            print(buf, CONFIG[:down_arrow])
        else
            print(buf, " ")
        end

        writeLine(buf, m, i, i == cursor)

        i != lastline && print(buf, "\r\n")
    end

    print(out, String(take!(buf)))
end

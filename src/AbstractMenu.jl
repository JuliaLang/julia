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

All subtypes must contain the feilds `pagesize::Int` and
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

# This function must be implemented for all menu types. It defines what
#   happens when a user presses the Enter key while the menu is open.
# If this function returns true, `request()` will exit.
pick(m::AbstractMenu, cursor::Int) = error("unimplemented")

# This function must be implemented for all menu types. It defines what
#   happends when a user cancels ('q' or ctrl-c) a menu. `request()` will
#   always exit after calling this function.
cancel(m::AbstractMenu) = error("unimplemented")

# This function must be implemented for all menu types. It should return
#   a list of strings to be displayed as options in the current page.
options(m::AbstractMenu) = error("unimplemented")

# This function must be implemented for all menu types. It should write
#   the option at index `idx` to the buffer. If cursor is `true` it
#   should also display the cursor
function writeLine(buf::IOBuffer, m::AbstractMenu, idx::Int, cur::Bool)
    error("unimplemented")
end



# OPTIONAL FUNCTIONS
# These functions do not need to be implemented for all Menu types
##################################################################

# If `header()` is defined for a specific menu type, display the header
#  above the menu when it is rendered to the screen.
header(m::AbstractMenu) = ""

# If `keypress()` is defined for a specific menu type, send any
#   non-standard keypres event to this function. If the function returns
#   true, `request()` will exit.
keypress(m::AbstractMenu, i::UInt32) = false




"""

    request(m::AbstractMenu)

Display the menu and enter interactive mode. Returns `m.selected` which
varies based on menu type.
"""
function request(m::AbstractMenu)
    cursor = 1

    menu_header = header(m)
    if menu_header != ""
        println(menu_header)
    end

    printMenu(m, cursor, init=true)

    enableRawMode()
    print("\x1b[?25l") # hide the cursor
    try
        while true
            c = readKey()

            if c == Int(ARROW_UP)

                if cursor > 1
                    # move selection up
                    cursor -= 1
                    # scroll the page
                    if cursor < (2+m.pageoffset) && m.pageoffset > 0
                        m.pageoffset -= 1
                    end
                elseif CONFIG[:scroll_wrap]
                    # wrap to bottom
                    cursor = length(options(m))
                    m.pageoffset = length(options(m)) - m.pagesize
                end

            elseif c == Int(ARROW_DOWN)

                if cursor < length(options(m))
                    # move selection up
                    cursor += 1
                    # scroll page
                    if cursor >= m.pagesize + m.pageoffset && m.pagesize + m.pageoffset < length(options(m))
                        m.pageoffset += 1
                    end
                elseif CONFIG[:scroll_wrap]
                    # wrap to top
                    cursor = 1
                    m.pageoffset = 0
                end

            elseif c == 13 # <enter>
                # will break if pick returns true
                pick(m, cursor) && break
            elseif c == UInt32('q') || c == 3 # ctrl-c (cancel)
                cancel(m)
                break
            else
                # will break if keypress returns true
                keypress(m, c) && break
            end

            printMenu(m, cursor)
        end
    finally
        # always disable raw mode even even if there is an
        #  exception in the above loop
        print("\x1b[?25h") # unhide cursor
        disableRawMode()
    end
    println()

    return m.selected
end


"""

    request(msg::AbstractString, m::AbstractMenu)

Shorthand for `println(msg); request(m)`.
"""
function request(msg::AbstractString, m::AbstractMenu)
    println(msg)
    request(m)
end



# The generic printMenu function is used for displaying the state of a
#   menu to the screen. Menus must implement `writeLine` and `options`
#   and have fields `pagesize::Int` and `pageoffset::Int` as part of
#   their type definition
function printMenu(m::AbstractMenu, cursor::Int; init::Bool=false)
    buf = IOBuffer()

    # Move the cursor to the beginning of where it should print
    # Don't do this on the initial print
    lines = m.pagesize-1
    if init
        m.pageoffset = 0
    else
        print(buf, "\x1b[999D\x1b[$(lines)A")
    end

    for i in (m.pageoffset+1):(m.pageoffset + m.pagesize)
        print(buf, "\x1b[2K")

        if i == m.pageoffset+1 && m.pageoffset > 0
            # first line && scrolled past first entry
            print(buf, CONFIG[:up_arrow])
        elseif i == m.pagesize+m.pageoffset && i != length(options(m))
            # last line && not last option
            print(buf, CONFIG[:down_arrow])
        else
            # non special line
            print(buf, " ")
        end

        writeLine(buf, m, i, i == cursor)

        # dont print an \r\n on the last line
        i != (m.pagesize+m.pageoffset) && print(buf, "\r\n")
    end

    print(String(take!(buf)))
end

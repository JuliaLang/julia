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
  - `options(m::AbstractMenu)`   # `numoptions` is an alternative
  - `writeline(buf::IO, m::AbstractMenu, idx::Int, iscursor::Bool)`

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

function getproperty(m::AbstractMenu, name::Symbol)
    if name === :pagesize
        return getfield(m, :pagesize)::Int
    elseif name === :pageoffset
        return getfield(m, :pageoffset)::Int
    end
    return getfield(m, name)
end


# TODO Julia2.0: get rid of parametric intermediate, making it just
#   abstract type ConfiguredMenu <: AbstractMenu end
# Or perhaps just make all menus ConfiguredMenus
# Also consider making `cursor` a mandatory field in the Menu structs
# instead of going via the RefValue in `request`.
abstract type _ConfiguredMenu{C} <: AbstractMenu end
const ConfiguredMenu = _ConfiguredMenu{<:AbstractConfig}

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
    writeline(buf::IO, m::AbstractMenu, idx::Int, iscursor::Bool)

Write the option at index `idx` to `buf`. `iscursor`, if `true`, indicates that this
item is at the current cursor position (the one that will be selected by hitting "Enter").

If `m` is a `ConfiguredMenu`, `TerminalMenus` will print the cursor indicator.
Otherwise the callee is expected to handle such printing.

!!! compat "Julia 1.6"
    `writeline` requires Julia 1.6 or higher.

    On older versions of Julia, this was
        `writeLine(buf::IO, m::AbstractMenu, idx, iscursor::Bool)`
    and `m` is assumed to be unconfigured. The selection and cursor indicators can be
    obtained from `TerminalMenus.CONFIG`.

    This older function is supported on all Julia 1.x versions but will be dropped in Julia 2.0.
"""
function writeline(buf::IO, m::AbstractMenu, idx::Int, iscursor::Bool)
    # error("unimplemented")    # TODO: use this in Julia 2.0
    writeLine(buf, m, idx, iscursor)
end


# OPTIONAL FUNCTIONS
# These functions do not need to be implemented for all menu types
##################################################################

"""
    header(m::AbstractMenu)::String

Return a header string to be printed above the menu.
Defaults to "".
"""
header(m::AbstractMenu) = ""

"""
    keypress(m::AbstractMenu, i::UInt32)::Bool

Handle any non-standard keypress event.
If `true` is returned, [`TerminalMenus.request`](@ref) will exit.
Defaults to `false`.
"""
keypress(m::AbstractMenu, i::UInt32) = false

"""
    numoptions(m::AbstractMenu)::Int

Return the number of options in menu `m`. Defaults to `length(options(m))`.

!!! compat "Julia 1.6"
    This function requires Julia 1.6 or later.
"""
numoptions(m::AbstractMenu) = length(options(m))

"""
    selected(m::AbstractMenu)

Return information about the user-selected option.
By default it returns `m.selected`.
"""
selected(m::AbstractMenu) = m.selected

"""
    request(m::AbstractMenu; cursor=1)

Display the menu and enter interactive mode. `cursor` indicates the item
number used for the initial cursor position. `cursor` can be either an
`Int` or a `RefValue{Int}`. The latter is useful for observation and
control of the cursor position from the outside.

Returns `selected(m)`.

!!! compat "Julia 1.6"
    The `cursor` argument requires Julia 1.6 or later.
"""
request(m::AbstractMenu; kwargs...) = request(default_terminal(), m; kwargs...)

function request(term::REPL.Terminals.TTYTerminal, m::AbstractMenu; cursor::Union{Int, Base.RefValue{Int}}=1, suppress_output=false)
    if cursor isa Int
        cursor = Ref(cursor)
    end

    state = nothing
    if !suppress_output
        state = printmenu(term.out_stream, m, cursor[], init=true)
    end

    raw_mode_enabled = try
        REPL.Terminals.raw!(term, true)
        true
    catch err
        suppress_output || @warn "TerminalMenus: Unable to enter raw mode: " exception=(err, catch_backtrace())
        false
    end
    # hide the cursor
    raw_mode_enabled && !suppress_output && print(term.out_stream, "\x1b[?25l")

    try
        while true
            lastoption = numoptions(m)
            c = readkey(term.in_stream)

            if c == Int(ARROW_UP)
                cursor[] = move_up!(m, cursor[], lastoption)
            elseif c == Int(ARROW_DOWN)
                cursor[] = move_down!(m, cursor[], lastoption)
            elseif c == Int(PAGE_UP)
                cursor[] = page_up!(m, cursor[], lastoption)
            elseif c == Int(PAGE_DOWN)
                cursor[] = page_down!(m, cursor[], lastoption)
            elseif c == Int(HOME_KEY)
                cursor[] = 1
                m.pageoffset = 0
            elseif c == Int(END_KEY)
                cursor[] = lastoption
                m.pageoffset = max(0, lastoption - m.pagesize)
            elseif c == 13 # <enter>
                # will break if pick returns true
                pick(m, cursor[]) && break
            elseif c == UInt32('q')
                cancel(m)
                break
            elseif c == 3 # ctrl-c
                cancel(m)
                ctrl_c_interrupt(m) ? throw(InterruptException()) : break
            else
                # will break if keypress returns true
                keypress(m, c) && break
            end

            if !suppress_output
                state = printmenu(term.out_stream, m, cursor[], oldstate=state)
            end
        end
    finally # always disable raw mode
        if raw_mode_enabled
            !suppress_output && print(term.out_stream, "\x1b[?25h") # unhide cursor
            REPL.Terminals.raw!(term, false)
        end
    end
    !suppress_output && println(term.out_stream)

    return selected(m)
end


"""
    request([term,] msg::AbstractString, m::AbstractMenu)

Shorthand for `println(msg); request(m)`.
"""
request(msg::AbstractString, m::AbstractMenu; kwargs...) = request(default_terminal(), msg, m; kwargs...)

function request(term::REPL.Terminals.TTYTerminal, msg::AbstractString, m::AbstractMenu; kwargs...)
    println(term.out_stream, msg)
    request(term, m; kwargs...)
end


function move_up!(m::AbstractMenu, cursor::Int, lastoption::Int=numoptions(m))
    if cursor > 1
        cursor -= 1 # move selection up
        if cursor < (2+m.pageoffset) && m.pageoffset > 0
            m.pageoffset -= 1 # scroll page up
        end
    elseif scroll_wrap(m)
        # wrap to bottom
        cursor = lastoption
        m.pageoffset = max(0, lastoption - m.pagesize)
    end
    return cursor
end

function move_down!(m::AbstractMenu, cursor::Int, lastoption::Int=numoptions(m))
    if cursor < lastoption
        cursor += 1 # move selection down
        pagepos = m.pagesize + m.pageoffset
        if pagepos <= cursor && pagepos < lastoption
            m.pageoffset += 1 # scroll page down
        end
    elseif scroll_wrap(m)
        # wrap to top
        cursor = 1
        m.pageoffset = 0
    end
    return cursor
end

function page_up!(m::AbstractMenu, cursor::Int, lastoption::Int=numoptions(m))
    # If we're at the bottom, move the page 1 less to move the cursor up from
    # the bottom entry, since we try to avoid putting the cursor at bounds.
    m.pageoffset -= m.pagesize - (cursor == lastoption ? 1 : 0)
    m.pageoffset = max(m.pageoffset, 0)
    return max(cursor - m.pagesize, 1)
end

function page_down!(m::AbstractMenu, cursor::Int, lastoption::Int=numoptions(m))
    m.pageoffset += m.pagesize - (cursor == 1 ? 1 : 0)
    m.pageoffset = max(0, min(m.pageoffset, lastoption - m.pagesize))
    return min(cursor + m.pagesize, lastoption)
end

"""
    printmenu(out, m::AbstractMenu, cursoridx::Int; init::Bool=false, oldstate=nothing) -> newstate

Display the state of a menu. `init=true` causes `m.pageoffset` to be initialized to start printing at
or just above the current cursor location; when `init` is false, the terminal will
preserve the current setting of `m.pageoffset` and overwrite the previous display.
Returns `newstate`, which can be passed in as `oldstate` on the next call to allow accurate
overwriting of the previous display.

!!! compat "Julia 1.6"
    `printmenu` requires Julia 1.6 or higher.

    On older versions of Julia, this was called `printMenu` and it lacked the `state` argument/return value.
    This older function is supported on all Julia 1.x versions but will be dropped in Julia 2.0.
"""
function printmenu(out::IO, m::AbstractMenu, cursoridx::Int; oldstate=nothing, init::Bool=false)
    # TODO Julia 2.0?: get rid of `init` and just use `oldstate`
    buf = IOBuffer()
    lastoption = numoptions(m)::Int
    ncleared = oldstate === nothing ? m.pagesize-1 : oldstate

    if init
        # like clamp, except this takes the min if max < min
        m.pageoffset = max(0, min(cursoridx - m.pagesize ÷ 2, lastoption - m.pagesize))
    else
        print(buf, "\r")
        if ncleared > 0
            # Move up `ncleared` lines. However, moving up zero lines
            # is interpreted as one line, so need to do this
            # conditionally. (More specifically, the `0` value means
            # to use the default, and for move up this is one.)
            print(buf, "\x1b[$(ncleared)A")
        end
    end

    nheaderlines = 0
    for headerline in split(header(m), "\n", keepempty=false)
        print(buf, "\x1b[2K", headerline, "\r\n")
        nheaderlines += 1
    end

    firstline = m.pageoffset+1
    lastline = min(m.pagesize+m.pageoffset, lastoption)

    for i in firstline:lastline
        # clearline
        print(buf, "\x1b[2K")

        upscrollable = i == firstline && m.pageoffset > 0
        downscrollable = i == lastline && i != lastoption

        if upscrollable && downscrollable
            print(buf, updown_arrow(m)::Union{Char,String})
        elseif upscrollable
            print(buf, up_arrow(m)::Union{Char,String})
        elseif downscrollable
            print(buf, down_arrow(m)::Union{Char,String})
        else
            print(buf, ' ')
        end

        printcursor(buf, m, i == cursoridx)
        writeline(buf, m, i, i == cursoridx)

        (i != lastline) && print(buf, "\r\n")
    end

    newstate = nheaderlines + lastline - firstline  # final line doesn't have `\n`

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

scroll_wrap(m::ConfiguredMenu) = scroll_wrap(m.config)
scroll_wrap(c::AbstractConfig) = scroll_wrap(c.config)
scroll_wrap(c::Config) = c.scroll_wrap
scroll_wrap(::AbstractMenu) = CONFIG[:scroll_wrap]::Bool

ctrl_c_interrupt(m::ConfiguredMenu) = ctrl_c_interrupt(m.config)
ctrl_c_interrupt(c::AbstractConfig) = ctrl_c_interrupt(c.config)
ctrl_c_interrupt(c::Config) = c.ctrl_c_interrupt
ctrl_c_interrupt(::AbstractMenu) = CONFIG[:ctrl_c_interrupt]::Bool

up_arrow(m::ConfiguredMenu) = up_arrow(m.config)
up_arrow(c::AbstractConfig) = up_arrow(c.config)
up_arrow(c::Config) = c.up_arrow
up_arrow(::AbstractMenu) = CONFIG[:up_arrow]::Char

down_arrow(m::ConfiguredMenu) = down_arrow(m.config)
down_arrow(c::AbstractConfig) = down_arrow(c.config)
down_arrow(c::Config) = c.down_arrow
down_arrow(::AbstractMenu) = CONFIG[:down_arrow]::Char

updown_arrow(m::ConfiguredMenu) = updown_arrow(m.config)
updown_arrow(c::AbstractConfig) = updown_arrow(c.config)
updown_arrow(c::Config) = c.updown_arrow
updown_arrow(::AbstractMenu) = CONFIG[:updown_arrow]::Char

printcursor(buf, m::ConfiguredMenu, iscursor::Bool) = print(buf, iscursor ? cursor(m.config) : ' ', ' ')
cursor(c::AbstractConfig) = cursor(c.config)
cursor(c::Config) = c.cursor
printcursor(buf, ::AbstractMenu, ::Bool) = nothing   # `writeLine` is expected to do the printing (get from CONFIG[:cursor])

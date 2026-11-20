# This file is a part of Julia. License is MIT: https://julialang.org/license

const SHIFT, ALT, CTRL = 1, 2, 4

"""
    KeyEvent(key, mods = 0)

A key pressed. `key` is the `Char` typed, or one of `:enter`, `:tab`, `:backspace`,
`:escape`, `:up`, `:down`, `:left`, `:right`, `:home`, `:end`, `:insert`, `:delete`,
`:pageup`, `:pagedown` and `:f1` to `:f12`. `mods` is a bit each for [`SHIFT`](@ref),
[`ALT`](@ref) and [`CTRL`](@ref): `^A` is `KeyEvent('a', CTRL)` and Shift-Tab is
`KeyEvent(:tab, SHIFT)`.
"""
struct KeyEvent
    key::Union{Char,Symbol}
    mods::Int
end
KeyEvent(key::Union{Char,Symbol}) = KeyEvent(key, 0)

"""
    MouseEvent(action, button, row, col, mods)

A mouse report, once [`mouse_reporting`](@ref) is on. `action` is `:press`, `:release`,
`:drag`, `:move` (the pointer moving with no button held, which only motion reporting
sends), `:wheelup`, `:wheeldown`, `:wheelleft` or `:wheelright`. `button` is 1 for the
left button, 2 the middle and 3 the right, 8 and 9 for the back and forward buttons, and 0
for the wheel and `:move`. `row` and `col` count from 1. `mods` is as for a [`KeyEvent`](@ref).
"""
struct MouseEvent
    action::Symbol
    button::Int
    row::Int
    col::Int
    mods::Int
end

"""
    PasteEvent(text)

A bracketed paste, once [`bracketed_paste`](@ref) is on: text, which should not be taken
as keys.
"""
struct PasteEvent
    text::String
end

"""
    FocusEvent(focused)

The terminal gained or lost the focus, once [`focus_reporting`](@ref) is on.
"""
struct FocusEvent
    focused::Bool
end

"""
    SchemeEvent(dark, background)

The terminal said whether its colours are dark (`dark` is `true` or `false`, else
`nothing`), or what its background colour is (`background`, as the terminal wrote it, such
as `"rgb:1e1e/1e1e/1e1e"`, else `""`). Asked for by [`scheme_reports`](@ref).
"""
struct SchemeEvent
    dark::Union{Nothing,Bool}
    background::String
end

"""
    LegacyMouseEvent()

A mouse report in the X10 encoding, which [`mouse_reporting`](@ref) does not ask for. It
means the terminal lost the SGR encoding while it went on reporting, as VS Code's does when
it reattaches a session, and the answer is to write `mouse_reporting(true)` again.
"""
struct LegacyMouseEvent end

const Event = Union{KeyEvent,MouseEvent,PasteEvent,FocusEvent,SchemeEvent,LegacyMouseEvent}

"""
    readevent(io::IO) -> Union{KeyEvent, MouseEvent, PasteEvent, FocusEvent, SchemeEvent,
                               LegacyMouseEvent, Nothing}

Read one event from a terminal in raw mode. A sequence that is not understood is read to its
end and returns `nothing`, so that none of it arrives as keys, and so does one cut off by the
end of the input.

A bare Escape is one with nothing already waiting behind it, since a terminal writes a
sequence in one write. Nothing is decided by waiting, so an `IOBuffer` can stand in for the
terminal.
"""
function readevent(io::IO)
    c = read(io, Char)
    c == '\e' || return key_event(c)
    bytesavailable(io) == 0 && return KeyEvent(:escape)
    return read_escape(io)
end

function key_event(c::Char)
    c == '\r' || c == '\n' ? KeyEvent(:enter) :
    c == '\t' ? KeyEvent(:tab) :
    c == '\x7f' || c == '\b' ? KeyEvent(:backspace) :
    c == '\0' ? KeyEvent(' ', CTRL) :
    c < ' ' ? KeyEvent(c + (c < '\x1b' ? 0x60 : 0x40), CTRL) :
    KeyEvent(c)
end

# after an Escape with more bytes waiting
function read_escape(io::IO)
    c = next_char(io)
    c === nothing && return nothing
    # with nothing behind it, as with a bare Escape, this is Alt and the key
    c in "[]O" && bytesavailable(io) == 0 && return KeyEvent(c, ALT)
    c == '[' && return read_csi(io)
    c == ']' && return read_osc(io)
    if c == 'O'
        f = next_char(io)
        return f === nothing ? nothing : csi_key(f, "")
    end
    if c == '\e'
        # Alt and an arrow, written as Escape and then the arrow's own sequence
        bytesavailable(io) == 0 && return KeyEvent(:escape, ALT)
        ev = read_escape(io)
        return ev isa KeyEvent ? KeyEvent(ev.key, ev.mods | ALT) : ev
    end
    ev = key_event(c)
    return KeyEvent(ev.key, ev.mods | ALT)
end

next_char(io::IO) = eof(io) ? nothing : read(io, Char)

function read_csi(io::IO)
    buf = IOBuffer()
    mouse = false
    while true
        c = next_char(io)
        c === nothing && return nothing
        position(buf) == 0 && c == '<' && (mouse = true)
        # a mouse report ends at `M` or `m`: xterm.js writes `<0;NaN;NaNm` for a place it
        # cannot name, which would end at the `N`
        (mouse ? c in "Mm" : '@' <= c <= '~') && return csi_event(String(take!(buf)), c, io)
        write(buf, c)
        position(buf) > 32 && return nothing
    end
end

function csi_event(params::String, final::Char, io::IO)
    if startswith(params, '<')
        return mouse_event(params[2:end], final == 'M')
    elseif isempty(params)
        final == 'M' && return read_x10(io)
        final == 'I' && return FocusEvent(true)
        final == 'O' && return FocusEvent(false)
    elseif final == 'n' && params in ("?997;1", "?997;2")
        return SchemeEvent(params == "?997;1", "")
    elseif final == '~' && params == "200"
        return read_paste(io)
    end
    return csi_key(final, params)
end

const CSI_KEYS = Dict('A' => :up, 'B' => :down, 'C' => :right, 'D' => :left, 'H' => :home,
                      'F' => :end, 'P' => :f1, 'Q' => :f2, 'R' => :f3, 'S' => :f4, 'M' => :enter)

const TILDE_KEYS = Dict(1 => :home, 2 => :insert, 3 => :delete, 4 => :end, 5 => :pageup,
                        6 => :pagedown, 7 => :home, 8 => :end, 11 => :f1, 12 => :f2, 13 => :f3,
                        14 => :f4, 15 => :f5, 17 => :f6, 18 => :f7, 19 => :f8, 20 => :f9,
                        21 => :f10, 23 => :f11, 24 => :f12)

function csi_key(final::Char, params::String)
    parts = split(params, ';')
    all(p -> isempty(p) || all(isdigit, p), parts) || return nothing
    # the modifiers are 1 + shift + 2 alt + 4 ctrl
    mods = length(parts) >= 2 && !isempty(parts[2]) ? (parse(Int, parts[2]) - 1) & 7 : 0
    final == 'Z' && return KeyEvent(:tab, SHIFT)
    key = if final == '~'
        isempty(parts[1]) ? nothing : get(TILDE_KEYS, parse(Int, parts[1]), nothing)
    else
        get(CSI_KEYS, final, nothing)
    end
    return key === nothing ? nothing : KeyEvent(key, mods)
end

# The button byte has the button in its low two bits, then shift, alt and ctrl, motion at 32,
# the wheel at 64 and the extra buttons at 128.
function mouse_event(params::String, pressed::Bool)
    m = match(r"^(\d+);(\d+);(\d+)$", params)
    m === nothing && return nothing
    b, col, row = parse.(Int, m.captures)
    mods = (b & 4 != 0 ? SHIFT : 0) | (b & 8 != 0 ? ALT : 0) | (b & 16 != 0 ? CTRL : 0)
    low = b & 3
    if b & 64 != 0
        return MouseEvent((:wheelup, :wheeldown, :wheelleft, :wheelright)[low+1], 0, row, col, mods)
    end
    button = b & 128 != 0 ? low + 8 : low + 1
    action = !pressed ? :release : b & 32 == 0 ? :press : low == 3 ? :move : :drag
    action === :move && (button = 0)
    return MouseEvent(action, button, row, col, mods)
end

# An X10 report's three bytes follow its final byte, each a value plus 32, so an Escape
# among them is the start of whatever came next.
function read_x10(io::IO)
    for _ in 1:3
        (eof(io) || peek(io, UInt8) == 0x1b) && break
        read(io, UInt8)
    end
    return LegacyMouseEvent()
end

function read_paste(io::IO)
    bytes = UInt8[]
    marker = codeunits("\e[201~")
    while !eof(io)
        push!(bytes, read(io, UInt8))
        n = length(bytes) - length(marker)
        n >= 0 && view(bytes, n+1:length(bytes)) == marker && return PasteEvent(String(resize!(bytes, n)))
    end
    return PasteEvent(String(bytes))
end

# An OSC ends at BEL or ST. Only the answer to the background colour's query is kept.
function read_osc(io::IO)
    buf = IOBuffer()
    while true
        c = next_char(io)
        c === nothing && return nothing
        c == '\a' && break
        if c == '\e'
            next_char(io)
            break
        end
        position(buf) < 256 && write(buf, c)
    end
    m = match(r"^11;([\x21-\x7e]{1,100})$", String(take!(buf)))
    return m === nothing ? nothing : SchemeEvent(nothing, m[1])
end

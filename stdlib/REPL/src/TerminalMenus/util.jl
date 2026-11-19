# This file is a part of Julia. License is MIT: https://julialang.org/license

# Enum for escaped (multi-byte) keys such as the arrows or the home/end keys
@enum(Key,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN)

readbyte(stream::IO=stdin) = read(stream, Char)

# Read the next key from stdin. It is also able to read several bytes for
#   escaped keys such as the arrow keys, home/end keys, etc.
# Escaped keys are returned using the `Key` enum.
readkey(stream::IO=stdin) = UInt32(_readkey(stream))
function _readkey(stream::IO=stdin)
    c = readbyte(stream)
    if c == '\x1b'
        bytesavailable(stream) < 1 && return '\x1b'
        # read the whole sequence, so an unknown one leaves no keys behind
        ev = read_escape(stream)
        ev isa KeyEvent || return '\x1b'
        return get(ESCAPED_KEYS, ev.key, '\x1b')
    end
    c == '\x16' && return PAGE_DOWN  # C-v
    c == '\x10' && return ARROW_UP   # C-p
    c == '\x0e' && return ARROW_DOWN # C-n
    return c
end

# the keys with any modifiers, and M-v, M-<, M->, M-H and M-F
const ESCAPED_KEYS = Dict{Union{Char,Symbol},Key}(
    :up => ARROW_UP, :down => ARROW_DOWN, :left => ARROW_LEFT, :right => ARROW_RIGHT,
    :home => HOME_KEY, :end => END_KEY, :delete => DEL_KEY, :pageup => PAGE_UP,
    :pagedown => PAGE_DOWN, 'v' => PAGE_UP, '<' => HOME_KEY, '>' => END_KEY, 'H' => HOME_KEY,
    'F' => END_KEY)

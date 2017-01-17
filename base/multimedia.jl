# This file is a part of Julia. License is MIT: http://julialang.org/license

module Multimedia

export Display, display, pushdisplay, popdisplay, displayable, redisplay,
    MIME, @MIME_str, reprmime, stringmime, istextmime,
    mimewritable, TextDisplay

###########################################################################
# We define a singleton type MIME{mime symbol} for each MIME type, so
# that Julia's dispatch and overloading mechanisms can be used to
# dispatch show and to add conversions for new types.

# defined in sysimg.jl for bootstrapping:
# immutable MIME{mime} end
# macro MIME_str(s)
import Base: MIME, @MIME_str

import Base: show, print, string, convert
MIME(s) = MIME{Symbol(s)}()
show{mime}(io::IO, ::MIME{mime}) = print(io, "MIME type ", string(mime))
print{mime}(io::IO, ::MIME{mime}) = print(io, mime)

###########################################################################
# For any type T one can define show(io, ::MIME"type", x::T) = ...
# in order to provide a way to export T as a given mime type.

"""
    mimewritable(mime, x)

Returns a boolean value indicating whether or not the object `x` can be written as the given
`mime` type. (By default, this is determined automatically by the existence of the
corresponding [`show`](@ref) method for `typeof(x)`.)
"""
mimewritable{mime}(::MIME{mime}, x) =
  method_exists(show, Tuple{IO, MIME{mime}, typeof(x)})

# it is convenient to accept strings instead of ::MIME
show(io::IO, m::AbstractString, x) = show(io, MIME(m), x)
mimewritable(m::AbstractString, x) = mimewritable(MIME(m), x)

verbose_show(io, m, x) = show(IOContext(io,limit=false), m, x)

"""
    reprmime(mime, x)

Returns an `AbstractString` or `Vector{UInt8}` containing the representation of
`x` in the requested `mime` type, as written by `show` (throwing a
`MethodError` if no appropriate `show` is available). An `AbstractString` is
returned for MIME types with textual representations (such as `"text/html"` or
`"application/postscript"`), whereas binary data is returned as
`Vector{UInt8}`. (The function `istextmime(mime)` returns whether or not Julia
treats a given `mime` type as text.)

As a special case, if `x` is an `AbstractString` (for textual MIME types) or a
`Vector{UInt8}` (for binary MIME types), the `reprmime` function assumes that
`x` is already in the requested `mime` format and simply returns `x`. This
special case does not apply to the `"text/plain"` MIME type. This is useful so
that raw data can be passed to `display(m::MIME, x)`.
"""
reprmime(m::MIME, x) = istextmime(m) ? _textreprmime(m, x) : _binreprmime(m, x)

# strings are shown escaped for text/plain
_textreprmime(m::MIME, x) = sprint(verbose_show, m, x)
_textreprmime(::MIME, x::AbstractString) = x
_textreprmime(m::MIME"text/plain", x::AbstractString) =
    sprint(verbose_show, m, x)

function _binreprmime(m::MIME, x)
    s = IOBuffer()
    verbose_show(s, m, x)
    take!(s)
end
_binreprmime(m::MIME, x::Vector{UInt8}) = x

"""
    stringmime(mime, x)

Returns an `AbstractString` containing the representation of `x` in the
requested `mime` type. This is similar to [`reprmime`](@ref) except
that binary data is base64-encoded as an ASCII string.
"""
stringmime(m::MIME, x) = istextmime(m) ? reprmime(m, x) : _binstringmime(m, x)

_binstringmime(m::MIME, x) = base64encode(verbose_show, m, x)
_binstringmime(m::MIME, x::Vector{UInt8}) = base64encode(write, x)

"""
    istextmime(m::MIME)

Determine whether a MIME type is text data. MIME types are assumed to be binary
data except for a set of types known to be text data (possibly Unicode).
"""
istextmime(m::MIME) = startswith(string(m), "text/")

# it is convenient to accept strings instead of ::MIME
istextmime(m::AbstractString) = istextmime(MIME(m))
reprmime(m::AbstractString, x) = reprmime(MIME(m), x)
stringmime(m::AbstractString, x) = stringmime(MIME(m), x)

for mime in ["application/atom+xml", "application/ecmascript",
             "application/javascript", "application/julia",
             "application/json", "application/postscript",
             "application/rdf+xml", "application/rss+xml",
             "application/x-latex", "application/xhtml+xml", "application/xml",
             "application/xml-dtd", "image/svg+xml", "model/vrml",
             "model/x3d+vrml", "model/x3d+xml"]
    istextmime(::MIME{Symbol(mime)}) = true
end

###########################################################################
# We have an abstract Display class that can be subclassed in order to
# define new rich-display output devices.  A typical subclass should
# overload display(d::Display, m::MIME, x) for supported MIME types m,
# (typically using reprmime or stringmime to get the MIME
# representation of x) and should also overload display(d::Display, x)
# to display x in whatever MIME type is preferred by the Display and
# is writable by x.  display(..., x) should throw a MethodError if x
# cannot be displayed.  The return value of display(...) is up to the
# Display type.

abstract Display

# it is convenient to accept strings instead of ::MIME
display(d::Display, mime::AbstractString, x) = display(d, MIME(mime), x)
display(mime::AbstractString, x) = display(MIME(mime), x)

"""
    displayable(mime) -> Bool
    displayable(d::Display, mime) -> Bool

Returns a boolean value indicating whether the given `mime` type (string) is displayable by
any of the displays in the current display stack, or specifically by the display `d` in the
second variant.
"""
displayable(d::Display, mime::AbstractString) = displayable(d, MIME(mime))
displayable(mime::AbstractString) = displayable(MIME(mime))

# simplest display, which only knows how to display text/plain

"""
    TextDisplay(io::IO)

Returns a `TextDisplay <: Display`, which displays any object as the text/plain MIME type
(by default), writing the text representation to the given I/O stream. (This is how
objects are printed in the Julia REPL.)
"""
immutable TextDisplay <: Display
    io::IO
end
display(d::TextDisplay, M::MIME"text/plain", x) = show(d.io, M, x)
display(d::TextDisplay, x) = display(d, MIME"text/plain"(), x)

# if you explicitly call display("text/foo", x), it should work on a TextDisplay:
displayable(d::TextDisplay, M::MIME) = istextmime(M)
function display(d::TextDisplay, M::MIME, x)
    displayable(d, M) || throw(MethodError(display, (d, M, x)))
    show(d.io, M, x)
end

import Base: close, flush
flush(d::TextDisplay) = flush(d.io)
close(d::TextDisplay) = close(d.io)

###########################################################################
# We keep a stack of Displays, and calling display(x) uses the topmost
# Display that is capable of displaying x (doesn't throw an error)

const displays = Display[]
function pushdisplay(d::Display)
    global displays
    push!(displays, d)
end
popdisplay() = pop!(displays)
function popdisplay(d::Display)
    for i = length(displays):-1:1
        if d == displays[i]
            return splice!(displays, i)
        end
    end
    throw(KeyError(d))
end
function reinit_displays()
    empty!(displays)
    pushdisplay(TextDisplay(STDOUT))
end

xdisplayable(D::Display, args...) = applicable(display, D, args...)

function display(x)
    for i = length(displays):-1:1
        if xdisplayable(displays[i], x)
            try
                return display(displays[i], x)
            catch e
                isa(e, MethodError) && e.f in (display, show) ||
                    rethrow()
            end
        end
    end
    throw(MethodError(display, (x,)))
end

function display(m::MIME, x)
    for i = length(displays):-1:1
        if xdisplayable(displays[i], m, x)
            try
                return display(displays[i], m, x)
            catch e
                isa(e, MethodError) && e.f == display ||
                    rethrow()
            end
        end
    end
    throw(MethodError(display, (m, x)))
end

displayable{D<:Display,mime}(d::D, ::MIME{mime}) =
  method_exists(display, Tuple{D, MIME{mime}, Any})

function displayable(m::MIME)
    for d in displays
        displayable(d, m) && return true
    end
    return false
end

###########################################################################
# The redisplay method can be overridden by a Display in order to
# update an existing display (instead of, for example, opening a new
# window), and is used by the IJulia interface to defer display
# until the next interactive prompt.  This is especially useful
# for Matlab/Pylab-like stateful plotting interfaces, where
# a plot is created and then modified many times (xlabel, title, etc.).

function redisplay(x)
    for i = length(displays):-1:1
        if xdisplayable(displays[i], x)
            try
                return redisplay(displays[i], x)
            catch e
                isa(e, MethodError) && e.f in (redisplay, display, show) ||
                    rethrow()
            end
        end
    end
    throw(MethodError(redisplay, (x,)))
end

function redisplay(m::Union{MIME,AbstractString}, x)
    for i = length(displays):-1:1
        if xdisplayable(displays[i], m, x)
            try
                return redisplay(displays[i], m, x)
            catch e
                isa(e, MethodError) && e.f in (redisplay, display) ||
                    rethrow()
            end
        end
    end
    throw(MethodError(redisplay, (m, x)))
end

# default redisplay is simply to call display
redisplay(d::Display, x) = display(d, x)
redisplay(d::Display, m::Union{MIME,AbstractString}, x) = display(d, m, x)

###########################################################################

end # module

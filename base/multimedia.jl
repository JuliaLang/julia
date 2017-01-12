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
"""
    show(stream, mime, x)

The [`display`](@ref) functions ultimately call `show` in order to write an object `x` as a
given `mime` type to a given I/O `stream` (usually a memory buffer), if possible. In order
to provide a rich multimedia representation of a user-defined type `T`, it is only necessary
to define a new `show` method for `T`, via: `show(stream, ::MIME"mime", x::T) = ...`,
where `mime` is a MIME-type string and the function body calls `write` (or similar) to write
that representation of `x` to `stream`. (Note that the `MIME""` notation only supports
literal strings; to construct `MIME` types in a more flexible manner use
`MIME{Symbol("")}`.)

For example, if you define a `MyImage` type and know how to write it to a PNG file, you
could define a function `show(stream, ::MIME"image/png", x::MyImage) = ...` to allow
your images to be displayed on any PNG-capable `Display` (such as IJulia). As usual, be sure
to `import Base.show` in order to add new methods to the built-in Julia function
`show`.

The default MIME type is `MIME"text/plain"`. There is a fallback definition for `text/plain`
output that calls `show` with 2 arguments. Therefore, this case should be handled by
defining a 2-argument `show(stream::IO, x::MyType)` method.

Technically, the `MIME"mime"` macro defines a singleton type for the given `mime` string,
which allows us to exploit Julia's dispatch mechanisms in determining how to display objects
of any given type.

The first argument to `show` can be an [`IOContext`](@ref) specifying output format properties.
See [`IOContext`](@ref) for details.
"""
show(io::IO, m::AbstractString, x) = show(io, MIME(m), x)
mimewritable(m::AbstractString, x) = mimewritable(MIME(m), x)

verbose_show(io, m, x) = show(IOContext(io,limit=false), m, x)

"""
    reprmime(mime, x)

Returns an `AbstractString` or `Vector{UInt8}` containing the representation of
`x` in the requested `mime` type, as written by `show` (throwing a
[`MethodError`](@ref) if no appropriate `show` is available). An `AbstractString` is
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

Returns a `TextDisplay <: Display`, which can display any object as the text/plain MIME type
(only), writing the text representation to the given I/O stream. (The text representation is
the same as the way an object is printed in the Julia REPL.)
"""
immutable TextDisplay <: Display
    io::IO
end
display(d::TextDisplay, M::MIME"text/plain", x) = show(d.io, M, x)
display(d::TextDisplay, x) = display(d, MIME"text/plain"(), x)

import Base: close, flush
flush(d::TextDisplay) = flush(d.io)
close(d::TextDisplay) = close(d.io)

###########################################################################
# We keep a stack of Displays, and calling display(x) uses the topmost
# Display that is capable of displaying x (doesn't throw an error)

const displays = Display[]
"""
    pushdisplay(d::Display)

Pushes a new display `d` on top of the global display-backend stack. Calling [`display(x)`](@ref) or
[`display(mime, x)`](@ref) will display `x` on the topmost compatible backend in the stack (i.e.,
the topmost backend that does not throw a [`MethodError`](@ref)).
"""
function pushdisplay(d::Display)
    global displays
    push!(displays, d)
end
"""
    popdisplay()
    popdisplay(d::Display)

Pop the topmost backend off of the display-backend stack, or the topmost copy of `d` in the
second variant.
"""
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

macro try_display(expr)
  quote
    try $(esc(expr))
    catch e
      isa(e, MethodError) && e.f in (display, redisplay, show) ||
        rethrow()
    end
  end
end

xdisplayable(D::Display, args...) = applicable(display, D, args...)

"""
    display(x)
    display(d::Display, x)
    display(mime, x)
    display(d::Display, mime, x)

Display `x` using the topmost applicable display in the display stack, typically using the
richest supported multimedia output for `x`, with plain-text [`STDOUT`](@ref) output as a fallback.
The `display(d, x)` variant attempts to display `x` on the given display `d` only, throwing
a [`MethodError`](@ref) if `d` cannot display objects of this type.

There are also two variants with a `mime` argument (a MIME type string, such as
`"image/png"`), which attempt to display `x` using the requested MIME type *only*, throwing
a [`MethodError`](@ref) if this type is not supported by either the display(s) or by `x`.
With these variants, one can also supply the "raw" data in the requested MIME type by passing
`x::AbstractString` (for MIME types with text-based storage, such as `"text/html"` or
`"application/postscript"`) or `x::Vector{UInt8}` (for binary MIME types).
"""
function display(x)
    for i = length(displays):-1:1
        xdisplayable(displays[i], x) &&
            @try_display return display(displays[i], x)
    end
    throw(MethodError(display, (x,)))
end

function display(m::MIME, x)
    for i = length(displays):-1:1
        xdisplayable(displays[i], m, x) &&
            @try_display return display(displays[i], m, x)
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

"""
    redisplay(x)
    redisplay(d::Display, x)
    redisplay(mime, x)
    redisplay(d::Display, mime, x)

By default, the `redisplay` functions simply call [`display`](@ref).
However, some display backends may override `redisplay` to modify an existing
display of `x` (if any).
Using `redisplay` is also a hint to the backend that `x` may be redisplayed
several times, and the backend may choose to defer the display until
(for example) the next interactive prompt.
"""
function redisplay(x)
    for i = length(displays):-1:1
        xdisplayable(displays[i], x) &&
            @try_display return redisplay(displays[i], x)
    end
    throw(MethodError(redisplay, (x,)))
end

function redisplay(m::Union{MIME,AbstractString}, x)
    for i = length(displays):-1:1
        xdisplayable(displays[i], m, x) &&
            @try_display return redisplay(displays[i], m, x)
    end
    throw(MethodError(redisplay, (m, x)))
end

# default redisplay is simply to call display
redisplay(d::Display, x) = display(d, x)
redisplay(d::Display, m::Union{MIME,AbstractString}, x) = display(d, m, x)

###########################################################################

end # module

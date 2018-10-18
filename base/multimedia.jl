# This file is a part of Julia. License is MIT: https://julialang.org/license

module Multimedia

export AbstractDisplay, display, pushdisplay, popdisplay, displayable, redisplay,
    MIME, @MIME_str, istextmime,
    showable, TextDisplay

###########################################################################
# We define a singleton type MIME{mime symbol} for each MIME type, so
# that Julia's dispatch and overloading mechanisms can be used to
# dispatch show and to add conversions for new types.

# defined in sysimg.jl for bootstrapping:
# struct MIME{mime} end
# macro MIME_str(s)
import .Base: MIME, @MIME_str
import .Base: show, print, string, convert, repr
MIME(s) = MIME{Symbol(s)}()
show(io::IO, ::MIME{mime}) where {mime} = print(io, "MIME type ", string(mime))
print(io::IO, ::MIME{mime}) where {mime} = print(io, mime)

###########################################################################
# For any type T one can define show(io, ::MIME"type", x::T) = ...
# in order to provide a way to export T as a given mime type.

"""
    showable(mime, x)

Returns a boolean value indicating whether or not the object `x` can be written
as the given `mime` type.

(By default, this is determined automatically by the existence of the
corresponding [`show`](@ref) method for `typeof(x)`.  Some types provide custom `showable`
methods; for example, if the available MIME formats depend on the *value* of `x`.)

# Examples
```jldoctest
julia> showable(MIME("text/plain"), rand(5))
true

julia> showable("img/png", rand(5))
false
```
"""
showable(::MIME{mime}, @nospecialize x) where {mime} = hasmethod(show, Tuple{IO, MIME{mime}, typeof(x)})
showable(m::AbstractString, @nospecialize x) = showable(MIME(m), x)

"""
    show(io, mime, x)

The [`display`](@ref) functions ultimately call `show` in order to write an object `x` as a
given `mime` type to a given I/O stream `io` (usually a memory buffer), if possible. In order
to provide a rich multimedia representation of a user-defined type `T`, it is only necessary
to define a new `show` method for `T`, via: `show(io, ::MIME"mime", x::T) = ...`,
where `mime` is a MIME-type string and the function body calls [`write`](@ref) (or similar) to write
that representation of `x` to `io`. (Note that the `MIME""` notation only supports
literal strings; to construct `MIME` types in a more flexible manner use
`MIME{Symbol("")}`.)

For example, if you define a `MyImage` type and know how to write it to a PNG file, you
could define a function `show(io, ::MIME"image/png", x::MyImage) = ...` to allow
your images to be displayed on any PNG-capable `AbstractDisplay` (such as IJulia). As usual, be sure
to `import Base.show` in order to add new methods to the built-in Julia function
`show`.

The default MIME type is `MIME"text/plain"`. There is a fallback definition for `text/plain`
output that calls `show` with 2 arguments. Therefore, this case should be handled by
defining a 2-argument `show(io::IO, x::MyType)` method.

Technically, the `MIME"mime"` macro defines a singleton type for the given `mime` string,
which allows us to exploit Julia's dispatch mechanisms in determining how to display objects
of any given type.

The first argument to `show` can be an [`IOContext`](@ref) specifying output format properties.
See [`IOContext`](@ref) for details.
"""
show(stream, mime, x)
show(io::IO, m::AbstractString, x) = show(io, MIME(m), x)

"""
    repr(mime, x; context=nothing)

Returns an `AbstractString` or `Vector{UInt8}` containing the representation of
`x` in the requested `mime` type, as written by [`show(io, mime, x)`](@ref) (throwing a
[`MethodError`](@ref) if no appropriate `show` is available). An `AbstractString` is
returned for MIME types with textual representations (such as `"text/html"` or
`"application/postscript"`), whereas binary data is returned as
`Vector{UInt8}`. (The function `istextmime(mime)` returns whether or not Julia
treats a given `mime` type as text.)

The optional keyword argument `context` can be set to `:key=>value` pair
or an `IO` or [`IOContext`](@ref) object whose attributes are used for the I/O
stream passed to `show`.

As a special case, if `x` is an `AbstractString` (for textual MIME types) or a
`Vector{UInt8}` (for binary MIME types), the `repr` function assumes that
`x` is already in the requested `mime` format and simply returns `x`. This
special case does not apply to the `"text/plain"` MIME type. This is useful so
that raw data can be passed to `display(m::MIME, x)`.

In particular, `repr("text/plain", x)` is typically a "pretty-printed" version
of `x` designed for human consumption.  See also [`repr(x)`](@ref) to instead
return a string corresponding to [`show(x)`](@ref) that may be closer to how
the value of `x` would be entered in Julia.

# Examples
```jldoctest
julia> A = [1 2; 3 4];

julia> repr("text/plain", A)
"2×2 Array{Int64,2}:\\n 1  2\\n 3  4"
```
"""
repr(m::MIME, x; context=nothing) = istextmime(m) ? _textrepr(m, x, context) : _binrepr(m, x, context)
repr(m::AbstractString, x; context=nothing) = repr(MIME(m), x; context=context)

# strings are shown escaped for text/plain
_textrepr(m::MIME, x, context) = String(__binrepr(m, x, context))
_textrepr(::MIME, x::AbstractString, context) = x
_textrepr(m::MIME"text/plain", x::AbstractString, context) = String(__binrepr(m, x, context))


function __binrepr(m::MIME, x, context)
    s = IOBuffer()
    if context === nothing
        show(s, m, x)
    else
        show(IOContext(s, context), m, x)
    end
    take!(s)
end
_binrepr(m::MIME, x, context) = __binrepr(m, x, context)
_binrepr(m::MIME, x::Vector{UInt8}, context) = x

"""
    istextmime(m::MIME)

Determine whether a MIME type is text data. MIME types are assumed to be binary
data except for a set of types known to be text data (possibly Unicode).

# Examples
```jldoctest
julia> istextmime(MIME("text/plain"))
true

julia> istextmime(MIME("img/png"))
false
```
"""
istextmime(m::MIME) = startswith(string(m), "text/")
istextmime(m::AbstractString) = istextmime(MIME(m))

for mime in ["application/atom+xml", "application/ecmascript",
             "application/javascript", "application/julia",
             "application/json", "application/postscript",
             "application/rdf+xml", "application/rss+xml",
             "application/x-latex", "application/xhtml+xml", "application/xml",
             "application/xml-dtd", "image/svg+xml", "model/vrml",
             "model/x3d+vrml", "model/x3d+xml"]
    global istextmime(::MIME{Symbol(mime)}) = true
end

###########################################################################
# We have an abstract AbstractDisplay class that can be subclassed in order to
# define new rich-display output devices.  A typical subclass should
# overload display(d::AbstractDisplay, m::MIME, x) for supported MIME types m,
# (typically using show, repr, ..., to get the MIME
# representation of x) and should also overload display(d::AbstractDisplay, x)
# to display x in whatever MIME type is preferred by the AbstractDisplay and
# is writable by x.  display(..., x) should throw a MethodError if x
# cannot be displayed.  The return value of display(...) is up to the
# AbstractDisplay type.

abstract type AbstractDisplay end

# it is convenient to accept strings instead of ::MIME
display(d::AbstractDisplay, mime::AbstractString, @nospecialize x) = display(d, MIME(mime), x)
display(mime::AbstractString, @nospecialize x) = display(MIME(mime), x)

"""
    displayable(mime) -> Bool
    displayable(d::AbstractDisplay, mime) -> Bool

Returns a boolean value indicating whether the given `mime` type (string) is displayable by
any of the displays in the current display stack, or specifically by the display `d` in the
second variant.
"""
displayable(d::AbstractDisplay, mime::AbstractString) = displayable(d, MIME(mime))
displayable(mime::AbstractString) = displayable(MIME(mime))

# simplest display, which only knows how to display text/plain

"""
    TextDisplay(io::IO)

Returns a `TextDisplay <: AbstractDisplay`, which displays any object as the text/plain MIME type
(by default), writing the text representation to the given I/O stream. (This is how
objects are printed in the Julia REPL.)
"""
struct TextDisplay <: AbstractDisplay
    io::IO
end
display(d::TextDisplay, M::MIME"text/plain", @nospecialize x) = show(d.io, M, x)
display(d::TextDisplay, @nospecialize x) = display(d, MIME"text/plain"(), x)

# if you explicitly call display("text/foo", x), it should work on a TextDisplay:
displayable(d::TextDisplay, M::MIME) = istextmime(M)
function display(d::TextDisplay, M::MIME, @nospecialize x)
    displayable(d, M) || throw(MethodError(display, (d, M, x)))
    show(d.io, M, x)
end

import Base: close, flush
flush(d::TextDisplay) = flush(d.io)
close(d::TextDisplay) = close(d.io)

###########################################################################
# We keep a stack of Displays, and calling display(x) uses the topmost
# AbstractDisplay that is capable of displaying x (doesn't throw an error)

const displays = AbstractDisplay[]

"""
    pushdisplay(d::AbstractDisplay)

Pushes a new display `d` on top of the global display-backend stack. Calling `display(x)` or
`display(mime, x)` will display `x` on the topmost compatible backend in the stack (i.e.,
the topmost backend that does not throw a [`MethodError`](@ref)).
"""
function pushdisplay(d::AbstractDisplay)
    global displays
    push!(displays, d)
end

"""
    popdisplay()
    popdisplay(d::AbstractDisplay)

Pop the topmost backend off of the display-backend stack, or the topmost copy of `d` in the
second variant.
"""
popdisplay() = pop!(displays)
function popdisplay(d::AbstractDisplay)
    for i = length(displays):-1:1
        if d == displays[i]
            return splice!(displays, i)
        end
    end
    throw(KeyError(d))
end
function reinit_displays()
    empty!(displays)
    pushdisplay(TextDisplay(stdout))
end

xdisplayable(D::AbstractDisplay, @nospecialize args...) = applicable(display, D, args...)

"""
    display(x)
    display(d::AbstractDisplay, x)
    display(mime, x)
    display(d::AbstractDisplay, mime, x)

AbstractDisplay `x` using the topmost applicable display in the display stack, typically using the
richest supported multimedia output for `x`, with plain-text [`stdout`](@ref) output as a fallback.
The `display(d, x)` variant attempts to display `x` on the given display `d` only, throwing
a [`MethodError`](@ref) if `d` cannot display objects of this type.

In general, you cannot assume that `display` output goes to `stdout` (unlike [`print(x)`](@ref) or
[`show(x)`](@ref)).  For example, `display(x)` may open up a separate window with an image.
`display(x)` means "show `x` in the best way you can for the current output device(s)."
If you want REPL-like text output that is guaranteed to go to `stdout`, use
[`show(stdout, "text/plain", x)`](@ref) instead.

There are also two variants with a `mime` argument (a MIME type string, such as
`"image/png"`), which attempt to display `x` using the requested MIME type *only*, throwing
a `MethodError` if this type is not supported by either the display(s) or by `x`. With these
variants, one can also supply the "raw" data in the requested MIME type by passing
`x::AbstractString` (for MIME types with text-based storage, such as text/html or
application/postscript) or `x::Vector{UInt8}` (for binary MIME types).
"""
function display(@nospecialize x)
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

function display(m::MIME, @nospecialize x)
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

displayable(d::D, ::MIME{mime}) where {D<:AbstractDisplay,mime} =
    hasmethod(display, Tuple{D,MIME{mime},Any})

function displayable(m::MIME)
    for d in displays
        displayable(d, m) && return true
    end
    return false
end

###########################################################################
# The redisplay method can be overridden by a AbstractDisplay in order to
# update an existing display (instead of, for example, opening a new
# window), and is used by the IJulia interface to defer display
# until the next interactive prompt.  This is especially useful
# for Matlab/Pylab-like stateful plotting interfaces, where
# a plot is created and then modified many times (xlabel, title, etc.).

"""
    redisplay(x)
    redisplay(d::AbstractDisplay, x)
    redisplay(mime, x)
    redisplay(d::AbstractDisplay, mime, x)

By default, the `redisplay` functions simply call [`display`](@ref).
However, some display backends may override `redisplay` to modify an existing
display of `x` (if any).
Using `redisplay` is also a hint to the backend that `x` may be redisplayed
several times, and the backend may choose to defer the display until
(for example) the next interactive prompt.
"""
function redisplay(@nospecialize x)
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

function redisplay(m::Union{MIME,AbstractString}, @nospecialize x)
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
redisplay(d::AbstractDisplay, @nospecialize x) = display(d, x)
redisplay(d::AbstractDisplay, m::Union{MIME,AbstractString}, @nospecialize x) = display(d, m, x)

###########################################################################

end # module

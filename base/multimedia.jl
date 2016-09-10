# This file is a part of Julia. License is MIT: http://julialang.org/license

module Multimedia

export Display, display, pushdisplay, popdisplay, displayable, redisplay,
   MIME, @MIME_str, reprmime, stringmime, istextmime,
   mimewritable, TextDisplay

###########################################################################
# We define a singleton type MIME{mime symbol} for each MIME type, so
# that Julia's dispatch and overloading mechanisms can be used to
# dispatch show and to add conversions for new types.

immutable MIME{mime} end

import Base: show, print, string, convert
MIME(s) = MIME{Symbol(s)}()
show{mime}(io::IO, ::MIME{mime}) = print(io, "MIME type ", string(mime))
print{mime}(io::IO, ::MIME{mime}) = print(io, mime)

macro MIME_str(s)
    :(MIME{$(Expr(:quote, Symbol(s)))})
end

###########################################################################
# For any type T one can define show(io, ::MIME"type", x::T) = ...
# in order to provide a way to export T as a given mime type.

mimewritable{mime}(::MIME{mime}, x) =
  method_exists(show, Tuple{IO, MIME{mime}, typeof(x)})

# it is convenient to accept strings instead of ::MIME
show(io::IO, m::AbstractString, x) = show(io, MIME(m), x)
mimewritable(m::AbstractString, x) = mimewritable(MIME(m), x)

###########################################################################
# MIME types are assumed to be binary data except for a set of types known
# to be text data (possibly Unicode).  istextmime(m) returns whether
# m::MIME is text data, and reprmime(m, x) returns x written to either
# a string (for text m::MIME) or a Vector{UInt8} (for binary m::MIME),
# assuming the corresponding write_mime method exists.  stringmime
# is like reprmime except that it always returns a string, which in the
# case of binary data is Base64-encoded.
#
# Also, if reprmime is passed a AbstractString for a text type or Vector{UInt8} for
# a binary type, the argument is assumed to already be in the corresponding
# format and is returned unmodified.  This is useful so that raw data can be
# passed to display(m::MIME, x).

verbose_show(io, m, x) = show(IOContext(io,limit=false), m, x)

macro textmime(mime)
    quote
        mimeT = MIME{Symbol($mime)}
        # avoid method ambiguities with the general definitions below:
        # (Q: should we treat Vector{UInt8} as a String?)
        Base.Multimedia.reprmime(m::mimeT, x::Vector{UInt8}) = sprint(verbose_show, m, x)
        Base.Multimedia.stringmime(m::mimeT, x::Vector{UInt8}) = reprmime(m, x)

        Base.Multimedia.istextmime(::mimeT) = true
        if $(mime != "text/plain") # strings are shown escaped for text/plain
            Base.Multimedia.reprmime(m::mimeT, x::AbstractString) = x
        end
        Base.Multimedia.reprmime(m::mimeT, x) = sprint(verbose_show, m, x)
        Base.Multimedia.stringmime(m::mimeT, x) = reprmime(m, x)
    end
end

istextmime(::MIME) = false
function reprmime(m::MIME, x)
    s = IOBuffer()
    verbose_show(s, m, x)
    takebuf_array(s)
end
reprmime(m::MIME, x::Vector{UInt8}) = x
stringmime(m::MIME, x) = base64encode(verbose_show, m, x)
stringmime(m::MIME, x::Vector{UInt8}) = base64encode(write, x)

# it is convenient to accept strings instead of ::MIME
istextmime(m::AbstractString) = istextmime(MIME(m))
reprmime(m::AbstractString, x) = reprmime(MIME(m), x)
stringmime(m::AbstractString, x) = stringmime(MIME(m), x)

for mime in ["application/atom+xml", "application/ecmascript", "application/javascript", "application/julia", "application/json", "application/postscript", "application/rdf+xml", "application/rss+xml", "application/x-latex", "application/xhtml+xml", "application/xml", "application/xml-dtd", "image/svg+xml", "model/vrml", "model/x3d+vrml", "model/x3d+xml", "text/calendar", "text/cmd", "text/css", "text/csv", "text/html", "text/javascript", "text/latex", "text/markdown", "text/n3", "text/plain", "text/richtext", "text/sgml", "text/tab-separated-values", "text/vcard", "text/vnd.graphviz", "text/x-setext", "text/x-vcalendar", "text/x-vcard", "text/xml"]
    @eval @textmime $mime
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
displayable(d::Display, mime::AbstractString) = displayable(d, MIME(mime))
displayable(mime::AbstractString) = displayable(MIME(mime))

# simplest display, which only knows how to display text/plain
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

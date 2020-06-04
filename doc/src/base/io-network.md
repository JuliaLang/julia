# I/O and Network

## General I/O

```@docs
Base.stdout
Base.stderr
Base.stdin
Base.open
Base.IOStream
Base.IOBuffer
Base.take!(::Base.GenericIOBuffer)
Base.fdio
Base.flush
Base.close
Base.write
Base.read
Base.read!
Base.readbytes!
Base.unsafe_read
Base.unsafe_write
Base.eachof
Base.peek
Base.position
Base.seek
Base.seekstart
Base.seekend
Base.skip
Base.mark
Base.unmark
Base.reset
Base.ismarked
Base.eof
Base.isreadonly
Base.iswritable
Base.isreadable
Base.isopen
Base.fd
Base.redirect_stdout
Base.redirect_stdout(::Function, ::Any)
Base.redirect_stderr
Base.redirect_stderr(::Function, ::Any)
Base.redirect_stdin
Base.redirect_stdin(::Function, ::Any)
Base.readchomp
Base.truncate
Base.skipchars
Base.countlines
Base.PipeBuffer
Base.readavailable
Base.IOContext
Base.IOContext(::IO, ::Pair)
Base.IOContext(::IO, ::IOContext)
```

## Text I/O

```@docs
Base.show(::IO, ::Any)
Base.summary
Base.print
Base.println
Base.printstyled
Base.sprint
Base.showerror
Base.dump
Meta.@dump
Base.readline
Base.readuntil
Base.readlines
Base.eachline
Base.displaysize
```

## Multimedia I/O

Just as text output is performed by [`print`](@ref) and user-defined types can indicate their textual
representation by overloading [`show`](@ref), Julia provides a standardized mechanism for rich multimedia
output (such as images, formatted text, or even audio and video), consisting of three parts:

  * A function [`display(x)`](@ref) to request the richest available multimedia display of a Julia object
    `x` (with a plain-text fallback).
  * Overloading [`show`](@ref) allows one to indicate arbitrary multimedia representations (keyed by standard
    MIME types) of user-defined types.
  * Multimedia-capable display backends may be registered by subclassing a generic [`AbstractDisplay`](@ref) type
    and pushing them onto a stack of display backends via [`pushdisplay`](@ref).

The base Julia runtime provides only plain-text display, but richer displays may be enabled by
loading external modules or by using graphical Julia environments (such as the IPython-based IJulia
notebook).

```@docs
Base.AbstractDisplay
Base.Multimedia.display
Base.Multimedia.redisplay
Base.Multimedia.displayable
Base.show(::IO, ::Any, ::Any)
Base.Multimedia.showable
Base.repr(::MIME, ::Any)
Base.MIME
Base.@MIME_str
```

As mentioned above, one can also define new display backends. For example, a module that can display
PNG images in a window can register this capability with Julia, so that calling [`display(x)`](@ref) on
types with PNG representations will automatically display the image using the module's window.

In order to define a new display backend, one should first create a subtype `D` of the abstract
class [`AbstractDisplay`](@ref).  Then, for each MIME type (`mime` string) that can be displayed on `D`, one should
define a function `display(d::D, ::MIME"mime", x) = ...` that displays `x` as that MIME type,
usually by calling [`show(io, mime, x)`](@ref) or [`repr(io, mime, x)`](@ref).
A [`MethodError`](@ref) should be thrown if `x` cannot be displayed
as that MIME type; this is automatic if one calls `show` or `repr`. Finally, one should define a function
`display(d::D, x)` that queries [`showable(mime, x)`](@ref) for the `mime` types supported by `D`
and displays the "best" one; a `MethodError` should be thrown if no supported MIME types are found
for `x`.  Similarly, some subtypes may wish to override [`redisplay(d::D, ...)`](@ref Base.Multimedia.redisplay). (Again, one should
`import Base.display` to add new methods to `display`.) The return values of these functions are
up to the implementation (since in some cases it may be useful to return a display "handle" of
some type).  The display functions for `D` can then be called directly, but they can also be invoked
automatically from [`display(x)`](@ref) simply by pushing a new display onto the display-backend stack
with:

```@docs
Base.Multimedia.pushdisplay
Base.Multimedia.popdisplay
Base.Multimedia.TextDisplay
Base.Multimedia.istextmime
```

## Network I/O

```@docs
Base.bytesavailable
Base.ntoh
Base.hton
Base.ltoh
Base.htol
Base.ENDIAN_BOM
```

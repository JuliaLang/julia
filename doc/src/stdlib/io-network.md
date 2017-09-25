# I/O and Network

## General I/O

```@docs
Base.STDOUT
Base.STDERR
Base.STDIN
Base.open
Base.IOBuffer
Base.take!(::Base.GenericIOBuffer)
Base.fdio
Base.flush
Base.close
Base.crc32c(::IO, ::Integer, ::UInt32)
Base.write
Base.read
Base.read!
Base.readbytes!
Base.unsafe_read
Base.unsafe_write
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
Base.Serializer.serialize
Base.Serializer.deserialize
Base.Serializer.writeheader
Base.Grisu.print_shortest
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
Base.show(::Any)
Base.showcompact
Base.summary
Base.print
Base.println
Base.print_with_color
Base.info
Base.warn
Base.logging
Base.Printf.@printf
Base.Printf.@sprintf
Base.sprint
Base.showerror
Base.dump
Meta.@dump
Base.readline
Base.readuntil
Base.readlines
Base.eachline
Base.Base64.Base64EncodePipe
Base.Base64.Base64DecodePipe
Base.Base64.base64encode
Base.Base64.base64decode
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
  * Multimedia-capable display backends may be registered by subclassing a generic `Display` type
    and pushing them onto a stack of display backends via [`pushdisplay`](@ref).

The base Julia runtime provides only plain-text display, but richer displays may be enabled by
loading external modules or by using graphical Julia environments (such as the IPython-based IJulia
notebook).

```@docs
Base.Multimedia.display
Base.Multimedia.redisplay
Base.Multimedia.displayable
Base.show(::Any, ::Any, ::Any)
Base.Multimedia.mimewritable
Base.Multimedia.reprmime
Base.Multimedia.stringmime
```

As mentioned above, one can also define new display backends. For example, a module that can display
PNG images in a window can register this capability with Julia, so that calling [`display(x)`](@ref) on
types with PNG representations will automatically display the image using the module's window.

In order to define a new display backend, one should first create a subtype `D` of the abstract
class `Display`.  Then, for each MIME type (`mime` string) that can be displayed on `D`, one should
define a function `display(d::D, ::MIME"mime", x) = ...` that displays `x` as that MIME type,
usually by calling [`reprmime(mime, x)`](@ref).  A `MethodError` should be thrown if `x` cannot be displayed
as that MIME type; this is automatic if one calls [`reprmime`](@ref). Finally, one should define a function
`display(d::D, x)` that queries [`mimewritable(mime, x)`](@ref) for the `mime` types supported by `D`
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

## Memory-mapped I/O

```@docs
Base.Mmap.Anonymous
Base.Mmap.mmap
Base.Mmap.sync!
```

## Network I/O

```@docs
Base.connect(::TCPSocket, ::Integer)
Base.connect(::AbstractString)
Base.listen(::Any)
Base.listen(::AbstractString)
Base.getaddrinfo
Base.getalladdrinfo
Base.getnameinfo
Base.getsockname
Base.getpeername
Base.IPv4
Base.IPv6
Base.nb_available
Base.accept
Base.listenany
Base.Filesystem.poll_fd
Base.Filesystem.poll_file
Base.Filesystem.watch_file
Base.bind
Base.send
Base.recv
Base.recvfrom
Base.setopt
Base.ntoh
Base.hton
Base.ltoh
Base.htol
Base.ENDIAN_BOM
```

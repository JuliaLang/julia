.. currentmodule:: Base

*****************
 I/O and Network
*****************

General I/O
-----------

.. data:: STDOUT

   Global variable referring to the standard out stream.

.. data:: STDERR

   Global variable referring to the standard error stream.

.. data:: STDIN

   Global variable referring to the standard input stream.

.. function:: open(file_name, [read, write, create, truncate, append]) -> IOStream

   .. Docstring generated from Julia source

   Open a file in a mode specified by five boolean arguments. The default is to open files for reading only. Returns a stream for accessing the file.

.. function:: open(file_name, [mode]) -> IOStream

   .. Docstring generated from Julia source

   Alternate syntax for open, where a string-based mode specifier is used instead of the five booleans. The values of ``mode`` correspond to those from ``fopen(3)`` or Perl ``open``, and are equivalent to setting the following boolean groups:

   ==== =================================
    r    read
    r+   read, write
    w    write, create, truncate
    w+   read, write, create, truncate
    a    write, create, append
    a+   read, write, create, append
   ==== =================================


.. function:: open(f::Function, args...)

   .. Docstring generated from Julia source

   Apply the function ``f`` to the result of ``open(args...)`` and close the resulting file descriptor upon completion.

   **Example**: ``open(readall, "file.txt")``

.. function:: IOBuffer() -> IOBuffer

   .. Docstring generated from Julia source

   Create an in-memory I/O stream.

.. function:: IOBuffer(size::Int)

   .. Docstring generated from Julia source

   Create a fixed size IOBuffer. The buffer will not grow dynamically.

.. function:: IOBuffer(string)

   .. Docstring generated from Julia source

   Create a read-only IOBuffer on the data underlying the given string

.. function:: IOBuffer([data,],[readable,writable,[maxsize]])

   .. Docstring generated from Julia source

   Create an IOBuffer, which may optionally operate on a pre-existing array. If the readable/writable arguments are given, they restrict whether or not the buffer may be read from or written to respectively. By default the buffer is readable but not writable. The last argument optionally specifies a size beyond which the buffer may not be grown.

.. function:: takebuf_array(b::IOBuffer)

   .. Docstring generated from Julia source

   Obtain the contents of an ``IOBuffer`` as an array, without copying. Afterwards, the IOBuffer is reset to its initial state.

.. function:: takebuf_string(b::IOBuffer)

   .. Docstring generated from Julia source

   Obtain the contents of an ``IOBuffer`` as a string, without copying. Afterwards, the IOBuffer is reset to its initial state.

.. function:: fdio([name::AbstractString, ]fd::Integer[, own::Bool]) -> IOStream

   .. Docstring generated from Julia source

   Create an ``IOStream`` object from an integer file descriptor. If ``own`` is ``true``\ , closing this object will close the underlying descriptor. By default, an ``IOStream`` is closed when it is garbage collected. ``name`` allows you to associate the descriptor with a named file.

.. function:: flush(stream)

   .. Docstring generated from Julia source

   Commit all currently buffered writes to the given stream.

.. function:: close(stream)

   .. Docstring generated from Julia source

   Close an I/O stream. Performs a ``flush`` first.

.. function:: write(stream, x)

   .. Docstring generated from Julia source

   Write the canonical binary representation of a value to the given stream. Returns the number of bytes written into the stream.

   You can write multiple values with the same :func:``write`` call. i.e. the following are equivalent:

   .. code-block:: julia

       write(stream, x, y...)
       write(stream, x) + write(stream, y...)

.. function:: read(stream, type)

   .. Docstring generated from Julia source

   Read a value of the given type from a stream, in canonical binary representation.

.. function:: read(stream, type, dims)

   .. Docstring generated from Julia source

   Read a series of values of the given type from a stream, in canonical binary representation. ``dims`` is either a tuple or a series of integer arguments specifying the size of ``Array`` to return.

.. function:: read!(stream, array::Array)

   .. Docstring generated from Julia source

   Read binary data from a stream, filling in the argument ``array``\ .

.. function:: readbytes!(stream, b::Vector{UInt8}, nb=length(b); all=true)

   .. Docstring generated from Julia source

   Read at most ``nb`` bytes from the stream into ``b``\ , returning the number of bytes read (increasing the size of ``b`` as needed).

   See ``readbytes`` for a description of the ``all`` option.

.. function:: readbytes(stream, nb=typemax(Int); all=true)

   .. Docstring generated from Julia source

   Read at most ``nb`` bytes from the stream, returning a ``Vector{UInt8}`` of the bytes read.

   If ``all`` is ``true`` (the default), this function will block repeatedly trying to read all requested bytes, until an error or end-of-file occurs. If ``all`` is ``false``\ , at most one ``read`` call is performed, and the amount of data returned is device-dependent. Note that not all stream types support the ``all`` option.

.. function:: position(s)

   .. Docstring generated from Julia source

   Get the current position of a stream.

.. function:: seek(s, pos)

   .. Docstring generated from Julia source

   Seek a stream to the given position.

.. function:: seekstart(s)

   .. Docstring generated from Julia source

   Seek a stream to its beginning.

.. function:: seekend(s)

   .. Docstring generated from Julia source

   Seek a stream to its end.

.. function:: skip(s, offset)

   .. Docstring generated from Julia source

   Seek a stream relative to the current position.

.. function:: mark(s)

   .. Docstring generated from Julia source

   Add a mark at the current position of stream ``s``.  Returns the marked position.

   See also :func:`unmark`, :func:`reset`, :func:`ismarked`

.. function:: unmark(s)

   .. Docstring generated from Julia source

   Remove a mark from stream ``s``.
   Returns ``true`` if the stream was marked, ``false`` otherwise.

   See also :func:`mark`, :func:`reset`, :func:`ismarked`

.. function:: reset(s)

   .. Docstring generated from Julia source

   Reset a stream ``s`` to a previously marked position, and remove the mark.
   Returns the previously marked position.
   Throws an error if the stream is not marked.

   See also :func:`mark`, :func:`unmark`, :func:`ismarked`

.. function:: ismarked(s)

   .. Docstring generated from Julia source

   Returns ``true`` if stream ``s`` is marked.

   See also :func:`mark`, :func:`unmark`, :func:`reset`

.. function:: eof(stream) -> Bool

   .. Docstring generated from Julia source

   Tests whether an I/O stream is at end-of-file. If the stream is not yet exhausted, this function will block to wait for more data if necessary, and then return ``false``\ . Therefore it is always safe to read one byte after seeing ``eof`` return ``false``\ . ``eof`` will return ``false`` as long as buffered data is still available, even if the remote end of a connection is closed.

.. function:: isreadonly(stream) -> Bool

   .. Docstring generated from Julia source

   Determine whether a stream is read-only.

.. function:: iswritable(io) -> Bool

   .. Docstring generated from Julia source

   Returns ``true`` if the specified IO object is writable (if that can be determined).

.. function:: isreadable(io) -> Bool

   .. Docstring generated from Julia source

   Returns ``true`` if the specified IO object is readable (if that can be determined).

.. function:: isopen(object) -> Bool

   .. Docstring generated from Julia source

   Determine whether an object - such as a stream, timer, or mmap – is not yet closed. Once an object is closed, it will never produce a new event. However, a closed stream may still have data to read in its buffer, use ``eof`` to check for the ability to read data. Use ``poll_fd`` to be notified when a stream might be writable or readable.

.. function:: serialize(stream, value)

   .. Docstring generated from Julia source

   Write an arbitrary value to a stream in an opaque format, such that it can be read back by ``deserialize``\ . The read-back value will be as identical as possible to the original. In general, this process will not work if the reading and writing are done by different versions of Julia, or an instance of Julia with a different system image. ``Ptr`` values are serialized as all-zero bit patterns (``NULL``\ ).

.. function:: deserialize(stream)

   .. Docstring generated from Julia source

   Read a value written by ``serialize``\ .

.. function:: print_escaped(io, str::AbstractString, esc::AbstractString)

   .. Docstring generated from Julia source

   General escaping of traditional C and Unicode escape sequences, plus any characters in esc are also escaped (with a backslash).

.. function:: print_unescaped(io, s::AbstractString)

   .. Docstring generated from Julia source

   General unescaping of traditional C and Unicode escape sequences. Reverse of :func:`print_escaped`.

.. function:: print_joined(io, items, delim, [last])

   .. Docstring generated from Julia source

   Print elements of ``items`` to ``io`` with ``delim`` between them. If ``last`` is specified, it is used as the final delimiter instead of ``delim``\ .

.. function:: print_shortest(io, x)

   .. Docstring generated from Julia source

   Print the shortest possible representation, with the minimum number of consecutive non-zero digits, of number ``x``\ , ensuring that it would parse to the exact same number.

.. function:: fd(stream)

   .. Docstring generated from Julia source

   Returns the file descriptor backing the stream or file. Note that this function only applies to synchronous ``File``\ 's and ``IOStream``\ 's not to any of the asynchronous streams.

.. function:: redirect_stdout()

   .. Docstring generated from Julia source

   Create a pipe to which all C and Julia level ``STDOUT`` output will be redirected. Returns a tuple ``(rd,wr)`` representing the pipe ends. Data written to ``STDOUT`` may now be read from the rd end of the pipe. The wr end is given for convenience in case the old ``STDOUT`` object was cached by the user and needs to be replaced elsewhere.

.. function:: redirect_stdout(stream)

   .. Docstring generated from Julia source

   Replace ``STDOUT`` by stream for all C and julia level output to ``STDOUT``\ . Note that ``stream`` must be a TTY, a ``Pipe`` or a ``TCPSocket``\ .

.. function:: redirect_stderr([stream])

   .. Docstring generated from Julia source

   Like redirect_stdout, but for STDERR

.. function:: redirect_stdin([stream])

   .. Docstring generated from Julia source

   Like redirect_stdout, but for STDIN. Note that the order of the return tuple is still (rd,wr), i.e. data to be read from STDIN, may be written to wr.

.. function:: readchomp(x)

   .. Docstring generated from Julia source

   Read the entirety of ``x`` as a string but remove trailing newlines. Equivalent to ``chomp(readall(x))``\ .

.. function:: truncate(file,n)

   .. Docstring generated from Julia source

   Resize the file or buffer given by the first argument to exactly ``n`` bytes, filling previously unallocated space with '\\0' if the file or buffer is grown

.. function:: skipchars(stream, predicate; linecomment::Char)

   .. Docstring generated from Julia source

   Advance the stream until before the first character for which ``predicate`` returns ``false``\ . For example ``skipchars(stream, isspace)`` will skip all whitespace. If keyword argument ``linecomment`` is specified, characters from that character through the end of a line will also be skipped.

.. function:: countlines(io,[eol::Char])

   .. Docstring generated from Julia source

   Read ``io`` until the end of the stream/file and count the number of lines. To specify a file pass the filename as the first argument. EOL markers other than '\\n' are supported by passing them as the second argument.

.. function:: PipeBuffer()

   .. Docstring generated from Julia source

   An IOBuffer that allows reading and performs writes by appending. Seeking and truncating are not supported. See IOBuffer for the available constructors.

.. function:: PipeBuffer(data::Vector{UInt8},[maxsize])

   .. Docstring generated from Julia source

   Create a PipeBuffer to operate on a data vector, optionally specifying a size beyond which the underlying Array may not be grown.

.. function:: readavailable(stream)

   .. Docstring generated from Julia source

   Read all available data on the stream, blocking the task only if no data is available. The result is a ``Vector{UInt8,1}``\ .

Text I/O
--------

.. function:: show(x)

   .. Docstring generated from Julia source

   Write an informative text representation of a value to the current output stream. New types should overload ``show(io, x)`` where the first argument is a stream. The representation used by ``show`` generally includes Julia-specific formatting and type information.

.. function:: showcompact(x)

   .. Docstring generated from Julia source

   Show a more compact representation of a value. This is used for printing array elements. If a new type has a different compact representation, it should overload ``showcompact(io, x)`` where the first argument is a stream.

.. function:: showall(x)

   .. Docstring generated from Julia source

   Similar to ``show``\ , except shows all elements of arrays.

.. function:: summary(x)

   .. Docstring generated from Julia source

   Return a string giving a brief description of a value. By default returns ``string(typeof(x))``\ , e.g. ``Int64``\ .

   For arrays, returns a string of size and type info, e.g. ``10-element Array{Int64,1}``\ .

.. function:: print(x)

   .. Docstring generated from Julia source

   Write (to the default output stream) a canonical (un-decorated) text representation of a value if there is one, otherwise call ``show``\ . The representation used by ``print`` includes minimal formatting and tries to avoid Julia-specific details.

.. function:: println(x)

   .. Docstring generated from Julia source

   Print (using :func:`print`) ``x`` followed by a newline.

.. function:: print_with_color(color::Symbol, [io], strings...)

   .. Docstring generated from Julia source

   Print strings in a color specified as a symbol, for example ``:red`` or ``:blue``\ .

.. function:: info(msg)

   .. Docstring generated from Julia source

   Display an informational message. Argument ``msg`` is a string describing the information to be displayed.

.. function:: warn(msg)

   .. Docstring generated from Julia source

   Display a warning. Argument ``msg`` is a string describing the warning to be displayed.

.. function:: @printf([io::IOStream], "%Fmt", args...)

   .. Docstring generated from Julia source

   Print ``args`` using C ``printf()`` style format specification string. Optionally, an ``IOStream`` may be passed as the first argument to redirect output.

.. function:: @sprintf("%Fmt", args...)

   .. Docstring generated from Julia source

   Return ``@printf`` formatted output as string.

   .. code-block:: julia

       julia> s = @sprintf "this is a %s %15.1f" "test" 34.567;

       julia> println(s)
       this is a test            34.6

.. function:: sprint(f::Function, args...)

   .. Docstring generated from Julia source

   Call the given function with an I/O stream and the supplied extra arguments. Everything written to this I/O stream is returned as a string.

.. function:: showerror(io, e)

   .. Docstring generated from Julia source

   Show a descriptive representation of an exception object.

.. function:: dump(x)

   .. Docstring generated from Julia source

   Show all user-visible structure of a value.

.. function:: xdump(x)

   .. Docstring generated from Julia source

   Show all structure of a value, including all fields of objects.

.. function:: readall(stream::IO)

   .. Docstring generated from Julia source

   Read the entire contents of an I/O stream as a string.

.. function:: readall(filename::AbstractString)

   .. Docstring generated from Julia source

   Open ``filename``\ , read the entire contents as a string, then close the file. Equivalent to ``open(readall, filename)``\ .

.. function:: readline(stream=STDIN)

   .. Docstring generated from Julia source

   Read a single line of text, including a trailing newline character (if one is reached before the end of the input), from the given ``stream`` (defaults to ``STDIN``\ ),

.. function:: readuntil(stream, delim)

   .. Docstring generated from Julia source

   Read a string, up to and including the given delimiter byte.

.. function:: readlines(stream)

   .. Docstring generated from Julia source

   Read all lines as an array.

.. function:: eachline(stream)

   .. Docstring generated from Julia source

   Create an iterable object that will yield each line from a stream.

.. function:: readdlm(source, delim::Char, T::Type, eol::Char; header=false, skipstart=0, skipblanks=true, use_mmap, ignore_invalid_chars=false, quotes=true, dims, comments=true, comment_char='#')

   .. Docstring generated from Julia source

   Read a matrix from the source where each line (separated by ``eol``\ ) gives one row, with elements separated by the given delimeter. The source can be a text file, stream or byte array. Memory mapped files can be used by passing the byte array representation of the mapped segment as source.

   If ``T`` is a numeric type, the result is an array of that type, with any non-numeric elements as ``NaN`` for floating-point types, or zero. Other useful values of ``T`` include ``ASCIIString``\ , ``AbstractString``\ , and ``Any``\ .

   If ``header`` is ``true``\ , the first row of data will be read as header and the tuple ``(data_cells, header_cells)`` is returned instead of only ``data_cells``\ .

   Specifying ``skipstart`` will ignore the corresponding number of initial lines from the input.

   If ``skipblanks`` is ``true``\ , blank lines in the input will be ignored.

   If ``use_mmap`` is ``true``\ , the file specified by ``source`` is memory mapped for potential speedups. Default is ``true`` except on Windows. On Windows, you may want to specify ``true`` if the file is large, and is only read once and not written to.

   If ``ignore_invalid_chars`` is ``true``\ , bytes in ``source`` with invalid character encoding will be ignored. Otherwise an error is thrown indicating the offending character position.

   If ``quotes`` is ``true``\ , column enclosed within double-quote (") characters are allowed to contain new lines and column delimiters. Double-quote characters within a quoted field must be escaped with another double-quote.  Specifying ``dims`` as a tuple of the expected rows and columns (including header, if any) may speed up reading of large files.  If ``comments`` is ``true``\ , lines beginning with ``comment_char`` and text following ``comment_char`` in any line are ignored.

.. function:: readdlm(source, delim::Char, eol::Char; options...)

   .. Docstring generated from Julia source

   If all data is numeric, the result will be a numeric array. If some elements cannot be parsed as numbers, a cell array of numbers and strings is returned.

.. function:: readdlm(source, delim::Char, T::Type; options...)

   .. Docstring generated from Julia source

   The end of line delimiter is taken as ``n``\ .

.. function:: readdlm(source, delim::Char; options...)

   .. Docstring generated from Julia source

   The end of line delimiter is taken as ``n``\ . If all data is numeric, the result will be a numeric array. If some elements cannot be parsed as numbers, a cell array of numbers and strings is returned.

.. function:: readdlm(source, T::Type; options...)

   .. Docstring generated from Julia source

   The columns are assumed to be separated by one or more whitespaces. The end of line delimiter is taken as ``n``\ .

.. function:: readdlm(source; options...)

   .. Docstring generated from Julia source

   The columns are assumed to be separated by one or more whitespaces. The end of line delimiter is taken as ``n``\ . If all data is numeric, the result will be a numeric array. If some elements cannot be parsed as numbers, a cell array of numbers and strings is returned.

.. function:: writedlm(f, A, delim='\\t', header_string=nothing)

   .. Docstring generated from Julia source

   Write ``A`` (a vector, matrix or an iterable collection of iterable rows) as text to ``f`` (either a filename string or an ``IO`` stream) using the given delimeter ``delim`` (which defaults to tab, but can be any printable Julia object, typically a ``Char`` or ``AbstractString``\ ). Optionally write a ``header_string`` to the file.

   For example, two vectors ``x`` and ``y`` of the same length can be written as two columns of tab-delimited text to ``f`` by either ``writedlm(f, [x y])`` or by ``writedlm(f, zip(x, y))``\ .

.. function:: readcsv(source, [T::Type]; options...)

   .. Docstring generated from Julia source

   Equivalent to ``readdlm`` with ``delim`` set to comma.

.. function:: writecsv(filename, A)

   .. Docstring generated from Julia source

   Equivalent to ``writedlm`` with ``delim`` set to comma.

.. function:: Base64EncodePipe(ostream)

   .. Docstring generated from Julia source

   Returns a new write-only I/O stream, which converts any bytes written to it into base64-encoded ASCII bytes written to ``ostream``\ . Calling ``close`` on the ``Base64Pipe`` stream is necessary to complete the encoding (but does not close ``ostream``\ ).

.. function:: Base64DecodePipe(istream)

   .. Docstring generated from Julia source

   Returns a new read-only I/O stream, which decodes base64-encoded data read from ``istream``\ .

.. function:: base64encode(writefunc, args...)
              base64encode(args...)

   .. Docstring generated from Julia source

   Given a ``write``\ -like function ``writefunc``\ , which takes an I/O stream as its first argument, ``base64(writefunc, args...)`` calls ``writefunc`` to write ``args...`` to a base64-encoded string, and returns the string. ``base64(args...)`` is equivalent to ``base64(write, args...)``\ : it converts its arguments into bytes using the standard ``write`` functions and returns the base64-encoded string.

.. function:: base64decode(string)

   .. Docstring generated from Julia source

   Decodes the base64-encoded ``string`` and returns a ``Vector{UInt8}`` of the decoded bytes.

Multimedia I/O
--------------

Just as text output is performed by ``print`` and user-defined types
can indicate their textual representation by overloading ``show``,
Julia provides a standardized mechanism for rich multimedia output
(such as images, formatted text, or even audio and video), consisting
of three parts:

* A function ``display(x)`` to request the richest available multimedia
  display of a Julia object ``x`` (with a plain-text fallback).
* Overloading ``writemime`` allows one to indicate arbitrary multimedia
  representations (keyed by standard MIME types) of user-defined types.
* Multimedia-capable display backends may be registered by subclassing
  a generic ``Display`` type and pushing them onto a stack of display
  backends via ``pushdisplay``.

The base Julia runtime provides only plain-text display, but richer
displays may be enabled by loading external modules or by using graphical
Julia environments (such as the IPython-based IJulia notebook).

.. function:: display(x)
              display(d::Display, x)
              display(mime, x)
              display(d::Display, mime, x)

   .. Docstring generated from Julia source

   Display ``x`` using the topmost applicable display in the display stack, typically using the richest supported multimedia output for ``x``\ , with plain-text ``STDOUT`` output as a fallback. The ``display(d, x)`` variant attempts to display ``x`` on the given display ``d`` only, throwing a ``MethodError`` if ``d`` cannot display objects of this type.

   There are also two variants with a ``mime`` argument (a MIME type string, such as ``"image/png"``\ ), which attempt to display ``x`` using the requested MIME type *only*, throwing a ``MethodError`` if this type is not supported by either the display(s) or by ``x``\ . With these variants, one can also supply the "raw" data in the requested MIME type by passing ``x::AbstractString`` (for MIME types with text-based storage, such as text/html or application/postscript) or ``x::Vector{UInt8}`` (for binary MIME types).

.. function:: redisplay(x)
              redisplay(d::Display, x)
              redisplay(mime, x)
              redisplay(d::Display, mime, x)

   .. Docstring generated from Julia source

   By default, the ``redisplay`` functions simply call ``display``\ . However, some display backends may override ``redisplay`` to modify an existing display of ``x`` (if any). Using ``redisplay`` is also a hint to the backend that ``x`` may be redisplayed several times, and the backend may choose to defer the display until (for example) the next interactive prompt.

.. function:: displayable(mime) -> Bool
              displayable(d::Display, mime) -> Bool

   .. Docstring generated from Julia source

   Returns a boolean value indicating whether the given ``mime`` type (string) is displayable by any of the displays in the current display stack, or specifically by the display ``d`` in the second variant.

.. function:: writemime(stream, mime, x)

   .. Docstring generated from Julia source

   The ``display`` functions ultimately call ``writemime`` in order to write an object ``x`` as a given ``mime`` type to a given I/O ``stream`` (usually a memory buffer), if possible. In order to provide a rich multimedia representation of a user-defined type ``T``\ , it is only necessary to define a new ``writemime`` method for ``T``\ , via: ``writemime(stream, ::MIME"mime", x::T) = ...``\ , where ``mime`` is a MIME-type string and the function body calls ``write`` (or similar) to write that representation of ``x`` to ``stream``\ . (Note that the ``MIME""`` notation only supports literal strings; to construct ``MIME`` types in a more flexible manner use ``MIME{symbol("")}``\ .)

   For example, if you define a ``MyImage`` type and know how to write it to a PNG file, you could define a function ``writemime(stream, ::MIME"image/png", x::MyImage) = ...`` to allow your images to be displayed on any PNG-capable ``Display`` (such as IJulia). As usual, be sure to ``import Base.writemime`` in order to add new methods to the built-in Julia function ``writemime``\ .

   Technically, the ``MIME"mime"`` macro defines a singleton type for the given ``mime`` string, which allows us to exploit Julia's dispatch mechanisms in determining how to display objects of any given type.

.. function:: mimewritable(mime, x)

   .. Docstring generated from Julia source

   Returns a boolean value indicating whether or not the object ``x`` can be written as the given ``mime`` type. (By default, this is determined automatically by the existence of the corresponding ``writemime`` function for ``typeof(x)``\ .)

.. function:: reprmime(mime, x)

   .. Docstring generated from Julia source

   Returns an ``AbstractString`` or ``Vector{UInt8}`` containing the representation of ``x`` in the requested ``mime`` type, as written by ``writemime`` (throwing a ``MethodError`` if no appropriate ``writemime`` is available). An ``AbstractString`` is returned for MIME types with textual representations (such as ``"text/html"`` or ``"application/postscript"``\ ), whereas binary data is returned as ``Vector{UInt8}``\ . (The function ``istext(mime)`` returns whether or not Julia treats a given ``mime`` type as text.)

   As a special case, if ``x`` is an ``AbstractString`` (for textual MIME types) or a ``Vector{UInt8}`` (for binary MIME types), the ``reprmime`` function assumes that ``x`` is already in the requested ``mime`` format and simply returns ``x``\ .

.. function:: stringmime(mime, x)

   .. Docstring generated from Julia source

   Returns an ``AbstractString`` containing the representation of ``x`` in the requested ``mime`` type. This is similar to ``reprmime`` except that binary data is base64-encoded as an ASCII string.

As mentioned above, one can also define new display backends. For
example, a module that can display PNG images in a window can register
this capability with Julia, so that calling ``display(x)`` on types
with PNG representations will automatically display the image using
the module's window.

In order to define a new display backend, one should first create a
subtype ``D`` of the abstract class ``Display``.  Then, for each MIME
type (``mime`` string) that can be displayed on ``D``, one should
define a function ``display(d::D, ::MIME"mime", x) = ...`` that
displays ``x`` as that MIME type, usually by calling ``reprmime(mime,
x)``.  A ``MethodError`` should be thrown if ``x`` cannot be displayed
as that MIME type; this is automatic if one calls ``reprmime``.
Finally, one should define a function ``display(d::D, x)`` that
queries ``mimewritable(mime, x)`` for the ``mime`` types supported by
``D`` and displays the "best" one; a ``MethodError`` should be thrown
if no supported MIME types are found for ``x``.  Similarly, some
subtypes may wish to override ``redisplay(d::D, ...)``.  (Again, one
should ``import Base.display`` to add new methods to ``display``.)
The return values of these functions are up to the implementation
(since in some cases it may be useful to return a display "handle" of
some type).  The display functions for ``D`` can then be called
directly, but they can also be invoked automatically from
``display(x)`` simply by pushing a new display onto the display-backend
stack with:

.. function:: pushdisplay(d::Display)

   .. Docstring generated from Julia source

   Pushes a new display ``d`` on top of the global display-backend stack. Calling ``display(x)`` or ``display(mime, x)`` will display ``x`` on the topmost compatible backend in the stack (i.e., the topmost backend that does not throw a ``MethodError``\ ).

.. function:: popdisplay()
              popdisplay(d::Display)

   .. Docstring generated from Julia source

   Pop the topmost backend off of the display-backend stack, or the topmost copy of ``d`` in the second variant.

.. function:: TextDisplay(stream)

   .. Docstring generated from Julia source

   Returns a ``TextDisplay <: Display``\ , which can display any object as the text/plain MIME type (only), writing the text representation to the given I/O stream. (The text representation is the same as the way an object is printed in the Julia REPL.)

.. function:: istext(m::MIME)

   .. Docstring generated from Julia source

   Determine whether a MIME type is text data.

Memory-mapped I/O
-----------------

.. function:: Mmap.Anonymous(name, readonly, create)

   .. Docstring generated from Julia source

   Create an ``IO``\ -like object for creating zeroed-out mmapped-memory that is not tied to a file for use in ``Mmap.mmap``\ . Used by ``SharedArray`` for creating shared memory arrays.

.. function:: Mmap.mmap(io::Union{IOStream,AbstractString,Mmap.AnonymousMmap}[, type::Type{Array{T,N}}, dims, offset]; grow::Bool=true, shared::Bool=true)
              Mmap.mmap(type::Type{Array{T,N}}, dims)

   .. Docstring generated from Julia source

   Create an ``Array`` whose values are linked to a file, using memory-mapping. This provides a convenient way of working with data too large to fit in the computer's memory.

   The type is an ``Array{T,N}`` with a bits-type element of ``T`` and dimension ``N`` that determines how the bytes of the array are interpreted. Note that the file must be stored in binary format, and no format conversions are possible (this is a limitation of operating systems, not Julia).

   ``dims`` is a tuple or single ``Integer`` specifying the size or length of the array.

   The file is passed via the stream argument, either as an open ``IOStream`` or filename string.  When you initialize the stream, use ``"r"`` for a "read-only" array, and ``"w+"`` to create a new array used to write values to disk.

   If no ``type`` argument is specified, the default is ``Vector{UInt8}``.

   Optionally, you can specify an offset (in bytes) if, for example, you want to skip over a header in the file. The default value for the offset is the current stream position for an ``IOStream``.

   The ``grow`` keyword argument specifies whether the disk file should be grown to accommodate the requested size of array (if the total file size is < requested array size). Write privileges are required to grow the file.

   The ``shared`` keyword argument specifies whether the resulting ``Array`` and changes made to it will be visible to other processes mapping the same file.

   For example, the following code::

      # Create a file for mmapping
      # (you could alternatively use mmap to do this step, too)
      A = rand(1:20, 5, 30)
      s = open("/tmp/mmap.bin", "w+")
      # We'll write the dimensions of the array as the first two Ints in the file
      write(s, size(A,1))
      write(s, size(A,2))
      # Now write the data
      write(s, A)
      close(s)

      # Test by reading it back in
      s = open("/tmp/mmap.bin")   # default is read-only
      m = read(s, Int)
      n = read(s, Int)
      A2 = Mmap.mmap(s, Matrix{Int}, (m,n))

   creates a ``m``-by-``n`` ``Matrix{Int}``, linked to the file associated with stream ``s``.

   A more portable file would need to encode the word size---32 bit or 64 bit---and endianness information in the header. In practice, consider encoding binary data using standard formats like HDF5 (which can be used with memory-mapping).

.. function:: Mmap.mmap(io, BitArray, [dims, offset])

   .. Docstring generated from Julia source

   Create a ``BitArray`` whose values are linked to a file, using memory-mapping; it has the same purpose, works in the same way, and has the same arguments, as :func:`mmap`, but the byte representation is different.

   **Example**:  ``B = Mmap.mmap(s, BitArray, (25,30000))``

   This would create a 25-by-30000 ``BitArray``, linked to the file associated with stream ``s``.

.. function:: Mmap.sync!(array)

   .. Docstring generated from Julia source

   Forces synchronization between the in-memory version of a memory-mapped ``Array`` or ``BitArray`` and the on-disk version.

Network I/O
-----------

.. function:: connect([host],port) -> TCPSocket

   .. Docstring generated from Julia source

   Connect to the host ``host`` on port ``port``

.. function:: connect(path) -> PipeEndpoint

   .. Docstring generated from Julia source

   Connect to the Named Pipe / Domain Socket at ``path``

.. function:: listen([addr,]port) -> TCPServer

   .. Docstring generated from Julia source

   Listen on port on the address specified by ``addr``\ . By default this listens on localhost only. To listen on all interfaces pass ``IPv4(0)`` or ``IPv6(0)`` as appropriate.

.. function:: listen(path) -> PipeServer

   .. Docstring generated from Julia source

   Create and listen on a Named Pipe / Domain Socket

.. function:: getaddrinfo(host)

   .. Docstring generated from Julia source

   Gets the IP address of the ``host`` (may have to do a DNS lookup)

.. function:: getsockname(sock::Union{TCPServer, TCPSocket}) -> (IPAddr,UInt16)

   .. Docstring generated from Julia source

   Get the IP address and the port that the given TCP socket is connected to (or bound to, in the case of TCPServer).

.. function:: parseip(addr)

   .. Docstring generated from Julia source

   Parse a string specifying an IPv4 or IPv6 ip address.

.. function:: IPv4(host::Integer) -> IPv4

   .. Docstring generated from Julia source

   Returns IPv4 object from ip address formatted as Integer

.. function:: IPv6(host::Integer) -> IPv6

   .. Docstring generated from Julia source

   Returns IPv6 object from ip address formatted as Integer

.. function:: nb_available(stream)

   .. Docstring generated from Julia source

   Returns the number of bytes available for reading before a read from this stream or buffer will block.

.. function:: accept(server[,client])

   .. Docstring generated from Julia source

   Accepts a connection on the given server and returns a connection to the client. An uninitialized client stream may be provided, in which case it will be used instead of creating a new stream.

.. function:: listenany(port_hint) -> (UInt16,TCPServer)

   .. Docstring generated from Julia source

   Create a ``TCPServer`` on any port, using hint as a starting point. Returns a tuple of the actual port that the server was created on and the server itself.

.. function:: poll_fd(fd, timeout_s::Real; readable=false, writable=false)

   .. Docstring generated from Julia source

   Monitor a file descriptor ``fd`` for changes in the read or write availability, and with a timeout given by ``timeout_s`` seconds.

   The keyword arguments determine which of read and/or write status should be monitored; at least one of them must be set to ``true``\ .

   The returned value is an object with boolean fields ``readable``\ , ``writable``\ , and ``timedout``\ , giving the result of the polling.

.. function:: poll_file(path, interval_s::Real, timeout_s::Real) -> (previous::StatStruct, current::StatStruct)

   .. Docstring generated from Julia source

   Monitor a file for changes by polling every ``interval_s`` seconds until a change occurs or ``timeout_s`` seconds have elapsed. The ``interval_s`` should be a long period; the default is 5.007 seconds.

   Returns a pair of ``StatStruct`` objects ``(previous, current)`` when a change is detected.

   To determine when a file was modified, compare ``mtime(prev) != mtime(current)`` to detect notification of changes. However, using ``watch_file`` for this operation is preferred, since it is more reliable and efficient, although in some situations it may not be available.

.. function:: watch_file(path, timeout_s::Real)

   .. Docstring generated from Julia source

   Watch file or directory ``path`` for changes until a change occurs or ``timeout_s`` seconds have elapsed.

   The returned value is an object with boolean fields ``changed``\ , ``renamed``\ , and ``timedout``\ , giving the result of watching the file.

   This behavior of this function varies slightly across platforms. See <https://nodejs.org/api/fs.html#fs_caveats> for more detailed information.

.. function:: bind(socket::Union{UDPSocket, TCPSocket}, host::IPAddr, port::Integer; ipv6only=false)

   .. Docstring generated from Julia source

   Bind ``socket`` to the given ``host:port``\ . Note that ``0.0.0.0`` will listen on all devices. ``ipv6only`` parameter disables dual stack mode. If it's ``true``\ , only IPv6 stack is created.

.. function:: send(socket::UDPSocket, host::IPv4, port::Integer, msg)

   .. Docstring generated from Julia source

   Send ``msg`` over ``socket`` to ``host:port``\ .

.. function:: recv(socket::UDPSocket)

   .. Docstring generated from Julia source

   Read a UDP packet from the specified socket, and return the bytes received. This call blocks.

.. function:: recvfrom(socket::UDPSocket) -> (address, data)

   .. Docstring generated from Julia source

   Read a UDP packet from the specified socket, returning a tuple of (address, data), where address will be either IPv4 or IPv6 as appropriate.

.. function:: setopt(sock::UDPSocket; multicast_loop = nothing, multicast_ttl=nothing, enable_broadcast=nothing, ttl=nothing)

   .. Docstring generated from Julia source

   Set UDP socket options. ``multicast_loop``\ : loopback for multicast packets (default: ``true``\ ). ``multicast_ttl``\ : TTL for multicast packets. ``enable_broadcast``\ : flag must be set to ``true`` if socket will be used for broadcast messages, or else the UDP system will return an access error (default: ``false``\ ). ``ttl``\ : Time-to-live of packets sent on the socket.

.. function:: ntoh(x)

   .. Docstring generated from Julia source

   Converts the endianness of a value from Network byte order (big-endian) to that used by the Host.

.. function:: hton(x)

   .. Docstring generated from Julia source

   Converts the endianness of a value from that used by the Host to Network byte order (big-endian).

.. function:: ltoh(x)

   .. Docstring generated from Julia source

   Converts the endianness of a value from Little-endian to that used by the Host.

.. function:: htol(x)

   .. Docstring generated from Julia source

   Converts the endianness of a value from that used by the Host to Little-endian.

.. data:: ENDIAN_BOM

   The 32-bit byte-order-mark indicates the native byte order of the host machine. Little-endian machines will contain the value 0x04030201. Big-endian machines will contain the value 0x01020304.

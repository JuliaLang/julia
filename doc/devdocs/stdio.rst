***************************************
printf() and stdio in the Julia runtime
***************************************

Libuv wrappers for stdio
------------------------

julia.h defines `libuv <http://docs.libuv.org>`_ wrappers for the
<stdio.h> streams::

    uv_stream_t *JL_STDIN;
    uv_stream_t *JL_STDOUT;
    uv_stream_t *JL_STDERR;

... and corresponding output functions::

    int jl_printf(uv_stream_t *s, const char *format, ...);
    int jl_vprintf(uv_stream_t *s, const char *format, va_list args);

These printf functions are used by :code:`julia/{src,ui}/*.c` wherever stdio
is needed to ensure that output buffering is handled in a unified
way.

In special cases, like signal handlers, where the full libuv
infrastructure is too heavy, :func:`jl_safe_printf` can be used to
:code:`write(2)` directly to :data:`STDERR_FILENO`::

    void jl_safe_printf(const char \*str, ...);



Interface between JL_STD* and Julia code
----------------------------------------

:data:`Base.STDIN`, :data:`Base.STDOUT` and :data:`Base.STDERR` are
bound to the :code:`JL_STD*` `libuv <http://docs.libuv.org>`_ streams
defined in the runtime.

Julia's :func:`__init__` function (inbase/sysimg.jl) calls
:func:`reinit_stdio` (in base/stream.jl) to create Julia objects
for :data:`Base.STDIN`, :data:`Base.STDOUT` and :data:`Base.STDERR`.

:func:`reinit_stdio` uses :func:`ccall` to retrieve pointers to
:code:`JL_STD*` and calls :func:`jl_uv_handle_type()` to inspect
the type of each stream.  It then creates a Julia :code:`Base.File`,
:code:`Base.TTY` or :code:`Base.Pipe` object to represent each
stream. e.g::

    $ julia -e 'typeof((STDIN, STDOUT, STDERR))'
    (TTY,TTY,TTY)

    $ julia -e 'println(typeof((STDIN, STDOUT, STDERR)))' < /dev/null 2>/dev/null
    (Base.FS.File,TTY,Base.FS.File)

    $ echo hello | julia -e 'println(typeof((STDIN, STDOUT, STDERR)))' | cat
    (Pipe,Pipe,TTY)

The :func:`Base.read()` and :func:`Base.write()` methods for these
streams use :func:`ccall` to call libuv wrappers in :code:`src/jl_uv.c`. e.g::

    stream.jl: function write(s::AsyncStream, p::Ptr, nb::Integer)
                   -> ccall(:jl_write_no_copy, ...)
      jl_uv.c:          -> int jl_write_no_copy(uv_stream_t *stream, ...)
                            -> uv_write(uvw, stream, buf, ...)

printf() during initialisation
------------------------------

The libuv streams relied apon by :func:`jl_printf` etc are not
available until mid-way through initialisation of the runtime (see
init.c, :func:`init_stdio`).  Error messages or warnings that need
to be printed before this are routed to the standard C library
:func:`fwrite` function by the following mechanism:

In sys.c the :code:`JL_STD*` stream pointers are statically initialised
to integer constants: STD*_FILENO (0, 1 and 2). In jl_uv.c the
:func:`jl_write` function checks its :code:`uv_stream_t* stream`
argument and calls :func:`fwrite` if stream is set to STDOUT_FILENO
or STDERR_FILENO.

This allows for uniform use of :func:`jl_printf()` throughout the
runtime regardless of whether or not any particular piece of code
is reachable before initialisation is complete.



Legacy ios.c library
--------------------

The :code:`julia/src/support/ios.c` library is inherited from `femptolisp <http://github.com/JeffBezanson/femtolisp>`_.
It provides cross-platform buffered file IO and in-memory temporary buffers.

:code:`ios.c` is still used by:

    - :code:`julia/src/flisp/*.c`
    - :code:`julia/src/dump.c` -- for serialisation file IO and for memory buffers.
    - :code:`base/iostream.jl` -- for file IO (see :code:`base/fs.jl` for libuv equivalent).

Use of :code:`ios.c` in these modules is mostly self contained and
seperated from the libuv io system. However, there is `one place
<http://github.com/JuliaLang/julia/blob/master/src/flisp/print.c#L654>`_
where femptolisp calls though to :func:`jl_printf` with a legacy :code:`ios_t` stream.

There is a hack in :code:`ios.h` that makes the :code:`ios_t.bm`
field line up with the :code:`uv_stream_t.type` and ensures that
the values used for :code:`ios_t.bm` to not overlap with valid
UV_HANDLE_TYPE values.  This allows :code:`uv_stream_t` pointers
to point to :code:`ios_t` streams.

This is needed because :func`jl_printf` caller :func`jl_static_show`
is passed an :code:`ios_t` stream by femptolisp's :func:`fl_print` function.
Julia's :func:`jl_write` function has special handling for this::

    if (stream->type > UV_HANDLE_TYPE_MAX) {
        return ios_write((ios_t*)stream, str, n);
    }

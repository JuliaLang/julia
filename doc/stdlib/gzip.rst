:mod:`GZip` --- Wrapper for gzip functions in zlib
=====================================================

.. module:: GZip
   :synopsis: Wrapper for gzip functions in zlib

.. note:: located in ``gzip.jl``

This module provides a wrapper for the gzip related functions of
(`zlib <http://zlib.net/>`_), a free, general-purpose, legally
unencumbered, lossless data-compression library. These functions
allow the reading and writing of gzip files.

It is currently based on ``zlib`` 1.2.7.

-----
Notes
-----

 * This interface is only for gzipped files, not the streaming zlib
   compression interface. Internally, it depends on/uses the streaming
   interface, but the gzip related functions are higher level
   functions pertaining to gzip files only.

 * :class:`GZipStream` is an implementation of :class:`IO` and can be used virtually
   anywhere :class:`IO` is used.

 * This implementation mimics the :class:`IOStream` implementation, and should
   be a drop-in replacement for :class:`IOStream`, with some exceptions:

   * :func:`seek_end` and :func:`truncate` are not available
   * :func:`readuntil` is available, but is not very efficient.
     (But :func:`readline` works fine.)

In addition to :func:`gzopen` and :func:`gzfdio`/:func:`gzdopen`, the
following :class:`IO`/:class:`IOStream` functions are supported:

  :func:`close()`
  :func:`flush()`
  :func:`seek()`
  :func:`skip()`
  :func:`position()`
  :func:`eof()`
  :func:`read()`
  :func:`readuntil()`
  :func:`readline()`
  :func:`write()`

Due to limitations in ``zlib``, :func:`seek_end` and :func:`truncate` are not available.

---------
Functions
---------

.. function:: gzopen(fname[, gzmode[, buf_size]])

   Opens a file with mode (default ``"r"``), setting internal buffer size
   to buf_size (default ``Z_DEFAULT_BUFSIZE=8192``), and returns a the
   file as a :class:`GZipStream`.

   ``gzmode`` must contain one of

   ==== =================================
    r    read
    w    write, create, truncate
    a    write, create, append
   ==== =================================

   In addition, gzmode may also contain

   ===== =================================
     x    create the file exclusively
          (fails if file exists)
    0-9   compression level
   ===== =================================

   and/or a compression strategy:

   ==== =================================
    f    filtered data
    h    Huffman-only compression
    R    run-length encoding
    F    fixed code compression
   ==== =================================

   Note that ``+`` is not allowed in gzmode.

   If an error occurs, ``gzopen`` throws a :class:`GZError`


.. function:: gzdopen(fd[, gzmode[, buf_size]])

   Create a :class:`GZipStream` object from an integer file descriptor.
   See :func:`gzopen` for ``gzmode`` and ``buf_size`` descriptions.

.. function:: gzdopen(s[, gzmode[, buf_size]])

   Create a :class:`GZipStream` object from :class:`IOStream` ``s``.

-----
Types
-----

.. type:: GZipStream(name, gz_file[, buf_size[, fd[, s]]])

   Subtype of :class:`IO` which wraps a gzip stream.  Returned by
   :func:`gzopen` and :func:`gzdopen`.

.. type:: GZError(err, err_str)

   gzip error number and string.  Possible error values:

   +---------------------+----------------------------------------+
   | ``Z_OK``            | No error                               |
   +---------------------+----------------------------------------+
   | ``Z_ERRNO``         | Filesystem error (consult ``errno()``) |
   +---------------------+----------------------------------------+
   | ``Z_STREAM_ERROR``  | Inconsistent stream state              |
   +---------------------+----------------------------------------+
   | ``Z_DATA_ERROR``    | Compressed data error                  |
   +---------------------+----------------------------------------+
   | ``Z_MEM_ERROR``     | Out of memory                          |
   +---------------------+----------------------------------------+
   | ``Z_BUF_ERROR``     | Input buffer full/output buffer empty  |
   +---------------------+----------------------------------------+
   | ``Z_VERSION_ERROR`` | zlib library version is incompatible   |
   |                     | with caller version                    |
   +---------------------+----------------------------------------+


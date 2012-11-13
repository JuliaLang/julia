:mod:`Zlib` --- Wrapper for zlib compress/uncompress
====================================================

.. module:: Zlib
   :synopsis: Zlib compress/uncompress wrapper

.. note:: located in ``zlib.jl``

This module provides a wrapper for the compress/uncompress and related
utility functions of (`zlib <http://zlib.net/>`_), a free,
general-purpose, legally unencumbered, lossless data-compression
library.  These functions allow the compression and decompression of
byte buffers (``Array{Uint8,1}``).

Note that the zlib stream functions are not yet wrapped.  

It is currently based on zlib 1.2.7.

Utility Functions
-----------------

.. function:: compress_bound(input_size)

   Returns the maximum size of the compressed output buffer for a
   given uncompressed input size.


.. function:: compress(source[, level])

   Compresses source using the given compression level, and returns
   the compressed buffer (``Array{Uint8,1}``).  ``level`` is an
   integer between 0 and 9, or one of ``Z_NO_COMPRESSION``,
   ``Z_BEST_SPEED``, ``Z_BEST_COMPRESSION``, or
   ``Z_DEFAULT_COMPRESSION``.  It defaults to
   ``Z_DEFAULT_COMPRESSION``.

   If an error occurs, ``compress`` throws a :class:`ZError` with more
   information about the error.


.. function:: compress_to_buffer(source, dest, level=Z_DEFAULT_COMPRESSION)

   Compresses the source buffer into the destination buffer, and
   returns the number of bytes written into dest.

   If an error occurs, ``uncompress`` throws a :class:`ZError` with more
   information about the error.


.. function:: uncompress(source[, uncompressed_size])

   Allocates a buffer of size ``uncompressed_size``, uncompresses
   source to this buffer using the given compression level, and
   returns the compressed buffer.  If ``uncompressed_size`` is not
   given, the size of the output buffer is estimated as
   ``2*length(source)``.  If the uncompressed_size is larger than
   uncompressed_size, the allocated buffer is grown and the
   uncompression is retried.

   If an error occurs, ``uncompress`` throws a :class:`ZError` with more
   information about the error.


.. function:: uncompress_to_buffer(source, dest)

   Uncompresses the source buffer into the destination buffer.
   Returns the number of bytes written into dest.  An error is thrown
   if the destination buffer does not have enough space.

   If an error occurs, ``uncompress_to_buffer`` throws a :class:`ZError`
   with more information about the error.


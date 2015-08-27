.. module:: Profile


.. _stdlib-profiling:

***********
 Profiling
***********

.. currentmodule:: Base


.. function:: @profile

   .. Docstring generated from Julia source

   .. code-block:: julia

       @profile

   ``@profile <expression>`` runs your expression while taking periodic backtraces. These are appended to an internal buffer of backtraces.

.. currentmodule:: Base.Profile
The methods in :mod:`Base.Profile` are not exported and need to be called e.g. as ``Profile.print()``.

.. function:: clear()

   .. Docstring generated from Julia source

   .. code-block:: julia

       clear()

   Clear any existing backtraces from the internal buffer.

.. function:: print([io::IO = STDOUT,] [data::Vector]; format = :tree, C = false, combine = true, cols = tty_cols())

   .. Docstring generated from Julia source

   ::

              print([io::IO = STDOUT,] [data::Vector]; format = :tree, C = false, combine = true, cols = tty_cols())

   Prints profiling results to ``io`` (by default, ``STDOUT``). If you
   do not supply a ``data`` vector, the internal buffer of accumulated
   backtraces will be used.  ``format`` can be ``:tree`` or
   ``:flat``. If ``C==true``, backtraces from C and Fortran code are
   shown. ``combine==true`` merges instruction pointers that
   correspond to the same line of code.  ``cols`` controls the width
   of the display.

   ::

              print([io::IO = STDOUT,] data::Vector, lidict::Dict; format = :tree, combine = true, cols = tty_cols())

   Prints profiling results to ``io``. This variant is used to examine
   results exported by a previous call to :func:`retrieve`.
   Supply the vector ``data`` of backtraces and a dictionary
   ``lidict`` of line information.

.. function:: print([io::IO = STDOUT,] data::Vector, lidict::Dict; format = :tree, combine = true, cols = tty_cols())

   .. Docstring generated from Julia source

   ::

              print([io::IO = STDOUT,] [data::Vector]; format = :tree, C = false, combine = true, cols = tty_cols())

   Prints profiling results to ``io`` (by default, ``STDOUT``). If you
   do not supply a ``data`` vector, the internal buffer of accumulated
   backtraces will be used.  ``format`` can be ``:tree`` or
   ``:flat``. If ``C==true``, backtraces from C and Fortran code are
   shown. ``combine==true`` merges instruction pointers that
   correspond to the same line of code.  ``cols`` controls the width
   of the display.

   ::

              print([io::IO = STDOUT,] data::Vector, lidict::Dict; format = :tree, combine = true, cols = tty_cols())

   Prints profiling results to ``io``. This variant is used to examine
   results exported by a previous call to :func:`retrieve`.
   Supply the vector ``data`` of backtraces and a dictionary
   ``lidict`` of line information.

.. function:: init(; n::Integer, delay::Float64)

   .. Docstring generated from Julia source

   .. code-block:: julia

       init(; n::Integer, delay::Float64)

   Configure the ``delay`` between backtraces (measured in seconds), and the number ``n`` of instruction pointers that may be stored. Each instruction pointer corresponds to a single line of code; backtraces generally consist of a long list of instruction pointers. Default settings can be obtained by calling this function with no arguments, and each can be set independently using keywords or in the order ``(n, delay)``\ .

.. function:: fetch() -> data

   .. Docstring generated from Julia source

   ::

              fetch() -> data

   Returns a reference to the internal buffer of backtraces. Note that
   subsequent operations, like :func:`clear`, can affect
   ``data`` unless you first make a copy. Note that the values in
   ``data`` have meaning only on this machine in the current session,
   because it depends on the exact memory addresses used in
   JIT-compiling. This function is primarily for internal use;
   :func:`retrieve` may be a better choice for most users.

.. function:: retrieve() -> data, lidict

   .. Docstring generated from Julia source

   .. code-block:: julia

       retrieve() -> data, lidict

   "Exports" profiling results in a portable format, returning the set of all backtraces (``data``\ ) and a dictionary that maps the (session-specific) instruction pointers in ``data`` to ``LineInfo`` values that store the file name, function name, and line number. This function allows you to save profiling results for future analysis.

.. function:: callers(funcname, [data, lidict], [filename=<filename>], [linerange=<start:stop>]) -> Vector{Tuple{count, linfo}}

   .. Docstring generated from Julia source

   ::

              callers(funcname, [data, lidict], [filename=<filename>], [linerange=<start:stop>]) -> Vector{Tuple{count, linfo}}

   Given a previous profiling run, determine who called a particular
   function. Supplying the filename (and optionally, range of line
   numbers over which the function is defined) allows you to
   disambiguate an overloaded method. The returned value is a vector
   containing a count of the number of calls and line information
   about the caller.  One can optionally supply backtrace data
   obtained from :func:`retrieve`; otherwise, the current internal profile
   buffer is used.

.. function:: clear_malloc_data()

   .. Docstring generated from Julia source

   ::

              clear_malloc_data()

   Clears any stored memory allocation data when running julia with
   ``--track-allocation``.  Execute the command(s) you want to test
   (to force JIT-compilation), then call :func:`clear_malloc_data`.
   Then execute your command(s) again, quit Julia, and examine the
   resulting ``*.mem`` files.


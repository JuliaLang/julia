.. module:: Libc


********************
 C Standard Library
********************

.. function:: malloc(size::Integer) -> Ptr{Void}

   .. Docstring generated from Julia source

   .. code-block:: julia

       malloc(size::Integer) -> Ptr{Void}

   Call ``malloc`` from the C standard library.

.. function:: calloc(num::Integer, size::Integer) -> Ptr{Void}

   .. Docstring generated from Julia source

   .. code-block:: julia

       calloc(num::Integer, size::Integer) -> Ptr{Void}

   Call ``calloc`` from the C standard library.

.. function:: realloc(addr::Ptr, size::Integer) -> Ptr{Void}

   .. Docstring generated from Julia source

   .. code-block:: julia

       realloc(addr::Ptr, size::Integer) -> Ptr{Void}

   Call ``realloc`` from the C standard library.

   See warning in the documentation for ``free`` regarding only using this on memory originally obtained from ``malloc``\ .

.. function:: free(addr::Ptr)

   .. Docstring generated from Julia source

   .. code-block:: julia

       free(addr::Ptr)

   Call ``free`` from the C standard library. Only use this on memory obtained from ``malloc``\ , not on pointers retrieved from other C libraries. ``Ptr`` objects obtained from C libraries should be freed by the free functions defined in that library, to avoid assertion failures if multiple ``libc`` libraries exist on the system.

.. function:: errno([code])

   .. Docstring generated from Julia source

   .. code-block:: julia

       errno([code])

   Get the value of the C library's ``errno``\ . If an argument is specified, it is used to set the value of ``errno``\ .

   The value of ``errno`` is only valid immediately after a ``ccall`` to a C library routine that sets it. Specifically, you cannot call ``errno`` at the next prompt in a REPL, because lots of code is executed between prompts.

.. function:: strerror(n=errno())

   .. Docstring generated from Julia source

   .. code-block:: julia

       strerror(n=errno())

   Convert a system call error code to a descriptive string

.. function:: GetLastError()

   Call the Win32 ``GetLastError`` function [only available on Windows].

.. function:: FormatMessage(n=GetLastError())

   Convert a Win32 system call error code to a descriptive string [only available on Windows].

.. function:: time(t::TmStruct)

   .. Docstring generated from Julia source

   .. code-block:: julia

       time()

   Get the system time in seconds since the epoch, with fairly high (typically, microsecond) resolution.

.. function:: strftime([format], time)

   .. Docstring generated from Julia source

   .. code-block:: julia

       strftime([format], time)

   Convert time, given as a number of seconds since the epoch or a ``TmStruct``\ , to a formatted string using the given format. Supported formats are the same as those in the standard C library.

.. function:: strptime([format], timestr)

   .. Docstring generated from Julia source

   .. code-block:: julia

       strptime([format], timestr)

   Parse a formatted time string into a ``TmStruct`` giving the seconds, minute, hour, date, etc. Supported formats are the same as those in the standard C library. On some platforms, timezones will not be parsed correctly. If the result of this function will be passed to ``time`` to convert it to seconds since the epoch, the ``isdst`` field should be filled in manually. Setting it to ``-1`` will tell the C library to use the current system settings to determine the timezone.

.. function:: TmStruct([seconds])

   .. Docstring generated from Julia source

   .. code-block:: julia

       TmStruct([seconds])

   Convert a number of seconds since the epoch to broken-down format, with fields ``sec``\ , ``min``\ , ``hour``\ , ``mday``\ , ``month``\ , ``year``\ , ``wday``\ , ``yday``\ , and ``isdst``\ .

.. function:: flush_cstdio()

   .. Docstring generated from Julia source

   .. code-block:: julia

       flush_cstdio()

   Flushes the C ``stdout`` and ``stderr`` streams (which may have been written to by external C code).

.. function:: msync(ptr, len, [flags])

   .. Docstring generated from Julia source

   ::

              msync(ptr, len, [flags])

   Forces synchronization of the :func:`mmap`\ ped memory region from ``ptr`` to ``ptr+len``. Flags defaults to ``MS_SYNC``, but can be a combination of ``MS_ASYNC``, ``MS_SYNC``, or ``MS_INVALIDATE``. See your platform man page for specifics. The flags argument is not valid on Windows.

   You may not need to call ``msync``, because synchronization is performed at intervals automatically by the operating system. However, you can call this directly if, for example, you are concerned about losing the result of a long-running calculation.

.. data:: MS_ASYNC

   Enum constant for :func:`msync`. See your platform man page for details. (not available on Windows).

.. data:: MS_SYNC

   Enum constant for :func:`msync`. See your platform man page for details. (not available on Windows).

.. data:: MS_INVALIDATE

   Enum constant for :func:`msync`. See your platform man page for details. (not available on Windows).

.. function:: mmap(len, prot, flags, fd, offset)

   Low-level interface to the ``mmap`` system call. See the man page.

.. function:: munmap(pointer, len)

   Low-level interface for unmapping memory (see the man page). With :func:`mmap_array` you do not need to call this directly; the memory is unmapped for you when the array goes out of scope.

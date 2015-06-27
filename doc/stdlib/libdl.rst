.. module:: Libdl

****************
 Dynamic Linker
****************

.. function:: dlopen(libfile::AbstractString[, flags::Integer])

   Load a shared library, returning an opaque handle. The optional flags argument is a bitwise-or of zero or more of the POSIX (and/or GNU libc and/or MacOS) dlopen command, if possible, or are ignored if the specified functionality is not available on the current platform.  The default is these flags, on POSIX platforms, is to specify symbols to be available for usage in other shared libraries, in situations where there are dependencies between shared libraries.
   

.. function:: dlopen_e(libfile::AbstractString[, flags::Integer])

   Similar to ``dlopen()``, except returns a ``NULL`` pointer instead of raising errors.
   

.. data:: RTLD_DEEPBIND

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_FIRST

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_GLOBAL

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_LAZY

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_LOCAL

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_NODELETE

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_NOLOAD

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. data:: RTLD_NOW

   Enum constant for :func:`dlopen`. See your platform man page for details, if applicable.

.. function:: dlsym(handle, sym)

   Look up a symbol from a shared library handle, return callable function pointer on success.
   

.. function:: dlsym_e(handle, sym)

   Look up a symbol from a shared library handle, silently return NULL pointer on lookup failure.
   

.. function:: dlclose(handle)

   Close shared library referenced by handle.
   

.. function:: find_library(names, locations)

   Searches for the first library in ``names`` in the paths in the that order) which can successfully be dlopen'd. On success, the return value will be one of the names (potentially prefixed by one of the paths in locations). This string can be assigned to a
   

.. data:: DL_LOAD_PATH

   When calling ``dlopen``, the paths in this list will be searched first, in order, before searching the
   system locations for a valid library handle.

.. currentmodule:: Base

*************
 C Interface
*************

.. function:: ccall((symbol, library) or fptr, RetType, (ArgType1, ...), ArgVar1, ...)

   Call function in C-exported shared library, specified by ``(function name, library)`` tuple, where each component is a AbstractString or :Symbol. Alternatively,
   ccall may be used to call a function pointer returned by dlsym, but note that this usage is generally discouraged to facilitate future static compilation.
   Note that the argument type tuple must be a literal tuple, and not a tuple-valued variable or expression.

.. function:: cglobal((symbol, library) or ptr [, Type=Void])

   Obtain a pointer to a global variable in a C-exported shared library, specified exactly as in ``ccall``.  Returns a ``Ptr{Type}``, defaulting to ``Ptr{Void}`` if no Type argument is supplied.  The values can be read or written by ``unsafe_load`` or ``unsafe_store!``, respectively.

.. function:: cfunction(fun::Function, RetType::Type, (ArgTypes...))

   Generate C-callable function pointer from Julia function. Type annotation of the return value in the
   callback function is a must for situations where Julia cannot infer the return type automatically.

   For example::

   	function foo()
   	    # body

   	    retval::Float64
   	end

   	bar = cfunction(foo, Float64, ())


.. function:: dlopen(libfile::AbstractString [, flags::Integer])

   Load a shared library, returning an opaque handle.

   The optional flags argument is a bitwise-or of zero or more of
   ``RTLD_LOCAL``, ``RTLD_GLOBAL``, ``RTLD_LAZY``, ``RTLD_NOW``, ``RTLD_NODELETE``,
   ``RTLD_NOLOAD``, ``RTLD_DEEPBIND``, and ``RTLD_FIRST``.  These are converted to
   the corresponding flags of the POSIX (and/or GNU libc and/or MacOS)
   dlopen command, if possible, or are ignored if the specified
   functionality is not available on the current platform.  The
   default is ``RTLD_LAZY|RTLD_DEEPBIND|RTLD_LOCAL``.  An important usage
   of these flags, on POSIX platforms, is to specify
   ``RTLD_LAZY|RTLD_DEEPBIND|RTLD_GLOBAL`` in order for the library's
   symbols to be available for usage in other shared libraries, in
   situations where there are dependencies between shared libraries.

.. function:: dlopen_e(libfile::AbstractString [, flags::Integer])

   Similar to :func:`dlopen`, except returns a ``NULL`` pointer instead of raising errors.

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

   Searches for the first library in ``names`` in the paths in the ``locations`` list, ``DL_LOAD_PATH``, or system
   library paths (in that order) which can successfully be dlopen'd. On success, the return value will be one of
   the names (potentially prefixed by one of the paths in locations). This string can be assigned to a ``global const``
   and used as the library name in future ``ccall``'s. On failure, it returns the empty string.

.. data:: DL_LOAD_PATH

   When calling ``dlopen``, the paths in this list will be searched first, in order, before searching the
   system locations for a valid library handle.

.. function:: c_malloc(size::Integer) -> Ptr{Void}

   Call ``malloc`` from the C standard library.

.. function:: c_calloc(num::Integer, size::Integer) -> Ptr{Void}

   Call ``calloc`` from the C standard library.

.. function:: c_realloc(addr::Ptr, size::Integer) -> Ptr{Void}

   Call ``realloc`` from the C standard library.

.. function:: c_free(addr::Ptr)

   Call ``free`` from the C standard library.

.. function:: unsafe_load(p::Ptr{T},i::Integer)

   Load a value of type ``T`` from the address of the ith element (1-indexed)
   starting at ``p``. This is equivalent to the C expression ``p[i-1]``.

.. function:: unsafe_store!(p::Ptr{T},x,i::Integer)

   Store a value of type ``T`` to the address of the ith element (1-indexed)
   starting at ``p``. This is equivalent to the C expression ``p[i-1] = x``.

.. function:: unsafe_copy!(dest::Ptr{T}, src::Ptr{T}, N)

   Copy ``N`` elements from a source pointer to a destination, with no checking. The
   size of an element is determined by the type of the pointers.

.. function:: unsafe_copy!(dest::Array, do, src::Array, so, N)

   Copy ``N`` elements from a source array to a destination, starting at offset ``so``
   in the source and ``do`` in the destination (1-indexed).

.. function:: copy!(dest, src)

   Copy all elements from collection ``src`` to array ``dest``. Returns ``dest``.

.. function:: copy!(dest, do, src, so, N)

   Copy ``N`` elements from collection ``src`` starting at offset ``so``, to
   array ``dest`` starting at offset ``do``. Returns ``dest``.

.. function:: pointer(a[, index])

   Get the native address of an array or string element. Be careful to
   ensure that a julia reference to ``a`` exists as long as this
   pointer will be used.

.. function:: pointer(type, int)

   Convert an integer to a pointer of the specified element type.

.. function:: pointer_to_array(p, dims[, own])

   Wrap a native pointer as a Julia Array object. The pointer element type determines
   the array element type. ``own`` optionally specifies whether Julia should take
   ownership of the memory, calling ``free`` on the pointer when the array is no
   longer referenced.

.. function:: pointer_from_objref(obj)

   Get the memory address of a Julia object as a ``Ptr``. The existence of the resulting
   ``Ptr`` will not protect the object from garbage collection, so you must ensure
   that the object remains referenced for the whole time that the ``Ptr`` will be used.

.. function:: unsafe_pointer_to_objref(p::Ptr)

   Convert a ``Ptr`` to an object reference. Assumes the pointer refers to a
   valid heap-allocated Julia object. If this is not the case, undefined behavior
   results, hence this function is considered "unsafe" and should be used with care.

.. function:: disable_sigint(f::Function)

   Disable Ctrl-C handler during execution of a function, for calling
   external code that is not interrupt safe. Intended to be called using ``do``
   block syntax as follows::

    disable_sigint() do
        # interrupt-unsafe code
        ...
    end

.. function:: reenable_sigint(f::Function)

   Re-enable Ctrl-C handler during execution of a function. Temporarily
   reverses the effect of ``disable_sigint``.

.. function:: errno([code])

   Get the value of the C library's ``errno``. If an argument is specified, it is
   used to set the value of ``errno``.

   The value of ``errno`` is only valid immediately after a ``ccall`` to a C
   library routine that sets it. Specifically, you cannot call ``errno`` at the next
   prompt in a REPL, because lots of code is executed between prompts.

.. function:: systemerror(sysfunc, iftrue)

   Raises a ``SystemError`` for ``errno`` with the descriptive string ``sysfunc`` if ``bool`` is true

.. function:: strerror(n)

   Convert a system call error code to a descriptive string

.. data:: Cchar

   Equivalent to the native ``char`` c-type

.. data:: Cuchar

   Equivalent to the native ``unsigned char`` c-type (UInt8)

.. data:: Cshort

   Equivalent to the native ``signed short`` c-type (Int16)

.. data:: Cushort

   Equivalent to the native ``unsigned short`` c-type (UInt16)

.. data:: Cint

   Equivalent to the native ``signed int`` c-type (Int32)

.. data:: Cuint

   Equivalent to the native ``unsigned int`` c-type (UInt32)

.. data:: Clong

   Equivalent to the native ``signed long`` c-type

.. data:: Culong

   Equivalent to the native ``unsigned long`` c-type

.. data:: Clonglong

   Equivalent to the native ``signed long long`` c-type (Int64)

.. data:: Culonglong

   Equivalent to the native ``unsigned long long`` c-type (UInt64)

.. data:: Cintmax_t

   Equivalent to the native ``intmax_t`` c-type (Int64)

.. data:: Cuintmax_t

   Equivalent to the native ``uintmax_t`` c-type (UInt64)

.. data:: Csize_t

   Equivalent to the native ``size_t`` c-type (UInt)

.. data:: Cssize_t

   Equivalent to the native ``ssize_t`` c-type

.. data:: Cptrdiff_t

   Equivalent to the native ``ptrdiff_t`` c-type (Int)

.. data:: Coff_t

   Equivalent to the native ``off_t`` c-type

.. data:: Cwchar_t

   Equivalent to the native ``wchar_t`` c-type (Int32)

.. data:: Cfloat

   Equivalent to the native ``float`` c-type (Float32)

.. data:: Cdouble

   Equivalent to the native ``double`` c-type (Float64)

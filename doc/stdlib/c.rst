.. currentmodule:: Base

*************
 C Interface
*************

.. function:: ccall((symbol, library) or function_pointer, ReturnType, (ArgumentType1, ...), ArgumentValue1, ...)

   .. Docstring generated from Julia source

   Call function in C-exported shared library, specified by ``(function name, library)`` tuple, where each component is a string or symbol.

   Note that the argument type tuple must be a literal tuple, and not a tuple-valued variable or expression. Alternatively, ``ccall`` may also be used to call a function pointer, such as one returned by ``dlsym``\ .

   Each ``ArgumentValue`` to the ``ccall`` will be converted to the corresponding ``ArgumentType``\ , by automatic insertion of calls to ``unsafe_convert(ArgumentType, cconvert(ArgumentType, ArgumentValue))``\ . (See also the documentation for each of these functions for further details.) In most cases, this simply results in a call to ``convert(ArgumentType, ArgumentValue)``\ .

.. function:: cglobal((symbol, library) [, type=Void])

   .. Docstring generated from Julia source

   Obtain a pointer to a global variable in a C-exported shared library, specified exactly as in ``ccall``\ . Returns a ``Ptr{Type}``\ , defaulting to ``Ptr{Void}`` if no Type argument is supplied. The values can be read or written by ``unsafe_load`` or ``unsafe_store!``\ , respectively.

.. function:: cfunction(function::Function, ReturnType::Type, (ArgumentTypes...))

   .. Docstring generated from Julia source

   Generate C-callable function pointer from Julia function. Type annotation of the return value in the callback function is a must for situations where Julia cannot infer the return type automatically.

   For example:

   .. code-block:: julia

       function foo()
           # body

           retval::Float64
       end

       bar = cfunction(foo, Float64, ())

.. function:: unsafe_convert(T,x)

   .. Docstring generated from Julia source

   Convert ``x`` to a value of type ``T``

   In cases where ``convert`` would need to take a Julia object and turn it into a ``Ptr``\ , this function should be used to define and perform that conversion.

   Be careful to ensure that a julia reference to ``x`` exists as long as the result of this function will be used. Accordingly, the argument ``x`` to this function should never be an expression, only a variable name or field reference. For example, ``x=a.b.c`` is acceptable, but ``x=[a,b,c]`` is not.

   The ``unsafe`` prefix on this function indicates that using the result of this function after the ``x`` argument to this function is no longer accessible to the program may cause undefined behavior, including program corruption or segfaults, at any later time.

.. function:: cconvert(T,x)

   .. Docstring generated from Julia source

   Convert ``x`` to a value of type ``T``\ , typically by calling ``convert(T,x)``

   In cases where ``x`` cannot be safely converted to ``T``\ , unlike ``convert``\ , ``cconvert`` may return an object of a type different from ``T``\ , which however is suitable for ``unsafe_convert`` to handle.

   Neither ``convert`` nor ``cconvert`` should take a Julia object and turn it into a ``Ptr``\ .

.. function:: unsafe_load(p::Ptr{T},i::Integer)

   .. Docstring generated from Julia source

   Load a value of type ``T`` from the address of the ith element (1-indexed) starting at ``p``\ . This is equivalent to the C expression ``p[i-1]``\ .

   The ``unsafe`` prefix on this function indicates that no validation is performed on the pointer ``p`` to ensure that it is valid. Incorrect usage may segfault your program or return garbage answers, in the same manner as C.

.. function:: unsafe_store!(p::Ptr{T},x,i::Integer)

   .. Docstring generated from Julia source

   Store a value of type ``T`` to the address of the ith element (1-indexed) starting at ``p``\ . This is equivalent to the C expression ``p[i-1] = x``\ .

   The ``unsafe`` prefix on this function indicates that no validation is performed on the pointer ``p`` to ensure that it is valid. Incorrect usage may corrupt or segfault your program, in the same manner as C.

.. function:: unsafe_copy!(dest::Ptr{T}, src::Ptr{T}, N)

   .. Docstring generated from Julia source

   Copy ``N`` elements from a source pointer to a destination, with no checking. The size of an element is determined by the type of the pointers.

   The ``unsafe`` prefix on this function indicates that no validation is performed on the pointers ``dest`` and ``src`` to ensure that they are valid. Incorrect usage may corrupt or segfault your program, in the same manner as C.

.. function:: unsafe_copy!(dest::Array, do, src::Array, so, N)

   .. Docstring generated from Julia source

   Copy ``N`` elements from a source array to a destination, starting at offset ``so`` in the source and ``do`` in the destination (1-indexed).

   The ``unsafe`` prefix on this function indicates that no validation is performed to ensure that N is inbounds on either array. Incorrect usage may corrupt or segfault your program, in the same manner as C.

.. function:: copy!(dest, src)

   .. Docstring generated from Julia source

   Copy all elements from collection ``src`` to array ``dest``\ . Returns ``dest``\ .

.. function:: copy!(dest, do, src, so, N)

   .. Docstring generated from Julia source

   Copy ``N`` elements from collection ``src`` starting at offset ``so``\ , to array ``dest`` starting at offset ``do``\ . Returns ``dest``\ .

.. function:: pointer(array [, index])

   .. Docstring generated from Julia source

   Get the native address of an array or string element. Be careful to ensure that a julia reference to ``a`` exists as long as this pointer will be used. This function is "unsafe" like ``unsafe_convert``\ .

   Calling ``Ref(array[, index])`` is generally preferable to this function.

.. function:: pointer_to_array(pointer, dims[, take_ownership::Bool])

   .. Docstring generated from Julia source

   Wrap a native pointer as a Julia Array object. The pointer element type determines the array element type. ``own`` optionally specifies whether Julia should take ownership of the memory, calling ``free`` on the pointer when the array is no longer referenced.

.. function:: pointer_from_objref(object_instance)

   .. Docstring generated from Julia source

   Get the memory address of a Julia object as a ``Ptr``\ . The existence of the resulting ``Ptr`` will not protect the object from garbage collection, so you must ensure that the object remains referenced for the whole time that the ``Ptr`` will be used.

.. function:: unsafe_pointer_to_objref(p::Ptr)

   .. Docstring generated from Julia source

   Convert a ``Ptr`` to an object reference. Assumes the pointer refers to a valid heap-allocated Julia object. If this is not the case, undefined behavior results, hence this function is considered "unsafe" and should be used with care.

.. function:: disable_sigint(f::Function)

   .. Docstring generated from Julia source

   Disable Ctrl-C handler during execution of a function, for calling external code that is not interrupt safe. Intended to be called using ``do`` block syntax as follows:

   .. code-block:: julia

       disable_sigint() do
           # interrupt-unsafe code
           ...
       end

.. function:: reenable_sigint(f::Function)

   .. Docstring generated from Julia source

   Re-enable Ctrl-C handler during execution of a function. Temporarily reverses the effect of ``disable_sigint``\ .

.. function:: systemerror(sysfunc, iftrue)

   .. Docstring generated from Julia source

   Raises a ``SystemError`` for ``errno`` with the descriptive string ``sysfunc`` if ``iftrue`` is ``true``

.. data:: Ptr{T}

   A memory address referring to data of type ``T``.
   However, there is no guarantee that the memory is actually valid,
   or that it actually represents data of the specified type.

.. data:: Ref{T}

   An object that safely references data of type ``T``.
   This type is guaranteed to point to valid, Julia-allocated memory
   of the correct type. The underlying data is protected from freeing by
   the garbage collector as long as the ``Ref`` itself is referenced.

   When passed as a ``ccall`` argument (either as a ``Ptr`` or ``Ref`` type),
   a ``Ref`` object will be converted to a native pointer to the data it references.

   There is no invalid (NULL) ``Ref``.

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

****************
 LLVM Interface
****************

.. function:: llvmcall(IR::String, ReturnType, (ArgumentType1, ...), ArgumentValue1, ...)
              llvmcall((declarations::String, IR::String), ReturnType, (ArgumentType1, ...), ArgumentValue1, ...)

   .. Docstring generated from Julia source

   Call LLVM IR string in the first argument. Similar to an LLVM function ``define`` block, arguments are available as consecutive unnamed SSA variables (%0, %1, etc.).

   The optional declarations string contains external functions declarations that are necessary for llvm to compile the IR string. Multiple declarations can be passed in by separating them with line breaks.

   Note that the argument type tuple must be a literal tuple, and not a tuple-valued variable or expression.

   Each ``ArgumentValue`` to ``llvmcall`` will be converted to the corresponding ``ArgumentType``\ , by automatic insertion of calls to ``unsafe_convert(ArgumentType, cconvert(ArgumentType, ArgumentValue))``\ . (see also the documentation for each of these functions for further details). In most cases, this simply results in a call to ``convert(ArgumentType, ArgumentValue)``\ .

   See ``test/llvmcall.jl`` for usage examples.


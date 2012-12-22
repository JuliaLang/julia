strpack.jl --- Convert Julia types <--> C structures
====================================================

.. .. module:: strpack.jl
   :synopsis: Convert Julia types <--> C structures

This module allows Julia composite types to be converted to a form suitable to pass as a structure input
to a ``ccall``. It can also convert C-structure outputs back into Julia types.

-------
Example
-------

Probably the easiest way to see how it works is to try an example. Let's create a C library as follows:

.. code-block:: c

    struct teststruct {
      int      int1;
      float    float1;
    };
    
    void getvalues(struct teststruct* ts)
    {
      ts->int1 = 7;
      ts->float1 = 3.7;
    }

The ``getvalues`` function just fills the two fields with specified values. Compile this as a shared library,
which on Linux is achieved with ``gcc -fPIC teststruct.c -shared -o libteststruct.so``.

Let's also create the Julia analog of this structure::

    type TestStruct
        int1::Int32
        float1::Float32
    end
    TestStruct(i, f) = TestStruct(convert(Int32, i), convert(Float32, f))

Note that C's ``int`` corresponds to ``Int32``. Let's initialize an object of this type::

    s = TestStruct(-1, 1.2)
    
Now we load the strpack "module", ``load("strpack")``, which also brings in ``iostring.jl``. We're going to
pack ``s`` into a form suitable to pass as the input to our C function ``getvalues``, which we do in the
following way::

    iostr = IOString()
    pack(iostr, s)

It's worth seeing what has happened here::

    julia> iostr
    IOString([0xff, 0xff, 0xff, 0xff, 0x9a, 0x99, 0x99, 0x3f],9)

The first 4 bytes correspond to the ``Int32`` representation of -1, and the last 4 to the ``Float32``
representation of 1.2. In other words, this is just a packed memory buffer encoding ``s``. (There are
subtleties such as data alignment, endian status, etc. strpack knows about this stuff, and users who need
to control its behavior manually can do so.)

Now we load our library and make a ``ccall``::

    const libtest = dlopen("libteststruct")
    ccall(dlsym(libtest, :getvalues), Void, (Ptr{Void},), iostr.data)

The C function ``getvalues`` stores its output in the buffer we provided as input. We unpack this buffer back
into a Julia type::

    seek(iostr, 0)   # "rewind" to the beginning of the buffer
    s2 = unpack(iostr, TestStruct)

Voila! You have the result back.
    
.. function:: pack(io, composite, [strategy])

    Create a packed buffer representation of ``composite`` in stream ``io``, using data alignment coded by
    ``strategy``. This buffer is suitable to pass as a ``struct`` argument in a ``ccall``.
    
.. function:: unpack(io, T, [strategy])

    Extract an instance of the Julia composite type ``T`` from the packed representation in the stream ``io``.
    ``io`` must be positioned at the beginning (using ``seek``). This allows you to read C ``struct`` outputs
    from ``ccall``.


******************************
Memory layout of Julia Objects
******************************

Object layout (jl_value_t)
--------------------------

.. sidebar:: `special case. <https://github.com/JuliaLang/julia/blob/master/src/jltypes.c#L2897>`_

    :code:`jl_tuple_type->type = jl_tuple_type`

The :code:`jl_value_t` struct defines the minimal header for a Julia
object in memory.
The :code:`type` field points to a
`jl_datatype_t <http://github.com/JuliaLang/julia/blob/master/src/julia.h#L204>`_ object,
(the jl_typeof() macro should be used to query it)::

    typedef struct _jl_value_t {
        struct _jl_value_t *type;
    } jl_value_t;

    #define jl_typeof(v) (((jl_value_t*)(v))->type)


The layout of the rest of the object is dependant on its type.

e.g. a :func:`Base.tuple` object has an array of pointers to the
objects contained by the tuple::

    typedef struct {
        struct _jl_value_t *type;
        size_t length;
        jl_value_t *data[];
    } jl_tuple_t;

e.g. a "boxed" uint16_t (created by :func:`jl_box_uint16`) is stored as
follows (assuming machine is 64-bit)::

    struct {
        struct _jl_value_t *type; -- 8 bytes
        uint16_t data;            -- 2 bytes
                                  -- 6 bytes padding
    };

Structs for the built-in types are `defined in julia.h <http://github.com/JuliaLang/julia/blob/master/src/julia.h#L69>`_. The corresponding global jl_datatype_t objects are created by `jl_init_types() <http://github.com/JuliaLang/julia/blob/master/src/jltypes.c#L2887>`_.


Garbage collector mark bit
--------------------------

The garbage collector uses the low bit of the :code:`jl_value_t.type`
pointer as a flag to mark reachable objects (see :code:`gcval_t`).
During each mark/sweep cycle, the gc sets the mark bit of each
reachable object, deallocates objects that are not marked, then
clears the mark bits. While the mark/sweep is in progress the
:code:`jl_value_t.type` pointer is altered by the mark bit. The gc
uses the :func:`gc_typeof` macro to retrieve the original type
pointer::

    #define gc_typeof(v) ((jl_value_t*)(((uptrint_t)jl_typeof(v))&~1UL))


Object allocation
-----------------

Storage for new objects is allocated by :func:`newobj` in julia_internal.h::

    STATIC_INLINE jl_value_t *newobj(jl_value_t *type, size_t nfields)
    {
        jl_value_t *jv = (jl_value_t*)allocobj((1+nfields) * sizeof(void*));
        jv->type = type;
        return jv;
    }

.. sidebar:: :ref:`man-singleton-types`


    Singleton types have only one instance and no data fields.
    Singleton instances use only 8 bytes.
    e.g. :data:`nothing::Void`.

    See :ref:`man-singleton-types` and :ref:`man-nothing`

Note that all objects are allocated in multiples of 8 bytes, so the
smallest object size is 16 bytes (8 byte type pointer + 8 bytes
data).  :func:`allocobj` in gc.c allocates memory for new objects.
Memory is allocated from a pool for objects up to 2048 bytes, or
by malloc() otherwise.

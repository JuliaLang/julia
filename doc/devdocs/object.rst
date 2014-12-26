******************************
Memory layout of Julia Objects
******************************

Object layout (jl_value_t)
--------------------------

The :code:`jl_value_t` struct defines the minimal header for a Julia
object in memory.  Every object begins with a pointer to a
:code:`jl_datatype_t` object::

    typedef struct _jl_value_t {
        struct _jl_value_t *type;
    } jl_value_t;

The layout of the rest of the object is dependant on its type.

e.g. a :func:`Base.tuple` object has an array of pointers to the
objects contained by the tuple::

    typedef struct {
        struct _jl_value_t *type;
        size_t length;
        jl_value_t *data[];
    } jl_tuple_t;

e.g. a "boxed" uint16_t (created by :func:`jl_box_uint16`) is stored as
follows::

    struct {
        struct _jl_value_t *type; -- 8 bytes
        uint16_t data;            -- 2 bytes
                                  -- 6 bytes padding
    };


Garbage collector mark bit
--------------------------

The garbage collector uses the low bit of the :code:`jl_value_t.type`
pointer as a flag to mark reachable objects (see :code:`gcval_t`).
During each mark/sweep cycle, the gc sets the mark bit of each
reachable object, deallocates objects that are not marked, then
clears the mark bits. While the mark/sweep is in progress the
:code:`jl_value_t.type` pointer is altered by the mark bit. The gc
uses the :func:`gc_typeof` macro to retrieve the original type
pointer.

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

Note that all objects are allocated in multiples of 8 bytes, so the
smallest object size is 16 bytes (8 byte type pointer + 8 bytes
data).  :func:`allocobj` in gc.c allocates memory for new objects.
Memory is allocated from a pool for objects up to 2048 bytes, or
by malloc() otherwise.::

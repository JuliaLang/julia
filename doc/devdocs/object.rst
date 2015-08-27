.. highlight:: c

.. currentmodule:: Base


******************************
Memory layout of Julia Objects
******************************

Object layout (jl_value_t)
--------------------------

The :c:type:`jl_value_t` struct is the name for a block of memory owned by the Julia Garbage Collector,
representing the data associated with a Julia object in memory.
Absent any type information, it is simply an opaque pointer::

    typedef struct jl_value_t* jl_pvalue_t;

Each :c:type:`jl_value_t` struct is contained in a :c:type:`jl_typetag_t` struct that contains metadata information
about the Julia object, such as its type and garbage collector (gc) reachability::

    typedef struct {
        opaque metadata;
        jl_value_t value;
    } jl_typetag_t;

The type of any Julia object is an instance of a leaf :c:type:`jl_datatype_t` object.
The :c:func:`jl_typeof` function can be used to query for it::

    jl_value_t *jl_typeof(jl_value_t *v);

The layout of the object depends on its type.
Reflection methods can be used to inspect that layout.
A field can be accessed by calling one of the get-field methods::

    jl_value_t *jl_get_nth_field_checked(jl_value_t *v, size_t i);
    jl_value_t *jl_get_field(jl_value_t *o, char *fld);

If the field types are known, a priori, to be all pointers,
the values can also be extracted directly as an array access::

    jl_value_t *v = value->fieldptr[n];

As an example, a "boxed" :c:type:`uint16_t` is stored as follows::

    struct {
        oqaque metadata;
        struct {
          uint16_t data;            -- 2 bytes
        } jl_value_t;
    };

This object is created by :c:func:`jl_box_uint16`.
Note that the :c:type:`jl_value_t` pointer references the data portion,
not the metadata at the top of the struct.

A value may be stored "unboxed" in many circumstances
(just the data, without the metadata, and possibly not even stored but just kept in registers),
so it is unsafe to assume that the address of a box is a unique identifier.
The "egal" test (corresponding to the :func:`is` function in Julia),
should instead be used to compare two unknown objects for equivalence::

    int jl_egal(jl_value_t *a, jl_value_t *b);

This optimization should be relatively transparent to the API,
since the object will be "boxed" on-demand, whenever a :c:type:`jl_value_t` pointer is needed.

Note that modification of a :c:type:`jl_value_t` pointer in memory is permitted only if the object is mutable.
Otherwise, modification of the value may corrupt the program and the result will be undefined.
The mutability property of a value can be queried for with::

    int jl_is_mutable(jl_value_t *v);

If the object being stored is a :c:type:`jl_value_t`, the Julia garbage collector must be notified also::

    void jl_gc_wb(jl_value_t *parent, jl_value_t *ptr);

However, the :ref:`man-embedding` section of the manual is also required reading at this point,

for covering other details of boxing and unboxing various types,
and understanding the gc interactions.

Mirror structs for some of the built-in types are `defined in julia.h <https://github.com/JuliaLang/julia/blob/master/src/julia.h>`_.
The corresponding global :c:type:`jl_datatype_t` objects are created by `jl_init_types in jltypes.c <https://github.com/JuliaLang/julia/blob/master/src/jltypes.c>`_.

Garbage collector mark bits
---------------------------

The garbage collector uses several bits from the metadata portion of the :c:type:`jl_typetag_t`
to track each object in the system.
Further details about this algorithm can be found in the comments of the `garbage collector implementation in gc.c
<https://github.com/JuliaLang/julia/blob/master/src/gc.c>`_.

Object allocation
-----------------

Most new objects are allocated by :c:func:`jl_new_structv`::

    jl_value_t *jl_new_struct(jl_datatype_t *type, ...);
    jl_value_t *jl_new_structv(jl_datatype_t *type, jl_value_t **args, uint32_t na);

Although, :obj:`isbits` objects can be also constructed directly from memory::

    jl_value_t *jl_new_bits(jl_value_t *bt, void *data)

And some objects have special constructors that must be used instead of the above functions:

Types::

    jl_datatype_t *jl_apply_type(jl_datatype_t *tc, jl_tuple_t *params);
    jl_datatype_t *jl_apply_array_type(jl_datatype_t *type, size_t dim);
    jl_uniontype_t *jl_new_uniontype(jl_tuple_t *types);

While these are the most commonly used options, there are more low-level constructors too,
which you can find declared in `julia.h <https://github.com/JuliaLang/julia/blob/master/src/julia.h>`_.
These are used in :c:func:`jl_init_types` to create the initial types needed to bootstrap the creation of the Julia system image.

Tuples::

    jl_tuple_t *jl_tuple(size_t n, ...);
    jl_tuple_t *jl_tuplev(size_t n, jl_value_t **v);
    jl_tuple_t *jl_alloc_tuple(size_t n);

The representation of tuples is highly unique in the Julia object representation ecosystem.
In some cases, a :func:`Base.tuple` object may be an array of pointers to the
objects contained by the tuple equivalent to::

    typedef struct {
        size_t length;
        jl_value_t *data[length];
    } jl_tuple_t;

However, in other cases, the tuple may be converted to an anonymous :obj:`isbits` type
and stored unboxed, or it may not stored at all (if it is not being used in a generic context as a :code:`jl_value_t*`).

Symbols::

    jl_sym_t *jl_symbol(const char *str);

Functions and LambdaStaticData::

    jl_function_t *jl_new_generic_function(jl_sym_t *name);
    jl_lambda_info_t *jl_new_lambda_info(jl_value_t *ast, jl_tuple_t *sparams);
    jl_function_t *jl_new_closure(jl_fptr_t proc, jl_value_t *env, jl_lambda_info_t *li);

Arrays::

    jl_array_t *jl_new_array(jl_value_t *atype, jl_tuple_t *dims);
    jl_array_t *jl_new_arrayv(jl_value_t *atype, ...);
    jl_array_t *jl_alloc_array_1d(jl_value_t *atype, size_t nr);
    jl_array_t *jl_alloc_array_2d(jl_value_t *atype, size_t nr, size_t nc);
    jl_array_t *jl_alloc_array_3d(jl_value_t *atype, size_t nr, size_t nc, size_t z);
    jl_array_t *jl_alloc_cell_1d(size_t n);

Note that many of these have alternative allocation functions for various special-purposes.
The list here reflects the more common usages, but a more complete list can be found by reading the `julia.h header file
<https://github.com/JuliaLang/julia/blob/master/src/julia.h>`_.

Internal to Julia, storage is typically allocated by :c:func:`newstruct` (or :func:`newobj` for the special types)::

    jl_value_t *newstruct(jl_value_t *type);
    jl_value_t *newobj(jl_value_t *type, size_t nfields);

And at the lowest level, memory is getting allocated by a call to the garbage collector (in ``gc.c``),
then tagged with its type::

    jl_value_t *jl_gc_allocobj(size_t nbytes);
    void jl_set_typeof(jl_value_t *v, jl_datatype_t *type);

.. sidebar:: :ref:`man-singleton-types`

    Singleton types have only one instance and no data fields.
    Singleton instances have a size of 0 bytes,
    and consist only of their metadata.
    e.g. :data:`nothing::Void`.

    See :ref:`man-singleton-types` and :ref:`man-nothing`

Note that all objects are allocated in multiples of 4 bytes and aligned to the platform pointer size.
Memory is allocated from a pool for smaller objects, or directly with :c:func:`malloc` for large objects.

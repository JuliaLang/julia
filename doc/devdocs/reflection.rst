****************************
Reflection and introspection
****************************

.. currentmodule:: Base

Julia provides a variety of runtime reflection capabilities.

.. rubric:: Module bindings

The exported names for a :obj:`Module` are available using :func:`names(m::Module) <names>`, which will return
an array of :obj:`Symbol` elements representing the exported bindings.
``names(m::Module, true)`` returns symbols for all bindings in ``m``, regardless of export status.

.. rubric:: DataType fields

The names of :obj:`DataType` fields may be interrogated
using :func:`fieldnames`. For example, given the following type, ``fieldnames(Point)`` returns an arrays of :obj:`Symbol`
elements representing the field names:

.. doctest::

    julia> type Point
               x::Int
               y
           end

    julia> fieldnames(Point)
    2-element Array{Symbol,1}:
     :x
     :y

The type of each field in a ``Point`` object is stored in the ``types`` field of the ``Point`` variable itself:

.. doctest::

    julia> Point.types
    svec(Int64,Any)

While ``x`` is annotated as an ``Int``, ``y`` was unannotated in the type definition, therefore ``y`` defaults to the ``Any`` type.

Types are themselves represented as a structure called :obj:`DataType`:

.. doctest::

    julia> typeof(Point)
    DataType

Note that ``fieldnames(DataType)`` gives the names for each field of :obj:`DataType` itself, and
one of these fields is the ``types`` field observed in the example above.

.. rubric:: Subtypes

The *direct* subtypes of any :obj:`DataType` may be listed using
:func:`subtypes`. For example, the abstract :obj:`DataType` :obj:`AbstractFloat`
has four (concrete) subtypes:

.. doctest::

    julia> subtypes(AbstractFloat)
    4-element Array{Any,1}:
     Base.MPFR.BigFloat
     Float16
     Float32
     Float64

Any abstract subtype will also be included in this list, but further subtypes
thereof will not; recursive application of :func:`subtypes` may be used to inspect
the full type tree.

.. rubric:: DataType layout

The internal representation of a :obj:`DataType` is critically important when interfacing with
C code and several functions are available to inspect these details.
:func:`isbits(T::DataType) <isbits>` returns true if ``T`` is
stored with C-compatible alignment.
:func:`fieldoffsets(T::DataType) <fieldoffsets>` returns the (byte) offset for each
field relative to the start of the type.

.. rubric:: Function methods

The methods of any generic function may be listed using :func:`methods`. The method dispatch
table may be searched for methods accepting a given type using :func:`methodswith`.

.. rubric:: Expansion and lowering

As discussed in the :ref:`Metaprogramming <man-metaprogramming>` section, the
:func:`macroexpand` function gives the unquoted and interpolated expression (``Expr``) form
for a given macro. To use ``macroexpand``, ``quote`` the expression block itself (otherwise,
the macro will be evaluated and the result will be passed instead!). For example:

.. doctest::

   julia> macroexpand( :(@edit println("")) )
   :(Base.edit(println,Base.typesof("")))

The functions :func:`Base.Meta.show_sexpr` and :func:`dump` are used to display S-expr style views
and depth-nested detail views for any expression.

Finally, the :func:`expand` function gives the ``lowered`` form of any expression and is of particular
interest for understanding both macros and top-level statements such as function declarations and
variable assignments:

.. doctest::

   julia> expand( :(f() = 1) )
   :($(Expr(:method, :f, :((top(svec))((top(apply_type))(Tuple),(top(svec))())), AST(:($(Expr(:lambda, Any[], Any[Any[],Any[],0,Any[]], :(begin  # none, line 1:
           return 1
       end))))), false)))

.. rubric:: Intermediate and compiled representations

Inspecting the lowered form for functions requires selection of the specific method to display,
because generic functions may have many methods with different type signatures. For this purpose,
method-specific code-lowering is available using :func:`code_lowered(f::Function, (Argtypes...)) <code_lowered>`,
and the type-inferred form is available using :func:`code_typed(f::Function, (Argtypes...)) <code_typed>`.
:func:`code_warntype(f::Function, (Argtypes...)) <code_warntype>` adds
highlighting to the output of :func:`code_typed` (see :ref:`man-code-warntype`).

Closer to the machine, the LLVM intermediate representation of a function may be printed using by
:func:`code_llvm(f::Function, (Argtypes...)) <code_llvm>`, and finally the compiled machine code is
available using :func:`code_native(f::Function, (Argtypes...) <code_native>` (this will trigger JIT
compilation/code generation for any function which has not previously been called).

For convenience, there are macro versions of the above functions which take standard function calls
and expand argument types automatically::

   julia> @code_llvm +(1,1)

   ; Function Attrs: sspreq
   define i64 @"julia_+_130862"(i64, i64) #0 {
   top:
       %2 = add i64 %1, %0, !dbg !8
       ret i64 %2, !dbg !8
   }

.. not testable due to name variations

(likewise ``@code_typed``, ``@code_warntype``, ``@code_lowered``, and ``@code_native``)

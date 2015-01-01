**********
Reflection
**********

In addition to the syntax-level introspection utilized in metaprogramming,
Julia provides several other runtime reflection capabilities.

**Type fields** The names of data type fields (or module members) may be interrogated
using :func:`names`. For example, given the following type::

	type Point
	  x::FloatingPoint
	  y
	end

``names(Point)`` will return the array ``Any[:x, :y]``. The type of
each field in a ``Point`` is stored in the ``types`` field of the Point object::

	julia> typeof(Point)
	DataType
	julia> Point.types
	(FloatingPoint,Any)

**Subtypes** The *direct* subtypes of any :obj:`DataType` may be listed using
:func:`subtypes`. For example, the abstract :obj:`DataType` :obj:`FloatingPoint`
has four (concrete) subtypes::
	
	julia> subtypes(FloatingPoint)
	4-element Array{Any,1}:
	 BigFloat
	 Float16
	 Float32
	 Float64

Any abstract subtype will also be included in this list, but further subtypes
thereof will not; recursive applications of :func:`subtypes` allow to build the
full type tree.

**Type internals** The internal representation of types is critically important
when interfacing with C code. :func:`isbits(T::DataType) <isbits>` returns true if `T` is
stored with C-compatible alignment. The offsets of each field may be listed
using :func:`fieldoffsets(T::DataType) <fieldoffsets>`.

**Function methods** The methods of any function may be listed using
:func:`methods`. 

**Function representations** Functions may be introspected at several levels
of representation. The lowered form of a function is available
using :func:`code_lowered(f::Function, (Args...)) <code_lowered>`, and the type-inferred lowered form
is available using :func:`code_typed(f::Function, (Args...)) <code_typed>`.

Closer to the machine, the LLVM intermediate representation of a function is
printed by :func:`code_llvm(f::Function, (Args...)) <code_llvm>`, and finally the resulting
assembly instructions (after JIT'ing step) are available using
:func:`code_native(f::Function, (Args...) <code_native>`.


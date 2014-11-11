.. _man-methods:

*********
 Methods  
*********

Recall from :ref:`man-functions` that a function is an object
that maps a tuple of arguments to a return value, or throws an exception
if no appropriate value can be returned. It is common for the same
conceptual function or operation to be implemented quite differently for
different types of arguments: adding two integers is very different from
adding two floating-point numbers, both of which are distinct from
adding an integer to a floating-point number. Despite their
implementation differences, these operations all fall under the general
concept of "addition". Accordingly, in Julia, these behaviors all belong
to a single object: the ``+`` function.

To facilitate using many different implementations of the same concept
smoothly, functions need not be defined all at once, but can rather be
defined piecewise by providing specific behaviors for certain
combinations of argument types and counts. A definition of one possible
behavior for a function is called a *method*. Thus far, we have
presented only examples of functions defined with a single method,
applicable to all types of arguments. However, the signatures of method
definitions can be annotated to indicate the types of arguments in
addition to their number, and more than a single method definition may
be provided. When a function is applied to a particular tuple of
arguments, the most specific method applicable to those arguments is
applied. Thus, the overall behavior of a function is a patchwork of the
behaviors of its various method definitions. If the patchwork is well
designed, even though the implementations of the methods may be quite
different, the outward behavior of the function will appear seamless and
consistent.

The choice of which method to execute when a function is applied is
called *dispatch*. Julia allows the dispatch process to choose which of
a function's methods to call based on the number of arguments given, and
on the types of all of the function's arguments. This is different than
traditional object-oriented languages, where dispatch occurs based only
on the first argument, which often has a special argument syntax, and is
sometimes implied rather than explicitly written as an
argument. [#]_ Using all of a function's arguments to
choose which method should be invoked, rather than just the first, is
known as `multiple dispatch
<http://en.wikipedia.org/wiki/Multiple_dispatch>`_. Multiple
dispatch is particularly useful for mathematical code, where it makes
little sense to artificially deem the operations to "belong" to one
argument more than any of the others: does the addition operation in
``x + y`` belong to ``x`` any more than it does to ``y``? The
implementation of a mathematical operator generally depends on the types
of all of its arguments. Even beyond mathematical operations, however,
multiple dispatch ends up being a powerful and convenient paradigm
for structuring and organizing programs.

.. [#] In C++ or Java, for example, in a method call like
  ``obj.meth(arg1,arg2)``, the object obj "receives" the method call and is
  implicitly passed to the method via the ``this`` keyword, rather then as an
  explicit method argument. When the current ``this`` object is the receiver of a
  method call, it can be omitted altogether, writing just ``meth(arg1,arg2)``,
  with this implied as the receiving object.


Defining Methods
----------------

Until now, we have, in our examples, defined only functions with a
single method having unconstrained argument types. Such functions behave
just like they would in traditional dynamically typed languages.
Nevertheless, we have used multiple dispatch and methods almost
continually without being aware of it: all of Julia's standard functions
and operators, like the aforementioned ``+`` function, have many methods
defining their behavior over various possible combinations of argument
type and count.

When defining a function, one can optionally constrain the types of
parameters it is applicable to, using the ``::`` type-assertion
operator, introduced in the section on :ref:`man-composite-types`:

.. doctest::

    julia> f(x::Float64, y::Float64) = 2x + y;

This function definition applies only to calls where ``x`` and ``y`` are
both values of type ``Float64``:

.. doctest::

    julia> f(2.0, 3.0)
    7.0

Applying it to any other types of arguments will result in a "no method"
error:

.. doctest::

    julia> f(2.0, 3)
    ERROR: `f` has no method matching f(::Float64, ::Int64)

    julia> f(float32(2.0), 3.0)
    ERROR: `f` has no method matching f(::Float32, ::Float64)

    julia> f(2.0, "3.0")
    ERROR: `f` has no method matching f(::Float64, ::ASCIIString)

    julia> f("2.0", "3.0")
    ERROR: `f` has no method matching f(::ASCIIString, ::ASCIIString)

As you can see, the arguments must be precisely of type ``Float64``.
Other numeric types, such as integers or 32-bit floating-point values,
are not automatically converted to 64-bit floating-point, nor are
strings parsed as numbers. Because ``Float64`` is a concrete type and
concrete types cannot be subclassed in Julia, such a definition can only
be applied to arguments that are exactly of type ``Float64``. It may
often be useful, however, to write more general methods where the
declared parameter types are abstract:

.. doctest::

    julia> f(x::Number, y::Number) = 2x - y;

    julia> f(2.0, 3)
    1.0

This method definition applies to any pair of arguments that are
instances of ``Number``. They need not be of the same type, so long as
they are each numeric values. The problem of handling disparate numeric
types is delegated to the arithmetic operations in the expression
``2x - y``.

To define a function with multiple methods, one simply defines the
function multiple times, with different numbers and types of arguments.
The first method definition for a function creates the function object,
and subsequent method definitions add new methods to the existing
function object. The most specific method definition matching the number
and types of the arguments will be executed when the function is
applied. Thus, the two method definitions above, taken together, define
the behavior for ``f`` over all pairs of instances of the abstract type
``Number`` — but with a different behavior specific to pairs of
``Float64`` values. If one of the arguments is a 64-bit float but the
other one is not, then the ``f(Float64,Float64)`` method cannot be
called and the more general ``f(Number,Number)`` method must be used:

.. doctest::

    julia> f(2.0, 3.0)
    7.0

    julia> f(2, 3.0)
    1.0

    julia> f(2.0, 3)
    1.0

    julia> f(2, 3)
    1

The ``2x + y`` definition is only used in the first case, while the
``2x - y`` definition is used in the others. No automatic casting or
conversion of function arguments is ever performed: all conversion in
Julia is non-magical and completely explicit. :ref:`man-conversion-and-promotion`, however, shows how clever
application of sufficiently advanced technology can be indistinguishable
from magic. [Clarke61]_

For non-numeric values, and for fewer or more than two arguments, the
function ``f`` remains undefined, and applying it will still result in a
"no method" error:

.. doctest::

    julia> f("foo", 3)
    ERROR: `f` has no method matching f(::ASCIIString, ::Int64)

    julia> f()
    ERROR: `f` has no method matching f()

You can easily see which methods exist for a function by entering the
function object itself in an interactive session:

.. doctest::

    julia> f
    f (generic function with 2 methods)

This output tells us that ``f`` is a function object with two
methods. To find out what the signatures of those methods are, use the
``methods`` function:

.. doctest::

    julia> methods(f)
    # 2 methods for generic function "f":
    f(x::Float64,y::Float64) at none:1
    f(x::Number,y::Number) at none:1

which shows that f has two methods, one taking two ``Float64``
arguments and one taking arguments of type ``Number``. It also
indicates the file and line number where the methods were defined:
because these methods were defined at the REPL, we get the apparent
line number ``none:1``.

In the absence of a type declaration with ``::``, the type of a method
parameter is ``Any`` by default, meaning that it is unconstrained since
all values in Julia are instances of the abstract type ``Any``. Thus, we
can define a catch-all method for ``f`` like so:

.. doctest::

    julia> f(x,y) = println("Whoa there, Nelly.");

    julia> f("foo", 1)
    Whoa there, Nelly.

This catch-all is less specific than any other possible method
definition for a pair of parameter values, so it is only be called on
pairs of arguments to which no other method definition applies.

Although it seems a simple concept, multiple dispatch on the types of
values is perhaps the single most powerful and central feature of the
Julia language. Core operations typically have dozens of methods::

    julia> methods(+)
    # 125 methods for generic function "+":
    +(x::Bool) at bool.jl:36
    +(x::Bool,y::Bool) at bool.jl:39
    +(y::FloatingPoint,x::Bool) at bool.jl:49
    +(A::BitArray{N},B::BitArray{N}) at bitarray.jl:848
    +(A::Union(DenseArray{Bool,N},SubArray{Bool,N,A<:DenseArray{T,N},I<:(Union(Range{Int64},Int64)...,)}),B::Union(DenseArray{Bool,N},SubArray{Bool,N,A<:DenseArray{T,N},I<:(Union(Range{Int64},Int64)...,)})) at array.jl:797
    +{S,T}(A::Union(SubArray{S,N,A<:DenseArray{T,N},I<:(Union(Range{Int64},Int64)...,)},DenseArray{S,N}),B::Union(SubArray{T,N,A<:DenseArray{T,N},I<:(Union(Range{Int64},Int64)...,)},DenseArray{T,N})) at array.jl:719
    +{T<:Union(Int16,Int32,Int8)}(x::T<:Union(Int16,Int32,Int8),y::T<:Union(Int16,Int32,Int8)) at int.jl:16
    +{T<:Union(UInt32,UInt16,UInt8)}(x::T<:Union(UInt32,UInt16,UInt8),y::T<:Union(UInt32,UInt16,UInt8)) at int.jl:20
    +(x::Int64,y::Int64) at int.jl:33
    +(x::UInt64,y::UInt64) at int.jl:34
    +(x::Int128,y::Int128) at int.jl:35
    +(x::UInt128,y::UInt128) at int.jl:36
    +(x::Float32,y::Float32) at float.jl:119
    +(x::Float64,y::Float64) at float.jl:120
    +(z::Complex{T<:Real},w::Complex{T<:Real}) at complex.jl:110
    +(x::Real,z::Complex{T<:Real}) at complex.jl:120
    +(z::Complex{T<:Real},x::Real) at complex.jl:121
    +(x::Rational{T<:Integer},y::Rational{T<:Integer}) at rational.jl:113
    +(x::Char,y::Char) at char.jl:23
    +(x::Char,y::Integer) at char.jl:26
    +(x::Integer,y::Char) at char.jl:27
    +(a::Float16,b::Float16) at float16.jl:132
    +(x::BigInt,y::BigInt) at gmp.jl:194
    +(a::BigInt,b::BigInt,c::BigInt) at gmp.jl:217
    +(a::BigInt,b::BigInt,c::BigInt,d::BigInt) at gmp.jl:223
    +(a::BigInt,b::BigInt,c::BigInt,d::BigInt,e::BigInt) at gmp.jl:230
    +(x::BigInt,c::UInt64) at gmp.jl:242
    +(c::UInt64,x::BigInt) at gmp.jl:246
    +(c::Union(UInt32,UInt16,UInt8,UInt64),x::BigInt) at gmp.jl:247
    +(x::BigInt,c::Union(UInt32,UInt16,UInt8,UInt64)) at gmp.jl:248
    +(x::BigInt,c::Union(Int64,Int16,Int32,Int8)) at gmp.jl:249
    +(c::Union(Int64,Int16,Int32,Int8),x::BigInt) at gmp.jl:250
    +(x::BigFloat,c::UInt64) at mpfr.jl:147
    +(c::UInt64,x::BigFloat) at mpfr.jl:151
    +(c::Union(UInt32,UInt16,UInt8,UInt64),x::BigFloat) at mpfr.jl:152
    +(x::BigFloat,c::Union(UInt32,UInt16,UInt8,UInt64)) at mpfr.jl:153
    +(x::BigFloat,c::Int64) at mpfr.jl:157
    +(c::Int64,x::BigFloat) at mpfr.jl:161
    +(x::BigFloat,c::Union(Int64,Int16,Int32,Int8)) at mpfr.jl:162
    +(c::Union(Int64,Int16,Int32,Int8),x::BigFloat) at mpfr.jl:163
    +(x::BigFloat,c::Float64) at mpfr.jl:167
    +(c::Float64,x::BigFloat) at mpfr.jl:171
    +(c::Float32,x::BigFloat) at mpfr.jl:172
    +(x::BigFloat,c::Float32) at mpfr.jl:173
    +(x::BigFloat,c::BigInt) at mpfr.jl:177
    +(c::BigInt,x::BigFloat) at mpfr.jl:181
    +(x::BigFloat,y::BigFloat) at mpfr.jl:328
    +(a::BigFloat,b::BigFloat,c::BigFloat) at mpfr.jl:339
    +(a::BigFloat,b::BigFloat,c::BigFloat,d::BigFloat) at mpfr.jl:345
    +(a::BigFloat,b::BigFloat,c::BigFloat,d::BigFloat,e::BigFloat) at mpfr.jl:352
    +(x::MathConst{sym},y::MathConst{sym}) at constants.jl:23
    +{T<:Number}(x::T<:Number,y::T<:Number) at promotion.jl:188
    +{T<:FloatingPoint}(x::Bool,y::T<:FloatingPoint) at bool.jl:46
    +(x::Number,y::Number) at promotion.jl:158
    +(x::Integer,y::Ptr{T}) at pointer.jl:68
    +(x::Bool,A::AbstractArray{Bool,N}) at array.jl:767
    +(x::Number) at operators.jl:71
    +(r1::OrdinalRange{T,S},r2::OrdinalRange{T,S}) at operators.jl:325
    +{T<:FloatingPoint}(r1::FloatRange{T<:FloatingPoint},r2::FloatRange{T<:FloatingPoint}) at operators.jl:331
    +(r1::FloatRange{T<:FloatingPoint},r2::FloatRange{T<:FloatingPoint}) at operators.jl:348
    +(r1::FloatRange{T<:FloatingPoint},r2::OrdinalRange{T,S}) at operators.jl:349
    +(r1::OrdinalRange{T,S},r2::FloatRange{T<:FloatingPoint}) at operators.jl:350
    +(x::Ptr{T},y::Integer) at pointer.jl:66
    +{S,T<:Real}(A::Union(SubArray{S,N,A<:DenseArray{T,N},I<:(Union(Range{Int64},Int64)...,)},DenseArray{S,N}),B::Range{T<:Real}) at array.jl:727
    +{S<:Real,T}(A::Range{S<:Real},B::Union(SubArray{T,N,A<:DenseArray{T,N},I<:(Union(Range{Int64},Int64)...,)},DenseArray{T,N})) at array.jl:736
    +(A::AbstractArray{Bool,N},x::Bool) at array.jl:766
    +{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti},B::SparseMatrixCSC{Tv,Ti}) at sparse/sparsematrix.jl:530
    +{TvA,TiA,TvB,TiB}(A::SparseMatrixCSC{TvA,TiA},B::SparseMatrixCSC{TvB,TiB}) at sparse/sparsematrix.jl:522
    +(A::SparseMatrixCSC{Tv,Ti<:Integer},B::Array{T,N}) at sparse/sparsematrix.jl:621
    +(A::Array{T,N},B::SparseMatrixCSC{Tv,Ti<:Integer}) at sparse/sparsematrix.jl:623
    +(A::SymTridiagonal{T},B::SymTridiagonal{T}) at linalg/tridiag.jl:45
    +(A::Tridiagonal{T},B::Tridiagonal{T}) at linalg/tridiag.jl:207
    +(A::Tridiagonal{T},B::SymTridiagonal{T}) at linalg/special.jl:99
    +(A::SymTridiagonal{T},B::Tridiagonal{T}) at linalg/special.jl:98
    +{T,MT,uplo}(A::Triangular{T,MT,uplo,IsUnit},B::Triangular{T,MT,uplo,IsUnit}) at linalg/triangular.jl:10
    +{T,MT,uplo1,uplo2}(A::Triangular{T,MT,uplo1,IsUnit},B::Triangular{T,MT,uplo2,IsUnit}) at linalg/triangular.jl:11
    +(Da::Diagonal{T},Db::Diagonal{T}) at linalg/diagonal.jl:44
    +(A::Bidiagonal{T},B::Bidiagonal{T}) at linalg/bidiag.jl:92
    +{T}(B::BitArray{2},J::UniformScaling{T}) at linalg/uniformscaling.jl:26
    +(A::Diagonal{T},B::Bidiagonal{T}) at linalg/special.jl:89
    +(A::Bidiagonal{T},B::Diagonal{T}) at linalg/special.jl:90
    +(A::Diagonal{T},B::Tridiagonal{T}) at linalg/special.jl:89
    +(A::Tridiagonal{T},B::Diagonal{T}) at linalg/special.jl:90
    +(A::Diagonal{T},B::Triangular{T,S<:AbstractArray{T,2},UpLo,IsUnit}) at linalg/special.jl:89
    +(A::Triangular{T,S<:AbstractArray{T,2},UpLo,IsUnit},B::Diagonal{T}) at linalg/special.jl:90
    +(A::Diagonal{T},B::Array{T,2}) at linalg/special.jl:89
    +(A::Array{T,2},B::Diagonal{T}) at linalg/special.jl:90
    +(A::Bidiagonal{T},B::Tridiagonal{T}) at linalg/special.jl:89
    +(A::Tridiagonal{T},B::Bidiagonal{T}) at linalg/special.jl:90
    +(A::Bidiagonal{T},B::Triangular{T,S<:AbstractArray{T,2},UpLo,IsUnit}) at linalg/special.jl:89
    +(A::Triangular{T,S<:AbstractArray{T,2},UpLo,IsUnit},B::Bidiagonal{T}) at linalg/special.jl:90
    +(A::Bidiagonal{T},B::Array{T,2}) at linalg/special.jl:89
    +(A::Array{T,2},B::Bidiagonal{T}) at linalg/special.jl:90
    +(A::Tridiagonal{T},B::Triangular{T,S<:AbstractArray{T,2},UpLo,IsUnit}) at linalg/special.jl:89
    +(A::Triangular{T,S<:AbstractArray{T,2},UpLo,IsUnit},B::Tridiagonal{T}) at linalg/special.jl:90
    +(A::Tridiagonal{T},B::Array{T,2}) at linalg/special.jl:89
    +(A::Array{T,2},B::Tridiagonal{T}) at linalg/special.jl:90
    +(A::Triangular{T,S<:AbstractArray{T,2},UpLo,IsUnit},B::Array{T,2}) at linalg/special.jl:89
    +(A::Array{T,2},B::Triangular{T,S<:AbstractArray{T,2},UpLo,IsUnit}) at linalg/special.jl:90
    +(A::SymTridiagonal{T},B::Triangular{T,S<:AbstractArray{T,2},UpLo,IsUnit}) at linalg/special.jl:98
    +(A::Triangular{T,S<:AbstractArray{T,2},UpLo,IsUnit},B::SymTridiagonal{T}) at linalg/special.jl:99
    +(A::SymTridiagonal{T},B::Array{T,2}) at linalg/special.jl:98
    +(A::Array{T,2},B::SymTridiagonal{T}) at linalg/special.jl:99
    +(A::Diagonal{T},B::SymTridiagonal{T}) at linalg/special.jl:107
    +(A::SymTridiagonal{T},B::Diagonal{T}) at linalg/special.jl:108
    +(A::Bidiagonal{T},B::SymTridiagonal{T}) at linalg/special.jl:107
    +(A::SymTridiagonal{T},B::Bidiagonal{T}) at linalg/special.jl:108
    +{T<:Number}(x::AbstractArray{T<:Number,N}) at abstractarray.jl:358
    +(A::AbstractArray{T,N},x::Number) at array.jl:770
    +(x::Number,A::AbstractArray{T,N}) at array.jl:771
    +(J1::UniformScaling{T<:Number},J2::UniformScaling{T<:Number}) at linalg/uniformscaling.jl:25
    +(J::UniformScaling{T<:Number},B::BitArray{2}) at linalg/uniformscaling.jl:27
    +(J::UniformScaling{T<:Number},A::AbstractArray{T,2}) at linalg/uniformscaling.jl:28
    +(J::UniformScaling{T<:Number},x::Number) at linalg/uniformscaling.jl:29
    +(x::Number,J::UniformScaling{T<:Number}) at linalg/uniformscaling.jl:30
    +{TA,TJ}(A::AbstractArray{TA,2},J::UniformScaling{TJ}) at linalg/uniformscaling.jl:33
    +{T}(a::HierarchicalValue{T},b::HierarchicalValue{T}) at pkg/resolve/versionweight.jl:19
    +(a::VWPreBuildItem,b::VWPreBuildItem) at pkg/resolve/versionweight.jl:82
    +(a::VWPreBuild,b::VWPreBuild) at pkg/resolve/versionweight.jl:120
    +(a::VersionWeight,b::VersionWeight) at pkg/resolve/versionweight.jl:164
    +(a::FieldValue,b::FieldValue) at pkg/resolve/fieldvalue.jl:41
    +(a::Vec2,b::Vec2) at graphics.jl:60
    +(bb1::BoundingBox,bb2::BoundingBox) at graphics.jl:123
    +(a,b,c) at operators.jl:82
    +(a,b,c,xs...) at operators.jl:83

Multiple dispatch together with the flexible parametric type system give
Julia its ability to abstractly express high-level algorithms decoupled
from implementation details, yet generate efficient, specialized code to
handle each case at run time.

Method Ambiguities
------------------

It is possible to define a set of function methods such that there is no
unique most specific method applicable to some combinations of
arguments:

.. doctest::

    julia> g(x::Float64, y) = 2x + y;

    julia> g(x, y::Float64) = x + 2y;
    Warning: New definition 
        g(Any,Float64) at none:1
    is ambiguous with: 
        g(Float64,Any) at none:1.
    To fix, define 
        g(Float64,Float64)
    before the new definition.

    julia> g(2.0, 3)
    7.0

    julia> g(2, 3.0)
    8.0

    julia> g(2.0, 3.0)
    7.0

Here the call ``g(2.0, 3.0)`` could be handled by either the
``g(Float64, Any)`` or the ``g(Any, Float64)`` method, and neither is
more specific than the other. In such cases, Julia warns you about this
ambiguity, but allows you to proceed, arbitrarily picking a method. You
should avoid method ambiguities by specifying an appropriate method for
the intersection case:

.. doctest::

    julia> g(x::Float64, y::Float64) = 2x + 2y;

    julia> g(x::Float64, y) = 2x + y;

    julia> g(x, y::Float64) = x + 2y;

    julia> g(2.0, 3)
    7.0

    julia> g(2, 3.0)
    8.0

    julia> g(2.0, 3.0)
    10.0

To suppress Julia's warning, the disambiguating method must be defined
first, since otherwise the ambiguity exists, if transiently, until the
more specific method is defined.

.. _man-parametric-methods:

Parametric Methods
------------------

Method definitions can optionally have type parameters immediately after
the method name and before the parameter tuple:

.. doctest::

    julia> same_type{T}(x::T, y::T) = true;

    julia> same_type(x,y) = false;

The first method applies whenever both arguments are of the same
concrete type, regardless of what type that is, while the second method
acts as a catch-all, covering all other cases. Thus, overall, this
defines a boolean function that checks whether its two arguments are of
the same type:

.. doctest::

    julia> same_type(1, 2)
    true

    julia> same_type(1, 2.0)
    false

    julia> same_type(1.0, 2.0)
    true

    julia> same_type("foo", 2.0)
    false

    julia> same_type("foo", "bar")
    true

    julia> same_type(int32(1), int64(2))
    false

This kind of definition of function behavior by dispatch is quite common
— idiomatic, even — in Julia. Method type parameters are not restricted
to being used as the types of parameters: they can be used anywhere a
value would be in the signature of the function or body of the function.
Here's an example where the method type parameter ``T`` is used as the
type parameter to the parametric type ``Vector{T}`` in the method
signature:

.. doctest::

    julia> myappend{T}(v::Vector{T}, x::T) = [v..., x]
    myappend (generic function with 1 method)

    julia> myappend([1,2,3],4)
    4-element Array{Int64,1}:
     1
     2
     3
     4

    julia> myappend([1,2,3],2.5)
    ERROR: `myappend` has no method matching myappend(::Array{Int64,1}, ::Float64)

    julia> myappend([1.0,2.0,3.0],4.0)
    4-element Array{Float64,1}:
     1.0
     2.0
     3.0
     4.0

    julia> myappend([1.0,2.0,3.0],4)
    ERROR: `myappend` has no method matching myappend(::Array{Float64,1}, ::Int64)

As you can see, the type of the appended element must match the element
type of the vector it is appended to, or a "no method" error is raised.
In the following example, the method type parameter ``T`` is used as the
return value:

.. doctest::

    julia> mytypeof{T}(x::T) = T
    mytypeof (generic function with 1 method)

    julia> mytypeof(1)
    Int64

    julia> mytypeof(1.0)
    Float64

Just as you can put subtype constraints on type parameters in type
declarations (see :ref:`man-parametric-types`), you
can also constrain type parameters of methods::

    same_type_numeric{T<:Number}(x::T, y::T) = true
    same_type_numeric(x::Number, y::Number) = false

    julia> same_type_numeric(1, 2)
    true

    julia> same_type_numeric(1, 2.0)
    false

    julia> same_type_numeric(1.0, 2.0)
    true

    julia> same_type_numeric("foo", 2.0)
    no method same_type_numeric(ASCIIString,Float64)

    julia> same_type_numeric("foo", "bar")
    no method same_type_numeric(ASCIIString,ASCIIString)

    julia> same_type_numeric(int32(1), int64(2))
    false

The ``same_type_numeric`` function behaves much like the ``same_type``
function defined above, but is only defined for pairs of numbers.

Note on Optional and keyword Arguments
--------------------------------------

As mentioned briefly in :ref:`man-functions`, optional arguments are
implemented as syntax for multiple method definitions. For example,
this definition::

    f(a=1,b=2) = a+2b

translates to the following three methods::

    f(a,b) = a+2b
    f(a) = f(a,2)
    f() = f(1,2)

Keyword arguments behave quite differently from ordinary positional arguments.
In particular, they do not participate in method dispatch. Methods are
dispatched based only on positional arguments, with keyword arguments processed
after the matching method is identified.

.. [Clarke61] Arthur C. Clarke, *Profiles of the Future* (1961): Clarke's Third Law.


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
<https://en.wikipedia.org/wiki/Multiple_dispatch>`_. Multiple
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
  implicitly passed to the method via the ``this`` keyword, rather than as an
  explicit method argument. When the current ``this`` object is the receiver of a
  method call, it can be omitted altogether, writing just ``meth(arg1,arg2)``,
  with ``this`` implied as the receiving object.


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
both values of type :obj:`Float64`:

.. doctest::

    julia> f(2.0, 3.0)
    7.0

Applying it to any other types of arguments will result in a :exc:`MethodError`:

.. doctest::

    julia> f(2.0, 3)
    ERROR: MethodError: `f` has no method matching f(::Float64, ::Int64)
    Closest candidates are:
      f(::Float64, !Matched::Float64)

    julia> f(Float32(2.0), 3.0)
    ERROR: MethodError: `f` has no method matching f(::Float32, ::Float64)
    Closest candidates are:
      f(!Matched::Float64, ::Float64)

    julia> f(2.0, "3.0")
    ERROR: MethodError: `f` has no method matching f(::Float64, ::String)
    Closest candidates are:
      f(::Float64, !Matched::Float64)

    julia> f("2.0", "3.0")
    ERROR: MethodError: `f` has no method matching f(::String, ::String)

As you can see, the arguments must be precisely of type :obj:`Float64`.
Other numeric types, such as integers or 32-bit floating-point values,
are not automatically converted to 64-bit floating-point, nor are
strings parsed as numbers. Because :obj:`Float64` is a concrete type and
concrete types cannot be subclassed in Julia, such a definition can only
be applied to arguments that are exactly of type :obj:`Float64`. It may
often be useful, however, to write more general methods where the
declared parameter types are abstract:

.. doctest::

    julia> f(x::Number, y::Number) = 2x - y;

    julia> f(2.0, 3)
    1.0

This method definition applies to any pair of arguments that are
instances of :obj:`Number`. They need not be of the same type, so long as
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
:obj:`Number` — but with a different behavior specific to pairs of
:obj:`Float64` values. If one of the arguments is a 64-bit float but the
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
:obj:`MethodError`:

.. doctest::

    julia> f("foo", 3)
    ERROR: MethodError: `f` has no method matching f(::String, ::Int64)
    Closest candidates are:
      f(!Matched::Number, ::Number)

    julia> f()
    ERROR: MethodError: `f` has no method matching f()

You can easily see which methods exist for a function by entering the
function object itself in an interactive session:

.. doctest::

    julia> f
    f (generic function with 2 methods)

This output tells us that ``f`` is a function object with two
methods. To find out what the signatures of those methods are, use the
:func:`methods` function:

.. doctest::

    julia> methods(f)
    # 2 methods for generic function "f":
    f(x::Float64, y::Float64) at none:1
    f(x::Number, y::Number) at none:1

which shows that ``f`` has two methods, one taking two :obj:`Float64`
arguments and one taking arguments of type :obj:`Number`. It also
indicates the file and line number where the methods were defined:
because these methods were defined at the REPL, we get the apparent
line number ``none:1``.

In the absence of a type declaration with ``::``, the type of a method
parameter is :obj:`Any` by default, meaning that it is unconstrained since
all values in Julia are instances of the abstract type :obj:`Any`. Thus, we
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
    # 139 methods for generic function "+":
    +(x::Bool) at bool.jl:33
    +(x::Bool,y::Bool) at bool.jl:36
    +(y::AbstractFloat,x::Bool) at bool.jl:46
    +(x::Int64,y::Int64) at int.jl:14
    +(x::Int8,y::Int8) at int.jl:14
    +(x::UInt8,y::UInt8) at int.jl:14
    +(x::Int16,y::Int16) at int.jl:14
    +(x::UInt16,y::UInt16) at int.jl:14
    +(x::Int32,y::Int32) at int.jl:14
    +(x::UInt32,y::UInt32) at int.jl:14
    +(x::UInt64,y::UInt64) at int.jl:14
    +(x::Int128,y::Int128) at int.jl:14
    +(x::UInt128,y::UInt128) at int.jl:14
    +(x::Float32,y::Float32) at float.jl:192
    +(x::Float64,y::Float64) at float.jl:193
    +(z::Complex{T<:Real},w::Complex{T<:Real}) at complex.jl:96
    +(x::Real,z::Complex{T<:Real}) at complex.jl:106
    +(z::Complex{T<:Real},x::Real) at complex.jl:107
    +(x::Rational{T<:Integer},y::Rational{T<:Integer}) at rational.jl:167
    +(a::Float16,b::Float16) at float16.jl:136
    +(x::Base.GMP.BigInt,y::Base.GMP.BigInt) at gmp.jl:243
    +(a::Base.GMP.BigInt,b::Base.GMP.BigInt,c::Base.GMP.BigInt) at gmp.jl:266
    +(a::Base.GMP.BigInt,b::Base.GMP.BigInt,c::Base.GMP.BigInt,d::Base.GMP.BigInt) at gmp.jl:272
    +(a::Base.GMP.BigInt,b::Base.GMP.BigInt,c::Base.GMP.BigInt,d::Base.GMP.BigInt,e::Base.GMP.BigInt) at gmp.jl:279
    +(x::Base.GMP.BigInt,c::Union{UInt32,UInt16,UInt8,UInt64}) at gmp.jl:291
    +(c::Union{UInt32,UInt16,UInt8,UInt64},x::Base.GMP.BigInt) at gmp.jl:295
    +(x::Base.GMP.BigInt,c::Union{Int16,Int32,Int8,Int64}) at gmp.jl:307
    +(c::Union{Int16,Int32,Int8,Int64},x::Base.GMP.BigInt) at gmp.jl:308
    +(x::Base.MPFR.BigFloat,y::Base.MPFR.BigFloat) at mpfr.jl:206
    +(x::Base.MPFR.BigFloat,c::Union{UInt32,UInt16,UInt8,UInt64}) at mpfr.jl:213
    +(c::Union{UInt32,UInt16,UInt8,UInt64},x::Base.MPFR.BigFloat) at mpfr.jl:217
    +(x::Base.MPFR.BigFloat,c::Union{Int16,Int32,Int8,Int64}) at mpfr.jl:221
    +(c::Union{Int16,Int32,Int8,Int64},x::Base.MPFR.BigFloat) at mpfr.jl:225
    +(x::Base.MPFR.BigFloat,c::Union{Float16,Float64,Float32}) at mpfr.jl:229
    +(c::Union{Float16,Float64,Float32},x::Base.MPFR.BigFloat) at mpfr.jl:233
    +(x::Base.MPFR.BigFloat,c::Base.GMP.BigInt) at mpfr.jl:237
    +(c::Base.GMP.BigInt,x::Base.MPFR.BigFloat) at mpfr.jl:241
    +(a::Base.MPFR.BigFloat,b::Base.MPFR.BigFloat,c::Base.MPFR.BigFloat) at mpfr.jl:318
    +(a::Base.MPFR.BigFloat,b::Base.MPFR.BigFloat,c::Base.MPFR.BigFloat,d::Base.MPFR.BigFloat) at mpfr.jl:324
    +(a::Base.MPFR.BigFloat,b::Base.MPFR.BigFloat,c::Base.MPFR.BigFloat,d::Base.MPFR.BigFloat,e::Base.MPFR.BigFloat) at mpfr.jl:331
    +(x::Irrational{sym},y::Irrational{sym}) at constants.jl:71
    +{T<:Number}(x::T<:Number,y::T<:Number) at promotion.jl:205
    +{T<:AbstractFloat}(x::Bool,y::T<:AbstractFloat) at bool.jl:43
    +(x::Number,y::Number) at promotion.jl:167
    +(x::Integer,y::Ptr{T}) at pointer.jl:70
    +(x::Bool,A::AbstractArray{Bool,N}) at array.jl:829
    +(x::Integer,y::Char) at char.jl:41
    +(x::Number) at operators.jl:72
    +(r1::OrdinalRange{T,S},r2::OrdinalRange{T,S}) at operators.jl:325
    +{T<:AbstractFloat}(r1::FloatRange{T<:AbstractFloat},r2::FloatRange{T<:AbstractFloat}) at operators.jl:331
    +(r1::FloatRange{T<:AbstractFloat},r2::FloatRange{T<:AbstractFloat}) at operators.jl:348
    +(r1::FloatRange{T<:AbstractFloat},r2::OrdinalRange{T,S}) at operators.jl:349
    +(r1::OrdinalRange{T,S},r2::FloatRange{T<:AbstractFloat}) at operators.jl:350
    +(x::Ptr{T},y::Integer) at pointer.jl:68
    +{S,T}(A::Range{S},B::Range{T}) at array.jl:773
    +{S,T}(A::Range{S},B::AbstractArray{T,N}) at array.jl:791
    +(A::AbstractArray{Bool,N},x::Bool) at array.jl:828
    +(A::BitArray{N},B::BitArray{N}) at bitarray.jl:926
    +(A::Union{DenseArray{Bool,N},SubArray{Bool,N,A<:DenseArray{T,N},I<:Tuple{Vararg{Union{Colon,Range{Int64},Int64}}},LD}},B::Union{DenseArray{Bool,N},SubArray{Bool,N,A<:DenseArray{T,N},I<:Tuple{Vararg{Union{Colon,Range{Int64},Int64}}},LD}}) at array.jl:859
    +(A::Base.LinAlg.SymTridiagonal{T},B::Base.LinAlg.SymTridiagonal{T}) at linalg/tridiag.jl:59
    +(A::Base.LinAlg.Tridiagonal{T},B::Base.LinAlg.Tridiagonal{T}) at linalg/tridiag.jl:254
    +(A::Base.LinAlg.Tridiagonal{T},B::Base.LinAlg.SymTridiagonal{T}) at linalg/special.jl:113
    +(A::Base.LinAlg.SymTridiagonal{T},B::Base.LinAlg.Tridiagonal{T}) at linalg/special.jl:112
    +(A::Base.LinAlg.UpperTriangular{T,S<:AbstractArray{T,2}},B::Base.LinAlg.UpperTriangular{T,S<:AbstractArray{T,2}}) at linalg/triangular.jl:164
    +(A::Base.LinAlg.LowerTriangular{T,S<:AbstractArray{T,2}},B::Base.LinAlg.LowerTriangular{T,S<:AbstractArray{T,2}}) at linalg/triangular.jl:165
    +(A::Base.LinAlg.UpperTriangular{T,S<:AbstractArray{T,2}},B::Base.LinAlg.UnitUpperTriangular{T,S<:AbstractArray{T,2}}) at linalg/triangular.jl:166
    +(A::Base.LinAlg.LowerTriangular{T,S<:AbstractArray{T,2}},B::Base.LinAlg.UnitLowerTriangular{T,S<:AbstractArray{T,2}}) at linalg/triangular.jl:167
    +(A::Base.LinAlg.UnitUpperTriangular{T,S<:AbstractArray{T,2}},B::Base.LinAlg.UpperTriangular{T,S<:AbstractArray{T,2}}) at linalg/triangular.jl:168
    +(A::Base.LinAlg.UnitLowerTriangular{T,S<:AbstractArray{T,2}},B::Base.LinAlg.LowerTriangular{T,S<:AbstractArray{T,2}}) at linalg/triangular.jl:169
    +(A::Base.LinAlg.UnitUpperTriangular{T,S<:AbstractArray{T,2}},B::Base.LinAlg.UnitUpperTriangular{T,S<:AbstractArray{T,2}}) at linalg/triangular.jl:170
    +(A::Base.LinAlg.UnitLowerTriangular{T,S<:AbstractArray{T,2}},B::Base.LinAlg.UnitLowerTriangular{T,S<:AbstractArray{T,2}}) at linalg/triangular.jl:171
    +(A::Base.LinAlg.AbstractTriangular{T,S<:AbstractArray{T,2}},B::Base.LinAlg.AbstractTriangular{T,S<:AbstractArray{T,2}}) at linalg/triangular.jl:172
    +(Da::Base.LinAlg.Diagonal{T},Db::Base.LinAlg.Diagonal{T}) at linalg/diagonal.jl:50
    +(A::Base.LinAlg.Bidiagonal{T},B::Base.LinAlg.Bidiagonal{T}) at linalg/bidiag.jl:111
    +{T}(B::BitArray{2},J::Base.LinAlg.UniformScaling{T}) at linalg/uniformscaling.jl:28
    +(A::Base.LinAlg.Diagonal{T},B::Base.LinAlg.Bidiagonal{T}) at linalg/special.jl:103
    +(A::Base.LinAlg.Bidiagonal{T},B::Base.LinAlg.Diagonal{T}) at linalg/special.jl:104
    +(A::Base.LinAlg.Diagonal{T},B::Base.LinAlg.Tridiagonal{T}) at linalg/special.jl:103
    +(A::Base.LinAlg.Tridiagonal{T},B::Base.LinAlg.Diagonal{T}) at linalg/special.jl:104
    +(A::Base.LinAlg.Diagonal{T},B::Array{T,2}) at linalg/special.jl:103
    +(A::Array{T,2},B::Base.LinAlg.Diagonal{T}) at linalg/special.jl:104
    +(A::Base.LinAlg.Bidiagonal{T},B::Base.LinAlg.Tridiagonal{T}) at linalg/special.jl:103
    +(A::Base.LinAlg.Tridiagonal{T},B::Base.LinAlg.Bidiagonal{T}) at linalg/special.jl:104
    +(A::Base.LinAlg.Bidiagonal{T},B::Array{T,2}) at linalg/special.jl:103
    +(A::Array{T,2},B::Base.LinAlg.Bidiagonal{T}) at linalg/special.jl:104
    +(A::Base.LinAlg.Tridiagonal{T},B::Array{T,2}) at linalg/special.jl:103
    +(A::Array{T,2},B::Base.LinAlg.Tridiagonal{T}) at linalg/special.jl:104
    +(A::Base.LinAlg.SymTridiagonal{T},B::Array{T,2}) at linalg/special.jl:112
    +(A::Array{T,2},B::Base.LinAlg.SymTridiagonal{T}) at linalg/special.jl:113
    +(A::Base.LinAlg.Diagonal{T},B::Base.LinAlg.SymTridiagonal{T}) at linalg/special.jl:121
    +(A::Base.LinAlg.SymTridiagonal{T},B::Base.LinAlg.Diagonal{T}) at linalg/special.jl:122
    +(A::Base.LinAlg.Bidiagonal{T},B::Base.LinAlg.SymTridiagonal{T}) at linalg/special.jl:121
    +(A::Base.LinAlg.SymTridiagonal{T},B::Base.LinAlg.Bidiagonal{T}) at linalg/special.jl:122
    +{Tv1,Ti1,Tv2,Ti2}(A_1::Base.SparseMatrix.SparseMatrixCSC{Tv1,Ti1},A_2::Base.SparseMatrix.SparseMatrixCSC{Tv2,Ti2}) at sparse/sparsematrix.jl:873
    +(A::Base.SparseMatrix.SparseMatrixCSC{Tv,Ti<:Integer},B::Array{T,N}) at sparse/sparsematrix.jl:885
    +(A::Array{T,N},B::Base.SparseMatrix.SparseMatrixCSC{Tv,Ti<:Integer}) at sparse/sparsematrix.jl:887
    +{P<:Base.Dates.Period}(Y::Union{SubArray{P<:Base.Dates.Period,N,A<:DenseArray{T,N},I<:Tuple{Vararg{Union{Colon,Range{Int64},Int64}}},LD},DenseArray{P<:Base.Dates.Period,N}},x::P<:Base.Dates.Period) at dates/periods.jl:50
    +{T<:Base.Dates.TimeType}(r::Range{T<:Base.Dates.TimeType},x::Base.Dates.Period) at dates/ranges.jl:39
    +{T<:Number}(x::AbstractArray{T<:Number,N}) at abstractarray.jl:442
    +{S,T}(A::AbstractArray{S,N},B::Range{T}) at array.jl:782
    +{S,T}(A::AbstractArray{S,N},B::AbstractArray{T,N}) at array.jl:800
    +(A::AbstractArray{T,N},x::Number) at array.jl:832
    +(x::Number,A::AbstractArray{T,N}) at array.jl:833
    +(x::Char,y::Integer) at char.jl:40
    +{N}(index1::Base.IteratorsMD.CartesianIndex{N},index2::Base.IteratorsMD.CartesianIndex{N}) at multidimensional.jl:121
    +(J1::Base.LinAlg.UniformScaling{T<:Number},J2::Base.LinAlg.UniformScaling{T<:Number}) at linalg/uniformscaling.jl:27
    +(J::Base.LinAlg.UniformScaling{T<:Number},B::BitArray{2}) at linalg/uniformscaling.jl:29
    +(J::Base.LinAlg.UniformScaling{T<:Number},A::AbstractArray{T,2}) at linalg/uniformscaling.jl:30
    +(J::Base.LinAlg.UniformScaling{T<:Number},x::Number) at linalg/uniformscaling.jl:31
    +(x::Number,J::Base.LinAlg.UniformScaling{T<:Number}) at linalg/uniformscaling.jl:32
    +{TA,TJ}(A::AbstractArray{TA,2},J::Base.LinAlg.UniformScaling{TJ}) at linalg/uniformscaling.jl:35
    +{T}(a::Base.Pkg.Resolve.VersionWeights.HierarchicalValue{T},b::Base.Pkg.Resolve.VersionWeights.HierarchicalValue{T}) at pkg/resolve/versionweight.jl:21
    +(a::Base.Pkg.Resolve.VersionWeights.VWPreBuildItem,b::Base.Pkg.Resolve.VersionWeights.VWPreBuildItem) at pkg/resolve/versionweight.jl:83
    +(a::Base.Pkg.Resolve.VersionWeights.VWPreBuild,b::Base.Pkg.Resolve.VersionWeights.VWPreBuild) at pkg/resolve/versionweight.jl:129
    +(a::Base.Pkg.Resolve.VersionWeights.VersionWeight,b::Base.Pkg.Resolve.VersionWeights.VersionWeight) at pkg/resolve/versionweight.jl:183
    +(a::Base.Pkg.Resolve.MaxSum.FieldValues.FieldValue,b::Base.Pkg.Resolve.MaxSum.FieldValues.FieldValue) at pkg/resolve/fieldvalue.jl:43
    +{P<:Base.Dates.Period}(x::P<:Base.Dates.Period,y::P<:Base.Dates.Period) at dates/periods.jl:43
    +{P<:Base.Dates.Period}(x::P<:Base.Dates.Period,Y::Union{SubArray{P<:Base.Dates.Period,N,A<:DenseArray{T,N},I<:Tuple{Vararg{Union{Colon,Range{Int64},Int64}}},LD},DenseArray{P<:Base.Dates.Period,N}}) at dates/periods.jl:49
    +(x::Base.Dates.Period,y::Base.Dates.Period) at dates/periods.jl:196
    +(x::Base.Dates.CompoundPeriod,y::Base.Dates.Period) at dates/periods.jl:197
    +(y::Base.Dates.Period,x::Base.Dates.CompoundPeriod) at dates/periods.jl:198
    +(x::Base.Dates.CompoundPeriod,y::Base.Dates.CompoundPeriod) at dates/periods.jl:199
    +(dt::Base.Dates.DateTime,y::Base.Dates.Year) at dates/arithmetic.jl:13
    +(dt::Base.Dates.Date,y::Base.Dates.Year) at dates/arithmetic.jl:17
    +(dt::Base.Dates.DateTime,z::Base.Dates.Month) at dates/arithmetic.jl:37
    +(dt::Base.Dates.Date,z::Base.Dates.Month) at dates/arithmetic.jl:43
    +(x::Base.Dates.Date,y::Base.Dates.Week) at dates/arithmetic.jl:60
    +(x::Base.Dates.Date,y::Base.Dates.Day) at dates/arithmetic.jl:62
    +(x::Base.Dates.DateTime,y::Base.Dates.Period) at dates/arithmetic.jl:64
    +(a::Base.Dates.TimeType,b::Base.Dates.Period,c::Base.Dates.Period) at dates/periods.jl:210
    +(a::Base.Dates.TimeType,b::Base.Dates.Period,c::Base.Dates.Period,d::Base.Dates.Period...) at dates/periods.jl:212
    +(x::Base.Dates.TimeType,y::Base.Dates.CompoundPeriod) at dates/periods.jl:216
    +(x::Base.Dates.CompoundPeriod,y::Base.Dates.TimeType) at dates/periods.jl:221
    +(x::Base.Dates.Instant) at dates/arithmetic.jl:4
    +(x::Base.Dates.TimeType) at dates/arithmetic.jl:8
    +(y::Base.Dates.Period,x::Base.Dates.TimeType) at dates/arithmetic.jl:66
    +{T<:Base.Dates.TimeType}(x::Base.Dates.Period,r::Range{T<:Base.Dates.TimeType}) at dates/ranges.jl:40
    +(a,b,c) at operators.jl:83
    +(a,b,c,xs...) at operators.jl:84

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
    WARNING: New definition
        g(Any, Float64) at none:1
    is ambiguous with:
        g(Float64, Any) at none:1.
    To fix, define
        g(Float64, Float64)
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

    julia> same_type(Int32(1), Int64(2))
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
    ERROR: MethodError: `myappend` has no method matching myappend(::Array{Int64,1}, ::Float64)
    Closest candidates are:
      myappend{T}(::Array{T,1}, !Matched::T)

    julia> myappend([1.0,2.0,3.0],4.0)
    4-element Array{Float64,1}:
     1.0
     2.0
     3.0
     4.0

    julia> myappend([1.0,2.0,3.0],4)
    ERROR: MethodError: `myappend` has no method matching myappend(::Array{Float64,1}, ::Int64)
    Closest candidates are:
      myappend{T}(::Array{T,1}, !Matched::T)

As you can see, the type of the appended element must match the element
type of the vector it is appended to, or else a :exc:`MethodError` is raised.
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
    no method same_type_numeric(String,Float64)

    julia> same_type_numeric("foo", "bar")
    no method same_type_numeric(String,String)

    julia> same_type_numeric(Int32(1), Int64(2))
    false

The ``same_type_numeric`` function behaves much like the ``same_type``
function defined above, but is only defined for pairs of numbers.

.. _man-note-on-optional-and-keyword-arguments:

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

This means that calling ``f()`` is equivalent to calling ``f(1,2)``. In
this case the result is ``5``, because ``f(1,2)`` invokes the first
method of ``f`` above. However, this need not always be the case. If you
define a fourth method that is more specialized for integers::

    f(a::Int,b::Int) = a-2b

then the result of both ``f()`` and ``f(1,2)`` is ``-3``. In other words,
optional arguments are tied to a function, not to any specific method of
that function. It depends on the types of the optional arguments which
method is invoked. When optional arguments are defined in terms of a global
variable, the type of the optional argument may even change at run-time.

Keyword arguments behave quite differently from ordinary positional arguments.
In particular, they do not participate in method dispatch. Methods are
dispatched based only on positional arguments, with keyword arguments processed
after the matching method is identified.

Function-like objects
---------------------

Methods are associated with types, so it is possible to make any arbitrary
Julia object "callable" by adding methods to its type.
(Such "callable" objects are sometimes called "functors.")

For example, you can define a type that stores the coefficients of a
polynomial, but behaves like a function evaluating the polynomial::

    immutable Polynomial{R}
        coeffs::Vector{R}
    end

    function (p::Polynomial)(x)
        v = p.coeffs[end]
        for i = (length(p.coeffs)-1):-1:1
            v = v*x + p.coeffs[i]
        end
        return v
    end

Notice that the function is specified by type instead of by name.
In the function body, ``p`` will refer to the object that was called.
A ``Polynomial`` can be used as follows::

    julia> p = Polynomial([1,10,100])
    Polynomial{Int64}([1,10,100])

    julia> p(3)
    931

This mechanism is also the key to how type constructors and closures
(inner functions that refer to their surrounding environment) work
in Julia, discussed :ref:`later in the manual <constructors-and-conversion>`.

Empty generic functions
-----------------------

Occasionally it is useful to introduce a generic function without yet adding
methods.
This can be used to separate interface definitions from implementations.
It might also be done for the purpose of documentation or code readability.
The syntax for this is an empty ``function`` block without a tuple of
arguments::

    function emptyfunc
    end

.. [Clarke61] Arthur C. Clarke, *Profiles of the Future* (1961): Clarke's Third Law.

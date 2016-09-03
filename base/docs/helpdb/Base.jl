# This file is a part of Julia. License is MIT: http://julialang.org/license

# Base

"""
    systemerror(sysfunc, iftrue)

Raises a `SystemError` for `errno` with the descriptive string `sysfunc` if `iftrue` is `true`
"""
systemerror

"""
    fill!(A, x)

Fill array `A` with the value `x`. If `x` is an object reference, all elements will refer to
the same object. `fill!(A, Foo())` will return `A` filled with the result of evaluating
`Foo()` once.
"""
fill!

"""
    read!(stream::IO, array::Union{Array, BitArray})
    read!(filename::AbstractString, array::Union{Array, BitArray})

Read binary data from an I/O stream or file, filling in `array`.
"""
read!

"""
    empty!(collection) -> collection

Remove all elements from a `collection`.
"""
empty!

"""
    asin(x)

Compute the inverse sine of `x`, where the output is in radians.
"""
asin

"""
    takebuf_array(b::IOBuffer)

Obtain the contents of an `IOBuffer` as an array, without copying. Afterwards, the
`IOBuffer` is reset to its initial state.
"""
takebuf_array

"""
    pointer(array [, index])

Get the native address of an array or string element. Be careful to ensure that a Julia
reference to `a` exists as long as this pointer will be used. This function is "unsafe" like
`unsafe_convert`.

Calling `Ref(array[, index])` is generally preferable to this function.
"""
pointer

"""
    //(num, den)

Divide two integers or rational numbers, giving a `Rational` result.
"""
Base.:(//)

"""
    isinteger(x) -> Bool

Test whether `x` or all its elements are numerically equal to some integer
"""
isinteger

"""
    ./(x, y)

Element-wise right division operator.
"""
Base.:(./)

"""
    prod!(r, A)

Multiply elements of `A` over the singleton dimensions of `r`, and write results to `r`.
"""
prod!

"""
    cummin(A, [dim])

Cumulative minimum along a dimension. The dimension defaults to 1.
"""
cummin

"""
    minabs!(r, A)

Compute the minimum absolute values over the singleton dimensions of `r`, and write values to `r`.
"""
minabs!

"""
    eigfact!(A, [B])

Same as [`eigfact`](:func:`eigfact`), but saves space by overwriting the input `A` (and
`B`), instead of creating a copy.
"""
eigfact!

"""
    cosh(x)

Compute hyperbolic cosine of `x`.
"""
cosh

"""
    precision(num::AbstractFloat)

Get the precision of a floating point number, as defined by the effective number of bits in
the mantissa.
"""
precision

"""
    promote_type(type1, type2)

Determine a type big enough to hold values of each argument type without loss, whenever
possible. In some cases, where no type exists to which both types can be promoted
losslessly, some loss is tolerated; for example, `promote_type(Int64,Float64)` returns
`Float64` even though strictly, not all `Int64` values can be represented exactly as
`Float64` values.
"""
promote_type

"""
```
.*(x, y)
```

Element-wise multiplication operator.
"""
Base.:(.*)

"""
    backtrace()

Get a backtrace object for the current program point.
"""
backtrace

"""
    -(x)

Unary minus operator.
"""
-(x)

"""
    -(x, y)

Subtraction operator.
"""
-(x, y)

"""
    Nullable(x)

Wrap value `x` in an object of type `Nullable`, which indicates whether a value is present.
`Nullable(x)` yields a non-empty wrapper, and `Nullable{T}()` yields an empty instance of a
wrapper that might contain a value of type `T`.
"""
Nullable

"""
    bits(n)

A string giving the literal bit representation of a number.
"""
bits

"""
    getindex(type[, elements...])

Construct a 1-d array of the specified type. This is usually called with the syntax
`Type[]`. Element values can be specified using `Type[a,b,c,...]`.
"""
getindex(::Type, elements...)

"""
    getindex(A, inds...)

Returns a subset of array `A` as specified by `inds`, where each `ind` may be an
`Int`, a `Range`, or a `Vector`. See the manual section on
[array indexing](:ref:`array indexing <man-array-indexing>`) for details.
"""
getindex(::AbstractArray, inds...)

"""
    getindex(collection, key...)

Retrieve the value(s) stored at the given key or index within a collection. The syntax
`a[i,j,...]` is converted by the compiler to `getindex(a, i, j, ...)`.
"""
getindex(collection, key...)

"""
    cconvert(T,x)

Convert `x` to a value of type `T`, typically by calling `convert(T,x)`

In cases where `x` cannot be safely converted to `T`, unlike [`convert`](:func:`convert`), `cconvert` may
return an object of a type different from `T`, which however is suitable for
[`unsafe_convert`](:func:`unsafe_convert`) to handle.

Neither `convert` nor `cconvert` should take a Julia object and turn it into a `Ptr`.
"""
cconvert

"""
    assert(cond)

Throw an [`AssertionError`](:obj:`AssertionError`) if `cond` is `false`.
Also available as the macro `@assert expr`.
"""
assert

"""
    sech(x)

Compute the hyperbolic secant of `x`
"""
sech

"""
    acos(x)

Compute the inverse cosine of `x`, where the output is in radians
"""
acos

"""
    unsafe_copy!(dest::Ptr{T}, src::Ptr{T}, N)

Copy `N` elements from a source pointer to a destination, with no checking. The size of an
element is determined by the type of the pointers.

The `unsafe` prefix on this function indicates that no validation is performed on the
pointers `dest` and `src` to ensure that they are valid. Incorrect usage may corrupt or
segfault your program, in the same manner as C.
"""
unsafe_copy!{T}(dest::Ptr{T}, src::Ptr{T}, N)

"""
    unsafe_copy!(dest::Array, do, src::Array, so, N)

Copy `N` elements from a source array to a destination, starting at offset `so` in the
source and `do` in the destination (1-indexed).

The `unsafe` prefix on this function indicates that no validation is performed to ensure
that N is inbounds on either array. Incorrect usage may corrupt or segfault your program, in
the same manner as C.
"""
unsafe_copy!(dest::Array, d, src::Array, so, N)

"""
    .^(x, y)

Element-wise exponentiation operator.
"""
Base.:(.^)

"""
    Float32(x [, mode::RoundingMode])

Create a Float32 from `x`. If `x` is not exactly representable then `mode` determines how
`x` is rounded.

```jldoctest
julia> Float32(1/3, RoundDown)
0.3333333f0

julia> Float32(1/3, RoundUp)
0.33333334f0
```

See [`RoundingMode`](:obj:`RoundingMode`) for available rounding modes.
"""
Float32

"""
    Mmap.mmap(io::Union{IOStream,AbstractString,Mmap.AnonymousMmap}[, type::Type{Array{T,N}}, dims, offset]; grow::Bool=true, shared::Bool=true)
           Mmap.mmap(type::Type{Array{T,N}}, dims)

Create an `Array` whose values are linked to a file, using memory-mapping. This provides a
convenient way of working with data too large to fit in the computer's memory.

The type is an `Array{T,N}` with a bits-type element of `T` and dimension `N` that
determines how the bytes of the array are interpreted. Note that the file must be stored in
binary format, and no format conversions are possible (this is a limitation of operating
systems, not Julia).

`dims` is a tuple or single `Integer` specifying the size or length of the array.

The file is passed via the stream argument, either as an open `IOStream` or filename string.
When you initialize the stream, use `"r"` for a "read-only" array, and `"w+"` to create a
new array used to write values to disk.

If no `type` argument is specified, the default is `Vector{UInt8}`.

Optionally, you can specify an offset (in bytes) if, for example, you want to skip over a
header in the file. The default value for the offset is the current stream position for an
`IOStream`.

The `grow` keyword argument specifies whether the disk file should be grown to accommodate
the requested size of array (if the total file size is < requested array size). Write
privileges are required to grow the file.

The `shared` keyword argument specifies whether the resulting `Array` and changes made to it
will be visible to other processes mapping the same file.

For example, the following code

```julia
# Create a file for mmapping
# (you could alternatively use mmap to do this step, too)
A = rand(1:20, 5, 30)
s = open("/tmp/mmap.bin", "w+")
# We'll write the dimensions of the array as the first two Ints in the file
write(s, size(A,1))
write(s, size(A,2))
# Now write the data
write(s, A)
close(s)

# Test by reading it back in
s = open("/tmp/mmap.bin")   # default is read-only
m = read(s, Int)
n = read(s, Int)
A2 = Mmap.mmap(s, Matrix{Int}, (m,n))
```

creates a `m`-by-`n` `Matrix{Int}`, linked to the file associated with stream `s`.

A more portable file would need to encode the word size -- 32 bit or 64 bit -- and endianness
information in the header. In practice, consider encoding binary data using standard formats
like HDF5 (which can be used with memory-mapping).
"""
Mmap.mmap(io, ::Type, dims, offset)

"""
    Mmap.mmap(io, BitArray, [dims, offset])

Create a `BitArray` whose values are linked to a file, using memory-mapping; it has the same
purpose, works in the same way, and has the same arguments, as [`mmap`](:func:`mmap`), but
the byte representation is different.

**Example**: `B = Mmap.mmap(s, BitArray, (25,30000))`

This would create a 25-by-30000 `BitArray`, linked to the file associated with stream `s`.
"""
Mmap.mmap(io, ::BitArray, dims = ?, offset = ?)

"""
    bessely0(x)

Bessel function of the second kind of order 0, ``Y_0(x)``.
"""
bessely0

"""
    any!(r, A)

Test whether any values in `A` along the singleton dimensions of `r` are `true`, and write
results to `r`.
"""
any!

"""
    filter!(function, collection)

Update `collection`, removing elements for which `function` is `false`.
For associative collections, the function is passed two arguments (key and value).

```jldoctest
julia> filter!(isodd, collect(1:10))
5-element Array{Int64,1}:
 1
 3
 5
 7
 9
```
"""
filter!

"""
    sizeof(T)

Size, in bytes, of the canonical binary representation of the given DataType `T`, if any.
"""
sizeof(::Type)

"""
    ===(x, y)
    ≡(x,y)

See the [`is`](:func:`is`) operator.
"""
Base.:(===)

"""
    ReadOnlyMemoryError()

An operation tried to write to memory that is read-only.
"""
ReadOnlyMemoryError

"""
    last(coll)

Get the last element of an ordered collection, if it can be computed in O(1) time. This is
accomplished by calling [`endof`](:func:`endof`) to get the last index. Returns the end
point of a [`Range`](:obj:`Range`) even if it is empty.
"""
last

"""
    sinh(x)

Compute hyperbolic sine of `x`.
"""
sinh

"""
    ceil([T,] x, [digits, [base]])

`ceil(x)` returns the nearest integral value of the same type as `x` that is greater than or
equal to `x`.

`ceil(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is not
representable.

`digits` and `base` work as for [`round`](:func:`round`).
"""
ceil

"""
    oftype(x, y)

Convert `y` to the type of `x` (`convert(typeof(x), y)`).
"""
oftype

"""
    maxabs!(r, A)

Compute the maximum absolute values over the singleton dimensions of `r`, and write values to `r`.
"""
maxabs!

"""
    isfinite(f) -> Bool

Test whether a number is finite.

```jldoctest
julia> isfinite(5)
true

julia> isfinite(NaN32)
false
```
"""
isfinite

"""
    push!(collection, items...) -> collection

Insert one or more `items` at the end of `collection`.

```jldoctest
julia> push!([1, 2, 3], 4, 5, 6)
6-element Array{Int64,1}:
 1
 2
 3
 4
 5
 6
```

Use [`append!`](:func:`append!`) to add all the elements of another collection to
`collection`. The result of the preceding example is equivalent to `append!([1, 2, 3], [4,
5, 6])`.
"""
push!

"""
    prevpow(a, x)

The largest `a^n` not greater than `x`, where `n` is a non-negative integer.
`a` must be greater than 1, and `x` must not be less than 1.
"""
prevpow

"""
    promote(xs...)

Convert all arguments to their common promotion type (if any), and return them all (as a tuple).
"""
promote

"""
    tan(x)

Compute tangent of `x`, where `x` is in radians.
"""
tan

"""
    fd(stream)

Returns the file descriptor backing the stream or file. Note that this function only applies
to synchronous `File`'s and `IOStream`'s not to any of the asynchronous streams.
"""
fd

"""
    ones(type, dims)

Create an array of all ones of specified type. The type defaults to `Float64` if not specified.
"""
ones(t,dims)

"""
    ones(A)

Create an array of all ones with the same element type and shape as `A`.
"""
ones(A)

"""
    reshape(A, dims)

Create an array with the same data as the given array, but with different dimensions.
"""
reshape

"""
    randsubseq!(S, A, p)

Like [`randsubseq`](:func:`randsubseq`), but the results are stored in `S`
(which is resized as needed).
"""
randsubseq!

"""
    maximum(A, dims)

Compute the maximum value of an array over the given dimensions.
"""
maximum(A,dims)

"""
    redisplay(x)
    redisplay(d::Display, x)
    redisplay(mime, x)
    redisplay(d::Display, mime, x)

By default, the `redisplay` functions simply call [`display`](:func:`display`).
However, some display backends may override `redisplay` to modify an existing
display of `x` (if any).
Using `redisplay` is also a hint to the backend that `x` may be redisplayed
several times, and the backend may choose to defer the display until
(for example) the next interactive prompt.
"""
redisplay

"""
    searchsorted(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

Returns the range of indices of `a` which compare as equal to `x` according to the order
specified by the `by`, `lt` and `rev` keywords, assuming that `a` is already sorted in that
order. Returns an empty range located at the insertion point if `a` does not contain values
equal to `x`.
"""
searchsorted

"""
    /(x, y)

Right division operator: multiplication of `x` by the inverse of `y` on the right. Gives
floating-point results for integer arguments.
"""
Base.:(/)

"""
    dump(x)

Show every part of the representation of a value.
"""
dump

"""
    sumabs(A, dims)

Sum absolute values of elements of an array over the given dimensions.
"""
sumabs(A, dims)

"""
    consume(task, values...)

Receive the next value passed to `produce` by the specified task. Additional arguments may
be passed, to be returned from the last `produce` call in the producer.
"""
consume

"""
    cummax(A, [dim])

Cumulative maximum along a dimension. The dimension defaults to 1.
"""
cummax

"""
    isinteractive() -> Bool

Determine whether Julia is running an interactive session.
"""
isinteractive

"""
    sum!(r, A)

Sum elements of `A` over the singleton dimensions of `r`, and write results to `r`.
"""
sum!

"""
    parentindexes(A)

From an array view `A`, returns the corresponding indexes in the parent.
"""
parentindexes

"""
    display(x)
    display(d::Display, x)
    display(mime, x)
    display(d::Display, mime, x)

Display `x` using the topmost applicable display in the display stack, typically using the
richest supported multimedia output for `x`, with plain-text [`STDOUT`](:obj:`STDOUT`) output as a fallback.
The `display(d, x)` variant attempts to display `x` on the given display `d` only, throwing
a `MethodError` if `d` cannot display objects of this type.

There are also two variants with a `mime` argument (a MIME type string, such as
`"image/png"`), which attempt to display `x` using the requested MIME type *only*, throwing
a `MethodError` if this type is not supported by either the display(s) or by `x`. With these
variants, one can also supply the "raw" data in the requested MIME type by passing
`x::AbstractString` (for MIME types with text-based storage, such as text/html or
application/postscript) or `x::Vector{UInt8}` (for binary MIME types).
"""
display

"""
    @spawnat

Accepts two arguments, `p` and an expression. A closure is created around the expression and
run asynchronously on process `p`. Returns a [`Future`](:obj:`Future`) to the result.
"""
:@spawnat

"""
    print_shortest(io, x)

Print the shortest possible representation, with the minimum number of consecutive non-zero
digits, of number `x`, ensuring that it would parse to the exact same number.
"""
print_shortest

"""
    tuple(xs...)

Construct a tuple of the given objects.
"""
tuple

"""
    eachmatch(r::Regex, s::AbstractString[, overlap::Bool=false])

Search for all matches of a the regular expression `r` in `s` and return a iterator over the
matches. If overlap is `true`, the matching sequences are allowed to overlap indices in the
original string, otherwise they must be from distinct character ranges.
"""
eachmatch

"""
    log10(x)

Compute the logarithm of `x` to base 10.
Throws [`DomainError`](:obj:`DomainError`) for negative `Real` arguments.
"""
log10

"""
    num2hex(f)

Get a hexadecimal string of the binary representation of a floating point number.

```jldoctest
julia> num2hex(2.2)
"400199999999999a"
```
"""
num2hex

"""
    displayable(mime) -> Bool
    displayable(d::Display, mime) -> Bool

Returns a boolean value indicating whether the given `mime` type (string) is displayable by
any of the displays in the current display stack, or specifically by the display `d` in the
second variant.
"""
displayable

"""
    truncate(file,n)

Resize the file or buffer given by the first argument to exactly `n` bytes, filling
previously unallocated space with '\\0' if the file or buffer is grown.
"""
truncate

"""
    exp10(x)

Compute ``10^x``.
"""
exp10

"""
    &(x, y)

Bitwise and.
"""
&

"""
    cumsum!(B, A, [dim])

Cumulative sum of `A` along a dimension, storing the result in `B`. The dimension defaults
to 1.
"""
cumsum!

"""
    select(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

Variant of `select!` which copies `v` before partially sorting it, thereby returning the
same thing as `select!` but leaving `v` unmodified.
"""
select

"""
    accept(server[,client])

Accepts a connection on the given server and returns a connection to the client. An
uninitialized client stream may be provided, in which case it will be used instead of
creating a new stream.
"""
accept

"""
    readstring(stream::IO)
    readstring(filename::AbstractString)

Read the entire contents of an I/O stream or a file as a string.
The text is assumed to be encoded in UTF-8.
"""
readstring

"""
    eachline(stream::IO)
    eachline(filename::AbstractString)

Create an iterable object that will yield each line from an I/O stream or a file.
The text is assumed to be encoded in UTF-8.
"""
eachline

"""
    complex(r, [i])

Convert real numbers or arrays to complex. `i` defaults to zero.
"""
complex

"""
    Mmap.Anonymous(name, readonly, create)

Create an `IO`-like object for creating zeroed-out mmapped-memory that is not tied to a file
for use in `Mmap.mmap`. Used by `SharedArray` for creating shared memory arrays.
"""
Mmap.Anonymous

"""
    erfi(x)

Compute the imaginary error function of `x`, defined by ``-i \\operatorname{erf}(ix)``.
"""
erfi

"""
    floor([T,] x, [digits, [base]])

`floor(x)` returns the nearest integral value of the same type as `x` that is less than or
equal to `x`.

`floor(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is
not representable.

`digits` and `base` work as for [`round`](:func:`round`).
"""
floor

"""
    ErrorException(msg)

Generic error type. The error message, in the `.msg` field, may provide more specific details.
"""
ErrorException

"""
    reverse(v [, start=1 [, stop=length(v) ]] )

Return a copy of `v` reversed from start to stop.
"""
reverse

"""
    reverse!(v [, start=1 [, stop=length(v) ]]) -> v

In-place version of [`reverse`](:func:`reverse`).
"""
reverse!

"""
    num(x)

Numerator of the rational representation of `x`.
"""
num

"""
    .<(x, y)

Element-wise less-than comparison operator.
"""
Base.:(.<)

"""
    UndefRefError()

The item or field is not defined for the given object.
"""
UndefRefError

"""
    bessely1(x)

Bessel function of the second kind of order 1, ``Y_1(x)``.
"""
bessely1

"""
    append!(collection, collection2) -> collection.

Add the elements of `collection2` to the end of `collection`.

```jldoctest
julia> append!([1],[2,3])
3-element Array{Int64,1}:
 1
 2
 3
```

```jldoctest
julia> append!([1, 2, 3], [4, 5, 6])
6-element Array{Int64,1}:
 1
 2
 3
 4
 5
 6
```

Use [`push!`](:func:`push!`) to add individual items to `collection` which are not already
themselves in another collection. The result is of the preceding example is equivalent to
`push!([1, 2, 3], 4, 5, 6)`.
"""
append!

"""
    skip(s, offset)

Seek a stream relative to the current position.
"""
skip

"""
    lu(A) -> L, U, p

Compute the LU factorization of `A`, such that `A[p,:] = L*U`.
"""
lu

"""
    setdiff!(s, iterable)

Remove each element of `iterable` from set `s` in-place.
"""
setdiff!

"""
    copysign(x, y)

Return `x` such that it has the same sign as `y`
"""
copysign

"""
    @show

Show an expression and result, returning the result.
"""
:@show

"""
    showcompact(x)

Show a compact representation of a value.

This is used for printing array elements without repeating type information (which would
be redundant with that printed once for the whole array), and without line breaks inside
the representation of an element.

To offer a compact representation different from its standard one, a custom type should
test `get(io, :compact, false)` in its normal `show` method.
"""
showcompact

"""
    erfc(x)

Compute the complementary error function of `x`, defined by ``1 - \\operatorname{erf}(x)``.
"""
erfc

"""
    getfield(value, name::Symbol)

Extract a named field from a `value` of composite type. The syntax `a.b` calls
`getfield(a, :b)`.
"""
getfield

"""
    besselj1(x)

Bessel function of the first kind of order 1, ``J_1(x)``.
"""
besselj1

"""
    select!(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

Partially sort the vector `v` in place, according to the order specified by `by`, `lt` and
`rev` so that the value at index `k` (or range of adjacent values if `k` is a range) occurs
at the position where it would appear if the array were fully sorted via a non-stable
algorithm. If `k` is a single index, that value is returned; if `k` is a range, an array of
values at those indices is returned. Note that `select!` does not fully sort the input
array.
"""
select!

"""
    maximum!(r, A)

Compute the maximum value of `A` over the singleton dimensions of `r`, and write results to `r`.
"""
maximum!

"""
    prod(A, dims)

Multiply elements of an array over the given dimensions.
"""
prod(A, dims)

"""
    log1p(x)

Accurate natural logarithm of `1+x`. Throws `DomainError` for `Real` arguments less than -1.

There is an experimental variant in the `Base.Math.JuliaLibm` module, which is typically
faster and more accurate.
"""
log1p

"""
    flipsign(x, y)

Return `x` with its sign flipped if `y` is negative. For example `abs(x) = flipsign(x,x)`.
"""
flipsign

"""
    lbeta(x, y)

Natural logarithm of the absolute value of the beta function ``\\log(|\\operatorname{B}(x,y)|)``.
"""
lbeta

"""
    randstring([rng,] len=8)

Create a random ASCII string of length `len`, consisting of upper- and
lower-case letters and the digits 0-9. The optional `rng` argument
specifies a random number generator, see [Random Numbers](:ref:`Random Numbers <random-numbers>`).
"""
randstring

"""
    Float64(x [, mode::RoundingMode])

Create a Float64 from `x`. If `x` is not exactly representable then `mode` determines how
`x` is rounded.

```jldoctest
julia> Float64(pi, RoundDown)
3.141592653589793

julia> Float64(pi, RoundUp)
3.1415926535897936
```

See [`RoundingMode`](:obj:`RoundingMode`) for available rounding modes.
"""
Float64

"""
    union(s1,s2...)
    ∪(s1,s2...)

Construct the union of two or more sets. Maintains order with arrays.
"""
union

"""
    realmax(T)

The highest finite value representable by the given floating-point DataType `T`.
"""
realmax

"""
    serialize(stream, value)

Write an arbitrary value to a stream in an opaque format, such that it can be read back by
[`deserialize`](:func:`deserialize`). The read-back value will be as identical as possible to the original. In
general, this process will not work if the reading and writing are done by different
versions of Julia, or an instance of Julia with a different system image. `Ptr` values are
serialized as all-zero bit patterns (`NULL`).
"""
serialize

"""
    sum(A, dims)

Sum elements of an array over the given dimensions.
"""
sum(A, dims)

"""
    typemin(T)

The lowest value representable by the given (real) numeric DataType `T`.
"""
typemin

"""
    typeof(x)

Get the concrete type of `x`.
"""
typeof

"""
    log(x)

Compute the natural logarithm of `x`. Throws `DomainError` for negative `Real` arguments.
Use complex negative arguments to obtain complex results.

There is an experimental variant in the `Base.Math.JuliaLibm` module, which is typically
faster and more accurate.
"""
log(x)

"""
    trunc([T,] x, [digits, [base]])

`trunc(x)` returns the nearest integral value of the same type as `x` whose absolute value
is less than or equal to `x`.

`trunc(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is
not representable.

`digits` and `base` work as for [`round`](:func:`round`).
"""
trunc

"""
    unsafe_convert(T,x)

Convert `x` to a value of type `T`

In cases where [`convert`](:func:`convert`) would need to take a Julia object
and turn it into a `Ptr`, this function should be used to define and perform
that conversion.

Be careful to ensure that a Julia reference to `x` exists as long as the result of this
function will be used. Accordingly, the argument `x` to this function should never be an
expression, only a variable name or field reference. For example, `x=a.b.c` is acceptable,
but `x=[a,b,c]` is not.

The `unsafe` prefix on this function indicates that using the result of this function after
the `x` argument to this function is no longer accessible to the program may cause undefined
behavior, including program corruption or segfaults, at any later time.
"""
unsafe_convert

"""
    erfinv(x)

Compute the inverse error function of a real `x`, defined by ``\\operatorname{erf}(\\operatorname{erfinv}(x)) = x``.
"""
erfinv

"""
    seek(s, pos)

Seek a stream to the given position.
"""
seek

"""
    besselj0(x)

Bessel function of the first kind of order 0, ``J_0(x)``.
"""
besselj0

"""
    erfcinv(x)

Compute the inverse error complementary function of a real `x`, defined by
``\\operatorname{erfc}(\\operatorname{erfcinv}(x)) = x``.
"""
erfcinv

"""
    minabs(A, dims)

Compute the minimum absolute values over given dimensions.
"""
minabs(A, dims)

"""
    popdisplay()
    popdisplay(d::Display)

Pop the topmost backend off of the display-backend stack, or the topmost copy of `d` in the
second variant.
"""
popdisplay

"""
    cglobal((symbol, library) [, type=Void])

Obtain a pointer to a global variable in a C-exported shared library, specified exactly as
in [`ccall`](:func:`ccall`).
Returns a `Ptr{Type}`, defaulting to `Ptr{Void}` if no `Type` argument is
supplied.
The values can be read or written by [`unsafe_load`](:func:`unsafe_load`) or [`unsafe_store!`](:func:`unsafe_store!`),
respectively.
"""
cglobal

"""
    one(x)

Get the multiplicative identity element for the type of `x` (`x` can also specify the type
itself). For matrices, returns an identity matrix of the appropriate size and type.
"""
one

"""
    endof(collection) -> Integer

Returns the last index of the collection.

```jldoctest
julia> endof([1,2,4])
3
```
"""
endof

"""
    Channel{T}(sz::Int)

Constructs a `Channel` that can hold a maximum of `sz` objects of type `T`. `put!` calls on
a full channel block till an object is removed with `take!`.

Other constructors:

- `Channel()` - equivalent to `Channel{Any}(32)`
- `Channel(sz::Int)` equivalent to `Channel{Any}(sz)`
"""
Channel

"""
    next(iter, state) -> item, state

For a given iterable object and iteration state, return the current item and the next iteration state.
"""
next

"""
    log2(x)

Compute the logarithm of `x` to base 2. Throws `DomainError` for negative `Real` arguments.
"""
log2

"""
    isnull(x)

Is the `Nullable` object `x` null, i.e. missing a value?
"""
isnull

"""
    abs2(x)

Squared absolute value of `x`.
"""
abs2

"""
    sizehint!(s, n)

Suggest that collection `s` reserve capacity for at least `n` elements. This can improve performance.
"""
sizehint!

"""
    OutOfMemoryError()

An operation allocated too much memory for either the system or the garbage collector to
handle properly.
"""
OutOfMemoryError

"""
    finalize(x)

Immediately run finalizers registered for object `x`.
"""
finalize

"""
    BoundsError([a],[i])

An indexing operation into an array, `a`, tried to access an out-of-bounds element, `i`.
"""
BoundsError

"""
    invoke(f, (types...), args...)

Invoke a method for the given generic function matching the specified types (as a tuple), on
the specified arguments. The arguments must be compatible with the specified types. This
allows invoking a method other than the most specific matching method, which is useful when
the behavior of a more general definition is explicitly needed (often as part of the
implementation of a more specific method of the same function).
"""
invoke

"""
    parse(str, start; greedy=true, raise=true)

Parse the expression string and return an expression (which could later be passed to eval
for execution). `start` is the index of the first character to start parsing. If `greedy` is
`true` (default), `parse` will try to consume as much input as it can; otherwise, it will
stop as soon as it has parsed a valid expression. Incomplete but otherwise syntactically
valid expressions will return `Expr(:incomplete, "(error message)")`. If `raise` is `true`
(default), syntax errors other than incomplete expressions will raise an error. If `raise`
is `false`, `parse` will return an expression that will raise an error upon evaluation.
"""
parse(str, start)

"""
    parse(str; raise=true)

Parse the expression string greedily, returning a single expression. An error is thrown if
there are additional characters after the first expression. If `raise` is `true` (default),
syntax errors will raise an error; otherwise, `parse` will return an expression that will
raise an error upon evaluation.
"""
parse(str)

"""
    parse(type, str, [base])

Parse a string as a number. If the type is an integer type, then a base can be specified
(the default is 10). If the type is a floating point type, the string is parsed as a decimal
floating point number. If the string does not contain a valid number, an error is raised.
"""
parse(T::Type, str, base=Int)

"""
    bkfact!(A) -> BunchKaufman

`bkfact!` is the same as [`bkfact`](:func:`bkfact`), but saves space by overwriting the
input `A`, instead of creating a copy.
"""
bkfact!

"""
    ^(x, y)

Exponentiation operator.
"""
Base.:(^)(x, y)

"""
    position(s)

Get the current position of a stream.
"""
position

"""
    selectperm(v, k, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

Return a partial permutation of the vector `v`, according to the order specified by
`by`, `lt` and `rev`, so that `v[output]` returns the first `k` (or range of adjacent values
if `k` is a range) values of a fully sorted version of `v`. If `k` is a single index
(Integer), an array of the first `k` indices is returned; if `k` is a range, an array of
those indices is returned. Note that the handling of integer values for `k` is different
from `select` in that it returns a vector of `k` elements instead of just the `k` th
element. Also note that this is equivalent to, but more efficient than, calling
`sortperm(...)[k]`
"""
selectperm

"""
    reinterpret(type, A)

Change the type-interpretation of a block of memory. For example,
`reinterpret(Float32, UInt32(7))` interprets the 4 bytes corresponding to `UInt32(7)` as a
`Float32`. For arrays, this constructs an array with the same binary data as the given
array, but with the specified element type.
"""
reinterpret

"""
    ~(x)

Bitwise not.
"""
~

"""
    bswap(n)

Byte-swap an integer.
"""
bswap

"""
    sumabs2!(r, A)

Sum squared absolute values of elements of `A` over the singleton dimensions of `r`, and
write results to `r`.
"""
sumabs2!

"""
    @sprintf("%Fmt", args...)

Return `@printf` formatted output as string.

    julia> s = @sprintf "this is a %s %15.1f" "test" 34.567;

    julia> println(s)
    this is a test            34.6
"""
:@sprintf

"""
    tanh(x)

Compute hyperbolic tangent of `x`.
"""
tanh

"""
    maxintfloat(T)

The largest integer losslessly representable by the given floating-point DataType `T`.
"""
maxintfloat

"""
    delete!(collection, key)

Delete the mapping for the given key in a collection, and return the collection.
"""
delete!

"""
    eps(T)

The distance between 1.0 and the next larger representable floating-point value of
`DataType` `T`. Only floating-point types are sensible arguments.
"""
eps(::Union{Type{BigFloat},Type{Float64},Type{Float32},Type{Float16}})

"""
    eps()

The distance between 1.0 and the next larger representable floating-point value of `Float64`.
"""
eps()

"""
    eps(x)

The distance between `x` and the next larger representable floating-point value of the same
`DataType` as `x`.
"""
eps(::AbstractFloat)

"""
    searchsortedfirst(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

Returns the index of the first value in `a` greater than or equal to `x`, according to the
specified order. Returns `length(a)+1` if `x` is greater than all values in `a`.
"""
searchsortedfirst

"""
    big(x)

Convert a number to a maximum precision representation (typically `BigInt` or `BigFloat`).
See [`BigFloat`](:obj:`BigFloat`) for information about some pitfalls with floating-point numbers.
"""
big

"""
    quit()

Quit the program indicating that the processes completed successfully. This function calls
`exit(0)` (see [`exit`](:func:`exit`)).
"""
quit

"""
    typejoin(T, S)

Compute a type that contains both `T` and `S`.
"""
typejoin

"""
    beta(x, y)

Euler integral of the first kind ``\\operatorname{B}(x,y) = \\Gamma(x)\\Gamma(y)/\\Gamma(x+y)``.
"""
beta

"""
    sin(x)

Compute sine of `x`, where `x` is in radians.
"""
sin

"""
    selectperm!(ix, v, k, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false,] [initialized=false])

Like `selectperm`, but accepts a preallocated index vector `ix`. If `initialized` is `false`
(the default), ix is initialized to contain the values `1:length(ix)`.
"""
selectperm!

"""
    precompile(f,args::Tuple{Vararg{Any}})

Compile the given function `f` for the argument tuple (of types) `args`, but do not execute it.
"""
precompile

"""
    asinh(x)

Compute the inverse hyperbolic sine of `x`.
"""
asinh

"""
    minimum(A, dims)

Compute the minimum value of an array over the given dimensions.
"""
minimum(A,dims)

"""
    view(A, inds...)

Like [`getindex`](:func:`getindex`), but returns a view into the parent array `A` with the
given indices instead of making a copy.  Calling [`getindex`](:func:`getindex`) or
[`setindex!`](:func:`setindex!`) on the returned [`SubArray`](:obj:`SubArray`) computes the
indices to the parent array on the fly without checking bounds.
"""
view

"""
    cot(x)

Compute the cotangent of `x`, where `x` is in radians.
"""
cot

"""
    get(collection, key, default)

Return the value stored for the given key, or the given default value if no mapping for the
key is present.
"""
get(collection,key,default)

"""
    get(f::Function, collection, key)

Return the value stored for the given key, or if no mapping for the key is present, return
`f()`.  Use [`get!`](:func:`get!`) to also store the default value in the dictionary.

This is intended to be called using `do` block syntax

```julia
get(dict, key) do
    # default value calculated here
    time()
end
```
"""
get

"""
    lufact!(A) -> LU

`lufact!` is the same as [`lufact`](:func:`lufact`), but saves space by overwriting the
input `A`, instead of creating a copy. An `InexactError` exception is thrown if the
factorisation produces a number not representable by the element type of `A`, e.g. for
integer types.
"""
lufact!

"""
    Mmap.sync!(array)

Forces synchronization between the in-memory version of a memory-mapped `Array` or
`BitArray` and the on-disk version.
"""
Mmap.sync!

"""
    csc(x)

Compute the cosecant of `x`, where `x` is in radians.
"""
csc

"""
    hash(x[, h::UInt])

Compute an integer hash code such that `isequal(x,y)` implies `hash(x)==hash(y)`. The
optional second argument `h` is a hash code to be mixed with the result.

New types should implement the 2-argument form, typically by calling the 2-argument `hash`
method recursively in order to mix hashes of the contents with each other (and with `h`).
Typically, any type that implements `hash` should also implement its own `==` (hence
`isequal`) to guarantee the property mentioned above.
"""
hash

"""
    atanh(x)

Compute the inverse hyperbolic tangent of `x`.
"""
atanh

"""
    read(stream::IO, T)

Read a single value of type `T` from `stream`, in canonical binary representation.
"""
read(stream, t)

"""
    shift!(collection) -> item

Remove the first `item` from `collection`.

```jldoctest
julia> A = [1, 2, 3, 4, 5, 6]
6-element Array{Int64,1}:
 1
 2
 3
 4
 5
 6

julia> shift!(A)
1

julia> A
5-element Array{Int64,1}:
 2
 3
 4
 5
 6
```
"""
shift!

"""
    spawn(command)

Run a command object asynchronously, returning the resulting [`Process`](:obj:`Process`) object.
"""
spawn

"""
    eta(x)

Dirichlet eta function ``\\eta(s) = \\sum^\\infty_{n=1}(-1)^{n-1}/n^{s}``.
"""
eta

"""
    isdefined([m::Module,] s::Symbol)
    isdefined(object, s::Symbol)
    isdefined(a::AbstractArray, index::Int)

Tests whether an assignable location is defined. The arguments can be a module and a symbol,
a composite object and field name (as a symbol), or an array and index. With a single
symbol argument, tests whether a global variable with that name is defined in
`current_module()`.
"""
isdefined

"""
    cotd(x)

Compute the cotangent of `x`, where `x` is in degrees.
"""
cotd

"""
    wait([x])

Block the current task until some event occurs, depending on the type of the argument:

* [`RemoteChannel`](:obj:`RemoteChannel`) : Wait for a value to become available on the specified remote channel.
* [`Future`](:obj:`Future`) : Wait for a value to become available for the specified future.
* [`Channel`](:obj:`Channel`): Wait for a value to be appended to the channel.
* [`Condition`](:obj:`Condition`): Wait for [`notify`](:func:`notify`) on a condition.
* [`Process`](:obj:`Process`): Wait for a process or process chain to exit. The `exitcode` field of a process
  can be used to determine success or failure.
* [`Task`](:obj:`Task`): Wait for a `Task` to finish, returning its result value. If the task fails with an
  exception, the exception is propagated (re-thrown in the task that called `wait`).
* [`RawFD`](:obj:`RawFD`): Wait for changes on a file descriptor (see [`poll_fd`](:func:`poll_fd`) for keyword arguments and return code)

If no argument is passed, the task blocks for an undefined period. A task can only be
restarted by an explicit call to `schedule` or `yieldto`.

Often `wait` is called within a `while` loop to ensure a waited-for condition is met before proceeding.
"""
wait

"""
    atexit(f)

Register a zero-argument function `f()` to be called at process exit. `atexit()` hooks are
called in last in first out (LIFO) order and run before object finalizers.
"""
atexit

"""
    copy(x)

Create a shallow copy of `x`: the outer structure is copied, but not all internal values.
For example, copying an array produces a new array with identically-same elements as the
original.
"""
copy

"""
    isempty(collection) -> Bool

Determine whether a collection is empty (has no elements).

```jldoctest
julia> isempty([])
true

julia> isempty([1 2 3])
false
```
"""
isempty

"""
    sumabs!(r, A)

Sum absolute values of elements of `A` over the singleton dimensions of `r`, and write
results to `r`.
"""
sumabs!

"""
    hex2num(str)

Convert a hexadecimal string to the floating point number it represents.
"""
hex2num

"""
    InexactError()

Type conversion cannot be done exactly.
"""
InexactError

"""
    typemax(T)

The highest value representable by the given (real) numeric `DataType`.
"""
typemax

"""
    all(A, dims)

Test whether all values along the given dimensions of an array are `true`.
"""
all(A::AbstractArray, dims)

"""
    DomainError()

The arguments to a function or constructor are outside the valid domain.
"""
DomainError

"""
    acosh(x)

Compute the inverse hyperbolic cosine of `x`.
"""
acosh

"""
    IntSet([itr])

Construct a sorted set of positive `Int`s generated by the given iterable object, or an
empty set. Implemented as a bit string, and therefore designed for dense integer sets. Only
`Int`s greater than 0 can be stored. If the set will be sparse (for example holding a few
very large integers), use [`Set`](:obj:`Set`) instead.
"""
IntSet

"""
    Task(func)

Create a `Task` (i.e. coroutine) to execute the given function (which must be
callable with no arguments). The task exits when this function returns.
"""
Task

"""
    pushdisplay(d::Display)

Pushes a new display `d` on top of the global display-backend stack. Calling `display(x)` or
`display(mime, x)` will display `x` on the topmost compatible backend in the stack (i.e.,
the topmost backend that does not throw a `MethodError`).
"""
pushdisplay

"""
    produce(value)

Send the given value to the last `consume` call, switching to the consumer task. If the next
`consume` call passes any values, they are returned by `produce`.
"""
produce

"""
    StackOverflowError()

The function call grew beyond the size of the call stack. This usually happens when a call
recurses infinitely.
"""
StackOverflowError

"""
    BigInt(x)

Create an arbitrary precision integer. `x` may be an `Int` (or anything that can be
converted to an `Int`).  The usual mathematical operators are defined for this type, and
results are promoted to a `BigInt`.

Instances can be constructed from strings via [`parse`](:func:`parse`), or using the `big`
string literal.
"""
BigInt

"""
    ==(x, y)

Generic equality operator, giving a single `Bool` result. Falls back to `===`. Should be
implemented for all types with a notion of equality, based on the abstract value that an
instance represents. For example, all numeric types are compared by numeric value, ignoring
type. Strings are compared as sequences of characters, ignoring encoding.

Follows IEEE semantics for floating-point numbers.

Collections should generally implement `==` by calling `==` recursively on all contents.

New numeric types should implement this function for two arguments of the new type, and
handle comparison to other types via promotion rules where possible.
"""
Base.:(==)

"""
    seekstart(s)

Seek a stream to its beginning.
"""
seekstart

"""
    nfields(x::DataType) -> Int

Get the number of fields of a `DataType`.
"""
nfields

"""
    show(stream, mime, x)

The `display` functions ultimately call `show` in order to write an object `x` as a
given `mime` type to a given I/O `stream` (usually a memory buffer), if possible. In order
to provide a rich multimedia representation of a user-defined type `T`, it is only necessary
to define a new `show` method for `T`, via: `show(stream, ::MIME"mime", x::T) = ...`,
where `mime` is a MIME-type string and the function body calls `write` (or similar) to write
that representation of `x` to `stream`. (Note that the `MIME""` notation only supports
literal strings; to construct `MIME` types in a more flexible manner use
`MIME{Symbol("")}`.)

For example, if you define a `MyImage` type and know how to write it to a PNG file, you
could define a function `show(stream, ::MIME"image/png", x::MyImage) = ...` to allow
your images to be displayed on any PNG-capable `Display` (such as IJulia). As usual, be sure
to `import Base.show` in order to add new methods to the built-in Julia function
`show`.

The default MIME type is `MIME"text/plain"`. There is a fallback definition for `text/plain`
output that calls `show` with 2 arguments. Therefore, this case should be handled by
defining a 2-argument `show(stream::IO, x::MyType)` method.

Technically, the `MIME"mime"` macro defines a singleton type for the given `mime` string,
which allows us to exploit Julia's dispatch mechanisms in determining how to display objects
of any given type.

The first argument to `show` can be an [`IOContext`](:obj:`IOContext`) specifying output format properties.
See [`IOContext`](:obj:`IOContext`) for details.
"""
show(stream, mime, x)

"""
    mean!(r, v)

Compute the mean of `v` over the singleton dimensions of `r`, and write results to `r`.
"""
mean!

"""
    isless(x, y)

Test whether `x` is less than `y`, according to a canonical total order. Values that are
normally unordered, such as `NaN`, are ordered in an arbitrary but consistent fashion. This
is the default comparison used by `sort`. Non-numeric types with a canonical total order
should implement this function. Numeric types only need to implement it if they have special
values such as `NaN`.
"""
isless

"""
    expm1(x)

Accurately compute ``e^x-1``.
"""
expm1

"""
    showerror(io, e)

Show a descriptive representation of an exception object.
"""
showerror

"""
    error(message::AbstractString)

Raise an `ErrorException` with the given message.
"""
error

"""
    sqrtm(A)

If `A` has no negative real eigenvalues, compute the principal matrix square root of `A`,
that is the unique matrix ``X`` with eigenvalues having positive real part such that
``X^2 = A``. Otherwise, a nonprincipal square root is returned.

If `A` is symmetric or Hermitian, its eigendecomposition ([`eigfact`](:func:`eigfact`)) is
used to compute the square root. Otherwise, the square root is determined by means of the
Björck-Hammarling method, which computes the complex Schur form ([`schur`](:func:`schur`))
and then the complex square root of the triangular factor.

[^BH83]: Åke Björck and Sven Hammarling, "A Schur method for the square root of a matrix", Linear Algebra and its Applications, 52-53, 1983, 127-140. [doi:10.1016/0024-3795(83)80010-X](http://dx.doi.org/10.1016/0024-3795(83)80010-X)

"""
sqrtm

"""
    unsafe_store!(p::Ptr{T}, x, [i::Integer=1])

Store a value of type `T` to the address of the ith element (1-indexed) starting at `p`.
This is equivalent to the C expression `p[i-1] = x`.

The `unsafe` prefix on this function indicates that no validation is performed on the
pointer `p` to ensure that it is valid. Incorrect usage may corrupt or segfault your
program, in the same manner as C.
"""
unsafe_store!

"""
    hessfact!(A)

`hessfact!` is the same as [`hessfact`](:func:`hessfact`), but saves space by overwriting
the input `A`, instead of creating a copy.
"""
hessfact!

"""
    readcsv(source, [T::Type]; options...)

Equivalent to `readdlm` with `delim` set to comma.
"""
readcsv

"""
    erfcx(x)

Compute the scaled complementary error function of `x`, defined by ``e^{x^2} \\operatorname{erfc}(x)``.
Note also that ``\\operatorname{erfcx}(-ix)`` computes the Faddeeva function ``w(x)``.
"""
erfcx

"""
    UndefVarError(var::Symbol)

A symbol in the current scope is not defined.
"""
UndefVarError

"""
    gc()

Perform garbage collection. This should not generally be used.
"""
gc

"""
    minimum!(r, A)

Compute the minimum value of `A` over the singleton dimensions of `r`, and write results to `r`.
"""
minimum!

"""
    .-(x, y)

Element-wise subtraction operator.
"""
Base.:(.-)

"""
    unsafe_trunc(T, x)

`unsafe_trunc(T, x)` returns the nearest integral value of type `T` whose absolute value is
less than or equal to `x`. If the value is not representable by `T`, an arbitrary value will
be returned.
"""
unsafe_trunc

"""
    parent(A)

Returns the "parent array" of an array view type (e.g., `SubArray`), or the array itself if
it is not a view.
"""
parent

"""
    nextpow(a, x)

The smallest `a^n` not less than `x`, where `n` is a non-negative integer. `a` must be
greater than 1, and `x` must be greater than 0.
"""
nextpow

"""
    gc_enable(on::Bool)

Control whether garbage collection is enabled using a boolean argument (`true` for enabled,
`false` for disabled). Returns previous GC state. Disabling garbage collection should be
used only with extreme caution, as it can cause memory use to grow without bound.
"""
gc_enable

"""
    atan(x)

Compute the inverse tangent of `x`, where the output is in radians.
"""
atan

"""
    isinf(f) -> Bool

Test whether a number is infinite.
"""
isinf

"""
    secd(x)

Compute the secant of `x`, where `x` is in degrees.
"""
secd

"""
    OverflowError()

The result of an expression is too large for the specified type and will cause a wraparound.
"""
OverflowError

"""
    object_id(x)

Get a hash value for `x` based on object identity. `object_id(x)==object_id(y)` if `x === y`.
"""
object_id

"""
    cat(dims, A...)

Concatenate the input arrays along the specified dimensions in the iterable `dims`. For
dimensions not in `dims`, all input arrays should have the same size, which will also be the
size of the output array along that dimension. For dimensions in `dims`, the size of the
output array is the sum of the sizes of the input arrays along that dimension. If `dims` is
a single number, the different arrays are tightly stacked along that dimension. If `dims` is
an iterable containing several dimensions, this allows one to construct block diagonal
matrices and their higher-dimensional analogues by simultaneously increasing several
dimensions for every new input array and putting zero blocks elsewhere. For example,
`cat([1,2], matrices...)` builds a block diagonal matrix, i.e. a block matrix with
`matrices[1]`, `matrices[2]`, ... as diagonal blocks and matching zero blocks away from the
diagonal.
"""
cat

"""
    show(x)

Write an informative text representation of a value to the current output stream. New types
should overload `show(io, x)` where the first argument is a stream. The representation used
by `show` generally includes Julia-specific formatting and type information.
"""
show(x)

"""
    Array(dims)

`Array{T}(dims)` constructs an uninitialized dense array with element type `T`. `dims` may
be a tuple or a series of integer arguments. The syntax `Array(T, dims)` is also available,
but deprecated.
"""
Array

"""
    isreal(x) -> Bool

Test whether `x` or all its elements are numerically equal to some real number.
"""
isreal

"""
    issubtype(type1, type2)

Return `true` if and only if all values of `type1` are also of `type2`. Can also be written
using the `<:` infix operator as `type1 <: type2`.
"""
issubtype(type1, type2)

"""
    finalizer(x, function)

Register a function `f(x)` to be called when there are no program-accessible references to
`x`. The behavior of this function is unpredictable if `x` is of a bits type.
"""
finalizer

"""
    csch(x)

Compute the hyperbolic cosecant of `x`.
"""
csch

"""
    sec(x)

Compute the secant of `x`, where `x` is in radians.
"""
sec

"""
    TypeError(func::Symbol, context::AbstractString, expected::Type, got)

A type assertion failure, or calling an intrinsic function with an incorrect argument type.
"""
TypeError

"""
    setfield!(value, name::Symbol, x)

Assign `x` to a named field in `value` of composite type. The syntax `a.b = c` calls
`setfield!(a, :b, c)`.
"""
setfield!

"""
    @printf([io::IOStream], "%Fmt", args...)

Print `args` using C `printf()` style format specification string.
Optionally, an [`IOStream`](:obj:`IOStream`)
may be passed as the first argument to redirect output.
"""
:@printf

"""
    countlines(io,[eol::Char])

Read `io` until the end of the stream/file and count the number of lines. To specify a file
pass the filename as the first argument. EOL markers other than '\\n' are supported by
passing them as the second argument.
"""
countlines

"""
    .\\(x, y)

Element-wise left division operator.
"""
Base.:(.\)(x,y)

"""
```
*(x, y...)
```

Multiplication operator. `x*y*z*...` calls this function with all arguments, i.e. `*(x, y, z, ...)`.
"""
Base.:(*)(x, y...)

"""
    time()

Get the system time in seconds since the epoch, with fairly high (typically, microsecond) resolution.
"""
time()

"""
    qr(A [,pivot=Val{false}][;thin=true]) -> Q, R, [p]

Compute the (pivoted) QR factorization of `A` such that either `A = Q*R` or `A[:,p] = Q*R`.
Also see `qrfact`. The default is to compute a thin factorization. Note that `R` is not
extended with zeros when the full `Q` is requested.
"""
qr

"""
    TextDisplay(stream)

Returns a `TextDisplay <: Display`, which can display any object as the text/plain MIME type
(only), writing the text representation to the given I/O stream. (The text representation is
the same as the way an object is printed in the Julia REPL.)
"""
TextDisplay

"""
    ismatch(r::Regex, s::AbstractString) -> Bool

Test whether a string contains a match of the given regular expression.
"""
ismatch

"""
    exp(x)

Compute ``e^x``.
"""
exp

"""
    matchall(r::Regex, s::AbstractString[, overlap::Bool=false]) -> Vector{AbstractString}

Return a vector of the matching substrings from [`eachmatch`](:func:`eachmatch`).
"""
matchall

"""
    get!(collection, key, default)

Return the value stored for the given key, or if no mapping for the key is present, store
`key => default`, and return `default`.
"""
get!(collection,key,default)

"""
    get!(f::Function, collection, key)

Return the value stored for the given key, or if no mapping for the key is present, store
`key => f()`, and return `f()`.

This is intended to be called using `do` block syntax:

    get!(dict, key) do
        # default value calculated here
        time()
    end
"""
get!(f::Function,collection,key)

"""
    @assert cond [text]

Throw an `AssertionError` if `cond` is `false`. Preferred syntax for writing assertions.
Message `text` is optionally displayed upon assertion failure.
"""
:@assert

"""
    deserialize(stream)

Read a value written by [`serialize`](:func:`serialize`). `deserialize` assumes the binary data read from
`stream` is correct and has been serialized by a compatible implementation of [`serialize`](:func:`serialize`).
It has been designed with simplicity and performance as a goal and does not validate
the data read. Malformed data can result in process termination. The caller has to ensure
the integrity and correctness of data read from `stream`.
"""
deserialize

"""
    first(coll)

Get the first element of an iterable collection. Returns the start point of a
[`Range`](:obj:`Range`) even if it is empty.
"""
first

"""
    median!(v)

Like [`median`](:func:`median`), but may overwrite the input vector.
"""
median!

"""
    cumprod!(B, A, [dim])

Cumulative product of `A` along a dimension, storing the result in `B`. The dimension defaults to 1.
"""
cumprod!

"""
    rethrow([e])

Throw an object without changing the current exception backtrace. The default argument is
the current exception (if called within a `catch` block).
"""
rethrow

"""
    reprmime(mime, x)

Returns an `AbstractString` or `Vector{UInt8}` containing the representation of `x` in the
requested `mime` type, as written by `show` (throwing a `MethodError` if no appropriate
`show` is available). An `AbstractString` is returned for MIME types with textual
representations (such as `"text/html"` or `"application/postscript"`), whereas binary data
is returned as `Vector{UInt8}`. (The function `istextmime(mime)` returns whether or not Julia
treats a given `mime` type as text.)

As a special case, if `x` is an `AbstractString` (for textual MIME types) or a
`Vector{UInt8}` (for binary MIME types), the `reprmime` function assumes that `x` is already
in the requested `mime` format and simply returns `x`.
"""
reprmime

"""
    @__FILE__ -> AbstractString

`@__FILE__` expands to a string with the absolute file path of the file containing the
macro. Returns `nothing` if run from a REPL or an empty string if evaluated by
`julia -e <expr>`. Alternatively see [`PROGRAM_FILE`](:data:`PROGRAM_FILE`).
"""
:@__FILE__

"""
    !(x)

Boolean not.
"""
Base.:(!)

"""
    length(collection) -> Integer

For ordered, indexable collections, the maximum index `i` for which `getindex(collection, i)`
is valid. For unordered collections, the number of elements.
"""
length(collection)

"""
    bkfact(A) -> BunchKaufman

Compute the Bunch-Kaufman [^Bunch1977] factorization of a real symmetric or complex Hermitian
matrix `A` and return a `BunchKaufman` object. The following functions are available for
`BunchKaufman` objects: [`size`](:func:`size`), `\\`, [`inv`](:func:`inv`), [`issymmetric`](:func:`issymmetric`), [`ishermitian`](:func:`ishermitian`).

[^Bunch1977]: J R Bunch and L Kaufman, Some stable methods for calculating inertia and solving symmetric linear systems, Mathematics of Computation 31:137 (1977), 163-179. [url](http://www.ams.org/journals/mcom/1977-31-137/S0025-5718-1977-0428694-0).

"""
bkfact

"""
    searchsortedlast(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

Returns the index of the last value in `a` less than or equal to `x`, according to the
specified order. Returns `0` if `x` is less than all values in `a`.
"""
searchsortedlast

"""
    InterruptException()

The process was stopped by a terminal interrupt (CTRL+C).
"""
InterruptException

"""
    den(x)

Denominator of the rational representation of `x`.
"""
den

"""
    issubnormal(f) -> Bool

Test whether a floating point number is subnormal.
"""
issubnormal

"""
    NullException()

An attempted access to a [`Nullable`](:obj:`Nullable`) with no defined value.
"""
NullException

"""
    .==(x, y)

Element-wise equality comparison operator.
"""
Base.:(.==)

"""
    cfunction(function::Function, ReturnType::Type, (ArgumentTypes...))

Generate C-callable function pointer from Julia function. Type annotation of the return
value in the callback function is a must for situations where Julia cannot infer the return
type automatically.

For example:

    function foo()
        # body

        retval::Float64
    end

    bar = cfunction(foo, Float64, ())
"""
cfunction

"""
    intersect(s1,s2...)
    ∩(s1,s2)

Construct the intersection of two or more sets.
Maintains order and multiplicity of the first argument for arrays and ranges.
"""
intersect

"""
    @spawn

Creates a closure around an expression and runs it on an automatically-chosen process,
returning a [`Future`](:obj:`Future`) to the result.
"""
:@spawn

"""
    promote_rule(type1, type2)

Specifies what type should be used by [`promote`](:func:`promote`) when given values of types `type1` and
`type2`. This function should not be called directly, but should have definitions added to
it for new types as appropriate.
"""
promote_rule

"""
    sumabs2(A, dims)

Sum squared absolute values of elements of an array over the given dimensions.
"""
sumabs2(A,dims)

"""
    showall(x)

Similar to [`show`](:func:`show`), except shows all elements of arrays.
"""
showall

"""
    mimewritable(mime, x)

Returns a boolean value indicating whether or not the object `x` can be written as the given
`mime` type. (By default, this is determined automatically by the existence of the
corresponding [`show`](:func:`show`) function for `typeof(x)`.)
"""
mimewritable

"""
    match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])

Search for the first match of the regular expression `r` in `s` and return a `RegexMatch`
object containing the match, or nothing if the match failed. The matching substring can be
retrieved by accessing `m.match` and the captured sequences can be retrieved by accessing
`m.captures` The optional `idx` argument specifies an index at which to start the search.
"""
match

"""
    qrfact!(A [,pivot=Val{false}])

`qrfact!` is the same as [`qrfact`](:func:`qrfact`) when `A` is a subtype of
`StridedMatrix`, but saves space by overwriting the input `A`, instead of creating a copy.
An `InexactError` exception is thrown if the factorisation produces a number not
representable by the element type of `A`, e.g. for integer types.
"""
qrfact!

"""
    coth(x)

Compute the hyperbolic cotangent of `x`.
"""
coth

"""
    start(iter) -> state

Get initial iteration state for an iterable object.
"""
start

"""
    readavailable(stream)

Read all available data on the stream, blocking the task only if no data is available. The
result is a `Vector{UInt8,1}`.
"""
readavailable

"""
    isa(x, type) -> Bool

Determine whether `x` is of the given `type`.
"""
isa

"""
    unsafe_load(p::Ptr{T}, [i::Integer=1])

Load a value of type `T` from the address of the ith element (1-indexed) starting at `p`.
This is equivalent to the C expression `p[i-1]`.

The `unsafe` prefix on this function indicates that no validation is performed on the
pointer `p` to ensure that it is valid. Incorrect usage may segfault your program or return
garbage answers, in the same manner as C.
"""
unsafe_load

"""
    catch_backtrace()

Get the backtrace of the current exception, for use within `catch` blocks.
"""
catch_backtrace

"""
    cos(x)

Compute cosine of `x`, where `x` is in radians.
"""
cos

"""
    maxabs(A, dims)

Compute the maximum absolute values over given dimensions.
"""
maxabs(A,dims)

"""
    done(iter, state) -> Bool

Test whether we are done iterating.
"""
done

"""
    convert(T, x)

Convert `x` to a value of type `T`.

If `T` is an `Integer` type, an [`InexactError`](:exc:`InexactError`) will be raised if `x`
is not representable by `T`, for example if `x` is not integer-valued, or is outside the
range supported by `T`.

```jldoctest
julia> convert(Int, 3.0)
3

julia> convert(Int, 3.5)
ERROR: InexactError()
 in convert(::Type{Int64}, ::Float64) at ./int.jl:330
 ...
```

If `T` is a [`AbstractFloat`](:obj:`AbstractFloat`) or [`Rational`](:obj:`Rational`) type,
then it will return the closest value to `x` representable by `T`.

```jldoctest
julia> x = 1/3
0.3333333333333333

julia> convert(Float32, x)
0.33333334f0

julia> convert(Rational{Int32}, x)
1//3

julia> convert(Rational{Int64}, x)
6004799503160661//18014398509481984
```

If `T` is a collection type and `x` a collection, the result of `convert(T, x)` may alias
`x`.
```jldoctest
julia> x = Int[1,2,3];

julia> y = convert(Vector{Int}, x);

julia> y === x
true
```
Similarly, if `T` is a composite type and `x` a related instance, the result of
`convert(T, x)` may alias part or all of `x`.
```jldoctest
julia> x = speye(5);

julia> typeof(x)
SparseMatrixCSC{Float64,Int64}

julia> y = convert(SparseMatrixCSC{Float64,Int64}, x);

julia> z = convert(SparseMatrixCSC{Float32,Int64}, y);

julia> y === x
true

julia> z === x
false

julia> z.colptr === x.colptr
true
```
"""
convert

"""
    applicable(f, args...) -> Bool

Determine whether the given generic function has a method applicable to the given arguments.

```jldoctest
julia> function f(x, y)
           x + y
       end;

julia> applicable(f, 1)
false

julia> applicable(f, 1, 2)
true
```
"""
applicable

"""
    fma(x, y, z)

Computes `x*y+z` without rounding the intermediate result `x*y`. On some systems this is
significantly more expensive than `x*y+z`. `fma` is used to improve accuracy in certain
algorithms. See [`muladd`](:func:`muladd`).
"""
fma

"""

    eigvals(A,[irange,][vl,][vu]) -> values

Returns the eigenvalues of `A`. If `A` is `Symmetric`, `Hermitian` or `SymTridiagonal`,
it is possible to calculate only a subset of the eigenvalues by specifying either a
`UnitRange` `irange` covering indices of the sorted eigenvalues, or a pair `vl` and `vu`
for the lower and upper boundaries of the eigenvalues.

For general non-symmetric matrices it is possible to specify how the matrix is balanced
before the eigenvector calculation. The option `permute=true` permutes the matrix to
become closer to upper triangular, and `scale=true` scales the matrix by its diagonal
elements to make rows and columns moreequal in norm. The default is `true` for both
options.
"""
eigvals

"""
    pointer_from_objref(object_instance)

Get the memory address of a Julia object as a `Ptr`. The existence of the resulting `Ptr`
will not protect the object from garbage collection, so you must ensure that the object
remains referenced for the whole time that the `Ptr` will be used.
"""
pointer_from_objref

"""
    copy!(dest, src)

Copy all elements from collection `src` to array `dest`. Returns `dest`.
"""
copy!(dest,src)

"""
    copy!(dest, do, src, so, N)

Copy `N` elements from collection `src` starting at offset `so`, to array `dest` starting at
offset `do`. Returns `dest`.
"""
copy!(dest,d,src,so,N)

"""
    qrfact(A) -> SPQR.Factorization

Compute the QR factorization of a sparse matrix `A`. A fill-reducing permutation is used.
The main application of this type is to solve least squares problems with `\\`. The function
calls the C library SPQR and a few additional functions from the library are wrapped but not
exported.
"""
qrfact(A)

"""
    +(x, y...)

Addition operator. `x+y+z+...` calls this function with all arguments, i.e. `+(x, y, z, ...)`.
"""
+

"""
    setindex!(A, X, inds...)

Store values from array `X` within some subset of `A` as specified by `inds`.
"""
setindex!(A::AbstractArray,X,inds...)

"""
    setindex!(collection, value, key...)

Store the given value at the given key or index within a collection. The syntax `a[i,j,...] =
x` is converted by the compiler to `(setindex!(a, x, i, j, ...); x)`.
"""
setindex!(collection,value,key...)

"""
    signif(x, digits, [base])

Rounds (in the sense of [`round`](:func:`round`)) `x` so that there are `digits` significant digits, under a
base `base` representation, default 10. E.g., `signif(123.456, 2)` is `120.0`, and
`signif(357.913, 4, 2)` is `352.0`.
"""
signif

"""
    full(F)

Reconstruct the matrix `A` from the factorization `F=factorize(A)`.
"""
full(F)

"""
    full(QRCompactWYQ[, thin=true]) -> Matrix

Converts an orthogonal or unitary matrix stored as a `QRCompactWYQ` object, i.e. in the
compact WY format [^Bischof1987], to a dense matrix.

Optionally takes a `thin` Boolean argument, which if `true` omits the columns that span the
rows of `R` in the QR factorization that are zero. The resulting matrix is the `Q` in a thin
QR factorization (sometimes called the reduced QR factorization). If `false`, returns a `Q`
that spans all rows of `R` in its corresponding QR factorization.
"""
full(::LinAlg.QRCompactWYQ, ?)

"""
    throw(e)

Throw an object as an exception.
"""
throw

"""
    issubset(a, b)
    ⊆(a,b) -> Bool
    ⊈(a,b) -> Bool
    ⊊(a,b) -> Bool

Determine whether every element of `a` is also in `b`, using [`in`](:func:`in`).
"""
issubset(a,b)

"""
    issubset(A, S) -> Bool
    ⊆(A,S) -> Bool

Return `true` if `A` is a subset of or equal to `S`.
"""
issubset

"""
    print_with_color(color::Symbol, [io], strings...)

Print strings in a color specified as a symbol.

`color` may take any of the values $(Base.available_text_colors_docstring).
"""
print_with_color

"""
    stringmime(mime, x)

Returns an `AbstractString` containing the representation of `x` in the requested `mime`
type. This is similar to [`reprmime`](:func:`reprmime`) except that binary data is base64-encoded as an ASCII string.
"""
stringmime

"""
    zero(x)

Get the additive identity element for the type of `x` (`x` can also specify the type itself).
"""
zero

"""
    any(A, dims)

Test whether any values along the given dimensions of an array are `true`.
"""
any(::AbstractArray,dims)

"""
    zeros(type, dims)

Create an array of all zeros of specified type.
The type defaults to Float64 if not specified.
"""
zeros(t,dims)

"""
    zeros(A)

Create an array of all zeros with the same element type and shape as `A`.
"""
zeros(A)

"""
    Symbol(x...) -> Symbol

Create a `Symbol` by concatenating the string representations of the arguments together.
"""
Symbol

"""
    zeta(s)

Riemann zeta function ``\\zeta(s)``.
"""
zeta(s)

"""
    isvalid(value) -> Bool

Returns `true` if the given value is valid for its type, which currently can be either
`Char` or `String`.
"""
isvalid(value)

"""
    isvalid(T, value) -> Bool

Returns `true` if the given value is valid for that type. Types currently can
be either `Char` or `String`. Values for `Char` can be of type `Char` or `UInt32`.
Values for `String` can be of that type, or `Vector{UInt8}`.
"""
isvalid(T,value)

"""
    unsigned(x) -> Unsigned

Convert a number to an unsigned integer. If the argument is signed, it is reinterpreted as
unsigned without checking for negative values.
"""
unsigned

"""
    midpoints(e)

Compute the midpoints of the bins with edges `e`. The result is a vector/range of length
`length(e) - 1`. Note: Julia does not ignore `NaN` values in the computation.
"""
midpoints

"""
    .+(x, y)

Element-wise addition operator.
"""
Base.:(.+)

"""
    reverseind(v, i)

Given an index `i` in `reverse(v)`, return the corresponding index in `v` so that
`v[reverseind(v,i)] == reverse(v)[i]`. (This can be nontrivial in the case where `v` is a
Unicode string.)
"""
reverseind

"""
    float(x)

Convert a number, array, or string to a `AbstractFloat` data type. For numeric data, the
smallest suitable `AbstractFloat` type is used. Converts strings to `Float64`.
"""
float

"""
    signbit(x)

Returns `true` if the value of the sign of `x` is negative, otherwise `false`.
"""
signbit

"""
    cscd(x)

Compute the cosecant of `x`, where `x` is in degrees.
"""
cscd

"""
    tryparse(type, str, [base])

Like [`parse`](:func:`parse`), but returns a [`Nullable`](:obj:`Nullable`) of the requested type. The result will be null if the
string does not contain a valid number.
"""
tryparse

"""
    all!(r, A)

Test whether all values in `A` along the singleton dimensions of `r` are `true`, and write results to `r`.
"""
all!

"""
    exit([code])

Quit (or control-D at the prompt). The default exit code is zero, indicating that the
processes completed successfully.
"""
exit

"""
    istextmime(m::MIME)

Determine whether a MIME type is text data.
"""
istextmime

"""
    skipchars(stream, predicate; linecomment::Char)

Advance the stream until before the first character for which `predicate` returns `false`.
For example `skipchars(stream, isspace)` will skip all whitespace. If keyword argument
`linecomment` is specified, characters from that character through the end of a line will
also be skipped.
"""
skipchars

"""
    realmin(T)

The smallest in absolute value non-subnormal value representable by the given floating-point DataType `T`.
"""
realmin

"""
    union!(s, iterable)

Union each element of `iterable` into set `s` in-place.
"""
union!

"""
    deepcopy(x)

Create a deep copy of `x`: everything is copied recursively, resulting in a fully
independent object. For example, deep-copying an array produces a new array whose elements
are deep copies of the original elements. Calling `deepcopy` on an object should generally
have the same effect as serializing and then deserializing it.

As a special case, functions can only be actually deep-copied if they are anonymous,
otherwise they are just copied. The difference is only relevant in the case of closures,
i.e. functions which may contain hidden internal references.

While it isn't normally necessary, user-defined types can override the default `deepcopy`
behavior by defining a specialized version of the function `deepcopy_internal(x::T, dict::ObjectIdDict)`
(which shouldn't otherwise be used), where `T` is the type to be specialized for, and `dict`
keeps track of objects copied so far within the recursion. Within the definition,
`deepcopy_internal` should be used in place of `deepcopy`, and the `dict` variable should be
updated as appropriate before returning.
"""
deepcopy

"""
    widen(x)

If `x` is a type, return a "larger" type (for numeric types, this will be
a type with at least as much range and precision as the argument, and usually more).
Otherwise `x` is converted to `widen(typeof(x))`.

```jldoctest
julia> widen(Int32)
Int64

julia> widen(1.5f0)
1.5
```
"""
widen

"""
    Set([itr])

Construct a [`Set`](:obj:`Set`) of the values generated by the given iterable object, or an
empty set. Should be used instead of [`IntSet`](:obj:`IntSet`) for sparse integer sets, or
for sets of arbitrary objects.
"""
Set

"""
    erf(x)

Compute the error function of `x`, defined by ``\\frac{2}{\\sqrt{\\pi}} \\int_0^x e^{-t^2} dt``
for arbitrary complex `x`.
"""
erf

"""
    signed(x)

Convert a number to a signed integer. If the argument is unsigned, it is reinterpreted as
signed without checking for overflow.
"""
signed

"""
    Val{c}

Create a "value type" out of `c`, which must be an `isbits` value. The intent of this
construct is to be able to dispatch on constants, e.g., `f(Val{false})` allows you to
dispatch directly (at compile-time) to an implementation `f(::Type{Val{false}})`, without
having to test the boolean value at runtime.
"""
Val

"""
    |(x, y)

Bitwise or.
"""
Base.:(|)

"""
    pop!(collection, key[, default])

Delete and return the mapping for `key` if it exists in `collection`, otherwise return
`default`, or throw an error if default is not specified.
"""
pop!(collection,key,?)

"""
    pop!(collection) -> item

Remove the last item in `collection` and return it.

```jldoctest
julia> A=[1, 2, 3, 4, 5, 6]
6-element Array{Int64,1}:
 1
 2
 3
 4
 5
 6

julia> pop!(A)
6

julia> A
5-element Array{Int64,1}:
 1
 2
 3
 4
 5
```
"""
pop!(collection)

"""
    seekend(s)

Seek a stream to its end.
"""
seekend

"""
    DivideError()

Integer division was attempted with a denominator value of 0.
"""
DivideError

"""
    unsafe_pointer_to_objref(p::Ptr)

Convert a `Ptr` to an object reference. Assumes the pointer refers to a valid heap-allocated
Julia object. If this is not the case, undefined behavior results, hence this function is
considered "unsafe" and should be used with care.
"""
unsafe_pointer_to_objref

"""
    dawson(x)

Compute the Dawson function (scaled imaginary error function) of `x`, defined by
``\\frac{\\sqrt{\\pi}}{2} e^{-x^2} \\operatorname{erfi}(x)``.
"""
dawson

"""
    \$(x, y)

Bitwise exclusive or.
"""
Base.:$(x, y)

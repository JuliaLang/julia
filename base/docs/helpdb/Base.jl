# This file is a part of Julia. License is MIT: https://julialang.org/license

# Base

"""
    fill!(A, x)

Fill array `A` with the value `x`. If `x` is an object reference, all elements will refer to
the same object. `fill!(A, Foo())` will return `A` filled with the result of evaluating
`Foo()` once.

# Examples
```jldoctest
julia> A = zeros(2,3)
2×3 Array{Float64,2}:
 0.0  0.0  0.0
 0.0  0.0  0.0

julia> fill!(A, 2.)
2×3 Array{Float64,2}:
 2.0  2.0  2.0
 2.0  2.0  2.0

julia> a = [1, 1, 1]; A = fill!(Vector{Vector{Int}}(3), a); a[1] = 2; A
3-element Array{Array{Int64,1},1}:
 [2, 1, 1]
 [2, 1, 1]
 [2, 1, 1]

julia> x = 0; f() = (global x += 1; x); fill!(Vector{Int}(3), f())
3-element Array{Int64,1}:
 1
 1
 1
```
"""
fill!

"""
    read!(stream::IO, array::Union{Array, BitArray})
    read!(filename::AbstractString, array::Union{Array, BitArray})

Read binary data from an I/O stream or file, filling in `array`.
"""
read!

"""
    pointer(array [, index])

Get the native address of an array or string element. Be careful to ensure that a Julia
reference to `a` exists as long as this pointer will be used. This function is "unsafe" like
`unsafe_convert`.

Calling `Ref(array[, index])` is generally preferable to this function.
"""
pointer

"""
    precision(num::AbstractFloat)

Get the precision of a floating point number, as defined by the effective number of bits in
the mantissa.
"""
precision

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
    bits(n)

A string giving the literal bit representation of a number.

# Examples
```jldoctest
julia> bits(4)
"0000000000000000000000000000000000000000000000000000000000000100"

julia> bits(2.2)
"0100000000000001100110011001100110011001100110011001100110011010"
```
"""
bits

"""
    getindex(collection, key...)

Retrieve the value(s) stored at the given key or index within a collection. The syntax
`a[i,j,...]` is converted by the compiler to `getindex(a, i, j, ...)`.

# Examples
```jldoctest
julia> A = Dict("a" => 1, "b" => 2)
Dict{String,Int64} with 2 entries:
  "b" => 2
  "a" => 1

julia> getindex(A, "a")
1
```
"""
getindex(collection, key...)

"""
    cconvert(T,x)

Convert `x` to a value to be passed to C code as type `T`, typically by calling `convert(T, x)`.

In cases where `x` cannot be safely converted to `T`, unlike [`convert`](@ref), `cconvert` may
return an object of a type different from `T`, which however is suitable for
[`unsafe_convert`](@ref) to handle. The result of this function should be kept valid (for the GC)
until the result of [`unsafe_convert`](@ref) is not needed anymore.
This can be used to allocate memory that will be accessed by the `ccall`.
If multiple objects need to be allocated, a tuple of the objects can be used as return value.

Neither `convert` nor `cconvert` should take a Julia object and turn it into a `Ptr`.
"""
cconvert

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
    Float32(x [, mode::RoundingMode])

Create a Float32 from `x`. If `x` is not exactly representable then `mode` determines how
`x` is rounded.

# Examples
```jldoctest
julia> Float32(1/3, RoundDown)
0.3333333f0

julia> Float32(1/3, RoundUp)
0.33333334f0
```

See [`RoundingMode`](@ref) for available rounding modes.
"""
Float32(x)

"""
    Mmap.mmap(io::Union{IOStream,AbstractString,Mmap.AnonymousMmap}[, type::Type{Array{T,N}}, dims, offset]; grow::Bool=true, shared::Bool=true)
           Mmap.mmap(type::Type{Array{T,N}}, dims)

Create an `Array` whose values are linked to a file, using memory-mapping. This provides a
convenient way of working with data too large to fit in the computer's memory.

The type is an `Array{T,N}` with a bits-type element of `T` and dimension `N` that
determines how the bytes of the array are interpreted. Note that the file must be stored in
binary format, and no format conversions are possible (this is a limitation of operating
systems, not Julia).

`dims` is a tuple or single [`Integer`](@ref) specifying the size or length of the array.

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
purpose, works in the same way, and has the same arguments, as [`mmap`](@ref Mmap.mmap), but
the byte representation is different.

**Example**: `B = Mmap.mmap(s, BitArray, (25,30000))`

This would create a 25-by-30000 `BitArray`, linked to the file associated with stream `s`.
"""
Mmap.mmap(io, ::BitArray, dims, offset)

"""
    sizeof(T)

Size, in bytes, of the canonical binary representation of the given DataType `T`, if any.

# Examples
```jldoctest
julia> sizeof(Float32)
4

julia> sizeof(Complex128)
16
```

If `T` does not have a specific size, an error is thrown.

```jldoctest
julia> sizeof(Base.LinAlg.LU)
ERROR: argument is an abstract type; size is indeterminate
Stacktrace:
 [1] sizeof(::Type{T} where T) at ./essentials.jl:150
```
"""
sizeof(::Type)

"""
    ReadOnlyMemoryError()

An operation tried to write to memory that is read-only.
"""
ReadOnlyMemoryError

"""
    ceil([T,] x, [digits, [base]])

`ceil(x)` returns the nearest integral value of the same type as `x` that is greater than or
equal to `x`.

`ceil(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is not
representable.

`digits` and `base` work as for [`round`](@ref).
"""
ceil

"""
    oftype(x, y)

Convert `y` to the type of `x` (`convert(typeof(x), y)`).
"""
oftype

"""
    push!(collection, items...) -> collection

Insert one or more `items` at the end of `collection`.

# Examples
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

Use [`append!`](@ref) to add all the elements of another collection to
`collection`. The result of the preceding example is equivalent to `append!([1, 2, 3], [4,
5, 6])`.
"""
push!

"""
    promote(xs...)

Convert all arguments to their common promotion type (if any), and return them all (as a tuple).

# Examples
```jldoctest
julia> promote(Int8(1), Float16(4.5), Float32(4.1))
(1.0f0, 4.5f0, 4.1f0)
```
"""
promote

"""
    fd(stream)

Returns the file descriptor backing the stream or file. Note that this function only applies
to synchronous `File`'s and `IOStream`'s not to any of the asynchronous streams.
"""
fd

"""
    ones([A::AbstractArray,] [T=eltype(A)::Type,] [dims=size(A)::Tuple])

Create an array of all ones with the same layout as `A`, element type `T` and size `dims`.
The `A` argument can be skipped, which behaves like `Array{Float64,0}()` was passed.
For convenience `dims` may also be passed in variadic form.

# Examples
```jldoctest
julia> ones(Complex128, 2, 3)
2×3 Array{Complex{Float64},2}:
 1.0+0.0im  1.0+0.0im  1.0+0.0im
 1.0+0.0im  1.0+0.0im  1.0+0.0im

julia> ones(1,2)
1×2 Array{Float64,2}:
 1.0  1.0

julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> ones(A)
2×2 Array{Int64,2}:
 1  1
 1  1

julia> ones(A, Float64)
2×2 Array{Float64,2}:
 1.0  1.0
 1.0  1.0

julia> ones(A, Bool, (3,))
3-element Array{Bool,1}:
 true
 true
 true
```
See also [`zeros`](@ref), [`similar`](@ref).
"""
ones

"""
    randsubseq!(S, A, p)

Like [`randsubseq`](@ref), but the results are stored in `S`
(which is resized as needed).
"""
randsubseq!

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
    tuple(xs...)

Construct a tuple of the given objects.

# Examples
```jldoctest
julia> tuple(1, 'a', pi)
(1, 'a', π = 3.1415926535897...)
```
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
    num2hex(f)

Get a hexadecimal string of the binary representation of a floating point number.

# Examples
```jldoctest
julia> num2hex(2.2)
"400199999999999a"
```
"""
num2hex

"""
    truncate(file,n)

Resize the file or buffer given by the first argument to exactly `n` bytes, filling
previously unallocated space with '\\0' if the file or buffer is grown.
"""
truncate

"""
    exp10(x)

Compute ``10^x``.

# Examples
```jldoctest
julia> exp10(2)
100.0

julia> exp10(0.2)
1.5848931924611136
```
"""
exp10

"""
    select(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

Variant of [`select!`](@ref) which copies `v` before partially sorting it, thereby returning the
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
    Mmap.Anonymous(name, readonly, create)

Create an `IO`-like object for creating zeroed-out mmapped-memory that is not tied to a file
for use in `Mmap.mmap`. Used by `SharedArray` for creating shared memory arrays.
"""
Mmap.Anonymous

"""
    floor([T,] x, [digits, [base]])

`floor(x)` returns the nearest integral value of the same type as `x` that is less than or
equal to `x`.

`floor(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is
not representable.

`digits` and `base` work as for [`round`](@ref).
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

# Examples
```jldoctest
julia> A = collect(1:5)
5-element Array{Int64,1}:
 1
 2
 3
 4
 5

julia> reverse(A)
5-element Array{Int64,1}:
 5
 4
 3
 2
 1

julia> reverse(A, 1, 4)
5-element Array{Int64,1}:
 4
 3
 2
 1
 5

julia> reverse(A, 3, 5)
5-element Array{Int64,1}:
 1
 2
 5
 4
 3
```
"""
reverse

"""
    reverse!(v [, start=1 [, stop=length(v) ]]) -> v

In-place version of [`reverse`](@ref).
"""
reverse!

"""
    UndefRefError()

The item or field is not defined for the given object.
"""
UndefRefError

"""
    append!(collection, collection2) -> collection.

Add the elements of `collection2` to the end of `collection`.

# Examples
```jldoctest
julia> append!([1],[2,3])
3-element Array{Int64,1}:
 1
 2
 3

julia> append!([1, 2, 3], [4, 5, 6])
6-element Array{Int64,1}:
 1
 2
 3
 4
 5
 6
```

Use [`push!`](@ref) to add individual items to `collection` which are not already
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
    copysign(x, y) -> z

Return `z` which has the magnitude of `x` and the same sign as `y`.

# Examples
```jldoctest
julia> copysign(1, -2)
-1

julia> copysign(-1, 2)
1
```
"""
copysign

"""
    getfield(value, name::Symbol)

Extract a named field from a `value` of composite type. The syntax `a.b` calls
`getfield(a, :b)`.

# Examples
```jldoctest
julia> a = 1//2
1//2

julia> getfield(a, :num)
1
```
"""
getfield

"""
    select!(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

Partially sort the vector `v` in place, according to the order specified by `by`, `lt` and
`rev` so that the value at index `k` (or range of adjacent values if `k` is a range) occurs
at the position where it would appear if the array were fully sorted via a non-stable
algorithm. If `k` is a single index, that value is returned; if `k` is a range, an array of
values at those indices is returned. Note that `select!` does not fully sort the input
array.

# Examples
```jldoctest
julia> a = [1, 2, 4, 3, 4]
5-element Array{Int64,1}:
 1
 2
 4
 3
 4

julia> select!(a, 4)
4

julia> a
5-element Array{Int64,1}:
 1
 2
 3
 4
 4

julia> a = [1, 2, 4, 3, 4]
5-element Array{Int64,1}:
 1
 2
 4
 3
 4

julia> select!(a, 4, rev=true)
2

julia> a
5-element Array{Int64,1}:
 4
 4
 3
 2
 1
```
"""
select!

"""
    Float64(x [, mode::RoundingMode])

Create a Float64 from `x`. If `x` is not exactly representable then `mode` determines how
`x` is rounded.

# Examples
```jldoctest
julia> Float64(pi, RoundDown)
3.141592653589793

julia> Float64(pi, RoundUp)
3.1415926535897936
```

See [`RoundingMode`](@ref) for available rounding modes.
"""
Float64(x)

"""
    union(s1,s2...)
    ∪(s1,s2...)

Construct the union of two or more sets. Maintains order with arrays.

# Examples
```jldoctest
julia> union([1, 2], [3, 4])
4-element Array{Int64,1}:
 1
 2
 3
 4

julia> union([1, 2], [2, 4])
3-element Array{Int64,1}:
 1
 2
 4

julia> union([4, 2], [1, 2])
3-element Array{Int64,1}:
 4
 2
 1
```
"""
union

"""
    realmax(T)

The highest finite value representable by the given floating-point DataType `T`.

# Examples
```jldoctest
julia> realmax(Float16)
Float16(6.55e4)

julia> realmax(Float32)
3.4028235f38
```
"""
realmax

"""
    serialize(stream, value)

Write an arbitrary value to a stream in an opaque format, such that it can be read back by
[`deserialize`](@ref). The read-back value will be as identical as possible to the original. In
general, this process will not work if the reading and writing are done by different
versions of Julia, or an instance of Julia with a different system image. `Ptr` values are
serialized as all-zero bit patterns (`NULL`).
"""
serialize

"""
    typemin(T)

The lowest value representable by the given (real) numeric DataType `T`.

# Examples
```jldoctest
julia> typemin(Float16)
-Inf16

julia> typemin(Float32)
-Inf32
```
"""
typemin

"""
    typeof(x)

Get the concrete type of `x`.
"""
typeof

"""
    trunc([T,] x, [digits, [base]])

`trunc(x)` returns the nearest integral value of the same type as `x` whose absolute value
is less than or equal to `x`.

`trunc(T, x)` converts the result to type `T`, throwing an `InexactError` if the value is
not representable.

`digits` and `base` work as for [`round`](@ref).
"""
trunc

"""
    unsafe_convert(T,x)

Convert `x` to a C argument of type `T`
where the input `x` must be the return value of `cconvert(T, ...)`.

In cases where [`convert`](@ref) would need to take a Julia object
and turn it into a `Ptr`, this function should be used to define and perform
that conversion.

Be careful to ensure that a Julia reference to `x` exists as long as the result of this
function will be used. Accordingly, the argument `x` to this function should never be an
expression, only a variable name or field reference. For example, `x=a.b.c` is acceptable,
but `x=[a,b,c]` is not.

The `unsafe` prefix on this function indicates that using the result of this function after
the `x` argument to this function is no longer accessible to the program may cause undefined
behavior, including program corruption or segfaults, at any later time.

See also [`cconvert`](@ref)
"""
unsafe_convert

"""
    seek(s, pos)

Seek a stream to the given position.
"""
seek

"""
    cglobal((symbol, library) [, type=Void])

Obtain a pointer to a global variable in a C-exported shared library, specified exactly as
in [`ccall`](@ref).
Returns a `Ptr{Type}`, defaulting to `Ptr{Void}` if no `Type` argument is
supplied.
The values can be read or written by [`unsafe_load`](@ref) or [`unsafe_store!`](@ref),
respectively.
"""
cglobal

"""
    endof(collection) -> Integer

Returns the last index of the collection.

# Examples
```jldoctest
julia> endof([1,2,4])
3
```
"""
endof

"""
    next(iter, state) -> item, state

For a given iterable object and iteration state, return the current item and the next iteration state.

# Examples
```jldoctest
julia> next(1:5, 3)
(3, 4)

julia> next(1:5, 5)
(5, 6)
```
"""
next

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

An indexing operation into an array, `a`, tried to access an out-of-bounds element at index `i`.

# Examples
```jldoctest
julia> A = ones(7);

julia> A[8]
ERROR: BoundsError: attempt to access 7-element Array{Float64,1} at index [8]
Stacktrace:
 [1] getindex(::Array{Float64,1}, ::Int64) at ./array.jl:586

julia> B = ones(2, 3);

julia> B[2, 4]
ERROR: BoundsError: attempt to access 2×3 Array{Float64,2} at index [2, 4]
Stacktrace:
 [1] getindex(::Array{Float64,2}, ::Int64, ::Int64) at ./array.jl:587

julia> B[9]
ERROR: BoundsError: attempt to access 2×3 Array{Float64,2} at index [9]
Stacktrace:
 [1] getindex(::Array{Float64,2}, ::Int64) at ./array.jl:586
```
"""
BoundsError

"""
    invoke(f, types <: Tuple, args...)

Invoke a method for the given generic function matching the specified types, on
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

```jldoctest
julia> parse("x = 3, y = 5", 7)
(:(y = 5), 13)

julia> parse("x = 3, y = 5", 5)
(:((3, y) = 5), 13)
```
"""
parse(str, start)

"""
    parse(str; raise=true)

Parse the expression string greedily, returning a single expression. An error is thrown if
there are additional characters after the first expression. If `raise` is `true` (default),
syntax errors will raise an error; otherwise, `parse` will return an expression that will
raise an error upon evaluation.

```jldoctest
julia> parse("x = 3")
:(x = 3)

julia> parse("x = ")
:($(Expr(:incomplete, "incomplete: premature end of input")))

julia> parse("1.0.2")
ERROR: ParseError("invalid numeric constant \\\"1.0.\\\"")
Stacktrace:
  [...]

julia> parse("1.0.2"; raise = false)
:($(Expr(:error, "invalid numeric constant \"1.0.\"")))
```
"""
parse(str)

"""
    parse(type, str, [base])

Parse a string as a number. If the type is an integer type, then a base can be specified
(the default is 10). If the type is a floating point type, the string is parsed as a decimal
floating point number. If the string does not contain a valid number, an error is raised.

```jldoctest
julia> parse(Int, "1234")
1234

julia> parse(Int, "1234", 5)
194

julia> parse(Int, "afc", 16)
2812

julia> parse(Float64, "1.2e-3")
0.0012
```
"""
parse(T::Type, str, base=Int)

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
from [`select`](@ref) in that it returns a vector of `k` elements instead of just the `k` th
element. Also note that this is equivalent to, but more efficient than, calling
`sortperm(...)[k]`.
"""
selectperm

"""
    reinterpret(type, A)

Change the type-interpretation of a block of memory.
For arrays, this constructs an array with the same binary data as the given
array, but with the specified element type.
For example,
`reinterpret(Float32, UInt32(7))` interprets the 4 bytes corresponding to `UInt32(7)` as a
[`Float32`](@ref).

!!! warning

    It is not allowed to `reinterpret` an array to an element type with a larger alignment then
    the alignment of the array. For a normal `Array`, this is the alignment of its element type.
    For a reinterpreted array, this is the alignment of the `Array` it was reinterpreted from.
    For example, `reinterpret(UInt32, UInt8[0, 0, 0, 0])` is not allowed but
    `reinterpret(UInt32, reinterpret(UInt8, Float32[1.0]))` is allowed.

# Examples
```jldoctest
julia> reinterpret(Float32, UInt32(7))
1.0f-44

julia> reinterpret(Float32, UInt32[1 2 3 4 5])
1×5 Array{Float32,2}:
 1.4013f-45  2.8026f-45  4.2039f-45  5.60519f-45  7.00649f-45
```
"""
reinterpret

"""
    bswap(n)

Byte-swap an integer. Flip the bits of its binary representation.

# Examples
```jldoctest
julia> a = bswap(4)
288230376151711744

julia> bswap(a)
4

julia> bin(1)
"1"

julia> bin(bswap(1))
"100000000000000000000000000000000000000000000000000000000"
```
"""
bswap

"""
    delete!(collection, key)

Delete the mapping for the given key in a collection, and return the collection.

# Examples
```jldoctest
julia> d = Dict("a"=>1, "b"=>2)
Dict{String,Int64} with 2 entries:
  "b" => 2
  "a" => 1

julia> delete!(d, "b")
Dict{String,Int64} with 1 entry:
  "a" => 1
```
"""
delete!

"""
    big(x)

Convert a number to a maximum precision representation (typically [`BigInt`](@ref) or
`BigFloat`). See [`BigFloat`](@ref) for information about some pitfalls with floating-point numbers.
"""
big

"""
    typejoin(T, S)

Compute a type that contains both `T` and `S`.
"""
typejoin

"""
    selectperm!(ix, v, k, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false,] [initialized=false])

Like [`selectperm`](@ref), but accepts a preallocated index vector `ix`. If `initialized` is `false`
(the default), ix is initialized to contain the values `1:length(ix)`.
"""
selectperm!

"""
    precompile(f,args::Tuple{Vararg{Any}})

Compile the given function `f` for the argument tuple (of types) `args`, but do not execute it.
"""
precompile

"""
    get(collection, key, default)

Return the value stored for the given key, or the given default value if no mapping for the
key is present.

# Examples
```jldoctest
julia> d = Dict("a"=>1, "b"=>2);

julia> get(d, "a", 3)
1

julia> get(d, "c", 3)
3
```
"""
get(collection,key,default)

"""
    get(f::Function, collection, key)

Return the value stored for the given key, or if no mapping for the key is present, return
`f()`.  Use [`get!`](@ref) to also store the default value in the dictionary.

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
    Mmap.sync!(array)

Forces synchronization between the in-memory version of a memory-mapped `Array` or
`BitArray` and the on-disk version.
"""
Mmap.sync!

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
    read(stream::IO, T)

Read a single value of type `T` from `stream`, in canonical binary representation.

    read(stream::IO, String)

Read the entirety of `stream`, as a String.
"""
read(stream, t)

"""
    shift!(collection) -> item

Remove the first `item` from `collection`.

# Examples
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

Run a command object asynchronously, returning the resulting `Process` object.
"""
spawn

"""
    isdefined(m::Module, s::Symbol)
    isdefined(object, s::Symbol)
    isdefined(object, index::Int)

Tests whether an assignable location is defined. The arguments can be a module and a symbol
or a composite object and field name (as a symbol) or index.
"""
isdefined

"""
    wait([x])

Block the current task until some event occurs, depending on the type of the argument:

* [`RemoteChannel`](@ref) : Wait for a value to become available on the specified remote
  channel.
* [`Future`](@ref) : Wait for a value to become available for the specified future.
* [`Channel`](@ref): Wait for a value to be appended to the channel.
* [`Condition`](@ref): Wait for [`notify`](@ref) on a condition.
* `Process`: Wait for a process or process chain to exit. The `exitcode` field of a process
  can be used to determine success or failure.
* [`Task`](@ref): Wait for a `Task` to finish, returning its result value. If the task fails
  with an exception, the exception is propagated (re-thrown in the task that called `wait`).
* `RawFD`: Wait for changes on a file descriptor (see [`poll_fd`](@ref) for keyword
  arguments and return code)

If no argument is passed, the task blocks for an undefined period. A task can only be
restarted by an explicit call to [`schedule`](@ref) or [`yieldto`](@ref).

Often `wait` is called within a `while` loop to ensure a waited-for condition is met before
proceeding.
"""
wait

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

# Examples
```jldoctest
julia> isempty([])
true

julia> isempty([1 2 3])
false
```
"""
isempty

"""
    hex2num(str)

Convert a hexadecimal string to the floating point number it represents.
"""
hex2num

"""
    InexactError(name::Symbol, T, val)

Cannot exactly convert `val` to type `T` in a method of function `name`.

# Examples
```jldoctest
julia> convert(Float64, 1+2im)
ERROR: InexactError: convert(Float64, 1 + 2im)
Stacktrace:
 [1] convert(::Type{Float64}, ::Complex{Int64}) at ./complex.jl:37
```
"""
InexactError

"""
    typemax(T)

The highest value representable by the given (real) numeric `DataType`.
"""
typemax

"""
    DomainError(val)
    DomainError(val, msg)

The argument `val` to a function or constructor is outside the valid domain.

# Examples
```jldoctest
julia> sqrt(-1)
ERROR: DomainError with -1:
sqrt will only return a complex result if called with a complex argument. Try sqrt(complex(x)).
Stacktrace:
 [1] sqrt(::Int64) at ./math.jl:443
```
"""
DomainError

"""
    Task(func)

Create a `Task` (i.e. coroutine) to execute the given function (which must be
callable with no arguments). The task exits when this function returns.

# Examples
```jldoctest
julia> a() = det(rand(1000, 1000));

julia> b = Task(a);
```

In this example, `b` is a runnable `Task` that hasn't started yet.
"""
Task

"""
    StackOverflowError()

The function call grew beyond the size of the call stack. This usually happens when a call
recurses infinitely.
"""
StackOverflowError

"""
    ==(x, y)

Generic equality operator, giving a single [`Bool`](@ref) result. Falls back to `===`.
Should be implemented for all types with a notion of equality, based on the abstract value
that an instance represents. For example, all numeric types are compared by numeric value,
ignoring type. Strings are compared as sequences of characters, ignoring encoding.

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
    nfields(x) -> Int

Get the number of fields in the given object.
"""
nfields

"""
    show(stream, mime, x)

The [`display`](@ref) functions ultimately call `show` in order to write an object `x` as a
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

The first argument to `show` can be an [`IOContext`](@ref) specifying output format properties.
See [`IOContext`](@ref) for details.
"""
show(stream, mime, x)

"""
    isless(x, y)

Test whether `x` is less than `y`, according to a canonical total order. Values that are
normally unordered, such as `NaN`, are ordered in an arbitrary but consistent fashion. This
is the default comparison used by [`sort`](@ref). Non-numeric types with a canonical total order
should implement this function. Numeric types only need to implement it if they have special
values such as `NaN`.
"""
isless

"""
    error(message::AbstractString)

Raise an `ErrorException` with the given message.
"""
error

"""
    readcsv(source, [T::Type]; options...)

Equivalent to [`readdlm`](@ref) with `delim` set to comma, and type optionally defined by `T`.
"""
readcsv

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

# Examples
```jldoctest
julia> a = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> s_a = Symmetric(a)
2×2 Symmetric{Int64,Array{Int64,2}}:
 1  2
 2  4

julia> parent(s_a)
2×2 Array{Int64,2}:
 1  2
 3  4
```
"""
parent

"""
    gc_enable(on::Bool)

Control whether garbage collection is enabled using a boolean argument (`true` for enabled,
`false` for disabled). Returns previous GC state. Disabling garbage collection should be
used only with extreme caution, as it can cause memory use to grow without bound.
"""
gc_enable

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
    issubtype(type1, type2)

Return `true` if and only if all values of `type1` are also of `type2`. Can also be written
using the `<:` infix operator as `type1 <: type2`.

# Examples
```jldoctest
julia> issubtype(Int8, Int32)
false

julia> Int8 <: Integer
true
```
"""
issubtype(type1, type2)

"""
    finalizer(x, f)

Register a function `f(x)` to be called when there are no program-accessible references to
`x`. The type of `x` must be a `mutable struct`, otherwise the behavior of this function is
unpredictable.
"""
finalizer

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
    ismatch(r::Regex, s::AbstractString) -> Bool

Test whether a string contains a match of the given regular expression.
"""
ismatch

"""
    matchall(r::Regex, s::AbstractString[, overlap::Bool=false]) -> Vector{AbstractString}

Return a vector of the matching substrings from [`eachmatch`](@ref).
"""
matchall

"""
    get!(collection, key, default)

Return the value stored for the given key, or if no mapping for the key is present, store
`key => default`, and return `default`.

# Examples
```jldoctest
julia> d = Dict("a"=>1, "b"=>2, "c"=>3);

julia> get!(d, "a", 5)
1

julia> get!(d, "d", 4)
4

julia> d
Dict{String,Int64} with 4 entries:
  "c" => 3
  "b" => 2
  "a" => 1
  "d" => 4
```
"""
get!(collection,key,default)

"""
    get!(f::Function, collection, key)

Return the value stored for the given key, or if no mapping for the key is present, store
`key => f()`, and return `f()`.

This is intended to be called using `do` block syntax:
```julia
get!(dict, key) do
    # default value calculated here
    time()
end
```
"""
get!(f::Function,collection,key)
:@assert

"""
    deserialize(stream)

Read a value written by [`serialize`](@ref). `deserialize` assumes the binary data read from
`stream` is correct and has been serialized by a compatible implementation of [`serialize`](@ref).
It has been designed with simplicity and performance as a goal and does not validate
the data read. Malformed data can result in process termination. The caller has to ensure
the integrity and correctness of data read from `stream`.
"""
deserialize

"""
    length(collection) -> Integer

For ordered, indexable collections, returns the maximum index `i` for which `getindex(collection, i)`
is valid.
For unordered collections, returns the number of elements.

# Examples
```jldoctest
julia> length(1:5)
5

julia> length([1; 2; 3; 4])
4
```
"""
length(collection)

"""
    InterruptException()

The process was stopped by a terminal interrupt (CTRL+C).
"""
InterruptException

"""
    issubnormal(f) -> Bool

Test whether a floating point number is subnormal.
"""
issubnormal

"""
    NullException()

An attempted access to a [`Nullable`](@ref) with no defined value.

# Examples
```jldoctest
julia> a = Nullable{Int}()
Nullable{Int64}()

julia> get(a)
ERROR: NullException()
Stacktrace:
 [1] get(::Nullable{Int64}) at ./nullable.jl:92
```
"""
NullException

"""
    cfunction(function::Function, ReturnType::Type, (ArgumentTypes...))

Generate C-callable function pointer from Julia function. Type annotation of the return
value in the callback function is a must for situations where Julia cannot infer the return
type automatically.

For example:

```julia
function foo()
    # body

    retval::Float64
end

bar = cfunction(foo, Float64, ())
```
"""
cfunction

"""
    intersect(s1,s2...)
    ∩(s1,s2)

Construct the intersection of two or more sets.
Maintains order and multiplicity of the first argument for arrays and ranges.

# Examples
```jldoctest
julia> intersect([1, 2, 3], [3, 4, 5])
1-element Array{Int64,1}:
 3

julia> intersect([1, 4, 4, 5, 6], [4, 6, 6, 7, 8])
3-element Array{Int64,1}:
 4
 4
 6
```
"""
intersect

"""
    promote_rule(type1, type2)

Specifies what type should be used by [`promote`](@ref) when given values of types `type1` and
`type2`. This function should not be called directly, but should have definitions added to
it for new types as appropriate.
"""
promote_rule

"""
    match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])

Search for the first match of the regular expression `r` in `s` and return a `RegexMatch`
object containing the match, or nothing if the match failed. The matching substring can be
retrieved by accessing `m.match` and the captured sequences can be retrieved by accessing
`m.captures` The optional `idx` argument specifies an index at which to start the search.
"""
match

"""
    start(iter) -> state

Get initial iteration state for an iterable object.

# Examples
```jldoctest
julia> start(1:5)
1

julia> start([1;2;3])
1

julia> start([4;2;3])
1
```
"""
start

"""
    isa(x, type) -> Bool

Determine whether `x` is of the given `type`. Can also be used as an infix operator, e.g.
`x isa type`.
"""
isa

"""
    done(iter, state) -> Bool

Test whether we are done iterating.

# Examples
```jldoctest
julia> done(1:5, 3)
false

julia> done(1:5, 5)
false

julia> done(1:5, 6)
true
```
"""
done

"""
    convert(T, x)

Convert `x` to a value of type `T`.

If `T` is an [`Integer`](@ref) type, an [`InexactError`](@ref) will be raised if `x`
is not representable by `T`, for example if `x` is not integer-valued, or is outside the
range supported by `T`.

# Examples
```jldoctest
julia> convert(Int, 3.0)
3

julia> convert(Int, 3.5)
ERROR: InexactError: convert(Int64, 3.5)
Stacktrace:
 [1] convert(::Type{Int64}, ::Float64) at ./float.jl:680
```

If `T` is a [`AbstractFloat`](@ref) or [`Rational`](@ref) type,
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

# Examples
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
algorithms. See [`muladd`](@ref).
"""
fma

"""
    copy!(dest, do, src, so, N)

Copy `N` elements from collection `src` starting at offset `so`, to array `dest` starting at
offset `do`. Returns `dest`.
"""
copy!(dest,d,src,so,N)

"""
    +(x, y...)

Addition operator. `x+y+z+...` calls this function with all arguments, i.e. `+(x, y, z, ...)`.
"""
+

"""
    setindex!(collection, value, key...)

Store the given value at the given key or index within a collection. The syntax `a[i,j,...] =
x` is converted by the compiler to `(setindex!(a, x, i, j, ...); x)`.
"""
setindex!(collection,value,key...)

"""
    zeros([A::AbstractArray,] [T=eltype(A)::Type,] [dims=size(A)::Tuple])

Create an array of all zeros with the same layout as `A`, element type `T` and size `dims`.
The `A` argument can be skipped, which behaves like `Array{Float64,0}()` was passed.
For convenience `dims` may also be passed in variadic form.

# Examples
```jldoctest
julia> zeros(1)
1-element Array{Float64,1}:
 0.0

julia> zeros(Int8, 2, 3)
2×3 Array{Int8,2}:
 0  0  0
 0  0  0

julia> A = [1 2; 3 4]
2×2 Array{Int64,2}:
 1  2
 3  4

julia> zeros(A)
2×2 Array{Int64,2}:
 0  0
 0  0

julia> zeros(A, Float64)
2×2 Array{Float64,2}:
 0.0  0.0
 0.0  0.0

julia> zeros(A, Bool, (3,))
3-element Array{Bool,1}:
 false
 false
 false
```
See also [`ones`](@ref), [`similar`](@ref).
"""
zeros

"""
    Symbol(x...) -> Symbol

Create a `Symbol` by concatenating the string representations of the arguments together.
"""
Symbol

"""
    isvalid(value) -> Bool

Returns `true` if the given value is valid for its type, which currently can be either
`Char` or `String`.
"""
isvalid(value)

"""
    isvalid(T, value) -> Bool

Returns `true` if the given value is valid for that type. Types currently can
be either `Char` or `String`. Values for `Char` can be of type `Char` or [`UInt32`](@ref).
Values for `String` can be of that type, or `Vector{UInt8}`.
"""
isvalid(T,value)

"""
    skipchars(stream, predicate; linecomment::Char)

Advance the stream until before the first character for which `predicate` returns `false`.
For example `skipchars(stream, isspace)` will skip all whitespace. If keyword argument
`linecomment` is specified, characters from that character through the end of a line will
also be skipped.
"""
skipchars

"""
    pop!(collection, key[, default])

Delete and return the mapping for `key` if it exists in `collection`, otherwise return
`default`, or throw an error if `default` is not specified.

# Examples
```jldoctest
julia> d = Dict("a"=>1, "b"=>2, "c"=>3);

julia> pop!(d, "a")
1

julia> pop!(d, "d")
ERROR: KeyError: key "d" not found
Stacktrace:
 [1] pop!(::Dict{String,Int64}, ::String) at ./dict.jl:539

julia> pop!(d, "e", 4)
4
```
"""
pop!(collection,key,default)

"""
    pop!(collection) -> item

Remove an item in `collection` and return it. If `collection` is an
ordered container, the last item is returned.

# Examples
```jldoctest
julia> A=[1, 2, 3]
3-element Array{Int64,1}:
 1
 2
 3

julia> pop!(A)
3

julia> A
2-element Array{Int64,1}:
 1
 2

julia> S = Set([1, 2])
Set([2, 1])

julia> pop!(S)
2

julia> S
Set([1])

julia> pop!(Dict(1=>2))
1 => 2
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

# Examples
```jldoctest
julia> 2/0
Inf

julia> div(2, 0)
ERROR: DivideError: integer division error
Stacktrace:
 [1] div(::Int64, ::Int64) at ./int.jl:183
```
"""
DivideError

"""
    Number

Abstract supertype for all number types.
"""
Number

"""
    Real <: Number

Abstract supertype for all real numbers.
"""
Real

"""
    AbstractFloat <: Real

Abstract supertype for all floating point numbers.
"""
AbstractFloat

"""
    Integer <: Real

Abstract supertype for all integers.
"""
Integer

"""
    Signed <: Integer

Abstract supertype for all signed integers.
"""
Signed

"""
    Unsigned <: Integer

Abstract supertype for all unsigned integers.
"""
Unsigned

"""
    Bool <: Integer

Boolean type.
"""
Bool

for bit in (16, 32, 64)
    @eval begin
        """
            Float$($bit) <: AbstractFloat

        $($bit)-bit floating point number type.
        """
        $(Symbol("Float", bit))
    end
end

for bit in (8, 16, 32, 64, 128)
    @eval begin
        """
            Int$($bit) <: Signed

        $($bit)-bit signed integer type.
        """
        $(Symbol("Int", bit))

        """
            UInt$($bit) <: Unsigned

        $($bit)-bit unsigned integer type.
        """
        $(Symbol("UInt", bit))
    end
end

"""
    Vector{T}(n)

Construct an uninitialized [`Vector{T}`](@ref) of length `n`.

# Examples
```julia-repl
julia> Vector{Float64}(3)
3-element Array{Float64,1}:
 6.90966e-310
 6.90966e-310
 6.90966e-310
```
"""
Vector{T}(n)

"""
    Matrix{T}(m, n)

Construct an uninitialized [`Matrix{T}`](@ref) of size `m`×`n`.

# Examples
```julia-repl
julia> Matrix{Float64}(2, 3)
2×3 Array{Float64,2}:
 6.93517e-310  6.93517e-310  6.93517e-310
 6.93517e-310  6.93517e-310  1.29396e-320
```
"""
Matrix{T}(m, n)

"""
    Array{T}(dims)
    Array{T,N}(dims)

Construct an uninitialized `N`-dimensional [`Array`](@ref)
containing elements of type `T`. `N` can either be supplied explicitly,
as in `Array{T,N}(dims)`, or be determined by the length or number of `dims`.
`dims` may be a tuple or a series of integer arguments corresponding to the lengths
in each dimension. If the rank `N` is supplied explicitly, then it must
match the length or number of `dims`.

# Examples
```julia-repl
julia> A = Array{Float64,2}(2, 3) # N given explicitly
2×3 Array{Float64,2}:
 6.90198e-310  6.90198e-310  6.90198e-310
 6.90198e-310  6.90198e-310  0.0

julia> B = Array{Float64}(2) # N determined by the input
2-element Array{Float64,1}:
 1.87103e-320
 0.0
```
"""
Array{T,N}(dims)

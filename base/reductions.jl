# This file is a part of Julia. License is MIT: https://julialang.org/license

if Int === Int32
    const SmallSigned = Union{Int8,Int16}
    const SmallUnsigned = Union{UInt8,UInt16}
else
    const SmallSigned = Union{Int8,Int16,Int32}
    const SmallUnsigned = Union{UInt8,UInt16,UInt32}
end



###### Specific reduction functions ######

## sum

"""
    sum(f, itr)

Sum the results of calling function `f` on each element of `itr`.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

# Examples
```jldoctest
julia> sum(abs2, [2; 3; 4])
29
```

Note the important difference between `sum(A)` and `reduce(+, A)` for arrays
with small integer eltype:

```jldoctest
julia> sum(Int8[100, 28])
128

julia> reduce(+, Int8[100, 28])
-128
```

In the former case, the integers are widened to system word size and therefore
the result is 128. In the latter case, no such widening happens and integer
overflow results in -128.
"""
sum(f, a) = mapreduce(f, add_sum, a)


"""
    sum(itr)

Returns the sum of all elements in a collection.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

# Examples
```jldoctest
julia> sum(1:20)
210
```
"""
sum(a) = sum(identity, a)
sum(a::AbstractArray{Bool}) = count(a)



"""
    Base.add_sum(x, y)

The reduction operator used in `sum`. The main difference from [`+`](@ref) is that small
integers are promoted to `Int`/`UInt`.
"""
add_sum(x, y) = x + y
add_sum(x::SmallSigned, y::SmallSigned) = Int(x) + Int(y)
add_sum(x::SmallUnsigned, y::SmallUnsigned) = UInt(x) + UInt(y)
add_sum(x::Real, y::Real)::Real = x + y

reduce_empty(::typeof(add_sum), T) = reduce_empty(+, T)
reduce_empty(::typeof(add_sum), ::Type{T}) where {T<:SmallSigned}  = zero(Int)
reduce_empty(::typeof(add_sum), ::Type{T}) where {T<:SmallUnsigned} = zero(UInt)

reduce_first(::typeof(add_sum), x) = reduce_first(+, x)
reduce_first(::typeof(add_sum), x::SmallSigned)   = Int(x)
reduce_first(::typeof(add_sum), x::SmallUnsigned) = UInt(x)


## prod
"""
    prod(f, itr)

Returns the product of `f` applied to each element of `itr`.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

# Examples
```jldoctest
julia> prod(abs2, [2; 3; 4])
576
```
"""
prod(f, a) = mapreduce(f, mul_prod, a)

"""
    prod(itr)

Returns the product of all elements of a collection.

The return type is `Int` for signed integers of less than system word size, and
`UInt` for unsigned integers of less than system word size.  For all other
arguments, a common return type is found to which all arguments are promoted.

# Examples
```jldoctest
julia> prod(1:20)
2432902008176640000
```
"""
prod(a) = mapreduce(identity, mul_prod, a)


"""
    Base.mul_prod(x, y)

The reduction operator used in `prod`. The main difference from [`*`](@ref) is that small
integers are promoted to `Int`/`UInt`.
"""
mul_prod(x, y) = x * y
mul_prod(x::SmallSigned, y::SmallSigned) = Int(x) * Int(y)
mul_prod(x::SmallUnsigned, y::SmallUnsigned) = UInt(x) * UInt(y)
mul_prod(x::Real, y::Real)::Real = x * y

reduce_empty(::typeof(mul_prod), T) = reduce_empty(*, T)
reduce_empty(::typeof(mul_prod), ::Type{T}) where {T<:SmallSigned}  = one(Int)
reduce_empty(::typeof(mul_prod), ::Type{T}) where {T<:SmallUnsigned} = one(UInt)

reduce_first(::typeof(mul_prod), x) = reduce_first(*, x)
reduce_first(::typeof(mul_prod), x::SmallSigned)   = Int(x)
reduce_first(::typeof(mul_prod), x::SmallUnsigned) = UInt(x)


## maximum & minimum
_fast(::typeof(min),x,y) = min(x,y)
_fast(::typeof(max),x,y) = max(x,y)
function _fast(::typeof(max), x::AbstractFloat, y::AbstractFloat)
    ifelse(isnan(x),
        x,
        ifelse(x > y, x, y))
end

function _fast(::typeof(min),x::AbstractFloat, y::AbstractFloat)
    ifelse(isnan(x),
        x,
        ifelse(x < y, x, y))
end

isbadzero(::typeof(max), x::AbstractFloat) = (x == zero(x)) & signbit(x)
isbadzero(::typeof(min), x::AbstractFloat) = (x == zero(x)) & !signbit(x)
isbadzero(op, x) = false
isgoodzero(::typeof(max), x) = isbadzero(min, x)
isgoodzero(::typeof(min), x) = isbadzero(max, x)

function mapreduce_impl(f, op::Union{typeof(max), typeof(min)},
                        A::AbstractArray, first::Int, last::Int)
    a1 = @inbounds A[first]
    v1 = mapreduce_first(f, op, a1)
    v2 = v3 = v4 = v1
    chunk_len = 256
    start = first + 1
    simdstop  = start + chunk_len - 4
    while simdstop <= last - 3
        # short circuit in case of NaN
        v1 == v1 || return v1
        v2 == v2 || return v2
        v3 == v3 || return v3
        v4 == v4 || return v4
        @inbounds for i in start:4:simdstop
            v1 = _fast(op, v1, f(A[i+0]))
            v2 = _fast(op, v2, f(A[i+1]))
            v3 = _fast(op, v3, f(A[i+2]))
            v4 = _fast(op, v4, f(A[i+3]))
        end
        checkbounds(A, simdstop+3)
        start += chunk_len
        simdstop += chunk_len
    end
    v = op(op(v1,v2),op(v3,v4))
    for i in start:last
        @inbounds ai = A[i]
        v = op(v, f(ai))
    end

    # enforce correct order of 0.0 and -0.0
    # e.g. maximum([0.0, -0.0]) === 0.0
    # should hold
    if isbadzero(op, v)
        for i in first:last
            x = @inbounds A[i]
            isgoodzero(op,x) && return x
        end
    end
    return v
end

"""
    maximum(f, itr)

Returns the largest result of calling function `f` on each element of `itr`.

# Examples
```jldoctest
julia> maximum(length, ["Julion", "Julia", "Jule"])
6
```
"""
maximum(f, a) = mapreduce(f, max, a)

"""
    minimum(f, itr)

Returns the smallest result of calling function `f` on each element of `itr`.

# Examples
```jldoctest
julia> minimum(length, ["Julion", "Julia", "Jule"])
4
```
"""
minimum(f, a) = mapreduce(f, min, a)

"""
    maximum(itr)

Returns the largest element in a collection.

# Examples
```jldoctest
julia> maximum(-20.5:10)
9.5

julia> maximum([1,2,3])
3
```
"""
maximum(a) = mapreduce(identity, max, a)

"""
    minimum(itr)

Returns the smallest element in a collection.

# Examples
```jldoctest
julia> minimum(-20.5:10)
-20.5

julia> minimum([1,2,3])
1
```
"""
minimum(a) = mapreduce(identity, min, a)

## all & any

"""
    any(itr) -> Bool

Test whether any elements of a boolean collection are `true`, returning `true` as
soon as the first `true` value in `itr` is encountered (short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `false` (or equivalently, if the input contains no `true` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> a = [true,false,false,true]
4-element Array{Bool,1}:
 1
 0
 0
 1

julia> any(a)
true

julia> any((println(i); v) for (i, v) in enumerate(a))
1
true

julia> any([missing, true])
true

julia> any([false, missing])
missing
```
"""
any(itr) = any(identity, itr)

"""
    all(itr) -> Bool

Test whether all elements of a boolean collection are `true`, returning `false` as
soon as the first `false` value in `itr` is encountered (short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `true` (or equivalently, if the input contains no `false` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> a = [true,false,false,true]
4-element Array{Bool,1}:
 1
 0
 0
 1

julia> all(a)
false

julia> all((println(i); v) for (i, v) in enumerate(a))
1
2
false

julia> all([missing, false])
false

julia> all([true, missing])
missing
```
"""
all(itr) = all(identity, itr)

"""
    any(p, itr) -> Bool

Determine whether predicate `p` returns `true` for any elements of `itr`, returning
`true` as soon as the first item in `itr` for which `p` returns `true` is encountered
(short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `false` (or equivalently, if the input contains no `true` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> any(i->(4<=i<=6), [3,5,7])
true

julia> any(i -> (println(i); i > 3), 1:10)
1
2
3
4
true

julia> any(i -> i > 0, [1, missing])
true

julia> any(i -> i > 0, [-1, missing])
missing

julia> any(i -> i > 0, [-1, 0])
false
```
"""
any(f, itr) = _any(f, itr, :)

function _any(f, itr, ::Colon)
    anymissing = false
    for x in itr
        v = f(x)
        if ismissing(v)
            anymissing = true
        elseif v
            return true
        end
    end
    return anymissing ? missing : false
end

"""
    all(p, itr) -> Bool

Determine whether predicate `p` returns `true` for all elements of `itr`, returning
`false` as soon as the first item in `itr` for which `p` returns `false` is encountered
(short-circuiting).

If the input contains [`missing`](@ref) values, return `missing` if all non-missing
values are `true` (or equivalently, if the input contains no `false` value), following
[three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic).

# Examples
```jldoctest
julia> all(i->(4<=i<=6), [4,5,6])
true

julia> all(i -> (println(i); i < 3), 1:10)
1
2
3
false

julia> all(i -> i > 0, [1, missing])
missing

julia> all(i -> i > 0, [-1, missing])
false

julia> all(i -> i > 0, [1, 2])
true
```
"""
all(f, itr) = _all(f, itr, :)

function _all(f, itr, ::Colon)
    anymissing = false
    for x in itr
        v = f(x)
        if ismissing(v)
            anymissing = true
        # this syntax allows throwing a TypeError for non-Bool, for consistency with any
        elseif v
            continue
        else
            return false
        end
    end
    return anymissing ? missing : true
end

## count

"""
    count(p, itr) -> Integer
    count(itr) -> Integer

Count the number of elements in `itr` for which predicate `p` returns `true`.
If `p` is omitted, counts the number of `true` elements in `itr` (which
should be a collection of boolean values).

# Examples
```jldoctest
julia> count(i->(4<=i<=6), [2,3,4,5,6])
3

julia> count([true, false, true, true])
3
```
"""
function count(pred, itr)
    n = 0
    for x in itr
        n += pred(x)::Bool
    end
    return n
end
function count(pred, a::AbstractArray)
    n = 0
    for i in eachindex(a)
        @inbounds n += pred(a[i])::Bool
    end
    return n
end
count(itr) = count(identity, itr)

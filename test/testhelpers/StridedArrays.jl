# This file is a part of Julia. License is MIT: https://julialang.org/license

# StridedArrays

# This test file defines array types and functions
# useful for testing the strided array interface.

module StridedArrays

using Test: @test_throws, @test

export strided_ptr
export check_strided_get
export check_strided_set
export check_strided_traits
export check_strides_throws
export Strider
export NonMemStridedArray


function strided_ptr(f, a::AbstractArray{T}) where {T}
    a_cconv = Base.cconvert(Ptr{T}, a)
    GC.@preserve a_cconv begin
        f(Base.unsafe_convert(Ptr{T}, a_cconv))
    end
end

function check_strides_throws(err, a)
    @test_throws err strides(a)
    @test !Base.isstrided(a)
end

function check_strided_traits(a::AbstractArray{T,N}) where {T,N}
    for trait in (Base.isstrided, Base.islinearstrided, Base.isdense,
                  Base.isunsafeloadable, Base.isunsafestorable)
        @test trait(typeof(a)) === trait(a)
    end
    isbitstype(T) || return
    Base.isdense(a) && @test Base.islinearstrided(a)
    Base.islinearstrided(a) && @test Base.isstrided(a)
    Base.isstrided(a) || return
    @test strides(a) isa NTuple{N, Int}
    @test Base.elsize(a) isa Int
    # A dim with a single index contributes nothing to any element address, so
    # its stride is unconstrained by the layout traits; only check longer dims.
    if Base.isdense(a)
        if !isempty(a)
            # Base.size_to_strides is internal, not public API
            expected = Base.size_to_strides(1, size(a)...)
            for d in 1:N
                size(a, d) > 1 && @test strides(a)[d]*Base.elsize(a) == expected[d]*Base.elsize(Array{T})
            end
        end
    elseif Base.islinearstrided(a) && !isempty(a)
        d0 = findfirst(>(1), size(a))
        if d0 !== nothing
            # dims before d0 are singletons, so this stride is the column-major spacing
            s = strides(a)[d0]
            expected = Base.size_to_strides(s, size(a)...)
            for d in 1:N
                size(a, d) > 1 && @test strides(a)[d] == expected[d]
            end
        end
    end
    if Base.isunsafeloadable(a)
        check_strided_get(a)
    end
    nothing
end

"""
    check_strided_get(a::AbstractArray{T,N})

Test that array `a` implements the strided array interface for reading.
Checks stride consistency and that `unsafe_load` matches `getindex`.
"""
function check_strided_get(a::AbstractArray{T,N})::Nothing where {T, N}
    if !isbitstype(eltype(a))
        error("a doesn't have isbits elements")
    end
    if !Base.isunsafeloadable(a)
        error("isunsafeloadable(a) is false")
    end
    # Putting strided_ptr before the loop means that strided_ptr shouldn't error for empty arrays
    strided_ptr(a) do a_ptr
        for d in 1:N
            if stride(a, d) != strides(a)[d]
                error("stride(a, d) doesn't equal strides(a)[d] for dimension $(d)")
            end
        end
        for i in CartesianIndices(a)
            el_ptr = a_ptr
            for d in 1:N
                stride_in_bytes = stride(a, d) * Base.elsize(typeof(a))
                first_idx = first(axes(a, d))
                el_ptr += (i[d] - first_idx) * stride_in_bytes
            end
            if unsafe_load(el_ptr) !== a[i]
                error("getindex and unsafe_load mismatch at index $(i)")
            end
        end
    end
    nothing
end

"""
    check_strided_set(a, b, c, check_equal)

Test that array `a` implements the strided array interface for writing.
Checks stride consistency and that `unsafe_store!` matches assignment.
The values from `c` are assigned to both `a` (via unsafe_store!) and `b` (via setindex!), so that after the function, both `a` and `b` should contain the values from `c`.
`b` and `c` are arrays with the same axes as `a`. `check_equal` is a function to compare `a` and `b` after assignment.
"""
function check_strided_set(a::AbstractArray{T,N}, b::AbstractArray{T,N}, c::AbstractArray{T,N}, check_equal)::Nothing where {T, N}
    if axes(a) != axes(b) || axes(a) != axes(c)
        throw(ArgumentError("a, b, and c must have the same axes"))
    end
    if !isbitstype(eltype(a))
        error("a doesn't have isbits elements")
    end
    if !Base.isunsafestorable(a)
        error("isunsafestorable(a) is false")
    end
    # Putting strided_ptr before the loop means that strided_ptr shouldn't error for empty arrays
    strided_ptr(a) do a_ptr
        for d in 1:N
            if stride(a, d) != strides(a)[d]
                error("stride(a, d) doesn't equal strides(a)[d] for dimension $(d)")
            end
        end
        for i in CartesianIndices(a)
            el_ptr = a_ptr
            for d in 1:N
                stride_in_bytes = stride(a, d) * Base.elsize(typeof(a))
                first_idx = first(axes(a, d))
                el_ptr += (i[d] - first_idx) * stride_in_bytes
            end
            unsafe_store!(el_ptr, c[i])
            b[i] = c[i]
        end
    end
    check_equal(a, b)
    nothing
end

"""
    Strider{T,N} <: AbstractArray{T,N}

An example array type that implements the strided array interface for custom strides.
"""
struct Strider{T,N} <: AbstractArray{T,N}
    data::Memory{T}
    offset::Int
    strides::NTuple{N,Int}
    size::NTuple{N,Int}
end
function Strider{T}(strides::NTuple{N}, size::NTuple{N}) where {T,N}
    offset = 1-sum(strides .* (strides .< 0) .* (size .- 1))
    data = Memory{T}(undef, sum(abs.(strides) .* (size .- 1)) + 1)
    Strider{T, N}(data, offset, strides, size)
end
function Strider(vec::AbstractArray{T}, strides::NTuple{N}, size::NTuple{N}, offset::Integer=0) where {T,N}
    S = Strider{T}(strides::NTuple{N}, size::NTuple{N})
    @assert length(vec)-offset >= length(S.data)
    copyto!(S.data, 1, vec, firstindex(vec)+offset, length(S.data))
    S
end
function Base.size(S::Strider)
    S.size
end
function Base.getindex(S::Strider{<:Any,N}, I::Vararg{Int,N}) where {N}
    S.data[sum(S.strides .* (I .- 1)) + S.offset]
end
function Base.setindex!(S::Strider{<:Any,N}, x, I::Vararg{Int,N}) where {N}
    S.data[sum(S.strides .* (I .- 1)) + S.offset] = x
    S
end
function Base.strides(S::Strider)
    S.strides
end
function Base.elsize(::Type{<:Strider{T}}) where {T}
    Base.elsize(Memory{T})
end
function Base.cconvert(::Type{Ptr{T}}, S::Strider{T}) where {T}
    memoryref(S.data, S.offset)
end
Base.isstrided(S::Type{<:Strider}) = true
Base.isunsafeloadable(::Type{<:Strider}) = true
Base.isunsafestorable(::Type{<:Strider}) = true

# Create a type to test strided array interface edge cases.
# This array is memory backed, but the NonMemStridedArrayCConvert wrapper hides this.
struct NonMemStridedArray{T, N} <: AbstractArray{T, N}
    a::Array{T, N}
end
Base.size(A::NonMemStridedArray) = size(A.a)
function Base.getindex(A::NonMemStridedArray{T, N}, I::Vararg{Int, N}) where {T, N}
    getindex(A.a, I...)
end
struct NonMemStridedArrayCConvert{C}
    c::C
end
function Base.cconvert(::Type{Ptr{T}}, A::NonMemStridedArray{T}) where T
    NonMemStridedArrayCConvert(Base.cconvert(Ptr{T}, A.a))
end
function Base.unsafe_convert(::Type{Ptr{T}}, c::NonMemStridedArrayCConvert) where T
    Base.unsafe_convert(Ptr{T}, c.c)
end
function Base.elsize(::Type{NonMemStridedArray{T, N}}) where {T, N}
    Base.elsize(Array{T, N})
end
Base.strides(A::NonMemStridedArray) = strides(A.a)
Base.isstrided(::Type{<:NonMemStridedArray}) = true
Base.isunsafeloadable(::Type{<:NonMemStridedArray}) = true

end # module StridedArrays

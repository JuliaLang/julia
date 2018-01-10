
"""
The `GenericString` can be used to test generic string APIs that program to
the `AbstractString` interface, in order to ensure that functions can work
with string types besides the standard `String` type.
"""
struct GenericString <: AbstractString
    string::AbstractString
end
Base.ncodeunits(s::GenericString) = ncodeunits(s.string)
Base.codeunit(s::GenericString) = codeunit(s.string)
Base.codeunit(s::GenericString, i::Integer) = codeunit(s.string, i)
Base.isvalid(s::GenericString, i::Integer) = isvalid(s.string, i)
Base.next(s::GenericString, i::Integer) = next(s.string, i)
Base.reverse(s::GenericString) = GenericString(reverse(s.string))
Base.reverse(s::SubString{GenericString}) =
    GenericString(typeof(s.string)(reverse(String(s))))

"""
The `GenericSet` can be used to test generic set APIs that program to
the `AbstractSet` interface, in order to ensure that functions can work
with set types besides the standard `Set` and `BitSet` types.
"""
struct GenericSet{T} <: AbstractSet{T}
    s::AbstractSet{T}
end

"""
The `GenericDict` can be used to test generic dict APIs that program to
the `AbstractDict` interface, in order to ensure that functions can work
with associative types besides the standard `Dict` type.
"""
struct GenericDict{K,V} <: AbstractDict{K,V}
    s::AbstractDict{K,V}
end

for (G, A) in ((GenericSet, AbstractSet),
               (GenericDict, AbstractDict))
    @eval begin
        Base.done(s::$G, state) = done(s.s, state)
        Base.next(s::$G, state) = next(s.s, state)
    end
    for f in (:eltype, :isempty, :length, :start)
        @eval begin
            Base.$f(s::$G) = $f(s.s)
        end
    end
end

Base.get(s::GenericDict, x, y) = get(s.s, x, y)

"""
The `GenericArray` can be used to test generic array APIs that program to
the `AbstractArray` interface, in order to ensure that functions can work
with array types besides the standard `Array` type.
"""
struct GenericArray{T,N} <: AbstractArray{T,N}
    a::Array{T,N}
end

GenericArray{T}(args...) where {T} = GenericArray(Array{T}(args...))
GenericArray{T,N}(args...) where {T,N} = GenericArray(Array{T,N}(args...))

Base.keys(a::GenericArray) = keys(a.a)
Base.axes(a::GenericArray) = axes(a.a)
Base.length(a::GenericArray) = length(a.a)
Base.size(a::GenericArray) = size(a.a)
Base.getindex(a::GenericArray, i...) = a.a[i...]
Base.setindex!(a::GenericArray, x, i...) = a.a[i...] = x

Base.similar(A::GenericArray, s::Integer...) = GenericArray(similar(A.a, s...))

"`guardsrand(f)` runs the function `f()` and then restores the
state of the global RNG as it was before."
function guardsrand(f::Function, r::AbstractRNG=Base.GLOBAL_RNG)
    old = copy(r)
    try
        f()
    finally
        copy!(r, old)
    end
end

"`guardsrand(f, seed)` is equivalent to running `srand(seed); f()` and
then restoring the state of the global RNG as it was before."
guardsrand(f::Function, seed::Union{Vector{UInt32},Integer}) = guardsrand() do
    srand(seed)
    f()
end


module RedefStruct

struct A
    x::Int
end

struct B{T<:AbstractFloat}
    y::T
end

struct C{T,N,A<:AbstractArray{T}}
    data::A

    function C{T,N,A}(data::AbstractArray{T}) where {T,N,A}
        ndims(data) == N-1 || error("wrong dimensionality")
        return new{T,N,A}(data)
    end
end
C(data::AbstractArray{T,N}) where {T,N} = C{T,N+1,typeof(data)}(data)

# Methods that take the type as an argument
fA(a::A) = true
fB(b::B{T}) where T = true
fC(c::C{T,N}) where {T,N} = N

# Methods that call the constructor
gA(x) = A(x)
gB(::Type{T}) where T = B{float(T)}(-2)
gC(sz) = C(zeros(sz))

end

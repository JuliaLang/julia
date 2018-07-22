# This file is a part of Julia. License is MIT: https://julialang.org/license

module TrickyArithmetic
    struct A
        x::Int
    end
    A(a::A) = a
    Base.convert(::Type{A}, i::Int) = A(i)
    Base.zero(::Union{A, Type{A}}) = A(0)
    Base.one(::Union{A, Type{A}}) = A(1)
    struct B
        x::Int
    end
    struct C
        x::Int
    end
    C(a::A) = C(a.x)
    Base.zero(::Union{C, Type{C}}) = C(0)
    Base.one(::Union{C, Type{C}}) = C(1)

    Base.:(*)(x::Int, a::A) = B(x*a.x)
    Base.:(*)(a::A, x::Int) = B(a.x*x)
    Base.:(*)(a::Union{A,B}, b::Union{A,B}) = B(a.x*b.x)
    Base.:(*)(a::Union{A,B,C}, b::Union{A,B,C}) = C(a.x*b.x)
    Base.:(+)(a::Union{A,B,C}, b::Union{A,B,C}) = C(a.x+b.x)
    Base.:(-)(a::Union{A,B,C}, b::Union{A,B,C}) = C(a.x-b.x)

    struct D{NT, DT}
        n::NT
        d::DT
    end
    D{NT, DT}(d::D{NT, DT}) where {NT, DT} = d # called by oneunit
    Base.zero(::Union{D{NT, DT}, Type{D{NT, DT}}}) where {NT, DT} = zero(NT) / one(DT)
    Base.one(::Union{D{NT, DT}, Type{D{NT, DT}}}) where {NT, DT} = one(NT) / one(DT)
    Base.convert(::Type{D{NT, DT}}, a::Union{A, B, C}) where {NT, DT} = NT(a) / one(DT)
    #Base.convert(::Type{D{NT, DT}}, a::D) where {NT, DT} = NT(a.n) / DT(a.d)

    Base.:(*)(a::D, b::D) = (a.n*b.n) / (a.d*b.d)
    Base.:(*)(a::D, b::Union{A,B,C}) = (a.n * b) / a.d
    Base.:(*)(a::Union{A,B,C}, b::D) = b * a
    Base.inv(a::Union{A,B,C}) = A(1) / a
    Base.inv(a::D) = a.d / a.n
    Base.:(/)(a::Union{A,B,C}, b::Union{A,B,C}) = D(a, b)
    Base.:(/)(a::D, b::Union{A,B,C}) = a.n / (a.d*b)
    Base.:(/)(a::Union{A,B,C,D}, b::D) = a * inv(b)
    Base.:(+)(a::Union{A,B,C}, b::D) = (a*b.d+b.n) / b.d
    Base.:(+)(a::D, b::Union{A,B,C}) = b + a
    Base.:(+)(a::D, b::D) = (a.n*b.d+a.d*b.n) / (a.d*b.d)
    Base.:(-)(a::Union{A,B,C}) = typeof(a)(a.x)
    Base.:(-)(a::D) = (-a.n) / a.d
    Base.:(-)(a::Union{A,B,C,D}, b::Union{A,B,C,D}) = a + (-b)

    Base.promote_rule(::Type{A}, ::Type{B}) = B
    Base.promote_rule(::Type{B}, ::Type{A}) = B
    Base.promote_rule(::Type{A}, ::Type{C}) = C
    Base.promote_rule(::Type{C}, ::Type{A}) = C
    Base.promote_rule(::Type{B}, ::Type{C}) = C
    Base.promote_rule(::Type{C}, ::Type{B}) = C
    Base.promote_rule(::Type{D{NT,DT}}, T::Type{<:Union{A,B,C}}) where {NT,DT} = D{promote_type(NT,T),DT}
    Base.promote_rule(T::Type{<:Union{A,B,C}}, ::Type{D{NT,DT}}) where {NT,DT} = D{promote_type(NT,T),DT}
    Base.promote_rule(::Type{D{NS,DS}}, ::Type{D{NT,DT}}) where {NS,DS,NT,DT} = D{promote_type(NS,NT),promote_type(DS,DT)}
end

# This file is a part of Julia. License is MIT: http://julialang.org/license

make_value{T<:Integer}(::Type{T}, i::Integer) = 3*i%T
make_value{T<:AbstractFloat}(::Type{T},i::Integer) = T(3*i)

typealias Vec{N,T} NTuple{N,Base.VecElement{T}}

# Crash report for #15244 motivated this test.
@generated function thrice_iota{N,T}(::Type{Vec{N,T}})
    :(tuple($([:(Base.VecElement(make_value($T,$i))) for i in 1:N]...)))
end

function call_iota(n::Integer,t::DataType)
    x = thrice_iota(Vec{n,t})
    @test x[1].value === make_value(t,1)
    @test x[n].value === make_value(t,n)
end

# Try various tuple lengths and element types
for i=1:20
    for t in [Bool, Int8, Int16, Int32, Int64, Float32, Float64]
        call_iota(i,t)
    end
end

# Another crash report for #15244 motivated this test.
immutable Bunch{N,T}
    elts::NTuple{N,Base.VecElement{T}}
end

unpeel(x) = x.elts[1].value

@test unpeel(Bunch{2,Float64}((Base.VecElement(5.0),
                               Base.VecElement(4.0)))) === 5.0

rewrap(x) = VecElement(x.elts[1].value+0)
b = Bunch((VecElement(1.0), VecElement(2.0)))

@test rewrap(b)===VecElement(1.0)

immutable Herd{N,T}
    elts::NTuple{N,Base.VecElement{T}}
    Herd{N,T}(elts::NTuple{N,T}) where {N,T} = new(ntuple(i->Base.VecElement{T}(elts[i]), N))
end

function check{N,T}(x::Herd{N,T})
    for i=1:N
        @test x.elts[i].value === N*N+i-1
    end
end

check(Herd{1,Int}((1,)))
check(Herd{2,Int}((4,5)))
check(Herd{4,Int}((16,17,18,19)))

immutable Gr{N, T}
    u::T
    v::Bunch{N,T}
    w::T
end

a = Vector{Gr{2,Float64}}(2)
a[2] = Gr(1.0, Bunch((VecElement(2.0), VecElement(3.0))), 4.0)
a[1] = Gr(5.0, Bunch((VecElement(6.0), VecElement(7.0))), 8.0)
@test a[2] == Gr(1.0, Bunch((VecElement(2.0), VecElement(3.0))), 4.0)

@test isa(VecElement((1,2)), VecElement{Tuple{Int,Int}})

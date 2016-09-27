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
    Herd(elts::NTuple{N,T}) = new(ntuple(i->Base.VecElement{T}(elts[i]), N))
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

import Core.Intrinsics
# Test intrinsics for #18445
# Explicit form
function trunc_vec{T1 <: Vec, T2 <: Vec}(::Type{T1}, x :: T2)
    Intrinsics.box(T1, Intrinsics.trunc_int(T1, Intrinsics.unbox(T2, x)))
end

@test trunc_vec(Vec{4, UInt8}, Vec((0xff00, 0xff01, 0x00ff, 0xf0f0))) == Vec((0x00, 0x01, 0xff, 0xf0))
@test Intrinsics.zext_int(Vec{4, UInt16}, Vec((0x00, 0x01, 0xff, 0xf0))) == Vec((0x0000, 0x0001, 0x00ff, 0x00f0))
@test Intrinsics.sext_int(Vec{4, UInt16}, Vec((0x00, 0x01, 0xff, 0xf0))) == Vec((0x0000, 0x0001, 0xffff, 0xfff0))
@test Intrinsics.add_int(Vec((1,2,3,4)), Vec((1,2,3,4))) == Vec((2,4,6,8))
@test Intrinsics.mul_float(Vec((1.0, 2.0, 1.0, 2.0)), Vec((0.5, 0.5, 1.5, 1.5))) == Vec((0.5, 1.0, 1.5, 3.0))

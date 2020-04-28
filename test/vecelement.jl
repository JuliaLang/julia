# This file is a part of Julia. License is MIT: https://julialang.org/license

make_value(::Type{T}, i::Integer) where {T<:Integer} = 3*i%T
make_value(::Type{T},i::Integer) where {T<:AbstractFloat} = T(3*i)

const Vec{N,T} = NTuple{N,Base.VecElement{T}}

# Crash report for #15244 motivated this test.
@generated function thrice_iota(::Type{Vec{N,T}}) where {N,T}
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

# Try various large tuple lengths and element types #20961
for i in (34, 36, 48, 64, 72, 80, 96)
    for t in [Bool, Int8, Int16, Int32, Int64, Float32, Float64]
        call_iota(i,t)
    end
end

# Another crash report for #15244 motivated this test.
struct Bunch{N,T}
    elts::NTuple{N,Base.VecElement{T}}
end

unpeel(x) = x.elts[1].value
@test unpeel(Bunch{2,Float64}((Base.VecElement(5.0),
                               Base.VecElement(4.0)))) === 5.0

rewrap(x) = VecElement(x.elts[1].value + 0)
let b = Bunch((VecElement(1.0), VecElement(2.0)))
    @test rewrap(b) === VecElement(1.0)
end

struct Herd{N,T}
    elts::NTuple{N,Base.VecElement{T}}
    Herd{N,T}(elts::NTuple{N,T}) where {N,T} = new(ntuple(i->Base.VecElement{T}(elts[i]), N))
end

function check(x::Herd{N,T}) where {N,T}
    for i=1:N
        @test x.elts[i].value === N*N+i-1
    end
end

check(Herd{1,Int}((1,)))
check(Herd{2,Int}((4,5)))
check(Herd{4,Int}((16,17,18,19)))

struct Gr{N, T}
    u::T
    v::Bunch{N,T}
    w::T
end

let a = Vector{Gr{2,Float64}}(undef, 2)
    a[2] = Gr(1.0, Bunch((VecElement(2.0), VecElement(3.0))), 4.0)
    a[1] = Gr(5.0, Bunch((VecElement(6.0), VecElement(7.0))), 8.0)
    @test a[2] == Gr(1.0, Bunch((VecElement(2.0), VecElement(3.0))), 4.0)
end

@test isa(VecElement((1,2)), VecElement{Tuple{Int,Int}})

# test for alignment agreement (#32414)
@noinline function bar32414(a)
    v = ntuple(w -> VecElement(Float64(10w)), Val(8))
    return a, (v, (a, (1e6, 1e9)))
end
@test bar32414(-35.0) === (-35.0, ((VecElement(10.0), VecElement(20.0), VecElement(30.0), VecElement(40.0), VecElement(50.0), VecElement(60.0), VecElement(70.0), VecElement(80.0)), (-35.0, (1.0e6, 1.0e9))))

# The following test mimic SIMD.jl
const _llvmtypes = Dict{DataType, String}(
    Float64 => "double",
    Float32 => "float",
    Int32 => "i32",
    Int64 => "i64"
)

@generated function vecadd(x::Vec{N, T}, y::Vec{N, T}) where {N, T}
    llvmT = _llvmtypes[T]
    func = T <: AbstractFloat ? "fadd" : "add"
    exp = """
    %3 = $(func) <$(N) x $(llvmT)> %0, %1
    ret <$(N) x $(llvmT)> %3
    """
    return quote
        Base.@_inline_meta
        Core.getfield(Base, :llvmcall)($exp, Vec{$N, $T}, Tuple{Vec{$N, $T}, Vec{$N, $T}}, x, y)
    end
end

function f20961(x::Vector{Vec{N, T}}, y::Vector{Vec{N, T}}) where{N, T}
    @inbounds begin
        a = x[1]
        b = y[1]
        return vecadd(a, b)
    end
end

# Test various SIMD Vectors with known good sizes
for T in (Float64, Float32, Int64, Int32)
    for N in 1:36
        a = ntuple(i -> VecElement(T(i)), N)
        result = ntuple(i -> VecElement(T(i+i)), N)
        b = vecadd(a, a)
        @test b == result
        b = f20961([a], [a])
        @test b == result
    end
end

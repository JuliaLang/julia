# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base: get_world_counter

# DO NOT ALTER ORDER OR SPACING OF METHODS BELOW
const lineoffset = @__LINE__
ambig(x, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Integer) = 3
ambig(x::Int, y::Int) = 4
ambig(x::Number, y) = 5
# END OF LINE NUMBER SENSITIVITY

# For curmod_*
include("testenv.jl")

@test length(methods(ambig)) == 5
@test length(Base.methods_including_ambiguous(ambig, Tuple)) == 5

@test length(methods(ambig, (Int, Int))) == 1
@test length(methods(ambig, (UInt8, Int))) == 0
@test length(Base.methods_including_ambiguous(ambig, (UInt8, Int))) == 2

@test ambig("hi", "there") == 1
@test ambig(3.1, 3.2) == 5
@test ambig(3, 4) == 4
@test_throws MethodError ambig(0x03, 4)
@test_throws MethodError ambig(0x03, 4)  # test that not inserted into cache

# Ensure it still works with potential inlining
callambig(x, y) = ambig(x, y)
@test_throws MethodError callambig(0x03, 4)

# Printing ambiguity errors
let err = try
              ambig(0x03, 4)
          catch _e_
              _e_
          end
    io = IOBuffer()
    Base.showerror(io, err)
    errstr = String(take!(io))
    @test occursin("  ambig(x, y::Integer)\n    @ $curmod_str", errstr)
    @test occursin("  ambig(x::Integer, y)\n    @ $curmod_str", errstr)
    @test occursin("Possible fix, define\n  ambig(::Integer, ::Integer)", errstr)
end

@test_warn "declares type variable S but does not use it" @eval ambig_with_bounds(x, ::Int, ::T) where {T<:Integer,S} = 0
@test_warn "declares type variable S but does not use it" @eval ambig_with_bounds(::Int, x, ::T) where {T<:Integer,S} = 1
let err = try
              ambig_with_bounds(1, 2, 3)
          catch _e_
              _e_
          end
    io = IOBuffer()
    Base.showerror(io, err)
    lines = split(String(take!(io)), '\n')
    @test lines[end-1] == "  ambig_with_bounds(::$Int, ::$Int, ::T) where T<:Integer"
end

## Other ways of accessing functions
# Test that non-ambiguous cases work
let io = IOBuffer()
    @test precompile(ambig, (Int, Int))
    cf = @eval @cfunction(ambig, Int, (Int, Int))
    @test ccall(cf, Int, (Int, Int), 1, 2) == 4
    @test length(code_lowered(ambig, (Int, Int))) == 1
    @test length(code_typed(ambig, (Int, Int))) == 1
end

# Test that ambiguous cases fail appropriately
let io = IOBuffer()
    @test !precompile(ambig, (UInt8, Int))
    cf = @eval @cfunction(ambig, Int, (UInt8, Int))  # test for a crash (doesn't throw an error)
    @test_throws(MethodError(ambig, (UInt8(1), Int(2)), get_world_counter()),
                 ccall(cf, Int, (UInt8, Int), 1, 2))
    @test_throws("Calling invoke(f, t, args...) would throw:\nMethodError: no method matching ambig",
                 which(ambig, (UInt8, Int)))
    @test length(code_typed(ambig, (UInt8, Int))) == 0
end

# Method overwriting doesn't destroy ambiguities
@test_throws MethodError ambig(2, 0x03)
ambig(x, y::Integer) = 3
@test_throws MethodError ambig(2, 0x03)

# Method overwriting by an ambiguity should also invalidate the method cache (#21963)
ambig(x::Union{Char, Int8}) = 'r'
@test ambig('c') == 'r'
@test ambig(Int8(1)) == 'r'
@test_throws MethodError ambig(Int16(1))
ambig(x::Union{Char, Int16}) = 's'
@test_throws MethodError ambig('c')
@test ambig(Int8(1)) == 'r'
@test ambig(Int16(1)) == 's'

# Automatic detection of ambiguities

const allowed_undefineds = Set([GlobalRef(Base, :active_repl)])

module Ambig1
ambig(x, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Integer) = 3
end

ambs = detect_ambiguities(Ambig1)
@test length(ambs) == 1

module Ambig2
ambig(x, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Integer) = 3
ambig(x::Number, y) = 4
end

ambs = detect_ambiguities(Ambig2)
@test length(ambs) == 2

module Ambig3
ambig(x, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Integer) = 3
ambig(x::Int, y::Int) = 4
end

ambs = detect_ambiguities(Ambig3)
@test length(ambs) == 1

module Ambig4
ambig(x, y) = 1
ambig(x::Int, y) = 2
ambig(x, y::Int) = 3
ambig(x::Int, y::Int) = 4
end
ambs = detect_ambiguities(Ambig4)
@test length(ambs) == 0

module Ambig5
ambig(x::Int8, y) = 1
ambig(x::Integer, y) = 2
ambig(x, y::Int) = 3
end
ambs = detect_ambiguities(Ambig5)
@test length(ambs) == 2

module Ambig48312
ambig(::Integer, ::Int) = 1
ambig(::Int, ::Integer) = 2
ambig(::Signed, ::Int) = 3
ambig(::Int, ::Signed) = 4
end
ambs = detect_ambiguities(Ambig48312)
@test length(ambs) == 1 # only ambiguous over (Int, Int), which is 3 or 4

module UnboundAmbig55868
    module B
        struct C end
        export C
        Base.@deprecate_binding D C
    end
    using .B
    export C, D
end
@test isempty(detect_unbound_args(UnboundAmbig55868))
@test isempty(detect_ambiguities(UnboundAmbig55868))

# Test that Core and Base are free of ambiguities
# not using isempty so this prints more information when it fails
@testset "detect_ambiguities" begin
    let ambig = Set(detect_ambiguities(Core, Base; recursive=true, ambiguous_bottom=false, allowed_undefineds))
        good = true
        for (sig1, sig2) in ambig
            @test sig1 === sig2 # print this ambiguity
            good = false
        end
        @test good
    end

    # some ambiguities involving Union{} type parameters may be expected, but not required
    let ambig = Set(detect_ambiguities(Core; recursive=true, ambiguous_bottom=true))
        @test isempty(ambig)
    end

    STDLIB_DIR = Sys.STDLIB
    STDLIBS = filter!(x -> x != "LinearAlgebra" && x != "SparseArrays" && # Some packages run this test themselves
                           isfile(joinpath(STDLIB_DIR, x, "src", "$(x).jl")),
                      readdir(STDLIB_DIR))

    # List standard libraries. Exclude modules such as Main, Base, and Core.
    let modules = [mod for (pkg, mod) in Base.loaded_modules if pkg.uuid !== nothing && String(pkg.name) in STDLIBS]
        @test isempty(detect_ambiguities(modules...; recursive=true, allowed_undefineds))
    end
end

amb_1(::Int8, ::Int) = 1
amb_1(::Integer, x) = 2
amb_1(x, ::Int) = 3
# if there is an ambiguity with some methods and not others, `methods`
# should return just the non-ambiguous ones, i.e. the ones that could actually
# be called.
@test length(methods(amb_1, Tuple{Integer, Int})) == 1

amb_2(::Int, y) = 1
amb_2(x, ::Int) = 2
amb_2(::Int8, y) = 3
@test length(methods(amb_2)) == 3  # make sure no duplicates

amb_3(::Int8, ::Int8) = 1
amb_3(::Int16, ::Int16) = 2
amb_3(::Integer, ::Integer) = 3
amb_3(::Integer, x) = 4
amb_3(x, ::Integer) = 5
# ambiguous definitions exist, but are covered by multiple more specific definitions
let ms = methods(amb_3).ms
    @test !Base.isambiguous(ms[4], ms[5])
end

amb_4(::Int8, ::Int8) = 1
amb_4(::Int16, ::Int16) = 2
amb_4(::Integer, x) = 4
amb_4(x, ::Integer) = 5
# as above, but without sufficient definition coverage
let ms = methods(amb_4).ms
    @test Base.isambiguous(ms[3], ms[4])
end

g16493(x::T, y::Integer) where {T<:Number} = 0
g16493(x::Complex{T}, y) where {T} = 1
let ms = methods(g16493, (Complex, Any))
    @test length(ms) == 1
    @test first(ms).sig == (Tuple{typeof(g16493), Complex{T}, Any} where T)
end

# issue #17350
module Ambig6
struct ScaleMinMax{To,From} end
map1(mapi::ScaleMinMax{To,From}, val::From) where {To<:Union{Float32,Float64},From<:Real} = 1
map1(mapi::ScaleMinMax{To,From}, val::Union{Real,Complex}) where {To<:Union{Float32,Float64},From<:Real} = 2
end

@test isempty(detect_ambiguities(Ambig6))

module Ambig7
struct T end
(::T)(x::Int8, y) = 1
(::T)(x, y::Int8) = 2
end
@test length(detect_ambiguities(Ambig7)) == 1

module Ambig17648
struct MyArray{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
end

foo(::Type{Array{T,N}}, A::MyArray{T,N}) where {T,N} = A.data
foo(::Type{Array{T,N}}, A::MyArray{T,N}) where {T<:AbstractFloat,N} = A.data
foo(::Type{Array{S,N}}, A::MyArray{T,N}) where {S<:AbstractFloat,N,T<:AbstractFloat} =
    copyto!(Array{S}(undef, unsize(A)), A.data)
foo(::Type{Array{S,N}}, A::AbstractArray{T,N}) where {S<:AbstractFloat,N,T<:AbstractFloat} =
    copyto!(Array{S}(undef, size(A)), A)
end

@test isempty(detect_ambiguities(Ambig17648))

module Ambig8
# complex / unsorted(-able) ambiguities
f(::Union{typeof(pi), Integer}) =  1
f(::Union{AbstractIrrational, Int}) =  2
f(::Irrational) = 3
f(::Signed) = 4
g(::Irrational) = 3
g(::Signed) = 4
g(::Union{typeof(pi), Integer}) =  1
g(::Union{AbstractIrrational, Int}) =  2
struct Irrational2 <: AbstractIrrational; end
end
@test isempty(methods(Ambig8.f, (Int,)))
@test isempty(methods(Ambig8.g, (Int,)))
for f in (Ambig8.f, Ambig8.g)
    @test length(methods(f, (Integer,))) == 2 # 3 is also acceptable
    @test length(methods(f, (Signed,))) == 1 # 2 is also acceptable
    @test length(Base.methods_including_ambiguous(f, (Signed,))) == 2
    @test f(0x00) == 1
    @test f(Ambig8.Irrational2()) == 2
    @test f(MathConstants.γ) == 3
    @test f(Int8(0)) == 4
    @test_throws MethodError f(0)
    @test_throws MethodError f(pi)
    let ambig = Ref{Int32}(0)
        ms = Base._methods_by_ftype(Tuple{typeof(f), Union{Int,AbstractIrrational}}, nothing, 10, Base.get_world_counter(), false, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test ms isa Vector
        @test length(ms) == 2
        @test ambig[] == 1
    end
    let ambig = Ref{Int32}(0)
        ms = Base._methods_by_ftype(Tuple{typeof(f), Union{Int,AbstractIrrational}}, nothing, -1, Base.get_world_counter(), false, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test ms isa Vector
        @test length(ms) == 2
        @test ambig[] == 1
    end
    let ambig = Ref{Int32}(0)
        ms = Base._methods_by_ftype(Tuple{typeof(f), Union{Int,AbstractIrrational}}, nothing, 10, Base.get_world_counter(), true, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test ms isa Vector
        @test length(ms) == 3
        @test ambig[] == 1
    end
    let ambig = Ref{Int32}(0)
        ms = Base._methods_by_ftype(Tuple{typeof(f), Union{Int,AbstractIrrational}}, nothing, -1, Base.get_world_counter(), true, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test ms isa Vector
        @test length(ms) == 3
        @test ambig[] == 1
    end
end

module Ambig9
f(x::Complex{<:Integer}) = 1
f(x::Complex{<:Rational}) = 2
end
@test !Base.isambiguous(methods(Ambig9.f)..., ambiguous_bottom=false)
@test Base.isambiguous(methods(Ambig9.f)..., ambiguous_bottom=true)
@test !Base.isambiguous(methods(Ambig9.f)...)
@test length(detect_ambiguities(Ambig9, ambiguous_bottom=false)) == 0
@test length(detect_ambiguities(Ambig9, ambiguous_bottom=true)) == 1
@test length(detect_ambiguities(Ambig9)) == 0

# issue #25341
module M25341
_totuple(::Type{Tuple{Vararg{E}}}, itr, s...) where {E} = E
end
@test length(detect_unbound_args(M25341; recursive=true)) == 1

# Test that Core and Base are free of UndefVarErrors
@testset "detect_unbound_args in Base and Core" begin
    # TODO: review this list and remove everything between test_broken and test
    let need_to_handle_undef_sparam =
            Set{Method}(detect_unbound_args(Core; recursive=true))
        @test isempty(need_to_handle_undef_sparam)
    end
    let need_to_handle_undef_sparam =
            Set{Method}(detect_unbound_args(Base; recursive=true, allowed_undefineds))
        pop!(need_to_handle_undef_sparam, which(Base._totuple, (Type{Tuple{Vararg{E}}} where E, Any, Any)))
        pop!(need_to_handle_undef_sparam, which(Base._eltype_ntuple, Tuple{Type{Tuple{Any}}}))
        pop!(need_to_handle_undef_sparam, which(Base.reduce_empty_iter, (Any, Tuple{Vararg{T}} where T, Base.HasEltype)))
        pop!(need_to_handle_undef_sparam, first(methods(Base.same_names)))
        @test_broken isempty(need_to_handle_undef_sparam)
        pop!(need_to_handle_undef_sparam, which(Base._cat, Tuple{Any, AbstractArray}))
        pop!(need_to_handle_undef_sparam, which(Base.float, Tuple{AbstractArray{Union{Missing, T},N} where {T, N}}))
        @test isempty(need_to_handle_undef_sparam)
    end
end

@testset "has_bottom_parameter with Union{} in tvar bound" begin
    @test Base.has_bottom_parameter(Ref{<:Union{}})
    @test Base.has_bottom_parameter(Core.TypeEgal{Ref{Union{}}})
end

# test a case where specificity is not transitive over subtyping
f35983(::T, ::T) where {T} = 1
f35983(::Type, ::Type) = 2
@test f35983(10, 12) == 1
@test f35983(Int32, Int32) == 2
@test f35983(Int32, Int64) == 2
@test f35983(Int32, Complex) == 2
@test only(Base.methods_including_ambiguous(f35983, (Type, Type))).sig == Tuple{typeof(f35983), Type, Type}
@test only(Base.methods(f35983, (Type, Type))).sig == Tuple{typeof(f35983), Type, Type}
@test length(Base.methods_including_ambiguous(f35983, (Any, Any))) == 2
@test first(Base.methods_including_ambiguous(f35983, (Any, Any))).sig == Tuple{typeof(f35983), Type, Type}
@test length(Base.methods(f35983, (Any, Any))) == 2
@test first(Base.methods(f35983, (Any, Any))).sig == Tuple{typeof(f35983), Type, Type}
let ambig = Ref{Int32}(0)
    ms = Base._methods_by_ftype(Tuple{typeof(f35983), Type, Type}, nothing, -1, Base.get_world_counter(), true, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
    @test ms isa Vector
    @test length(ms) == 1
    @test ambig[] == 0
end
f35983(::Type{Int16}, ::Any) = 3
@test length(Base.methods_including_ambiguous(f35983, (Type, Type))) == 2
@test length(Base.methods(f35983, (Type, Type))) == 1
let ambig = Ref{Int32}(0)
    ms = Base._methods_by_ftype(Tuple{typeof(f35983), Type, Type}, nothing, -1, Base.get_world_counter(), true, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
    @test ms isa Vector
    @test length(ms) == 2
    @test ambig[] == 1
end

struct B38280 <: Real; val; end
let ambig = Ref{Int32}(0)
    ms = Base._methods_by_ftype(Tuple{Type{B38280}, Any}, nothing, 1, Base.get_world_counter(), false, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
    @test ms isa Vector
    @test length(ms) == 1
    @test ambig[] == 1
end

fnoambig(::Int,::Int) = 1
fnoambig(::Int,::Any) = 2
fnoambig(::Any,::Int) = 3
fnoambig(::Any,::Any) = 4
let has_ambig = Ref(Int32(0))
    ms = Base._methods_by_ftype(Tuple{typeof(fnoambig), Any, Any}, nothing, 4, Base.get_world_counter(), false, Ref(typemin(UInt)), Ref(typemax(UInt)), has_ambig)
    @test ms isa Vector
    @test length(ms) == 4
    @test has_ambig[] == 1 # 0 is better, but expensive and probably unnecessary to compute
end

# issue #11407
f11407(::Dict{K,V}, ::Dict{Any,V}) where {K,V} = 1
f11407(::Dict{K,V}, ::Dict{K,Any}) where {K,V} = 2
@test_throws MethodError f11407(Dict{Any,Any}(), Dict{Any,Any}()) # ambiguous
@test f11407(Dict{Any,Int}(), Dict{Any,Int}()) == 1
@test_warn "declares type variable V but does not use it" @eval f11407(::Dict{Any,Any}, ::Dict{Any,Any}) where {K,V} = 3
@test f11407(Dict{Any,Any}(), Dict{Any,Any}()) == 3

# issue #12814
abstract type A12814{N, T} end
struct B12814{N, T} <: A12814{N, T}
    x::NTuple{N, T}
end
(::Type{T})(x::X) where {T <: A12814, X <: Array} = 1
@test_throws MethodError B12814{3, Float64}([1, 2, 3]) # ambiguous
@test B12814{3,Float64}((1, 2, 3)).x === (1.0, 2.0, 3.0)

# issue #43040
module M43040
   using Test
   struct C end
   @test_warn "declares type variable T but does not use it" @eval M43040 stripType(::Type{C}) where {T} = C # where {T} is intentionally incorrect
end

@test isempty(detect_ambiguities(M43040; recursive=true))

cc46601(T::Type{<:Core.IntrinsicFunction}, x) = 1
cc46601(::Type{T}, x::Number) where {T<:AbstractChar} = 2
cc46601(T::Type{<:Nothing}, x) = 3
cc46601(::Type{T}, x::T) where {T<:Number} = 4
cc46601(::Type{T}, arg) where {T<:VecElement} = 5
cc46601(::Type{T}, x::Number) where {T<:Number} = 6
@test length(methods(cc46601, Tuple{Type{<:Integer}, Integer})) == 2
@test length(Base.methods_including_ambiguous(cc46601, Tuple{Type{<:Integer}, Integer})) == 6
cc46601(::Type{T}, x::Int) where {T<:AbstractString} = 7
@test length(methods(cc46601, Tuple{Type{<:Integer}, Integer})) == 2
@test length(Base.methods_including_ambiguous(cc46601, Tuple{Type{<:Integer}, Integer})) == 7

# Issue #55231
struct U55231{P} end
struct V55231{P} end
U55231(::V55231) = nothing
(::Type{T})(::V55231) where {T<:U55231} = nothing
@test length(methods(U55231)) == 1
U55231(a, b) = nothing
@test length(methods(U55231)) == 2
struct S55231{P} end
struct T55231{P} end
(::Type{T})(::T55231) where {T<:S55231} = nothing
S55231(::T55231) = nothing
@test length(methods(S55231)) == 1
S55231(a, b) = nothing
@test length(methods(S55231)) == 2

ambig10() = 1
ambig10(a::Vararg{Any}) = 2
ambig10(a::Vararg{Union{Int32,Int64}}) = 6
ambig10(a::Vararg{Matrix}) = 4
ambig10(a::Vararg{Number}) = 7
ambig10(a::Vararg{N}) where {N<:Number} = 5
let ambig = Ref{Int32}(0)
    ms = Base._methods_by_ftype(Tuple{typeof(ambig10), Vararg}, nothing, -1, Base.get_world_counter(), false, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
    @test ms isa Vector
    @test length(ms) == 6
    @test_broken ambig[] == 0
end
let ambig = Ref{Int32}(0)
    ms = Base._methods_by_ftype(Tuple{typeof(ambig10), Vararg{Number}}, nothing, -1, Base.get_world_counter(), false, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
    @test ms isa Vector
    @test length(ms) == 4
    @test_broken ambig[] == 0
    @test ms[1].method === which(ambig10, ())
    @test ms[2].method === which(ambig10, (Vararg{Union{Int32, Int64}},))
    @test ms[3].method === which(ambig10, Tuple{Vararg{N}} where N<:Number,)
    @test ms[4].method === which(ambig10, (Vararg{Number},))
end

# issue #62262: an ambiguity can be resolved by the *union* of several more
# specific methods, without any single method covering the intersection
module Ambig62262
abstract type MyAbstractMat end
struct MatA <: MyAbstractMat end
struct MatB <: MyAbstractMat end
struct SpecialMat <: MyAbstractMat end
h(::MyAbstractMat, ::SpecialMat) = 1
h(::Union{MatA,MatB}, ::MyAbstractMat) = 2
h(::MatA, ::SpecialMat) = 3
h(::MatB, ::SpecialMat) = 4
end
@test Ambig62262.h(Ambig62262.MatA(), Ambig62262.SpecialMat()) == 3
@test Ambig62262.h(Ambig62262.MatB(), Ambig62262.SpecialMat()) == 4
@test isempty(detect_ambiguities(Ambig62262))

# ... but since `morespecific` is not transitive, a method that is more specific
# than both ambiguous methods does not resolve them if it loses back to one of
# them through a specificity cycle (here two steps long)
module AmbigUnionCycle
abstract type LikeSigned end
struct LikeInt <: LikeSigned end
abstract type LikeString end
struct LikeStr <: LikeString end
struct LikeMissing end
s(x::Union{LikeSigned,LikeMissing}, y::LikeInt...) = 1            # m1
s(x::Union{LikeInt,LikeString,LikeMissing}, y::LikeSigned...) = 2 # m2
s(x::T, y::T...) where {T<:Union{LikeInt,LikeString}} = 3         # covers the LikeInt part, but loses to method 5 (below)
s(x::LikeMissing, y::LikeInt...) = 4                              # genuinely covers the LikeMissing part
s(x::Union{LikeSigned,LikeStr}, y::LikeStr...) = 5               # more specific than method 3
s(x::LikeSigned, y::LikeStr...) = 6                              # more specific than 5; method 2 is more specific than this
end
let m1 = which(AmbigUnionCycle.s, Tuple{Union{AmbigUnionCycle.LikeSigned,AmbigUnionCycle.LikeMissing}, Vararg{AmbigUnionCycle.LikeInt}}),
    m2 = which(AmbigUnionCycle.s, Tuple{Union{AmbigUnionCycle.LikeInt,AmbigUnionCycle.LikeString,AmbigUnionCycle.LikeMissing}, Vararg{AmbigUnionCycle.LikeSigned}})
    # method 3 "covers" the LikeInt part but is in a specificity cycle (2 ≻ 6 ≻ 5 ≻ 3 ≻ 2)
    @test Base.isambiguous(m1, m2)
end
@test_throws MethodError AmbigUnionCycle.s(AmbigUnionCycle.LikeInt()) # genuinely ambiguous
@test AmbigUnionCycle.s(AmbigUnionCycle.LikeMissing()) == 4
@test AmbigUnionCycle.s(AmbigUnionCycle.LikeInt(), AmbigUnionCycle.LikeInt()) == 3

# the transitive loser rejection in `isambiguous` is region-blind: a candidate
# resolver may be rejected because a loser is `morespecific` than it globally,
# even when the loser's overlap with it inside the intersection is itself
# resolved by other methods. Here dispatch resolves every point of the
# intersection of methods 1 and 2 (methods 7 and 8 rescue the only points where
# the loser chain 2 ≻ 6 ≻ 5 ≻ 3 overlaps method 3). The region-aware
# union-coverage resolution in `ml_matches` (gf.c) sees that the cycle members
# are fully covered over this query before they can disqualify method 3, so the
# pair is correctly reported as resolved.
module AmbigLoserRegion
abstract type LikeSigned end
struct LikeInt <: LikeSigned end
abstract type LikeString end
struct LikeStr <: LikeString end
struct LikeMissing end
s(x::Union{LikeSigned,LikeMissing}, y::LikeInt...) = 1             # m1
s(x::Union{LikeInt,LikeString,LikeMissing}, y::LikeSigned...) = 2  # m2
s(x::T, y::T...) where {T<:Union{LikeInt,LikeString}} = 3          # resolves the (LikeInt, LikeInt...) region
s(x::LikeMissing, y::LikeInt...) = 4                               # resolves the LikeMissing region
s(x::Union{LikeSigned,LikeStr,LikeMissing}, y::LikeStr...) = 5     # more specific than method 3
s(x::Union{LikeSigned,LikeMissing}, y::LikeStr...) = 6             # more specific than 5; method 2 is more specific than this
s(x::LikeInt) = 7                                                  # resolves the arity-1 LikeInt point
s(x::LikeMissing) = 8                                              # resolves the arity-1 LikeMissing point
end
# dispatch is fully resolved over the intersection of methods 1 and 2 ...
@test AmbigLoserRegion.s(AmbigLoserRegion.LikeInt()) == 7
@test AmbigLoserRegion.s(AmbigLoserRegion.LikeMissing()) == 8
@test AmbigLoserRegion.s(AmbigLoserRegion.LikeInt(), AmbigLoserRegion.LikeInt()) == 3
@test AmbigLoserRegion.s(AmbigLoserRegion.LikeMissing(), AmbigLoserRegion.LikeInt()) == 4
let m1 = which(AmbigLoserRegion.s, Tuple{Union{AmbigLoserRegion.LikeSigned,AmbigLoserRegion.LikeMissing}, Vararg{AmbigLoserRegion.LikeInt}}),
    m2 = which(AmbigLoserRegion.s, Tuple{Union{AmbigLoserRegion.LikeInt,AmbigLoserRegion.LikeString,AmbigLoserRegion.LikeMissing}, Vararg{AmbigLoserRegion.LikeSigned}})
    @test !Base.isambiguous(m1, m2)
end

# complement of #62262: if the more specific methods only cover *part* of the
# intersection, the uncovered part is still a genuine ambiguity (so the union
# check must not over-resolve)
module Ambig62262Partial
abstract type MyAbstractMat end
struct MatA <: MyAbstractMat end
struct MatB <: MyAbstractMat end
struct SpecialMat <: MyAbstractMat end
h(::MyAbstractMat, ::SpecialMat) = 1
h(::Union{MatA,MatB}, ::MyAbstractMat) = 2
h(::MatA, ::SpecialMat) = 3  # covers only the MatA part of the intersection
end
@test length(detect_ambiguities(Ambig62262Partial)) == 1
@test Ambig62262Partial.h(Ambig62262Partial.MatA(), Ambig62262Partial.SpecialMat()) == 3          # covered -> resolved
@test_throws MethodError Ambig62262Partial.h(Ambig62262Partial.MatB(), Ambig62262Partial.SpecialMat()) # uncovered -> ambiguous

# a 3-way (non-transitive) specificity cycle is a real dispatch ambiguity that
# `_methods_by_ftype` reports via `has_ambig`, even though every *pair* of the
# three methods has a clear winner
module AmbigCycle3
f(::T, ::Vararg{T}) where {T<:Integer} = 1    # mT
f(::Integer, ::Vararg{String}) = 2            # mStr
f(::Integer, ::Vararg{Union{Int,String}}) = 3 # mU
end
let mT   = which(AmbigCycle3.f, Tuple{T, Vararg{T}} where T<:Integer),
    mStr = which(AmbigCycle3.f, Tuple{Integer, Vararg{String}}),
    mU   = which(AmbigCycle3.f, Tuple{Integer, Vararg{Union{Int,String}}})
    # specificity cycle mT ≻ mStr ≻ mU ≻ mT: no unique most specific method
    @test Base.morespecific(mT, mStr) && Base.morespecific(mStr, mU) && Base.morespecific(mU, mT)
    @test !(Base.morespecific(mStr, mT) || Base.morespecific(mU, mStr) || Base.morespecific(mT, mU))
    # every pair is pairwise ordered, but each pair still participates in the
    # unresolved cycle over their shared region, so all are ambiguous in context
    @test Base.isambiguous(mT, mStr)
    @test Base.isambiguous(mStr, mU)
    @test Base.isambiguous(mU, mT)
end
@test_throws MethodError AmbigCycle3.f(3) # genuinely ambiguous in dispatch
let ambig = Ref{Int32}(0)
    Base._methods_by_ftype(Tuple{typeof(AmbigCycle3.f), Int}, nothing, -1, Base.get_world_counter(), true, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
    @test ambig[] == 1
end
# querying a single method signature must also report the cycle (`has_ambig`):
# inference consumes that result, and effects for a signature covering the
# throwing call `f(3)` above must not be inferred `:nothrow`
@test !Base.infer_effects(AmbigCycle3.f, Tuple{Integer, Vararg{Union{Int,String}}}).nothrow
let ambig = Ref{Int32}(0)
    Base._methods_by_ftype(Tuple{typeof(AmbigCycle3.f), Integer, Vararg{Union{Int,String}}}, nothing, -1, Base.get_world_counter(), true, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
    @test ambig[] == 1
end
# `detect_ambiguities` queries each method signature on its own, where the
# cycle now surfaces via `has_ambig`, so pairwise detection reports the 3-way
# ambiguity
@test !isempty(detect_ambiguities(AmbigCycle3))

# same 3-way cycle, but defined in the order that used to trigger the
# `LATEST_ONLY` slurp early-out in `get_intersect_visitor`: `mU` is inserted
# before `mStr`, so when `mStr` (a strict subtype of `mU`, which dominates `mT`
# and is thus `METHOD_SIG_LATEST_ONLY`) is added, the scan used to stop after
# recording only the `(mStr, mU)` pair and never record that `mT` is
# morespecific than `mStr`. That omission left `mStr`'s interference set empty,
# so the cycle was invisible and `f(1)` silently dispatched to `mStr` instead of
# raising an ambiguity. Insertion order must not change the observed relation.
module AmbigCycle3Reorder
f(::T, ::Vararg{T}) where {T<:Integer} = 1    # mT
f(::Integer, ::Vararg{Union{Int,String}}) = 3 # mU  (inserted before mStr; dominates mT)
f(::Integer, ::Vararg{String}) = 2            # mStr (⊊ mU)
end
let mT   = which(AmbigCycle3Reorder.f, Tuple{T, Vararg{T}} where T<:Integer),
    mStr = which(AmbigCycle3Reorder.f, Tuple{Integer, Vararg{String}}),
    mU   = which(AmbigCycle3Reorder.f, Tuple{Integer, Vararg{Union{Int,String}}})
    @test Base.morespecific(mT, mStr) && Base.morespecific(mStr, mU) && Base.morespecific(mU, mT)
    @test !(Base.morespecific(mStr, mT) || Base.morespecific(mU, mStr) || Base.morespecific(mT, mU))
    @test Base.isambiguous(mT, mStr)
    @test Base.isambiguous(mStr, mU)
    @test Base.isambiguous(mU, mT)
end
@test_throws MethodError AmbigCycle3Reorder.f(1) # genuinely ambiguous in dispatch
let ambig = Ref{Int32}(0)
    Base._methods_by_ftype(Tuple{typeof(AmbigCycle3Reorder.f), Int}, nothing, -1, Base.get_world_counter(), true, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
    @test ambig[] == 1
end
@test !isempty(detect_ambiguities(AmbigCycle3Reorder))

nothing

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

module UnboundDetect
    # some matching calls leave the parameter unbound (with example given)
    unbound1(x::Type{<:T}) where {T} = T                       # f(Union{})
    unbound2(x::Vector{<:T}) where {T} = T                     # f(Vector{Union{}}())
    unbound3(x::T) where {T>:Int} = T                          # f(2.0)
    unbound4(x::Vector{Union{T,Int}}) where {T} = T            # f(Int[])
    unbound5(x::S) where {T, S<:Union{T,Int}} = T              # f(1)
    unbound6(x::S) where {T, S<:Tuple{Vararg{T}}} = T          # f(())
    unbound7(x::Vector{S}) where {T, S<:AbstractVector{T}} = T # f(Union{}[])
    unbound8(x::Type{Union{T,Missing}}) where {T} = T          # f(Missing)
    unbound9(x::Ref{Vector{<:T}}) where {T} = T                # f(Ref{Vector}(...))
    unbound10(x::Type{<:T}, y::Type{<:S}) where {T, S} = T     # f(Union{}, Int): S is never read, but T is
    unbound13(x::Type{<:T}) where {T} = @isdefined(T) ? T : 0
    unused1(x::Type{<:T}) where {T} = 0
    unused2(x::Type{<:T}) where {T} = @isdefined(T)
    # every matching call pins the parameter
    bound1(x::T) where {T} = T
    bound2(x::Type{T}) where {T} = T
    bound3(x::S) where {T, S<:AbstractVector{T}} = T
    bound4(x::Vector{T}) where {T>:Int} = T
    bound5(x::T, y::T) where {T} = T
    bound6(x::Tuple{Vararg{Int,N}}) where {N} = N
    bound7(x::Union{Vector{T}, Ref{T}}) where {T} = T
    bound8(x::Ref{Vector{T}}) where {T} = T
    bound9(x::Tuple{<:T}) where {T} = T
    # issue #58427: every match pins `T`, but a `Vector{Missing}` argument
    # does so only by absorbing the `T` arm as `T = Union{}`
    unbound14(x::Vector{Union{Missing, T}}) where {T<:Real} = T
    # issue #59023: the `Vector{T}` arm cannot be absorbed into `Nothing`,
    # so every match genuinely pins `T` — an accepted false positive: the
    # arm-must-be-exposed refinement is deliberately not implemented
    unbound15(x::Type{Union{Nothing, Vector{T}}}) where {T} = T
    # the calls that would leave the parameter unbound all dispatch to a more
    # specific method
    shadowed1(x::Type{<:AbstractArray{T}}) where {T} = T
    shadowed1(x::Type{Union{}}) = 0
    shadowed2(x::Int, y::T...) where {T} = T
    shadowed2(x::Int) = 0
    shadowed3(x::Type{A}) where {T, A<:AbstractArray{T}} = T
    shadowed3(x::Type{Union{}}) = 0
    # with the `Union{}` member shadowed, any other value of the range
    # variable pins `T` through its bound (even an abstract one)
    shadowed4(x::Type{<:T}) where {T} = T
    shadowed4(x::Type{Union{}}) = 0
    # a `Union{}` shadow does not help when other calls also leave the
    # parameter unbound (here: `f(Int)` never touches `T`)
    notshadowed1(x::Type{<:Union{T,Int}}) where {T} = T
    notshadowed1(x::Type{Union{}}) = 0
    # when the range variable occurs in another slot, the `Type{Union{}}`
    # probe signature is not covered by the method itself, so the lookup
    # finding the broad fallback proves nothing: `f(Union{}, Ref{Union{}}())`
    # still dispatches to the `where`-method and leaves `T` unbound
    notshadowed2(x::Type{A}, y::Ref{A}) where {T, A<:AbstractArray{T}} = T
    notshadowed2(x::Type, y::Any) = 0
    # a shadowed slot pins `T` only through a bare lower-bound occurrence,
    # which is indefinite when `T`'s declared lower bound is not `Union{}`
    # (`f(Float64)` leaves `T` unbound: the least solution unions in `Int`)
    notshadowed3(x::Type{<:T}) where {T>:Int} = T
    notshadowed3(x::Type{Union{}}) = 0
    # a zero-length-vararg shadow rescues chained bounds too, but not when a
    # nonempty call can still leave the chain unpinned (`f(1, Ref{Union{}}())`)
    shadowed5(x::Int, y::S...) where {T, U<:T, S<:U} = T
    shadowed5(x::Int) = 0
    notshadowed4(x::Int, y::Ref{S}...) where {T, U<:T, S<:U} = T
    notshadowed4(x::Int) = 0
    # a parameter of an argument's constructor that is also the declared type
    # of one of its always-initialized fields cannot be `Union{}` (no
    # instance would exist), so its bound pins T; without such a field it
    # can, and T may be unbound
    struct WithField{S, A<:Tuple}
        x::A
    end
    struct WithoutField{S, A<:Tuple} end
    fieldpins(x::WithField{<:Any, <:Tuple{Ref{Type{T}}, Vararg{Any}}}) where {T} = T
    nofieldpins(x::WithoutField{<:Any, <:Tuple{Ref{Type{T}}, Vararg{Any}}}) where {T} = T
    # an incomplete `new` inner constructor can leave the field `#undef`, so
    # `Incomplete{Union{}}()` is constructible and `T` may be unbound
    mutable struct Incomplete{A}
        x::A
        Incomplete{A}() where {A} = new()
    end
    unbound11(x::Incomplete{<:T}) where {T} = T                # f(Incomplete{Union{}}())
    # a field-pinned constructor parameter contributes only a lower bound,
    # indefinite when `T`'s declared lower bound is not `Union{}`
    unbound12(x::WithField{<:Any, <:T}) where {T>:Tuple{}} = T # f(WithField{1,Tuple{Int}}((1,)))
    # issue #54893: `Foo54893(1.0)` leaves `T` unbound; the constructor
    # signature spells `Type{Foo54893}` with the struct's own variable
    # object, which must not be mistaken for an occurrence of `T`
    struct Foo54893{T>:Int}
        x::T
    end
end
let unbound = Set{Method}(detect_unbound_args(UnboundDetect))
    tested = 0
    for name in names(UnboundDetect; all=true)
        startswith(String(name), '#') && continue
        f = getglobal(UnboundDetect, name)
        f isa Function || continue
        ms = [m for m in methods(f, UnboundDetect) if m.sig isa UnionAll]
        isempty(ms) && continue
        m = only(ms)
        should_flag = startswith(String(name), "unbound") ||
                      startswith(String(name), "notshadowed") ||
                      name === :nofieldpins
        @test (m in unbound) == should_flag context=name
        tested += 1
    end
    @test tested == 37
    let ms = filter(m -> m.sig isa UnionAll, collect(methods(UnboundDetect.Foo54893)))
        @test only(ms) in unbound
    end
end

# Test that Core and Base are free of UndefVarErrors
@testset "detect_unbound_args in Base and Core" begin
    let need_to_handle_undef_sparam =
            Set{Method}(detect_unbound_args(Core; recursive=true))
        @test isempty(need_to_handle_undef_sparam)
    end
    let need_to_handle_undef_sparam =
            Set{Method}(detect_unbound_args(Base; recursive=true, allowed_undefineds))
        # reviewed and expected
        expected_undef_sparam = Any[
            Tuple{typeof(Base._totuple), Type{Tuple{Vararg{E}}}, Any, Vararg{Any}} where E,
            # the raw reads are `@isdefined`-guarded and safe at runtime, but
            # verifying that the guard dominates the read would need dataflow
            Tuple{typeof(Base._eltype_ntuple), Type{<:Tuple{Vararg{E}}}} where E,
            Tuple{typeof(Base.reduce_empty_iter), Any, Tuple{Vararg{T}}, Base.HasEltype} where T,
            # `T` is unbound only for element type `Missing`, whose calls dispatch to the more specific `float(::AbstractArray{Missing})`
            Tuple{typeof(float), AbstractArray{Union{Missing, T}}} where T,
            # `N` is unbound only for element type `Union{}`, whose calls are dispatch-ambiguous with the `<:ScalarIndex` and `<:AbstractCartesianIndex{0}` methods, erroring before the body
            Tuple{typeof(Base._trimmedindex), AbstractArray{<:Base.AbstractCartesianIndex{N}}} where N,
            Tuple{typeof(Base._trimmedshape), AbstractArray{<:Base.AbstractCartesianIndex{N}}, Vararg{Any}} where N,
            # `N` is unbound only for `Flatten{Union{}}`, whose calls are dispatch-ambiguous with the `I<:NamedTuple` method
            Tuple{typeof(eltype), Type{Base.Iterators.Flatten{I}}} where {N, I<:NTuple{N, Any}},
            # the sparams are unbound only for the argument `Union{}`, whose calls are dispatch-ambiguous among these four methods
            Tuple{typeof(similar), Type{<:Base.CodeUnits{T}}, NTuple{N, Int} where N} where T,
            Tuple{typeof(similar), Type{TA}, NTuple{N, Int} where N} where {T, N, O, P, TA<:Base.ReinterpretArray{T, N, O, P}},
            Tuple{typeof(similar), Type{TA}, NTuple{N, Int} where N} where {T, N, P, TA<:Base.ReshapedArray{T, N, P}},
            Tuple{typeof(similar), Type{TA}, NTuple{N, Int} where N} where {T, N, P, TA<:SubArray{T, N, P}},
        ]
        for sig in expected_undef_sparam
            m = which(sig)
            @test m in need_to_handle_undef_sparam context=sig
            delete!(need_to_handle_undef_sparam, m)
        end
        @test_broken isempty(need_to_handle_undef_sparam)
        # TODO: not yet investigated or fixed — review this list and empty
        # it, e.g. by adding a `::Type{Union{}}` method that shadows the
        # problematic calls
        todo_undef_sparam = Any[
            # each entry notes an example call that throws UndefVarError from the body
            Tuple{Type{Base.IteratorEltype}, Type{Base.Iterators.ProductIterator{T}}} where {N, T<:NTuple{N, Any}}, # IteratorEltype(ProductIterator{Union{}})
            Tuple{Type{Base.IteratorEltype}, Type{Base.Iterators.Zip{Is}}} where {N, Is<:NTuple{N, Any}}, # IteratorEltype(Zip{Union{}})
            Tuple{Type{Base.IteratorSize}, Type{Base.Iterators.ProductIterator{T}}} where {N, T<:NTuple{N, Any}}, # IteratorSize(ProductIterator{Union{}})
            Tuple{Type{Base.IteratorSize}, Type{Base.Iterators.Zip{Is}}} where {N, Is<:NTuple{N, Any}}, # IteratorSize(Zip{Union{}})
            Tuple{typeof(Base._cat), Any, Vararg{AbstractArray{T}}} where T, # cat(dims=Val(1)): only the `catdim::Int` zero-array case is shadowed
            Tuple{typeof(Base._counttuple), Type{<:NTuple{N, Any}}} where N, # _counttuple(Union{})
            Tuple{typeof(Base.Broadcast._maxndims), Type{<:Tuple{T, Vararg}}} where T, # _maxndims(Union{})
            Tuple{typeof(Base._nt_names), Type{T}} where {names, T<:NamedTuple{names}}, # _nt_names(Union{})
            Tuple{typeof(Base.Iterators._prod_eltype), Type{I}} where {N, I<:NTuple{N, Any}}, # eltype(ProductIterator{Union{}})
            Tuple{typeof(Base.Enums.basetype), Type{<:Enum{T}}} where T<:Integer, # basetype(Union{})
            Tuple{typeof(eltype), Type{<:Base.RSplitIterator{<:SubString{T}}}} where T, # eltype(RSplitIterator{Union{}})
            Tuple{typeof(eltype), Type{<:Base.SplitIterator{<:SubString{T}}}} where T, # eltype(SplitIterator{Union{}})
            Tuple{typeof(eltype), Type{Base.Iterators.Zip{Is}}} where {N, Is<:NTuple{N, Any}}, # eltype(Zip{Union{}})
            Tuple{typeof(ndims), Type{<:Base.Broadcast.Broadcasted{<:Any, <:NTuple{N, Any}}}} where N, # ndims(Broadcasted{DefaultArrayStyle{1}, Union{}})
            Tuple{typeof(ndims), Type{<:Base.Broadcast.Broadcasted{<:Base.Broadcast.AbstractArrayStyle{N}, Nothing}}} where N, # ndims(Broadcasted{Union{}, Nothing})
        ]
        for sig in todo_undef_sparam
            m = which(sig)
            @test m in need_to_handle_undef_sparam context=sig
            delete!(need_to_handle_undef_sparam, m)
        end
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

nothing

# review of the de Bruijn refactor: `has_bottom_parameter` must account for a
# binder's `Union{}` bound per occurrence, not once at the wrapper, so a
# `Union` arm without the variable keeps the intersection inhabited
module AmbigBottomBound
const C = Union{Int, Vector{T}} where T<:Union{}
f(::Type{C}, ::Number,  ::Integer) = 1
f(::Type{C}, ::Integer, ::Number)  = 2
end
let ms = collect(methods(AmbigBottomBound.f))
    @test !Base.has_bottom_parameter(AmbigBottomBound.C)
    @test Base.isambiguous(ms[1], ms[2])
end

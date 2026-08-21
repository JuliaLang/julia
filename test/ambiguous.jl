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
    # all three matches are required as witnesses: `Union{AbstractIrrational, Int}` is
    # morespecific than `Signed`, so without `Union{typeof(pi), Integer}`
    # (unordered with it) the report would read as resolved at `(Int,)`,
    # where the call is really ambiguous
    @test length(Base.methods_including_ambiguous(f, (Signed,))) == 3
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
    unbound16(x::Type{<:Ref{<:Vector{T}}}) where {T} = T       # f(Ref{Union{}})
    unbound17(x::Vector{Ref{S}}) where {T, S<:T} = T           # f(Vector{Ref{Union{}}}())
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
let unbound = Set{Method}(detect_unbound_args(UnboundDetect; ambiguous_bottom=true)),
    unbound_nobottom = Set{Method}(detect_unbound_args(UnboundDetect))
    # parameters left unbound only by calls with `Union{}` type parameters
    bottom_only = (:unbound1, :unbound2, :unbound7, :unbound10, :unbound11, :unbound13,
                   :unbound16, :unbound17, :notshadowed2, :notshadowed4, :nofieldpins)
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
        @test (m in unbound_nobottom) == (should_flag && name ∉ bottom_only) context=name
        tested += 1
    end
    @test tested == 39
    let ms = filter(m -> m.sig isa UnionAll, collect(methods(UnboundDetect.Foo54893)))
        @test only(ms) in unbound
        @test only(ms) in unbound_nobottom
    end
end

# Test that Core and Base are free of UndefVarErrors
@testset "detect_unbound_args in Base and Core" begin
    let need_to_handle_undef_sparam =
            Set{Method}(detect_unbound_args(Core; recursive=true, ambiguous_bottom=true))
        @test isempty(need_to_handle_undef_sparam)
    end
    let need_to_handle_undef_sparam =
            Set{Method}(detect_unbound_args(Base; recursive=true, allowed_undefineds))
        # the parameters left unbound by non-`Union{}` calls (see the reviewed list below)
        expected_undef_sparam = Any[
            Tuple{typeof(Base._totuple), Type{Tuple{Vararg{E}}}, Any, Vararg{Any}} where E,
            Tuple{typeof(Base._eltype_ntuple), Type{<:Tuple{Vararg{E}}}} where E,
            Tuple{typeof(Base.reduce_empty_iter), Any, Tuple{Vararg{T}}, Base.HasEltype} where T,
            Tuple{typeof(float), AbstractArray{Union{Missing, T}}} where T,
            Tuple{typeof(Base._cat), Any, Vararg{AbstractArray{T}}} where T,
        ]
        for sig in expected_undef_sparam
            m = which(sig)
            @test m in need_to_handle_undef_sparam context=sig
            delete!(need_to_handle_undef_sparam, m)
        end
        @test isempty(need_to_handle_undef_sparam)
    end
    let need_to_handle_undef_sparam =
            Set{Method}(detect_unbound_args(Base; recursive=true, ambiguous_bottom=true, allowed_undefineds))
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

# issue #62262: an ambiguity can be resolved by the union of several more
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
s(x::T, y::T...) where {T<:Union{LikeInt,LikeString}} = 3         # m3 covers the LikeInt part, but loses to method 5 (below)
s(x::LikeMissing, y::LikeInt...) = 4                              # m4 covers the LikeMissing part
s(x::Union{LikeSigned,LikeStr}, y::LikeStr...) = 5                # m5 more specific than method 3
s(x::LikeSigned, y::LikeStr...) = 6                               # m6 more specific than 5; method 2 is more specific than this
end
let m1 = which(AmbigUnionCycle.s, Tuple{Union{AmbigUnionCycle.LikeSigned,AmbigUnionCycle.LikeMissing}, Vararg{AmbigUnionCycle.LikeInt}}),
    m2 = which(AmbigUnionCycle.s, Tuple{Union{AmbigUnionCycle.LikeInt,AmbigUnionCycle.LikeString,AmbigUnionCycle.LikeMissing}, Vararg{AmbigUnionCycle.LikeSigned}})
    # method 3 "covers" the LikeInt part but is in a specificity cycle (2 ≻ 6 ≻ 5 ≻ 3 ≻ 2)
    @test Base.isambiguous(m1, m2)
end
@test_throws MethodError AmbigUnionCycle.s(AmbigUnionCycle.LikeInt())
@test AmbigUnionCycle.s(AmbigUnionCycle.LikeMissing()) == 4
@test AmbigUnionCycle.s(AmbigUnionCycle.LikeInt(), AmbigUnionCycle.LikeInt()) == 3

# the transitive loser rejection in `isambiguous` is region-blind: a candidate
# resolver may be rejected because a loser is `morespecific` than it globally,
# even when the loser's overlap with it inside the intersection is itself
# resolved by other methods. Here dispatch resolves every point of the
# intersection of methods 1 and 2 (methods 7 and 8 cover the only points where
# the loser chain 2 ≻ 6 ≻ 5 ≻ 3 overlaps method 3). The region-aware
# union-coverage resolution computes that the cycle members are fully covered
# over this query before they can disqualify method 3, so the pair is correctly
# reported as resolved.
module AmbigLoserRegion
abstract type LikeSigned end
struct LikeInt <: LikeSigned end
abstract type LikeString end
struct LikeStr <: LikeString end
struct LikeMissing end
s(x::Union{LikeSigned,LikeMissing}, y::LikeInt...) = 1             # m1
s(x::Union{LikeInt,LikeString,LikeMissing}, y::LikeSigned...) = 2  # m2
s(x::T, y::T...) where {T<:Union{LikeInt,LikeString}} = 3          # m3 resolves the (LikeInt, LikeInt...) region
s(x::LikeMissing, y::LikeInt...) = 4                               # m4 resolves the LikeMissing region
s(x::Union{LikeSigned,LikeStr,LikeMissing}, y::LikeStr...) = 5     # m5 more specific than method 3
s(x::Union{LikeSigned,LikeMissing}, y::LikeStr...) = 6             # m6 more specific than 5; method 2 is more specific than this
s(x::LikeInt) = 7                                                  # m7 resolves the arity-1 LikeInt point
s(x::LikeMissing) = 8                                              # m8 resolves the arity-1 LikeMissing point
end
# dispatch is fully resolved over the intersection of methods 1 and 2
@test AmbigLoserRegion.s(AmbigLoserRegion.LikeInt()) == 7
@test AmbigLoserRegion.s(AmbigLoserRegion.LikeMissing()) == 8
@test AmbigLoserRegion.s(AmbigLoserRegion.LikeInt(), AmbigLoserRegion.LikeInt()) == 3
@test AmbigLoserRegion.s(AmbigLoserRegion.LikeMissing(), AmbigLoserRegion.LikeInt()) == 4
let m1 = which(AmbigLoserRegion.s, Tuple{Union{AmbigLoserRegion.LikeSigned,AmbigLoserRegion.LikeMissing}, Vararg{AmbigLoserRegion.LikeInt}}),
    m2 = which(AmbigLoserRegion.s, Tuple{Union{AmbigLoserRegion.LikeInt,AmbigLoserRegion.LikeString,AmbigLoserRegion.LikeMissing}, Vararg{AmbigLoserRegion.LikeSigned}})
    @test !Base.isambiguous(m1, m2)
end

# the order the dominating covers are consulted in must not affect the answer:
# here the first recorded cover of method 1 (method 2) fails the dominance
# transfer because the mutual partner (method 3) beats it, but the later cover
# (method 4) certifies every removal, so the query is resolved and no ambiguity
# may be reported
module AmbigCoverRetry
abstract type LikeSigned end
struct LikeInt <: LikeSigned end
abstract type LikeString end
struct LikeStr <: LikeString end
struct LikeMissing end
struct Unrelated end
s(x::Union{LikeSigned,LikeMissing}, y::LikeInt...) = 1         # m1: mutual with m3
s(x::T, y::T...) where {T<:Union{LikeInt,LikeString}} = 2      # m2: first cover of m1, loses to m3
s(x::Union{LikeSigned,LikeStr,LikeMissing}, y::LikeStr...) = 3 # m3: mutual with m1, beats m2
s(x::LikeInt) = 4                                              # m4: beats m1, m2 and m3, certifying their removal
s(x::Unrelated) = 5                                            # keeps the query below from being fully covered
end
@test AmbigCoverRetry.s(AmbigCoverRetry.LikeInt()) == 4
@test AmbigCoverRetry.s(AmbigCoverRetry.Unrelated()) == 5
@test_throws MethodError AmbigCoverRetry.s(AmbigCoverRetry.LikeMissing()) # m1~m3 is genuinely ambiguous outside the query below
for include_ambiguous in (false, true)
    ambig = Ref{Int32}(0)
    matches = Base._methods_by_ftype(
        Tuple{typeof(AmbigCoverRetry.s), Union{AmbigCoverRetry.LikeInt, AmbigCoverRetry.Unrelated}},
        nothing, -1, Base.get_world_counter(), include_ambiguous,
        Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
    @test ambig[] == 0
    @test length(matches) == 2
    @test Set(m.method for m in matches) ==
        Set([which(AmbigCoverRetry.s, Tuple{AmbigCoverRetry.LikeInt}), which(AmbigCoverRetry.s, Tuple{AmbigCoverRetry.Unrelated})])
end

# a dominance-dropped match must still report the mutual pairs it witnesses even
# when the partner is the minmax method, which the sort never visits (so
# `check_fully_ambiguous` never runs for it): here method 2 is mutually
# ambiguous with method 1 (the minmax) and is dropped because method 3 covers
# it, but method 1 beats method 3, so removing method 2 leaves nothing blocking
# method 1 over the contested region -- the unordered-blockers scan in
# `check_dominance_transfer` is the only witness of this genuine ambiguity
module AmbigMinmaxPartner
abstract type LikeSigned end
struct LikeInt <: LikeSigned end
abstract type LikeString end
struct LikeStr <: LikeString end
struct LikeMissing end
s(x::LikeInt, y::LikeInt...) = 1                               # m1: minmax
s(x::Union{LikeInt,LikeMissing,LikeString}, y::LikeStr...) = 2 # m2: partial, mutual with m1
s(x::Union{LikeInt,LikeStr}, y::LikeInt...) = 3                # m3: m1 ≻ m3 ≻ m2 (non-transitive)
end
module AmbigMinmaxPartnerReorder # same set, m3 inserted before m2
abstract type LikeSigned end
struct LikeInt <: LikeSigned end
abstract type LikeString end
struct LikeStr <: LikeString end
struct LikeMissing end
s(x::LikeInt, y::LikeInt...) = 1                               # m1
s(x::Union{LikeInt,LikeStr}, y::LikeInt...) = 3                # m3
s(x::Union{LikeInt,LikeMissing,LikeString}, y::LikeStr...) = 2 # m2
end
for M in (AmbigMinmaxPartner, AmbigMinmaxPartnerReorder)
    m1 = which(M.s, Tuple{M.LikeInt, Vararg{M.LikeInt}})
    m2 = which(M.s, Tuple{Union{M.LikeInt,M.LikeMissing,M.LikeString}, Vararg{M.LikeStr}})
    m3 = which(M.s, Tuple{Union{M.LikeInt,M.LikeStr}, Vararg{M.LikeInt}})
    @test Base.morespecific(m1, m3) && Base.morespecific(m3, m2)
    @test !Base.morespecific(m3, m1) && !Base.morespecific(m2, m3)
    @test !Base.morespecific(m1, m2) && !Base.morespecific(m2, m1) # unordered
    @test_throws MethodError M.s(M.LikeInt())
    @test Base.isambiguous(m1, m2)
    for include_ambiguous in (false, true)
        ambig = Ref{Int32}(0)
        matches = Base._methods_by_ftype(
            Tuple{typeof(M.s), M.LikeInt, Vararg{M.LikeInt}}, nothing, -1,
            Base.get_world_counter(), include_ambiguous,
            Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test ambig[] == 1
        # under include_ambiguous the dropped method 2 is kept to witness the pair
        @test Set(m.method for m in matches) == (include_ambiguous ? Set([m1, m2]) : Set([m1]))
    end
end

# complement of #62262: if the more specific methods only cover part of the
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
@test Ambig62262Partial.h(Ambig62262Partial.MatA(), Ambig62262Partial.SpecialMat()) == 3
@test_throws MethodError Ambig62262Partial.h(Ambig62262Partial.MatB(), Ambig62262Partial.SpecialMat())

# a 3-way (non-transitive) specificity cycle is a real dispatch ambiguity that
# `_methods_by_ftype` reports via `has_ambig`, even though every pair of the
# three methods has a clear winner
module AmbigCycle3
f(::T, ::Vararg{T}) where {T<:Integer} = 1    # mT
f(::Integer, ::Vararg{String}) = 2            # mStr
f(::Integer, ::Vararg{Union{Int,String}}) = 3 # mU
end
let mT   = which(AmbigCycle3.f, Tuple{T, Vararg{T}} where T<:Integer),
    mStr = which(AmbigCycle3.f, Tuple{Integer, Vararg{String}}),
    mU   = which(AmbigCycle3.f, Tuple{Integer, Vararg{Union{Int,String}}})
    # specificity cycle mT ≻ mStr ≻ mU ≻ mT
    @test Base.morespecific(mT, mStr) && Base.morespecific(mStr, mU) && Base.morespecific(mU, mT)
    @test !(Base.morespecific(mStr, mT) || Base.morespecific(mU, mStr) || Base.morespecific(mT, mU))
    # every pair is pairwise ordered, but each pair still participates in the
    # unresolved cycle over their shared region, so all are ambiguous in context
    @test Base.isambiguous(mT, mStr)
    @test Base.isambiguous(mStr, mU)
    @test Base.isambiguous(mU, mT)
end
@test_throws MethodError AmbigCycle3.f(3)
let ambig = Ref{Int32}(0)
    Base._methods_by_ftype(Tuple{typeof(AmbigCycle3.f), Int}, nothing, -1, Base.get_world_counter(), true, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
    @test ambig[] == 1
end
@test !Base.infer_effects(AmbigCycle3.f, Tuple{Integer, Vararg{Union{Int,String}}}).nothrow
let ambig = Ref{Int32}(0)
    Base._methods_by_ftype(Tuple{typeof(AmbigCycle3.f), Integer, Vararg{Union{Int,String}}}, nothing, -1, Base.get_world_counter(), true, Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
    @test ambig[] == 1
end
@test !isempty(detect_ambiguities(AmbigCycle3))

# same 3-way cycle, but defined in reverse order
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

# Unit tests for the pkgimage edge verifier `Compiler.ReinferUtils.verify_call`,
# driven directly with `Method` edges: methods defined after the "recorded"
# expected set play the role of methods loaded between image build and image
# load, and the verifier must agree with what a fresh `ml_matches` would report.
module VerifyCallEdge
# The recorded state: sig below is union-split so that no single method fully
# covers it (no minmax match shields the sort), jointly covered by fE2 and fD.
f(::Integer, ::Vararg{Union{Int,String}}) = 1  # fE2: the Integer half
f(::Int, ::Vararg{String}) = 2                 # fC: concrete cover with an empty interference set
f(::Char, ::Vararg{Union{Int,String}}) = 3     # fD: the Char half, interferes with nothing
end
let verify_call = Base.Compiler.ReinferUtils.verify_call,
    method_in_interferences = Base.Compiler.ReinferUtils.method_in_interferences,
    f = VerifyCallEdge.f,
    mE2 = which(f, Tuple{Integer, Vararg{Union{Int,String}}}),
    mC = which(f, Tuple{Int, Vararg{String}}),
    mD = which(f, Tuple{Char, Vararg{Union{Int,String}}}),
    sig = Tuple{typeof(f), Union{Int,Char}, Vararg{Union{Int,String}}},
    expecteds = Core.svec(mE2, mC, mD)
    # the unchanged recorded state revalidates
    let (minw, maxw) = verify_call(sig, expecteds, 1, 3, Base.get_world_counter(), true, Any[])
        @test maxw == typemax(UInt)
    end
    # A newcomer that strictly beats an expected method but is resolved by the
    # empty-interference-set cover mC keeps the edge valid: the fresh sort prunes
    # it silently, and the interference fast path may accept it directly (the
    # `Union{}` bottom-slurp shape).
    VerifyCallEdge.eval(:(f(::Integer, ::Vararg{String}) = 4)) # N: mC ≻ N ≻ mE2
    mN = which(f, Tuple{Integer, Vararg{String}})
    @test Base.morespecific(mC, mN) && Base.morespecific(mN, mE2)
    @test isempty(mC.interferences) && typeintersect(sig, mN.sig) <: mC.sig
    let (minw, maxw) = verify_call(sig, expecteds, 1, 3, Base.get_world_counter(), true, Any[])
        @test maxw == typemax(UInt)
    end
end

# Same recorded shape, but the newcomers close a specificity cycle
# mE2 ≻ X ≻ N ≻ mE2 through X -- invisible to the scanned interference sets,
# since every expected method it intersects strictly beats it -- which must
# invalidate: the fresh sort drags mE2 into the SCC (where it stops being an
# admissible cover for X), reporting an extra match and an ambiguity. The fast
# path cannot see X itself; it has to notice that N gained a strict beater that
# could sit on a cycle and leave the decision to the full lookup.
module VerifyCallCycle
h(::Integer, ::Vararg{Union{Int,String}}) = 1  # hE2: the Integer half
h(::Int, ::Vararg{String}) = 2                 # hC: concrete cover with an empty interference set
h(::Char, ::Vararg{Union{Int,String}}) = 3     # hD: the Char half, interferes with nothing
end
let verify_call = Base.Compiler.ReinferUtils.verify_call,
    method_in_interferences = Base.Compiler.ReinferUtils.method_in_interferences,
    h = VerifyCallCycle.h,
    mE2 = which(h, Tuple{Integer, Vararg{Union{Int,String}}}),
    mC = which(h, Tuple{Int, Vararg{String}}),
    mD = which(h, Tuple{Char, Vararg{Union{Int,String}}}),
    sig = Tuple{typeof(h), Union{Int,Char}, Vararg{Union{Int,String}}},
    expecteds = Core.svec(mE2, mC, mD)
    VerifyCallCycle.eval(:(h(::T, ::Vararg{T}) where {T<:Integer} = 5)) # X
    VerifyCallCycle.eval(:(h(::Integer, ::Vararg{String}) = 4))         # N
    mX = which(h, Tuple{T, Vararg{T}} where T<:Integer)
    mN = which(h, Tuple{Integer, Vararg{String}})
    @test Base.morespecific(mC, mN) && Base.morespecific(mN, mE2)
    @test Base.morespecific(mE2, mX) && Base.morespecific(mX, mN)
    @test isempty(mC.interferences) && isempty(mD.interferences)
    @test !method_in_interferences(mX, mE2) # X is invisible to the scan
    @test typeintersect(sig, mN.sig) <: mC.sig
    # The ground truth the verifier must agree with. Note the fresh sort's
    # treatment of the cycle is sensitive to the match-list order, hence to the
    # definition order of the newcomers: defined the other way around, it
    # happens to prune the cycle cleanly. If a sort change makes this assertion
    # fail, the construction needs re-arming, not the verifier weakening.
    let ambig = Ref{Int32}(0)
        ms = Base._methods_by_ftype(sig, nothing, -1, Base.get_world_counter(), false,
                                    Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test length(ms) == 4 && ambig[] == 1
    end
    let (minw, maxw) = verify_call(sig, expecteds, 1, 3, Base.get_world_counter(), true, Any[])
        @test maxw == 0
    end
end

# A new method that is ambiguous with the expected one must invalidate even when
# the include_ambiguous=false match set is unchanged (the newcomer is pruned
# because the expected method covers their overlap, yet dispatch in the contested
# region now throws a MethodError the compiled code never accounted for): the
# fast path bails on the mutual pair and the fallback rejects on has_ambig.
module VerifyCallAmbig
g(x::Integer, y) = 1
end
let verify_call = Base.Compiler.ReinferUtils.verify_call,
    g = VerifyCallAmbig.g,
    mS = which(g, Tuple{Integer, Any}),
    sig = Tuple{typeof(g), Int, Any},
    expecteds = Core.svec(mS)
    let (minw, maxw) = verify_call(sig, expecteds, 1, 1, Base.get_world_counter(), true, Any[])
        @test maxw == typemax(UInt)
    end
    VerifyCallAmbig.eval(:(g(x, y::String) = 2))
    mN = which(g, Tuple{Any, String})
    @test Base.isambiguous(mS, mN)
    let ambig = Ref{Int32}(0) # match set unchanged, only the flag reports the change
        ms = Base._methods_by_ftype(sig, nothing, -1, Base.get_world_counter(), false,
                                    Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test length(ms) == 1 && (ms[1]::Core.MethodMatch).method === mS && ambig[] == 1
    end
    let (minw, maxw) = verify_call(sig, expecteds, 1, 1, Base.get_world_counter(), true, Any[])
        @test maxw == 0
    end
end

# The mutual-pair shape: the canonical `Type{Union{}}`-slurp recovery.
# Where the newcomer is mutually ambiguous with the fully-covering expected owner
# (their only overlap is the corner the slurp resolves), and the fresh sort
# keeps the pair silent through the owner's minmax exemption; the fast path
# must certify exactly that and accept without the full lookup.
module VerifyCallSlurp
abstract type QA end
abstract type QB end
q(::Type{<:QA}) = 1        # qA: the recorded owner, fully covers the edge below
q(::Type{Union{}}) = 0     # qS: the slurp, with an empty interference set
# the control family: same shape, but no slurp to resolve the corner
abstract type UA end
abstract type UB end
u(::Type{<:UA}) = 1        # uA
end
let verify_call = Base.Compiler.ReinferUtils.verify_call,
    method_in_interferences = Base.Compiler.ReinferUtils.method_in_interferences,
    q = VerifyCallSlurp.q,
    mA = which(q, Tuple{Type{VerifyCallSlurp.QA}}),
    mS = which(q, Tuple{Type{Union{}}}),
    sig = Tuple{typeof(q), Type{T}} where T<:VerifyCallSlurp.QA,
    expecteds = Core.svec(mS, mA)
    # the unchanged recorded state revalidates
    let (minw, maxw) = verify_call(sig, expecteds, 1, 2, Base.get_world_counter(), true, Any[])
        @test maxw == typemax(UInt)
    end
    VerifyCallSlurp.eval(:(q(::Type{<:QB}) = 2))
    mB = which(q, Tuple{Type{VerifyCallSlurp.QB}})
    # the mutual pair is recorded on both edges; the slurp beats both
    @test method_in_interferences(mA, mB) && method_in_interferences(mB, mA)
    @test isempty(mS.interferences) && method_in_interferences(mS, mB)
    # ground truth: the fresh sort prunes the newcomer silently (the owner's
    # minmax exemption is what keeps the mutual pair from being flagged)
    let ambig = Ref{Int32}(0)
        ms = Base._methods_by_ftype(sig, nothing, -1, Base.get_world_counter(), false,
                                    Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test length(ms) == 2 && ambig[] == 0
    end
    let (minw, maxw) = verify_call(sig, expecteds, 1, 2, Base.get_world_counter(), true, Any[])
        @test maxw == typemax(UInt)
    end
    # the fallback would also revalidate this edge (the pruned match set is
    # unchanged), so additionally pin that the fast-path predicate itself
    # accepts, and that it rejects when the owner does not fully cover the query
    let newcomer_prunes_silently = Base.Compiler.ReinferUtils.newcomer_prunes_silently,
        ti = typeintersect(sig, mB.sig)
        @test newcomer_prunes_silently(mA, mB, ti, sig, expecteds, 1, 2, Base.get_world_counter())
        let sig2 = Tuple{typeof(q), Type{T}} where T, ti2 = typeintersect(sig2, mB.sig)
            @test !newcomer_prunes_silently(mA, mB, ti2, sig2, Core.svec(mS, mA, mB), 1, 3, Base.get_world_counter())
        end
    end
end
# The control: the same mutual corner pair with no slurp cover must invalidate due to the added ambiguity.
let verify_call = Base.Compiler.ReinferUtils.verify_call,
    u = VerifyCallSlurp.u,
    uA = which(u, Tuple{Type{VerifyCallSlurp.UA}}),
    sig = Tuple{typeof(u), Type{T}} where T<:VerifyCallSlurp.UA,
    expecteds = Core.svec(uA)
    let (minw, maxw) = verify_call(sig, expecteds, 1, 1, Base.get_world_counter(), true, Any[])
        @test maxw == typemax(UInt)
    end
    VerifyCallSlurp.eval(:(u(::Type{<:UB}) = 2))
    let ambig = Ref{Int32}(0)
        ms = Base._methods_by_ftype(sig, nothing, -1, Base.get_world_counter(), false,
                                    Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test length(ms) == 1 && ambig[] == 1
    end
    let (minw, maxw) = verify_call(sig, expecteds, 1, 1, Base.get_world_counter(), true, Any[])
        @test maxw == 0
    end
end

# A method is selected only when it is more specific than every other
# applicable method, so a match that is itself dominated can still make a call
# ambiguous by blocking the would-be winner. Here 3 ≻ 2, 2 ≻ 1 and 3 is unordered
# with 1: nothing beats both others, so the call is ambiguous, and method 1 has to
# be reported for the error to name the blocking pair (reporting method 3 alone
# would come out as `no method matching`).
module AmbigNoBeatAll
abstract type Top end
abstract type MidA <: Top end
abstract type MidB <: Top end
struct A1 <: MidA end
struct B1 <: MidB end
g(::MidA, ::Union{A1,MidB}) = 1
g(::MidA, ::Vararg{B1}) = 2
g(::Union{A1,B1}, ::Vararg{Union{B1,MidA}}) = 3
end
let A1 = AmbigNoBeatAll.A1, B1 = AmbigNoBeatAll.B1,
    ms = sort(collect(methods(AmbigNoBeatAll.g)), by = m -> m.line),
    (m1, m2, m3) = (ms[1], ms[2], ms[3])
    @test Base.morespecific(m3, m2) && Base.morespecific(m2, m1)
    @test !Base.morespecific(m3, m1) && !Base.morespecific(m1, m3) # unordered
    @test_throws MethodError AmbigNoBeatAll.g(A1(), B1())
    let ambig = Ref{Int32}(0)
        matches = Base._methods_by_ftype(
            Tuple{typeof(AmbigNoBeatAll.g), A1, B1}, nothing, -1,
            Base.get_world_counter(), true,
            Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test ambig[] == 1
        # method 2 is dominated by 3 and blocks nothing, so it need not be listed
        @test Set(m.method for m in matches) == Set([m1, m3])
    end
    let err = try AmbigNoBeatAll.g(A1(), B1()) catch e; e end
        errstr = sprint(showerror, err)
        @test occursin("is ambiguous", errstr)
        @test occursin("::Union{$A1, $(AmbigNoBeatAll.MidB)}", errstr) # method 1
    end
end

# Adding a method that some existing method is more specific than cannot turn an
# ambiguous call into a resolved one: selection requires beating every applicable
# method, and the new method is one more thing to beat. That is what lets the
# method table backedges skip invalidation here, so a caller compiled while the
# call was ambiguous (and therefore inferred to always throw) stays valid. n.b.
# the converse direction is not covered: a new method can turn a resolved call
# ambiguous without `is_replacing` noticing, which is a pre-existing hole.
module AmbigResolveByExclusion
abstract type Top end
abstract type MidA <: Top end
abstract type MidB <: Top end
struct A1 <: MidA end
struct B1 <: MidB end
g(::MidA, ::Union{A1,MidB}) = 1                  # unordered with method 2 below
g(::Union{A1,B1}, ::Vararg{Union{B1,MidA}}) = 2  # more specific than method 3 below
end
ambig_exclusion_caller() = AmbigResolveByExclusion.g(AmbigResolveByExclusion.A1(),
                                                    AmbigResolveByExclusion.B1())
@test_throws MethodError ambig_exclusion_caller() # also compiles the caller
@eval AmbigResolveByExclusion g(::MidA, ::Vararg{B1}) = 3 # loses to 2, beats 1
@test_throws MethodError ambig_exclusion_caller() # still blocked by method 1
@test_throws MethodError AmbigResolveByExclusion.g(AmbigResolveByExclusion.A1(),
                                                  AmbigResolveByExclusion.B1())

# A match that is dominated over its whole region can be dropped from the report,
# since it can never be selected -- but only as long as, at every point of the
# region, the ambiguities it takes part in stay witnessed by the matches that
# remain applicable there. Here method 1 is covered over the query region by the
# union of methods 5 (the `(A1, A1)` part) and 3 (the `(A1, B1)` part), which
# each strictly beat it. But method 1 is the only match blocking method 4 at
# `(A1, B1)`: method 3, the cover applying there, is itself beaten by method 4,
# so it blocks nothing. A region-blind transfer check accepts method 5 (which
# blocks method 4, but only where it applies) and drops method 1, leaving the
# reported subset at `(A1, B1)` appearing to have resolved that to method 4.
# Method 3 also sits in a specificity cycle with methods 1 and 2 (1 ≻ 2 ≻ 3 ≻ 1),
# which is where a transfer check that ignores cycles would additionally lose the
# ambiguity flag.
module AmbigTransferRegion
abstract type Top end
abstract type MidA <: Top end
abstract type MidB <: Top end
struct A1 <: MidA end
struct A2 <: MidA end
struct B1 <: MidB end
struct B2 <: MidB end
f(::MidA, ::Union{A1,MidB}) = 1                  # covered by the union of 5 and 3
f(::T, ::Vararg{T}) where {T<:MidA} = 2          # beaten by 1, beats 3
f(::MidA, ::Vararg{B1}) = 3                      # beats 1, beaten by 2 (the cycle)
f(::Union{A1,B1}, ::Vararg{Union{B1,MidA}}) = 4  # covers 3, unordered with 1 and 5
f(::MidA, ::A1) = 5                              # beats 1 and 2
end
let A1 = AmbigTransferRegion.A1, B1 = AmbigTransferRegion.B1,
    ms = sort(collect(methods(AmbigTransferRegion.f)), by = m -> m.line),
    (m1, m2, m3, m4, m5) = ms
    # the specificity cycle that makes the dominance-transfer check necessary
    @test Base.morespecific(m1, m2) && Base.morespecific(m2, m3) && Base.morespecific(m3, m1)
    # method 1 is dominated at every point of its region (by method 5 at
    # `(A1, A1)`, by method 3 at `(A1, B1)`) yet unordered with method 4, which
    # neither of those covers beats
    @test !Base.morespecific(m1, m4) && !Base.morespecific(m4, m1)
    @test Base.morespecific(m5, m1) && Base.morespecific(m3, m1)
    @test !Base.morespecific(m5, m4) && !Base.morespecific(m3, m4)
    let ambig = Ref{Int32}(0)
        matches = Base._methods_by_ftype(
            Tuple{typeof(AmbigTransferRegion.f), Union{A1,B1}, Vararg{Union{A1,B1}}},
            nothing, -1, Base.get_world_counter(), true,
            Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test ambig[] == 1
        # Methods 4 and 5 witness the ambiguity at `(A1, A1)`. Method 1 is
        # covered over this whole region by the union of 5 and 3, so it can
        # never be selected -- but it must stay reported anyway, because it is
        # the only match blocking method 4 at `(A1, B1)`: dropping it would make
        # the reported subset applicable at that point read as resolved to
        # method 4, while the call is really ambiguous (below).
        @test Set(m.method for m in matches) == Set([m1, m4, m5])
        # every point of the region keeps its ambiguity witnessed by the
        # reported matches applicable there
        applicable_at = [m.method for m in matches if Tuple{typeof(AmbigTransferRegion.f), A1, B1} <: m.method.sig]
        @test m1 in applicable_at && m4 in applicable_at
    end
    @test Base.isambiguous(m1, m4) # visible over the pair's own intersection
    # `(A1, B1)` applies methods 1, 3 and 4, and nothing beats all of them, so it
    # is ambiguous and method 1 must be reported as one of the blocking pair
    @test_throws MethodError AmbigTransferRegion.f(A1(), B1())
    let ambig = Ref{Int32}(0)
        matches = Base._methods_by_ftype(
            Tuple{typeof(AmbigTransferRegion.f), A1, B1}, nothing, -1,
            Base.get_world_counter(), true,
            Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test ambig[] == 1
        @test m1 in [m.method for m in matches]
    end
    @test_throws MethodError AmbigTransferRegion.f(A1(), A1())
end

# A fully-covering match that strictly beats nothing is never reached by the
# result sort's DFS (which only descends to strictly-morespecific matches), so
# when a minmax method exists it used to be skipped -- and dropped -- without the
# dominance-transfer check that every other removal runs. The specificity
# triangle here (3 ≻ 1 ≻ 2, with 2 and 3 unordered) makes the skipped method 2
# the only match blocking method 3 at `(P,)`: method 3 beats the minmax method 1,
# so dropping method 2 leaves the report with no witness of the ambiguity at all,
# only a strictly ordered pair reading as resolved to method 3.
module AmbigMinmaxSkip
abstract type Top end
struct P <: Top end
struct Q <: Top end
struct R <: Top end
f(::T, ::Vararg{T}) where {T<:Union{P,Q}} = 1  # minmax over the query below: beats 2, beaten by 3
f(::Union{P,Q}, ::Vararg{R}) = 2               # fully covers, beats nothing, unordered with 3
f(::P, ::Vararg{P}) = 3                        # partial, beats 1, unordered with 2
end
let ms = sort(collect(methods(AmbigMinmaxSkip.f)), by = m -> m.line),
    (m1, m2, m3) = ms,
    P = AmbigMinmaxSkip.P,
    Q = AmbigMinmaxSkip.Q
    # the intransitive specificity triangle
    @test Base.morespecific(m3, m1) && Base.morespecific(m1, m2)
    @test !Base.morespecific(m2, m3) && !Base.morespecific(m3, m2)
    let ambig = Ref{Int32}(0)
        matches = Base._methods_by_ftype(
            Tuple{typeof(AmbigMinmaxSkip.f), T} where T<:Union{P,Q},
            nothing, -1, Base.get_world_counter(), true,
            Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test ambig[] == 1
        @test Set(m.method for m in matches) == Set([m1, m2, m3])
        # the ambiguity at `(P,)` stays witnessed by the reported matches
        # applicable there
        applicable_at = [m.method for m in matches if Tuple{typeof(AmbigMinmaxSkip.f), P} <: m.method.sig]
        @test m2 in applicable_at && m3 in applicable_at
    end
    @test_throws MethodError AmbigMinmaxSkip.f(P())
    @test AmbigMinmaxSkip.f(Q()) == 1
end

# the same triangle plus a method 4 resolving the contested region: certifying
# the removal of method 2 needs the union of the minmax method 1 (blocked over
# `(P,)` where method 3 beats it) and method 4 (which blocks method 3 there), so
# a dominance transfer consulting only the minmax method would report a spurious
# ambiguity for a query dispatch fully resolves
module AmbigMinmaxUnion
abstract type Top end
struct P <: Top end
struct Q <: Top end
struct R <: Top end
f(::T, ::Vararg{T}) where {T<:Union{P,Q}} = 1  # minmax over the query below: beats 2, beaten by 3 and 4
f(::Union{P,Q}, ::Vararg{R}) = 2               # fully covers, beats nothing, unordered with 3
f(::P, ::Vararg{P}) = 3                        # partial, beats 1, unordered with 2, beaten by 4
f(::P) = 4                                     # beats 1, 2 and 3; covers the contested region `(P,)`
end
let ms = sort(collect(methods(AmbigMinmaxUnion.f)), by = m -> m.line),
    (m1, m2, m3, m4) = ms,
    P = AmbigMinmaxUnion.P,
    Q = AmbigMinmaxUnion.Q
    @test Base.morespecific(m3, m1) && Base.morespecific(m1, m2)
    @test !Base.morespecific(m2, m3) && !Base.morespecific(m3, m2)
    @test Base.morespecific(m4, m1) && Base.morespecific(m4, m2) && Base.morespecific(m4, m3)
    # dispatch resolves every point of the query
    @test AmbigMinmaxUnion.f(P()) == 4
    @test AmbigMinmaxUnion.f(Q()) == 1
    for include_ambiguous in (false, true)
        ambig = Ref{Int32}(0)
        matches = Base._methods_by_ftype(
            Tuple{typeof(AmbigMinmaxUnion.f), T} where T<:Union{P,Q},
            nothing, -1, Base.get_world_counter(), include_ambiguous,
            Ref{UInt}(typemin(UInt)), Ref{UInt}(typemax(UInt)), ambig)
        @test ambig[] == 0
        @test Set(m.method for m in matches) == Set([m1, m4])
    end
end

module NoLosersBit
f(::Any) = 1
end
let NO_LOSERS = Base.ReinferUtils.METHOD_SIG_NO_LOSERS
    @test !iszero(only(methods(NoLosersBit.f)).dispatch_status & NO_LOSERS)
    # a specialization beats the fallback: it never gets the bit, while the
    # fallback (which still beats nothing) keeps it
    @eval NoLosersBit f(::Int) = 2
    @test !iszero(which(NoLosersBit.f, Tuple{Any}).dispatch_status & NO_LOSERS)
    @test iszero(which(NoLosersBit.f, Tuple{Int}).dispatch_status & NO_LOSERS)
    # every member of a specificity cycle beats another member (which loses to it)
    for mod in (AmbigCycle3, AmbigCycle3Reorder), m in methods(mod.f)
        @test iszero(m.dispatch_status & NO_LOSERS)
    end
    # every member of a ambiguity still sets the bit
    for m in methods(amb_2)
        @test !iszero(m.dispatch_status & NO_LOSERS)
    end
    # a type-equal replacement records a recency-tiebreak win over the method
    # it replaces, so it must not inherit the predecessor's bit
    @eval NoLosersBit f(::Any) = 3
    @test iszero(which(NoLosersBit.f, Tuple{Any}).dispatch_status & NO_LOSERS)
end

nothing

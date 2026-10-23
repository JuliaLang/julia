# This file is a part of Julia. License is MIT: https://julialang.org/license

# Trait slots (doc/src/devdocs/trait-slots.md): methods guarded on the label a trait
# gives an argument type. Definitions are at top level, so that each testset runs in a
# world that sees them. Guarded methods on Base functions would change dispatch for
# other tests sharing this process, so the example that adds them runs in a subprocess.

using Test
using Base.Experimental: @guarded, @trait

abstract type Backend end
struct CPU <: Backend end
struct GPU <: Backend end

# a device array, and a trait that follows the parents of wrappers
struct DevArray{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
end
Base.size(x::DevArray) = size(x.data)
Base.getindex(x::DevArray, i::Int...) = x.data[i...]

@trait backend::Backend = CPU
@trait backend(DevArray) = GPU
@trait backend(SubArray{T,N,P}) = backend(P)
@trait backend(Base.ReshapedArray{T,N,P}) = backend(P)
@trait backend(PermutedDimsArray{T,N,perm,iperm,P}) = backend(P)

const x0 = [1.0 2.0; 3.0 4.0]
const d0 = DevArray(x0)

kind(a::AbstractArray) = :cpu
@guarded kind(a::AbstractArray) where {backend(a)::GPU} = :gpu
kind_static(a) = kind(a)
kind_dynamic(a) = kind(Base.inferencebarrier(a))

@testset "trait slots: nested wrappers" begin
    for a in Any[d0, view(d0, :, 1), vec(PermutedDimsArray(d0, (2, 1))), view(vec(d0), 2:3)]
        @test kind_static(a) === kind_dynamic(a) === :gpu
    end
    for a in Any[x0, view(x0, :, 1), vec(PermutedDimsArray(x0, (2, 1)))]
        @test kind_static(a) === kind_dynamic(a) === :cpu
    end
    @test occursin("guarded by {backend(a)::$GPU}", string(methods(kind)))
end

# inference resolves a call when the argument type determines the label, even if
# the parent of a wrapper is only bounded
@testset "trait slots: inference" begin
    V{P} = SubArray{Float64,1,P,Tuple{Base.Slice{Base.OneTo{Int}},Int},true}
    for (T, r) in Any[typeof(view(d0, :, 1)) => :gpu, V{<:DevArray} => :gpu,
                      Matrix{Float64} => :cpu, V{Matrix{Float64}} => :cpu]
        @test only(code_typed(kind_static, (T,)))[1].code[end] == Core.ReturnNode(QuoteNode(r))
    end
    for T in Any[AbstractMatrix{Float64}, V{<:AbstractMatrix{Float64}}]
        @test !Base.infer_effects(kind_static, (T,)).nothrow
    end
    # the label of a type parameter is the one its values share
    @test backend(V{DevArray{Float64,2}}) === GPU
    @test backend(V{DevArray}) === GPU
    @test backend(V{AbstractMatrix{Float64}}) === CPU
    @test backend(V{<:DevArray}) === GPU
    @test backend(SubArray) === CPU
    @test backend(DevArray) === GPU
end

# a type variable may be `Union{}`, which has the default label, unless it is the type
# of a field: no wrapper holds a value of type `Union{}`
struct Box{T} end
@trait backend(Box{T}) = backend(T)
@testset "trait slots: bottom parameters" begin
    slot(T) = ccall(:jl_trait_slots, Any, (Any, UInt), Tuple{typeof(kind), T}, Base.get_world_counter())[1][3]
    @test backend(Box{Union{}}) === CPU
    @test slot(Box{T} where T<:DevArray) == Union{CPU, GPU}
    @test slot(SubArray{Float64,1,P} where P<:DevArray) === GPU
    @test slot(Type{T} where T<:DevArray) == Union{CPU, GPU}
    @test backend(Box{T} where {L<:DevArray, L<:T<:DevArray}) === CPU # not normalized, unlike in a Tuple
    bk(x) = :cpu
    @guarded bk(x) where {backend(x)::GPU} = :gpu
    @test invoke(bk, Tuple{Box{T} where T<:DevArray}, Box{Union{}}()) === :cpu
    # deep nesting inside a union
    D = DevArray{Float64,1}
    for _ = 1:40
        D = Box{D}
    end
    @test backend(D) === GPU
    @test slot(Union{D, Int8, Int16, Int32, Float32}) == Union{CPU, GPU}
end

# a type argument `X` has the label of `X`
targ(::Type) = :cpu
@guarded targ(t::Type) where {backend(t)::GPU} = :gpu
@testset "trait slots: type arguments" begin
    @test targ(DevArray{Float64,2}) === targ(typeof(view(d0, :, 1))) === :gpu
    @test targ(DevArray) === :gpu
    @test targ(Matrix{Float64}) === targ(AbstractArray) === :cpu
    @test targ(Base.inferencebarrier(DevArray{Int,1})) === :gpu
end

# a wrapper from another package joins by adding one trait method
struct MyWrapper{T,N,P<:AbstractArray{T,N}} <: AbstractArray{T,N}
    parent::P
end
Base.size(w::MyWrapper) = size(w.parent)
const W = MyWrapper(d0)

@testset "trait slots: before teaching backend about MyWrapper" begin
    @test kind_static(W) === kind_dynamic(W) === :cpu
end

@trait backend(MyWrapper{T,N,P}) = backend(P)

@testset "trait slots: after teaching backend about MyWrapper" begin
    @test kind_static(W) === kind_dynamic(W) === :gpu
    @test kind_static(view(MyWrapper(view(d0, :, 1)), 1:1)) === :gpu
end

# ordering: (SubArray; Any) and (AbstractArray; GPU) are ambiguous, and so are
# (DevArray; Any) and (AbstractArray; GPU): a signature never implies a guard
op(a::AbstractArray) = :generic
op(a::SubArray) = :view
op(a::DevArray) = :dev
@guarded op(a::AbstractArray) where {backend(a)::GPU} = :gpu

@testset "trait slots: ambiguities" begin
    @test op(x0) === :generic
    @test op(view(x0, :, 1)) === :view
    @test op(vec(d0)) === :gpu
    for a in (view(d0, :, 1), d0)
        err = try op(a); nothing; catch e; e; end
        @test err isa MethodError
        @test occursin("ambiguous", sprint(showerror, err))
    end
end

@guarded op(a::SubArray) where {backend(a)::GPU} = :gpu_view

@testset "trait slots: tie-breaker" begin
    @test op(view(d0, :, 1)) === :gpu_view
    @test op(view(x0, :, 1)) === :view
end

# --- worlds and invalidation ----------------------------------------------------

wf(x) = :cpu
const W_BEFORE_TRAIT = Base.get_world_counter()
@trait wtrait::Backend = CPU
@trait wtrait(Int) = GPU
@guarded wf(x) where {wtrait(x)::GPU} = :gpu

@testset "trait slots: old-world calls ignore later slots" begin
    @test wf(1) === :gpu
    @test Base.invoke_in_world(W_BEFORE_TRAIT, wf, 1) === :cpu
    @test wf(1.0) === :cpu
end

# selections do not outlive a change of the table, in either world order, for
# callers that selected the guarded method or the unguarded one
ef(x) = :cpu
@trait etrait::Backend = CPU
@guarded ef(x) where {etrait(x)::GPU} = :gpu
callef(x) = ef(x)
@test callef(1) === :cpu
@test ef(Base.inferencebarrier(1)) === :cpu
const W_E = Base.get_world_counter()
@trait etrait(Int) = GPU

@testset "trait slots: adding an entry" begin
    @test callef(1) === :gpu
    @test Base.invoke_in_world(W_E, callef, 1) === :cpu
    @test Base.invoke_in_world(W_E, ef, 1) === :cpu
    @test ef(Base.inferencebarrier(1)) === :gpu
end

struct EBox{T} end
@trait etrait(Int) = CPU
@trait etrait(EBox{T}) = etrait(T)
const W_E2 = Base.get_world_counter()

@testset "trait slots: replacing an entry" begin
    @test callef(1) === :cpu
    @test callef(EBox{Int}()) === :cpu
    @test Base.invoke_in_world(W_E, callef, 1) === :cpu
end

@trait etrait(Int) = GPU

@testset "trait slots: replacing a delegated entry" begin
    @test callef(1) === :gpu
    @test callef(EBox{Int}()) === ef(Base.inferencebarrier(EBox{Int}())) === :gpu
    @test Base.invoke_in_world(W_E2, callef, EBox{Int}()) === :cpu
end

Base.Experimental.delete_trait_entry(@__MODULE__, etrait, Int)

@testset "trait slots: deleting an entry" begin
    @test callef(1) === :cpu
    @test ef(Base.inferencebarrier(1)) === :cpu
    @test callef(EBox{Int}()) === :cpu
end

# entries on an abstract type family apply to its subtypes
abstract type AbstractDev{T,N} <: AbstractArray{T,N} end
struct Dev2{T,N} <: AbstractDev{T,N} end
Base.size(::Dev2) = (0,)
@trait backend(AbstractDev) = GPU

@testset "trait slots: inherited entries" begin
    @test backend(Dev2{Int,1}) === GPU
    @test kind(Dev2{Int,1}()) === kind(view(Dev2{Int,1}(), 1:0)) === :gpu
    @test only(code_typed(kind_static, (SubArray{Int,1,P,Tuple{UnitRange{Int}},true} where P<:AbstractDev,)))[1].code[end] ==
          Core.ReturnNode(QuoteNode(:gpu))
end

# the first guarded method of a function invalidates its callers
plainf(x) = :cpu
callplainf(x) = plainf(x)
@test callplainf(1) === :cpu
@guarded plainf(x::Int) where {wtrait(x)::GPU} = :gpu

@testset "trait slots: first guarded method" begin
    @test callplainf(1) === :gpu
    @test callplainf(1.0) === :cpu
end

# adding or deleting an unguarded method of a function with guarded methods
@trait falsetrait::Val = Val{false}
@guarded ff(x::Number) where {falsetrait(x)::Val{true}} = :gpu
callff(x) = ff(x)
@test_throws MethodError callff(1)
ff(x::Number) = :cpu

@testset "trait slots: adding an unguarded method" begin
    @test callff(1) === :cpu
end

Base.delete_method(which(ff, (Number,)))

@testset "trait slots: deleting an unguarded method" begin
    @test_throws MethodError callff(1)
end

# the same, with the method coming from a package image
@testset "trait slots: unguarded method loaded from a package" begin
    mktempdir() do dir
        write(joinpath(dir, "TraitHost.jl"), "module TraitHost\nfunction f end\nend\n")
        write(joinpath(dir, "TraitExt.jl"), """
            module TraitExt
            import TraitHost
            TraitHost.f(x::Number) = :cpu
            TraitHost.f(x::String) = :string
            end
            """)
        pushfirst!(LOAD_PATH, dir)
        pushfirst!(DEPOT_PATH, dir)
        try
            Base.compilecache(Base.identify_package("TraitHost"))
            Base.compilecache(Base.identify_package("TraitExt"))
            @eval module PkgFallback
                using Test
                using Base.Experimental: @guarded
                import TraitHost
                @guarded TraitHost.f(x::Number) where {$falsetrait(x)::Val{true}} = :gpu
                caller(x) = TraitHost.f(x)
                @test_throws MethodError caller(1)
                import TraitExt
                @test caller(1) === :cpu
                @test caller("x") === :string
            end
        finally
            popfirst!(LOAD_PATH)
            popfirst!(DEPOT_PATH)
        end
    end
end

# --- dispatch details ----------------------------------------------------------

# matching with an unknown slot neither selects the guarded method nor lets
# another one cover it, even with a warm dispatch cache
@testset "trait slots: unknown slots" begin
    @test kind(d0) === :gpu
    tt = Tuple{typeof(kind), typeof(d0)}
    function matches_with(slots)
        mn, mx, amb = Ref(typemin(UInt)), Ref(typemax(UInt)), Ref{Int32}(0)
        ccall(:jl_matching_methods_with_slots, Any,
              (Any, Any, Cint, Cint, UInt, Ptr{UInt}, Ptr{UInt}, Ptr{Int32}, Any, Cint),
              tt, nothing, -1, 0, Base.get_world_counter(), mn, mx, amb, slots, false)::Vector{Any}
    end
    for slots in (nothing, Core.svec(1, backend, Any))
        ms = matches_with(slots)
        @test length(ms) == 2 && !any(m -> m.method.guards !== nothing && m.fully_covers, ms)
    end
    @test only(matches_with(Core.svec(1, backend, GPU))).method.guards !== nothing
    @test only(matches_with(Core.svec(1, backend, CPU))).method.guards === nothing
end

# dispatch cache entries never cover a guarded method's region
@trait vtrait::Backend = CPU
@trait vtrait(Val{X}) = vtrait(X)
@trait vtrait(DevArray) = GPU
wid(@nospecialize(x)) = :generic
wid(@nospecialize(x::Int)) = :int
@guarded wid(@nospecialize(x::Val)) where {vtrait(x)::GPU} = :gpu

@testset "trait slots: dispatch cache" begin
    for (v, r) in Any[(1.0, :generic), (1, :int), (Val(1), :generic), (Val(DevArray), :gpu),
                      (Val(2), :generic), (Val(DevArray{Int,1}), :gpu), (Val(Array), :generic)]
        @test wid(Base.inferencebarrier(v)) === r
    end
end

# a static parameter that the compilable signature leaves undetermined is bound per call
struct SparamTag end
@trait sparam_trait::Backend = CPU
sp(::SparamTag, ::Union{Int, NTuple{N,Int}}...) where {N} = N
@guarded sp(t::SparamTag, ::Union{Int, NTuple{N,Int}}...) where {N, sparam_trait(t)::GPU} = -N

@testset "trait slots: static parameters of widened signatures" begin
    @test sp(SparamTag(), 1, 2, 3, 4, 5, 6, (7, 8)) === 2
    @test_throws UndefVarError(:N, :static_parameter) sp(SparamTag(), 1, 2, 3, 4, 5, 6, 7)
end

# --- `@guarded` -------------------------------------------------------------------

@trait falsetrait2::Val = Val{false}
notatrait(::Type) = Val(false)

@testset "trait slots: @guarded" begin
    @test_throws "declared with `@trait`" @eval @guarded nt(x) where {notatrait(x)::Val{true}} = 1
    @test_throws "not an argument" @eval @guarded nt(x::Type{T}) where {T, falsetrait(T)::Val{true}} = 1
    @test_throws "only once" @eval @guarded dup(x) where {falsetrait(x)::Val{true}, falsetrait(x)::Val} = 1
    @test_throws "Vararg" @eval @guarded va(x...) where {falsetrait(x)::Val{true}} = 1
    # a definition that fails leaves no guards behind
    @test_throws UndefVarError @eval @guarded failing(x::NoSuchType) where {falsetrait(x)::Val{true}} = 1
    @eval failing(x) = :plain
    @test only(methods(failing)).guards === nothing
    # a definition evaluated in the signature, even of the same function, does not take the guards
    @eval @guarded nested(x::(@eval(nested(y::String) = :inner); Int), y) where {falsetrait2(x)::Val{true}} = :outer
    @test only(methods(nested, (String,))).guards === nothing
    @test only(methods(nested, (Int, Any))).guards !== nothing
    @test nested("a") === :inner
    @test_throws MethodError nested(1, 2)
    @eval @guarded nested2(x::(@eval(@guarded nested2(y::String) where {falsetrait(y)::Val{true}} = 1); Int)) where {falsetrait2(x)::Val{true}} = 2
    @test all(m -> m.guards !== nothing, methods(nested2))
end

# --- declarations ---------------------------------------------------------------

module OtherOwner
using Base.Experimental: @trait
abstract type Kind end
struct K1 <: Kind end
struct K2 <: Kind end
@trait okind::Kind = K1
struct Own{T} end
end
struct NotSealed{T} end

@testset "trait slots: declarations" begin
    M = OtherOwner
    # only the module of the trait or of the type family declares entries
    @test_throws "only the module" @eval @trait OtherOwner.okind(Some) = OtherOwner.K2
    @eval M @trait okind(Own{T}) = okind(T)
    @test M.okind(M.Own{Int}) === M.K1
    # and only the declaring module replaces or deletes an entry
    @test_throws "another module" @eval @trait OtherOwner.okind(OtherOwner.Own) = OtherOwner.K2
    @test_throws "another module" Base.Experimental.delete_trait_entry(@__MODULE__, M.okind, M.Own)
    @test_throws "no entry" Base.Experimental.delete_trait_entry(@__MODULE__, backend, NotSealed)
    # an entry is for a whole type family, and gives a concrete label below the top
    @test_throws "whole type family" @eval @trait backend(Vector) = GPU
    @test_throws "whole type family" @eval @trait backend(Base.StridedReinterpretArray{T,N,A}) = backend(A)
    @test_throws "concrete subtype" @eval @trait backend(NotSealed) = Backend
    @test_throws "concrete subtype" @eval @trait backend(NotSealed) = OtherOwner.K1
    @test_throws "abstract type cannot delegate" @eval @trait backend(AbstractArray{T,N}) = backend(T)
    @test_throws "exactly one parameter" @eval @trait backend(NotSealed{T}) = backend(S)
    @test_throws ErrorException @eval @trait backend(Type) = GPU
    @test_throws "cannot have trait entries" @eval @trait backend(DataType) = GPU
    # an entry on an abstract family seals its subtypes
    @test_throws "supertype has an entry" @eval @trait backend(Dev2) = CPU
    @test_throws "subtype has an entry" @eval @trait backend(AbstractArray) = CPU
    # declaring a trait
    @test_throws "already declared" ccall(:jl_trait_declare, Cvoid, (Any, Any, Any, Any), backend, Backend, CPU, @__MODULE__)
    @test_throws "not concrete" @eval @trait badtop::GPU = GPU
    @test_throws "concrete subtype" @eval @trait baddefault::Backend = Int
end

# --- reflection -------------------------------------------------------------------

module GuardReflection
using Base.Experimental: @guarded
import ..backend, ..GPU, ..CPU
rf(a::AbstractArray) = :generic
@guarded rf(a::AbstractArray) where {backend(a)::GPU} = :gpu
rf(a::SubArray) = :view
rg(a::AbstractArray) = :generic
@guarded rg(a::AbstractArray) where {backend(a)::GPU} = :gpu
rg(a::SubArray) = :view
@guarded rg(a::SubArray) where {backend(a)::GPU} = :gpu_view
@guarded rh(a::AbstractArray) where {backend(a)::GPU} = :gpu
@guarded rh(a::AbstractArray) where {backend(a)::CPU} = :cpu
end

function guardedmethod(f, argtype, bound)
    return only(m for m in methods(f)
                if m.sig == Tuple{typeof(f), argtype} &&
                   (bound === nothing ? m.guards === nothing : (m.guards !== nothing && m.guards[3] === bound)))
end

@testset "trait slots: isambiguous and detect_ambiguities" begin
    M = GuardReflection
    rf_gen = guardedmethod(M.rf, AbstractArray, nothing)
    rf_gpu = guardedmethod(M.rf, AbstractArray, GPU)
    rf_view = guardedmethod(M.rf, SubArray, nothing)
    @test !Base.isambiguous(rf_gpu, rf_gen)
    @test Base.isambiguous(rf_gpu, rf_view)
    @test !Base.isambiguous(guardedmethod(M.rg, AbstractArray, GPU), guardedmethod(M.rg, SubArray, nothing))
    @test !Base.isambiguous(guardedmethod(M.rh, AbstractArray, GPU), guardedmethod(M.rh, AbstractArray, CPU))
    @test Set(only(detect_ambiguities(M))) == Set((rf_gpu, rf_view))
end

@testset "trait slots: which, hasmethod and invoke" begin
    M = GuardReflection
    gv = view(d0, :, 1)
    @test which(M.rf, Tuple{typeof(d0)}) === guardedmethod(M.rf, AbstractArray, GPU)
    @test which(M.rg, Tuple{typeof(gv)}) === guardedmethod(M.rg, SubArray, GPU)
    @test_throws ErrorException which(M.rf, Tuple{typeof(gv)})
    @test hasmethod(M.rh, Tuple{typeof(x0)})
    # abstract argument types: a guard is not proven on all of them
    @test which(M.rf, Tuple{AbstractArray}) === guardedmethod(M.rf, AbstractArray, nothing)
    @test !hasmethod(M.rh, Tuple{AbstractArray})
    # so `invoke` reaches the unguarded method of a wrapper, as a tie-breaker may want to
    @test invoke(M.rg, Tuple{SubArray}, gv) === :view
    @test invoke(M.rf, Tuple{typeof(d0)}, d0) === :gpu
    invoke_self(x) = invoke(M.rf, Tuple{typeof(x)}, x)
    @test invoke_self(d0) === :gpu
    @test invoke_self(x0) === :generic
end

# a tie-breaker that covers only part of the ambiguous region leaves the ambiguity;
# one per member of a `Union` resolves every call, but `isambiguous` does not see that
module TieBreakers
using Base.Experimental: @guarded
import ..Backend, ..CPU
abstract type Device <: Backend end
struct DevA <: Device end
struct DevB <: Device end
using Base.Experimental: @trait
@trait devkind::Backend = CPU
@trait devkind(Int) = DevB
@guarded pf(x::Number) where {devkind(x)::Device} = :device
pf(x::Int) = :int
@guarded pf(x::Int) where {devkind(x)::DevA} = :int_a
@guarded uf(x::Number) where {devkind(x)::Union{DevA,DevB}} = :device
uf(x::Int) = :int
@guarded uf(x::Int) where {devkind(x)::DevA} = :int_a
@guarded uf(x::Int) where {devkind(x)::DevB} = :int_b
end

@testset "trait slots: partial tie-breakers" begin
    M = TieBreakers
    @test_throws MethodError M.pf(1)
    @test M.uf(1) === :int_b
    ambs = detect_ambiguities(M)
    @test length(ambs) == 2
    @test Set((guardedmethod(M.pf, Number, M.Device), guardedmethod(M.pf, Int, nothing))) in Set.(ambs)
end

# constructors: a missing method depends on the table too
module Ctors
struct C end
end
@trait ckind::Backend = CPU
@guarded Ctors.C(x::Int) where {ckind(x)::GPU} = Ctors.C()
hasctor() = hasmethod(Ctors.C, Tuple{Int})
@test !hasctor()
@trait ckind(Int) = GPU
@testset "trait slots: constructors" begin
    @test hasctor()
    @test Ctors.C(1) === Ctors.C()
end

# guarded methods in an overlay method table are invalidated when the table changes
Base.Experimental.@MethodTable GuardOverlay
@trait okind2::Backend = CPU
ovf(x) = :cpu
# `@guarded` does not compose with `@overlay`, so set the guards like it does
Base.Experimental.@overlay GuardOverlay ovf(x::Base.Experimental.set_pending_guards(Int, ovf, Core.svec(1, okind2, GPU))) = :gpu
@testset "trait slots: overlay method tables" begin
    m = only(Base.MethodList(GuardOverlay).ms)
    # give the overlay method a specialization with a backedge
    mi = Base.specialize_method(m, Tuple{typeof(ovf), Int}, Core.svec())
    log = ccall(:jl_debug_method_invalidation, Any, (Cint,), 1)
    try
        @trait okind2(Int) = GPU
    finally
        ccall(:jl_debug_method_invalidation, Any, (Cint,), 0)
    end
    @test m in log
end

# an overlay method hides a native one only where its guards are proven
ovg(x) = "cpu"
Base.Experimental.@overlay GuardOverlay ovg(x::Base.Experimental.set_pending_guards(Any, ovg, Core.svec(1, okind2, GPU))) = 1
@trait okind2(Box{T}) = okind2(T)
@trait okind2(DevArray) = GPU
@testset "trait slots: overlay fallbacks" begin
    world = Base.get_world_counter()
    mt = Base.Compiler.OverlayMethodTable(world, GuardOverlay)
    for (T, native) in Any[Box{DevArray{Int,1}} => false, (Box{T} where T<:DevArray) => true]
        sig = Tuple{typeof(ovg), T}
        slots = ccall(:jl_trait_slots, Any, (Any, UInt), sig, world)[1]
        ms = Base.Compiler.findall(sig, mt; slots).matches
        @test any(m -> m.method.guards === nothing, ms) == native
    end
end

# a lookup folded by inference depends on the table
@trait tkind::Backend = CPU
@guarded tk(x::Int) where {tkind(x)::GPU} = :gpu
tk_hasmethod() = hasmethod(tk, Tuple{Int})
tk_invoke(x::Int) = invoke(tk, Tuple{Int}, x)
@test !tk_hasmethod()
@test_throws MethodError tk_invoke(1)
const W_TK = Base.get_world_counter()
@trait tkind(Int) = GPU

@testset "trait slots: lookups after an entry is added" begin
    @test tk_hasmethod()
    @test tk_invoke(1) === :gpu
    @test !Base.invoke_in_world(W_TK, tk_hasmethod)
end

# --- Base integration (subprocess) ------------------------------------------------

# A mock device array that disallows scalar indexing: guarded methods on Base's
# broadcast style and `copyto!` route any nesting of wrappers to it.
const gpu_example = raw"""
using Test, LinearAlgebra
using Base.Broadcast: Broadcasted
using Base.Experimental: @guarded, @trait

abstract type Backend end
struct Unclassified <: Backend end
struct GPU <: Backend end
struct MockGPUArray{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
end
Base.size(x::MockGPUArray) = size(x.data)
Base.getindex(::MockGPUArray, ::Int...) = error("scalar indexing")
Base.setindex!(::MockGPUArray, v, ::Int...) = error("scalar indexing")

@trait backend::Backend = Unclassified
@trait backend(MockGPUArray) = GPU
@trait backend(SubArray{T,N,P}) = backend(P)
@trait backend(Base.ReshapedArray{T,N,P}) = backend(P)
@trait backend(Transpose{T,P}) = backend(P)

# what a kernel would see: the wrappers around the device buffer
dev(x) = x
dev(x::MockGPUArray) = x.data
dev(x::SubArray) = view(dev(parent(x)), x.indices...)
dev(x::Base.ReshapedArray) = reshape(dev(parent(x)), size(x))
dev(x::Transpose) = transpose(dev(parent(x)))
dev(bc::Broadcasted) = Broadcast.broadcasted(bc.f, map(dev, bc.args)...)

struct MockStyle{N} <: Broadcast.AbstractArrayStyle{N} end
MockStyle{M}(::Val{N}) where {M,N} = MockStyle{N}()
@guarded Base.Broadcast.BroadcastStyle(a::Type{A}) where {T, N, A<:AbstractArray{T,N}, backend(a)::GPU} = MockStyle{N}()
Base.similar(::Broadcasted{<:MockStyle}, ::Type{T}, dims) where {T} = MockGPUArray(Array{T}(undef, length.(dims)))
const KERNELS = Ref(0)
function Base.copyto!(dest::AbstractArray, bc::Broadcasted{<:MockStyle})
    KERNELS[] += 1
    copyto!(dev(dest), Broadcast.instantiate(dev(bc)))
    return dest
end
@guarded function Base.copyto!(dst::AbstractArray, src::AbstractArray) where {backend(dst)::GPU}
    KERNELS[] += 1
    copyto!(dev(dst), dev(src))
    return dst
end

x0 = [1.0 2.0; 3.0 4.0]
x = MockGPUArray(copy(x0))
@testset "broadcast over nested wrappers" begin
    for f in (a -> vec(transpose(a)) .+ 1, a -> view(a, 2:2, :) .* 2,
              a -> view(reshape(transpose(a), 4), 2:3) .+ view(vec(a), 1:2))
        k = KERNELS[]
        r = f(x)
        @test r isa MockGPUArray && KERNELS[] == k + 1
        @test r.data == f(x0)
    end
    copyto!(view(transpose(x), :, 1), [10.0, 20.0])
    @test x.data == [10.0 20.0; 3.0 4.0]
    @test view(x0, :, 1) .+ 1 isa Vector{Float64}
end
"""

@testset "trait slots: Base integration" begin
    cmd = `$(Base.julia_cmd()) --startup-file=no -e $gpu_example`
    @test success(pipeline(cmd; stdout, stderr))
end

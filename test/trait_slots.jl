# This file is a part of Julia. License is MIT: https://julialang.org/license

# Trait slots (doc/src/devdocs/trait-slots.md): methods guarded on a trait of an
# argument type. Definitions are at top level, so that each testset runs in a world
# that sees them. Guarded methods on Base functions would change dispatch for other
# tests sharing this process, so the example that adds them runs in a subprocess.

using Test
using Base.Experimental: @guarded

abstract type Backend end
struct CPU <: Backend end
struct GPU <: Backend end

# a device array, and a trait that follows the parents of wrappers
struct DevArray{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
end
Base.size(x::DevArray) = size(x.data)
Base.getindex(x::DevArray, i::Int...) = x.data[i...]

backend(::Type) = CPU()
backend(::Type{Type{T}}) where {T} = backend(T)
backend(::Type{<:DevArray}) = GPU()
backend(::Type{<:SubArray{<:Any,<:Any,P}}) where {P} = backend(P)
backend(::Type{<:Base.ReshapedArray{<:Any,<:Any,P}}) where {P} = backend(P)
backend(::Type{<:PermutedDimsArray{<:Any,<:Any,<:Any,<:Any,P}}) where {P} = backend(P)

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
    # inference resolves the call when the slot is known, not when it is unknown;
    # `backend(::Type)` does not specialize, so a CPU slot exercises the trait's exact inference
    @test Base.infer_effects(kind_static, (typeof(view(d0, :, 1)),)).nothrow
    @test Base.infer_effects(kind_static, (Matrix{Float64},)).nothrow
    @test !Base.infer_effects(kind_static, (AbstractMatrix{Float64},)).nothrow
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

backend(::Type{<:MyWrapper{<:Any,<:Any,P}}) where {P} = backend(P)

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
wtrait(::Type) = CPU()
wtrait(::Type{Int}) = GPU()
@guarded wf(x) where {wtrait(x)::GPU} = :gpu

@testset "trait slots: old-world calls ignore later slots" begin
    @test wf(1) === :gpu
    @test Base.invoke_in_world(W_BEFORE_TRAIT, wf, 1) === :cpu
    @test wf(1.0) === :cpu
end

# selections do not outlive a change of the trait, in either world order
ef(x) = :cpu
etrait(::Type) = CPU()
@guarded ef(x) where {etrait(x)::GPU} = :gpu
callef(x) = ef(x)
const W_E = Base.get_world_counter()
etrait(::Type{Int}) = GPU()

@testset "trait slots: trait changes" begin
    @test callef(1) === :gpu
    @test Base.invoke_in_world(W_E, callef, 1) === :cpu
    @test Base.invoke_in_world(W_E, ef, 1) === :cpu
    @test ef(Base.inferencebarrier(1)) === :gpu
end

Base.delete_method(which(etrait, (Type{Int},)))

@testset "trait slots: deleting a trait method" begin
    @test callef(1) === :cpu
    @test ef(Base.inferencebarrier(1)) === :cpu
end

# the trait's value comes from a helper; a new trait method only invalidates its dependents
helper(::Type) = CPU()
residency(::Type{T}) where {T} = helper(T)
rk(a::AbstractArray) = :host
@guarded rk(a::AbstractArray) where {residency(a)::GPU} = :device
rk_static(a::Matrix{Float64}) = rk(a)
rk_static(a::Vector{Int}) = rk(a)
struct UnrelatedArray <: AbstractArray{Int,1} end

@test rk_static(x0) === :host
@test rk_static([1, 2]) === :host
helper(::Type{Matrix{Float64}}) = GPU()
const rk_int_ci = only(Base.specializations(which(rk_static, (Vector{Int},)))).cache
residency(::Type{UnrelatedArray}) = CPU()

@testset "trait slots: invalidation through the trait" begin
    @test rk_static(x0) === :device
    @test rk_int_ci.max_world == typemax(UInt)
    @test rk_static([1, 2]) === :host
end

# the same with a `@nospecializeinfer` trait, whose inferred CodeInstance does not
# match the specialization the lookup selects
nhelper(::Type) = CPU()
Base.@nospecializeinfer ntrait(@nospecialize(A::Type)) = nhelper(A)
nf(x) = :cpu
@guarded nf(x) where {ntrait(x)::GPU} = :gpu
callnf(x) = nf(x)
@test callnf(1) === :cpu
nhelper(::Type{Int}) = GPU()

@testset "trait slots: invalidation through a nospecializeinfer trait" begin
    @test callnf(1) === :gpu
end

# an uninferred trait: calls stay dynamic, and their selection stays in its world
module UninferredTrait
import ..CPU
Base.Experimental.@compiler_options compile=min infer=false
uhelper(::Type) = CPU()
utrait(::Type{T}) where {T} = uhelper(T)
end
uk(a::AbstractArray) = :host
@guarded uk(a::AbstractArray) where {UninferredTrait.utrait(a)::GPU} = :device
uk_static(a::Matrix{Float64}) = uk(a)
@test uk_static(x0) === :host
const uk_world = Base.get_world_counter()
UninferredTrait.uhelper(::Type{Matrix{Float64}}) = GPU()

@testset "trait slots: uninferred trait" begin
    @test uk_static(x0) === :device
    @test Base.invoke_in_world(uk_world, uk, x0) === :host
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
falsetrait(::Type) = Val(false)
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
vtrait(::Type) = CPU()
vtrait(::Type{Val{:gpu}}) = GPU()
wid(@nospecialize(x)) = :generic
wid(@nospecialize(x::Int)) = :int
@guarded wid(@nospecialize(x::Val)) where {vtrait(x)::GPU} = :gpu

@testset "trait slots: dispatch cache" begin
    for (v, r) in Any[(1.0, :generic), (1, :int), (Val(1), :generic), (Val(:gpu), :gpu),
                      (Val(2), :generic), (Val(:gpu), :gpu)]
        @test wid(Base.inferencebarrier(v)) === r
    end
end

# a static parameter that the compilable signature leaves undetermined is bound per call
struct SparamTag end
sparam_trait(::Type) = nothing
sp(::SparamTag, ::Union{Int, NTuple{N,Int}}...) where {N} = N
@guarded sp(t::SparamTag, ::Union{Int, NTuple{N,Int}}...) where {N, sparam_trait(t)::SparamTag} = -N

@testset "trait slots: static parameters of widened signatures" begin
    @test sp(SparamTag(), 1, 2, 3, 4, 5, 6, (7, 8)) === 2
    @test_throws UndefVarError(:N, :static_parameter) sp(SparamTag(), 1, 2, 3, 4, 5, 6, 7)
end

# --- `@guarded` -------------------------------------------------------------------

falsetrait2(::Type) = Val(false)

@testset "trait slots: @guarded" begin
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

# a trait whose values are types: the slot type is `Core.Typeof(v)`, as in inference
typetrait(::Type{T}) where {T} = T <: Number ? Vector : Int
tf(x) = :cpu
@guarded tf(x) where {typetrait(x)::Core.TypeEgal{Vector}} = :vector
calltf(x) = tf(x)

@testset "trait slots: type-valued trait" begin
    @test tf(1) === calltf(1) === tf(Base.inferencebarrier(1)) === :vector
    @test tf("x") === calltf("x") === :cpu
    @test which(tf, Tuple{Int}).guards !== nothing
    @test which(tf, Tuple{String}).guards === nothing
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
devkind(::Type) = CPU()
devkind(::Type{Int}) = DevB()
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

# a lookup folded by inference depends on the trait even when the trait threw
throwing_kind(::Type) = error("no kind yet")
@guarded tk(x::Int) where {throwing_kind(x)::GPU} = :gpu
tk_hasmethod() = hasmethod(tk, Tuple{Int})
tk_invoke(x::Int) = invoke(tk, Tuple{Int}, x)
@test !tk_hasmethod()
@test_throws MethodError tk_invoke(1)
throwing_kind(::Type{Int}) = GPU()

@testset "trait slots: lookups after the trait is defined" begin
    @test tk_hasmethod()
    @test tk_invoke(1) === :gpu
end

# --- Base integration (subprocess) ------------------------------------------------

# A mock device array that disallows scalar indexing: guarded methods on Base's
# broadcast style and `copyto!` route any nesting of wrappers to it.
const gpu_example = raw"""
using Test, LinearAlgebra
using Base.Broadcast: Broadcasted
using Base.Experimental: @guarded

struct GPU end
struct MockGPUArray{T,N} <: AbstractArray{T,N}
    data::Array{T,N}
end
Base.size(x::MockGPUArray) = size(x.data)
Base.getindex(::MockGPUArray, ::Int...) = error("scalar indexing")
Base.setindex!(::MockGPUArray, v, ::Int...) = error("scalar indexing")

backend(::Type) = nothing
backend(::Type{Type{T}}) where {T} = backend(T)
backend(::Type{<:MockGPUArray}) = GPU()
backend(::Type{<:SubArray{<:Any,<:Any,P}}) where {P} = backend(P)
backend(::Type{<:Base.ReshapedArray{<:Any,<:Any,P}}) where {P} = backend(P)
backend(::Type{<:Transpose{<:Any,P}}) where {P} = backend(P)

# what a kernel would see: the wrappers around the device buffer
dev(x) = x
dev(x::MockGPUArray) = x.data
dev(x::SubArray) = view(dev(parent(x)), x.indices...)
dev(x::Base.ReshapedArray) = reshape(dev(parent(x)), size(x))
dev(x::Transpose) = transpose(dev(parent(x)))
dev(bc::Broadcasted) = Broadcast.broadcasted(bc.f, map(dev, bc.args)...)

struct MockStyle{N} <: Broadcast.AbstractArrayStyle{N} end
MockStyle{M}(::Val{N}) where {M,N} = MockStyle{N}()
@guarded Base.Broadcast.BroadcastStyle(::Type{A}) where {T, N, A<:AbstractArray{T,N}, backend(A)::GPU} = MockStyle{N}()
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

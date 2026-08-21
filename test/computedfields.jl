# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for computed field types (field generators): struct field types that are
# expressions of the type parameter values, e.g. `data::NTuple{(R::Int)*(C::Int), T}`.

using Test

struct StaticMatrix{R,C,T}
    data::NTuple{(R::Int)*(C::Int), T}
end

@testset "basic computed field types" begin
    @test fieldtype(StaticMatrix{2,3,Float64}, 1) === NTuple{6,Float64}
    @test isbitstype(StaticMatrix{2,3,Float64})
    @test sizeof(StaticMatrix{2,3,Float64}) == 48
    m = StaticMatrix{2,3,Float64}((1.0,2,3,4,5,6))
    @test m.data === (1.0,2.0,3.0,4.0,5.0,6.0)
    # inference of getfield on a concrete instantiation is precise
    @test Base.infer_return_type(m -> m.data, (StaticMatrix{2,3,Float64},)) == NTuple{6,Float64}
end

struct SymmetricTensor{order,dim,T}
    data::NTuple{div((dim::Int)*(dim+1),2)^div(order::Int,2), T}
end

@testset "composite arithmetic" begin
    @test fieldtype(SymmetricTensor{4,3,Float64}, 1) === NTuple{36,Float64}
    @test fieldtype(SymmetricTensor{2,3,Float64}, 1) === NTuple{6,Float64}
end

struct Neg{N}
    data::NTuple{(N::Int)-1, Int}
end

@testset "throwing generator degrades to Union{}" begin
    @test fieldtype(Neg{3}, 1) === NTuple{2,Int}
    @test fieldtype(Neg{0}, 1) === Union{}
    @test_throws Exception Neg{0}((1,))
    # non-Int parameters degrade too (typeassert throws)
    @test fieldtype(Neg{:sym}, 1) === Union{}
    # through the partial-application reflection API the error propagates instead
    @test_throws Exception Base.fieldtype_generator(Neg){0}
    # and types without computed field types have no generator
    @test_throws ArgumentError Base.fieldtype_generator(Some{Int})
end

@testset "partial instantiation" begin
    # computed field types are not part of the type system: `fieldtype` on a
    # non-concrete instantiation throws
    @test_throws ErrorException fieldtype(StaticMatrix{2}, 1)
    @test_throws ErrorException fieldtype(StaticMatrix, 1)
    # the declared slot records the source AST
    dt = Base.unwrap_unionall(StaticMatrix)
    marker = dt.types[1]
    @test marker isa Core.ComputedFieldType
    # partially applied field generator (reflection-level, not a type),
    # applied with apply_type like a partially applied UnionAll
    g = Base.fieldtype_generator(StaticMatrix{2})
    @test g{3, Float64} === (NTuple{6,Float64},)
    g2 = g{3}
    @test g2 isa Core.FieldtypeGenerator
    @test g2{Float64} === (NTuple{6,Float64},)
    @test Base.fieldtype_generator(StaticMatrix){2, 3, Float64} === (NTuple{6,Float64},)
    # a type with no remaining free parameters yields the tuple directly
    @test Base.fieldtype_generator(StaticMatrix{2,3,Float64}) === (NTuple{6,Float64},)
    @test_throws ErrorException g{3, Float64, Int}  # too many parameters
    # inference of code that manipulates partial instantiations must not crash
    mk(f, ::Type{StaticMatrix{R,C,T}}) where {R,C,T} = StaticMatrix{R,C,T}(ntuple(f, R*C))
    @test Base.infer_return_type(mk, (Function, Type{<:StaticMatrix})) isa Type
end

struct MixedFields{N,T}
    len::Int
    data::NTuple{2*(N::Int), T}
    tag::T
end

@testset "mixed plain and computed fields" begin
    @test fieldtype(MixedFields{3,Float32}, 1) === Int
    @test fieldtype(MixedFields{3,Float32}, 2) === NTuple{6,Float32}
    @test fieldtype(MixedFields{3,Float32}, 3) === Float32
    mx = MixedFields{3,Float32}(1, (1,2,3,4,5,6), 2.0f0)
    @test mx.data === (1.0f0,2.0f0,3.0f0,4.0f0,5.0f0,6.0f0)
end

# identical redefinition keeps the type identity
const StaticMatrixAlias = StaticMatrix
struct StaticMatrix{R,C,T}
    data::NTuple{(R::Int)*(C::Int), T}
end
@testset "redefinition equivalence" begin
    @test StaticMatrixAlias === StaticMatrix
end

global nonconst_dim::Int = 3

# Generators execute in RCJulia (restricted-capability) mode, so no purity
# proof is required at definition time: definitions that the old effects-proof
# design rejected are now legal, and any restricted operation the generator
# attempts traps deterministically at instantiation time instead, degrading
# the field types to `Union{}`. Errors propagate (rather than degrade) through
# explicit `fieldtype_generator` application.
@testset "restricted evaluation of field generators" begin
    # unconstrained parameters need no type annotations: dispatch resolves
    # dynamically in the frozen definition world
    @eval struct DynUntypedParams{R,C}
        data::NTuple{R*C, Int}
    end
    @test fieldtype(DynUntypedParams{2,3}, 1) === NTuple{6,Int}
    # a parameter combination whose expression yields a non-Int degrades
    @test fieldtype(DynUntypedParams{2,:sym}, 1) === Union{}
    # an inconsistent generator is a legal definition, but the restricted
    # operation (task-local RNG state) traps at instantiation time
    @eval struct BadRandom{N}
        data::NTuple{rand(1:(N::Int)), Int}
    end
    @test fieldtype(BadRandom{3}, 1) === Union{}
    @test_throws CapabilityError Base.fieldtype_generator(BadRandom{3})
    # reading a non-constant global traps likewise
    @eval struct BadGlobal{N}
        data::NTuple{(N::Int)*nonconst_dim, Int}
    end
    @test fieldtype(BadGlobal{3}, 1) === Union{}
    @test_throws CapabilityError Base.fieldtype_generator(BadGlobal{3})
    # loops have backedges; iteration in a generator must be recursion
    @eval loopsize(n::Int) = (s = 0; for i = 1:n; s += i; end; s)
    @eval struct LoopyGrid{N}
        data::NTuple{loopsize(N::Int), Int}
    end
    @test fieldtype(LoopyGrid{3}, 1) === Union{}
    @test_throws CapabilityError Base.fieldtype_generator(LoopyGrid{3})
end

# When instantiation degraded a computed field type to `Union{}` (throw-free),
# *construction* re-derives the field types with errors propagating, so the
# failure surfaces as the generator's underlying error instead of an opaque
# convert-to-`Union{}` message.
@testset "construction re-derives failed generators" begin
    # default (generated) inner constructor
    @eval struct CtorBadGlobal{N}
        tup::NTuple{(N::Int)*nonconst_dim, Float64}
    end
    @test fieldtype(CtorBadGlobal{2}, 1) === Union{}  # instantiation stays throw-free
    @test_throws CapabilityError CtorBadGlobal{2}((1.0, 2.0))
    # user inner constructor with explicit `new`
    @eval struct CtorBadInner{N}
        tup::NTuple{(N::Int)*nonconst_dim, Float64}
        CtorBadInner{N}(t) where {N} = new{N}(t)
    end
    @test_throws CapabilityError CtorBadInner{2}((1.0, 2.0))
    # a generator that succeeds but yields a non-type gets a descriptive error
    @eval struct CtorNonType{N}
        tup::((N::Int); nothing)
    end
    err = try CtorNonType{2}(()); nothing catch e; e end
    @test err isa ErrorException && occursin("not a valid field type", err.msg)
    # healthy structs still construct (and convert) precisely
    @test StaticMatrix{2,2,Int}((1, 2, 3, 4)).data === (1, 2, 3, 4)
end

# helper redefinition must not affect field types (frozen definition-time semantics)
module FrozenSemantics
    mysize(n::Int) = 2n
    @noinline bigsize(n::Int) = n <= 0 ? 0 : 4n + 1
    struct Grid{N}
        data::NTuple{mysize(N::Int), Float64}
    end
    struct BigGrid{N}
        data::NTuple{bigsize(N::Int), Float64}
    end
end

@testset "world-age freezing" begin
    @test fieldtype(FrozenSemantics.Grid{3}, 1) === NTuple{6,Float64}
    @test fieldtype(FrozenSemantics.BigGrid{3}, 1) === NTuple{13,Float64}
    # redefine the helpers
    FrozenSemantics.eval(:(mysize(n::Int) = 100n))
    FrozenSemantics.eval(:(@noinline bigsize(n::Int) = 1000n))
    @test FrozenSemantics.mysize(5) == 500
    # fresh instantiations still use the definition-time semantics
    @test fieldtype(FrozenSemantics.Grid{5}, 1) === NTuple{10,Float64}
    @test fieldtype(FrozenSemantics.BigGrid{5}, 1) === NTuple{21,Float64}
end

@testset "definition world token form" begin
    fg = Base.unwrap_unionall(StaticMatrix).name.fieldgen
    @test fg isa Core.SimpleVector && length(fg) == 2
    tok = fg[2]
    @test tok isa Core.WorldToken
    # invoking the generator through the stored token reproduces the field types
    @test Core._rcjulia_call(tok, fg[1], 2, 3, Float64) === (NTuple{6,Float64},)
end

# self-referential parameter arithmetic. n.b. mutable: for immutable types an
# inline-allocatable self-reference in a computed field would recurse during
# layout (the definition-time `references_name` inline-alloc cycle breaker
# cannot see through ComputedFieldType markers) -- a known limitation for now.
mutable struct Countdown{N}
    val::Int
    next::Union{Nothing, Countdown{max((N::Int)-1, 0)}}
end

@testset "self-referential computed parameters" begin
    @test fieldtype(Countdown{2}, 2) === Union{Nothing, Countdown{1}}
    # exact self-reference resolves to the in-flight type during instantiation
    @test fieldtype(Countdown{0}, 2) === Union{Nothing, Countdown{0}}
    c = Countdown{1}(1, Countdown{0}(0, nothing))
    @test c.next.val == 0
end

# user-defined inner constructor: `new` resolves field types at runtime
struct WithCtor{N}
    data::NTuple{2*(N::Int), Int}
    function WithCtor{N}(x::Int) where {N}
        return new{N}(ntuple(Returns(x), 2N))
    end
end

@testset "user-defined inner constructor" begin
    w = WithCtor{2}(7)
    @test w.data === (7, 7, 7, 7)
    @test_throws Exception WithCtor{2}((1, 2))  # no matching method
end

@testset "vouching via @assume_effects on a helper" begin
    @eval module Vouched
        Base.@assume_effects :foldable function opaquesize(n::Int)
            # something the compiler could prove anyway, but via assertion
            return 3n
        end
        struct VGrid{N}
            data::NTuple{opaquesize(N::Int), Int}
        end
    end
    @test fieldtype(Vouched.VGrid{2}, 1) === NTuple{6,Int}
end

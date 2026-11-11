# This file is a part of Julia. License is MIT: https://julialang.org/license

module lattice

using Test

include("setup_Compiler.jl")

# Algebraic properties of the inference lattices, checked over a sample of elements that
# mixes the extended lattice elements (`MustAlias`, `Conditional`, `PartialStruct`, ...)
# with plain types and constants. Inference relies on these for correctness: e.g.
# `update_bestguess!` and `stupdate!` only `tmerge` a new element if it is not already
# `⊑` the old one, so `a ⊑ b` must imply that merging `a` into `b` adds nothing.

using .Compiler: Const, PartialStruct, Conditional, MustAlias, InterConditional, InterMustAlias,
    PartialOpaque, widenconst

struct LatticeTestField{T}
    f::T
end
struct LatticeTestFields
    f1::Union{Int,Nothing}
    f2::Any
end

const F = LatticeTestField{Union{Int,String}}
const FF = LatticeTestFields

plain_elements() = Any[
    Union{}, Any, Int, String, Nothing, Union{Int,String}, Union{Int,Nothing}, Integer,
    Const(1), Const("x"), Const(nothing), Const(true), Const(false), Bool,
    PartialStruct(Compiler.fallback_lattice, FF, Any[Int, Any]),
    PartialStruct(Compiler.fallback_lattice, FF, Any[Const(1), Any]),
    PartialStruct(Compiler.fallback_lattice, F, Any[Int]),
    F, FF,
]

lattice_oc_source1(x) = x
lattice_oc_source2(x) = x
function opaque_elements()
    parent = Base.method_instance(lattice_oc_source1, (Int,))
    m1 = only(methods(lattice_oc_source1))
    m2 = only(methods(lattice_oc_source2))
    OC = Core.OpaqueClosure{Tuple{Int},Int}
    return Any[
        PartialOpaque(OC, Const(()), parent, m1),
        PartialOpaque(OC, Const((1,)), parent, m1),
        PartialOpaque(OC, Tuple{Int}, parent, m1),
        PartialOpaque(OC, Const(()), parent, m2),
        PartialOpaque(OC, Const(()), Base.method_instance(lattice_oc_source2, (Int,)), m1),
        OC,
    ]
end

alias_elements(::Type{MustAlias}) = Any[
    MustAlias(2, 0, F, 1, Union{Int,String}),
    MustAlias(2, 0, F, 1, Int),
    MustAlias(2, 0, F, 1, Const(1)),
    MustAlias(2, 0, FF, 1, Union{Int,Nothing}),
    MustAlias(2, 0, FF, 2, Any),
    MustAlias(3, 0, F, 1, Union{Int,String}),
    # aliases of different definitions of the same slot
    MustAlias(2, 1, F, 1, Int),
    MustAlias(2, 2, F, 1, Union{Int,String}),
]
alias_elements(::Type{InterMustAlias}) = Any[
    InterMustAlias(2, F, 1, Union{Int,String}),
    InterMustAlias(2, F, 1, Int),
    InterMustAlias(2, F, 1, Const(1)),
    InterMustAlias(2, FF, 1, Union{Int,Nothing}),
    InterMustAlias(2, FF, 2, Any),
    InterMustAlias(3, F, 1, Union{Int,String}),
]

conditional_elements(::Type{Conditional}) = Any[
    Conditional(2, 0, Int, String),
    Conditional(2, 0, Int, Union{}),
    Conditional(3, 0, Int, String),
]
conditional_elements(::Type{InterConditional}) = Any[
    InterConditional(2, Int, String),
    InterConditional(2, Int, Union{}),
    InterConditional(3, Int, String),
]

function check_lattice_properties(𝕃, elements)
    ⊑(@nospecialize(a), @nospecialize(b)) = Compiler.:⊑(𝕃, a, b)
    tmerge(@nospecialize(a), @nospecialize(b)) = Compiler.tmerge(𝕃, a, b)
    for a in elements
        @test a ⊑ a
        @test Union{} ⊑ a
        @test a ⊑ Any
    end
    for a in elements, b in elements
        m = tmerge(a, b)
        # `tmerge` is an upper bound of both of its arguments
        @test a ⊑ m
        @test b ⊑ m
        @test Compiler.is_lattice_equal(𝕃, a, b) == (a ⊑ b && b ⊑ a)
        # merged aliases still alias a field of a container with a definite layout
        if m isa Union{MustAlias,InterMustAlias}
            @test Compiler.maybe_const_fldidx(widenconst(m.vartyp), m.fldidx) == m.fldidx
        end
        # merging an element that is already below `b` must not escape `b`
        if a ⊑ b
            @test m ⊑ b
        end
    end
    for a in elements, b in elements, c in elements
        if a ⊑ b && b ⊑ c
            @test a ⊑ c
        end
    end
end

@testset "intra-procedural inference lattice" begin
    𝕃 = Compiler.typeinf_lattice(Compiler.NativeInterpreter())
    elements = Any[plain_elements(); opaque_elements(); conditional_elements(Conditional)]
    Compiler.has_mustalias(𝕃) && append!(elements, alias_elements(MustAlias))
    check_lattice_properties(𝕃, elements)
end

@testset "inter-procedural result lattice" begin
    𝕃 = Compiler.ipo_lattice(Compiler.NativeInterpreter())
    elements = Any[plain_elements(); opaque_elements(); conditional_elements(InterConditional)]
    Compiler.has_mustalias(Compiler.typeinf_lattice(Compiler.NativeInterpreter())) &&
        append!(elements, alias_elements(InterMustAlias))
    check_lattice_properties(𝕃, elements)
end

@testset "is_valid_lattice" begin
    intra = Compiler.typeinf_lattice(Compiler.NativeInterpreter())
    ipo = Compiler.ipo_lattice(Compiler.NativeInterpreter())
    limited = Compiler.LimitedAccuracy(Int, Compiler.IdSet{Compiler.InferenceState}())
    # an element is valid in a lattice when any layer of the lattice has it
    for (elem, in_intra, in_ipo) in Any[
        (Int, true, true),
        (Const(1), true, true),
        (PartialStruct(Compiler.fallback_lattice, F, Any[Int]), true, true),
        (limited, true, true),
        (Conditional(2, 0, Int, String), true, false),
        (InterConditional(2, Int, String), false, true),
        (MustAlias(2, 0, F, 1, Int), Compiler.has_mustalias(intra), false),
        (1, false, false),
    ]
        @test Compiler.is_valid_lattice(intra, elem) === in_intra
        @test Compiler.is_valid_lattice(ipo, elem) === in_ipo
    end
    @test Compiler.is_valid_lattice(Compiler.JLTypeLattice(), Int)
    @test !Compiler.is_valid_lattice(Compiler.JLTypeLattice(), Const(1))
end

@testset "lattice layer fallbacks" begin
    @test_throws "least-precise lattice" Compiler.widenlattice(Compiler.JLTypeLattice())
    𝕃 = Compiler.typeinf_lattice(Compiler.NativeInterpreter())
    @test !Compiler.is_const_prop_profitable_arg(𝕃, Int)
    @test !Compiler.is_const_prop_profitable_arg(𝕃, Compiler.PartialTypeVar(TypeVar(:T), true, true))
    @test Compiler.is_const_prop_profitable_arg(𝕃, Const(1))
    # the two-argument forms use the fallback lattice
    @test Compiler.:⊏(Const(1), Int) && !Compiler.:⊏(Int, Int)
    @test Compiler.is_lattice_equal(Const(nothing), Nothing)
    @test !Compiler.is_lattice_equal(Const(1), Int)
end

@testset "issimplertype" begin
    𝕃 = Compiler.typeinf_lattice(Compiler.NativeInterpreter())
    issimplertype(@nospecialize(a), @nospecialize(b)) = Compiler.issimplertype(𝕃, a, b)
    for C in (Conditional, InterConditional)
        c, c′, other = conditional_elements(C)
        @test issimplertype(c, Const(true))
        @test issimplertype(c, c′)
        @test !issimplertype(c, other)
        @test !issimplertype(c, Bool)
    end
    for A in (MustAlias, InterMustAlias)
        a, a′ = alias_elements(A)
        other = only(filter(x -> x.slot == 3, alias_elements(A)))
        @test issimplertype(a, a′)
        @test !issimplertype(a, other)
        @test !issimplertype(a, Int)
    end
    po, po′ = opaque_elements()
    @test issimplertype(po, po)
    @test !issimplertype(po, po′)
    @test !issimplertype(po, widenconst(po))
end

@testset "tmeet of PartialOpaque" begin
    𝕃 = Compiler.typeinf_lattice(Compiler.NativeInterpreter())
    po = opaque_elements()[1]
    po_wide = PartialOpaque(Core.OpaqueClosure{Tuple{Int}}, po.env, po.parent, po.source)
    @test Compiler.tmeet(𝕃, po, Core.OpaqueClosure) === po
    @test Compiler.tmeet(𝕃, po, String) === Union{}
    met = Compiler.tmeet(𝕃, po_wide, widenconst(po))
    @test met isa PartialOpaque && Compiler.is_lattice_equal(𝕃, met, po)
end

end # module lattice

# This file is a part of Julia. License is MIT: https://julialang.org/license

module lattice

using Test

include("setup_Compiler.jl")

# Algebraic properties of the inference lattices, checked over a sample of elements that
# mixes the extended lattice elements (`MustAlias`, `Conditional`, `PartialStruct`, ...)
# with plain types and constants. Inference relies on these for correctness: e.g.
# `update_bestguess!` and `stupdate!` only `tmerge` a new element if it is not already
# `⊑` the old one, so `a ⊑ b` must imply that merging `a` into `b` adds nothing.

using .Compiler: Const, PartialStruct, Conditional, MustAlias, InterConditional, InterMustAlias, widenconst

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
    Const(1), Const("x"), Const(nothing), Const(true), Bool,
    PartialStruct(Compiler.fallback_lattice, FF, Any[Int, Any]),
    PartialStruct(Compiler.fallback_lattice, FF, Any[Const(1), Any]),
    PartialStruct(Compiler.fallback_lattice, F, Any[Int]),
    F, FF,
]

alias_elements(::Type{MustAlias}) = Any[
    MustAlias(2, 0, F, 1, Union{Int,String}),
    MustAlias(2, 0, F, 1, Int),
    MustAlias(2, 0, F, 1, Const(1)),
    MustAlias(2, 0, FF, 1, Union{Int,Nothing}),
    MustAlias(2, 0, FF, 2, Any),
    MustAlias(3, 0, F, 1, Union{Int,String}),
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

# `tmerge(Const(true), cnd::Conditional)` correctly gives `Conditional(slot, Any, cnd.elsetype)`,
# but `⊑` only places `Const(true)` below the degenerate `Conditional(slot, Any, Union{})`,
# although the else branch is irrelevant for a value that is always `true` (and likewise for
# `Const(false)`)
function known_conditional_upper_bound_issue(@nospecialize(a), @nospecialize(b))
    function issue(@nospecialize(c), @nospecialize(cnd))
        c isa Const && cnd isa Union{Conditional,InterConditional} || return false
        return (c.val === true && cnd.elsetype !== Union{}) ||
               (c.val === false && cnd.thentype !== Union{})
    end
    return issue(a, b) || issue(b, a)
end

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
        if known_conditional_upper_bound_issue(a, b)
            @test_broken a ⊑ m && b ⊑ m
            continue
        end
        @test a ⊑ m
        @test b ⊑ m
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
    elements = Any[plain_elements(); conditional_elements(Conditional)]
    Compiler.has_mustalias(𝕃) && append!(elements, alias_elements(MustAlias))
    check_lattice_properties(𝕃, elements)
end

@testset "inter-procedural result lattice" begin
    𝕃 = Compiler.ipo_lattice(Compiler.NativeInterpreter())
    elements = Any[plain_elements(); conditional_elements(InterConditional)]
    Compiler.has_mustalias(Compiler.typeinf_lattice(Compiler.NativeInterpreter())) &&
        append!(elements, alias_elements(InterMustAlias))
    check_lattice_properties(𝕃, elements)
end

end # module lattice

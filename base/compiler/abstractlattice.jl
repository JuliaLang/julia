abstract type AbstractLattice; end
function widenlattice end

"""
    struct JLTypeLattice

A singleton type representing the lattice of Julia types, without any inference
extensions.
"""
struct JLTypeLattice <: AbstractLattice; end
widenlattice(::JLTypeLattice) = error("Type lattice is the least-precise lattice available")
is_valid_lattice(::JLTypeLattice, @nospecialize(elem)) = isa(elem, Type)

"""
    struct ConstsLattice

A lattice extending `JLTypeLattice` and adjoining `Const` and `PartialTypeVar`.
"""
struct ConstsLattice <: AbstractLattice; end
widenlattice(::ConstsLattice) = JLTypeLattice()
is_valid_lattice(lattice::ConstsLattice, @nospecialize(elem)) =
    is_valid_lattice(widenlattice(lattice), elem) || isa(elem, Const) || isa(elem, PartialTypeVar)

"""
    struct PartialsLattice{L}

A lattice extending lattice `L` and adjoining `PartialStruct` and `PartialOpaque`.
"""
struct PartialsLattice{L <: AbstractLattice} <: AbstractLattice
    parent::L
end
widenlattice(L::PartialsLattice) = L.parent
is_valid_lattice(lattice::PartialsLattice, @nospecialize(elem)) =
    is_valid_lattice(widenlattice(lattice), elem) ||
    isa(elem, PartialStruct) || isa(elem, PartialOpaque)

"""
    struct ConditionalsLattice{L}

A lattice extending lattice `L` and adjoining `Conditional`.
"""
struct ConditionalsLattice{L <: AbstractLattice} <: AbstractLattice
    parent::L
end
widenlattice(L::ConditionalsLattice) = L.parent
is_valid_lattice(lattice::ConditionalsLattice, @nospecialize(elem)) =
    is_valid_lattice(widenlattice(lattice), elem) || isa(elem, Conditional)

struct InterConditionalsLattice{L <: AbstractLattice} <: AbstractLattice
    parent::L
end
widenlattice(L::InterConditionalsLattice) = L.parent
is_valid_lattice(lattice::InterConditionalsLattice, @nospecialize(elem)) =
    is_valid_lattice(widenlattice(lattice), elem) || isa(elem, InterConditional)

const AnyConditionalsLattice{L} = Union{ConditionalsLattice{L}, InterConditionalsLattice{L}}
const BaseInferenceLattice = typeof(ConditionalsLattice(PartialsLattice(ConstsLattice())))
const IPOResultLattice = typeof(InterConditionalsLattice(PartialsLattice(ConstsLattice())))

"""
    struct InferenceLattice{L}

The full lattice used for abstract interpretation during inference. Takes
a base lattice and adjoins `LimitedAccuracy`.
"""
struct InferenceLattice{L} <: AbstractLattice
    parent::L
end
widenlattice(L::InferenceLattice) = L.parent
is_valid_lattice(lattice::InferenceLattice, @nospecialize(elem)) =
    is_valid_lattice(widenlattice(lattice), elem) || isa(elem, LimitedAccuracy)

"""
    struct OptimizerLattice

The lattice used by the optimizer. Extends
`BaseInferenceLattice` with `MaybeUndef`.
"""
struct OptimizerLattice <: AbstractLattice; end
widenlattice(L::OptimizerLattice) = BaseInferenceLattice.instance
is_valid_lattice(lattice::OptimizerLattice, @nospecialize(elem)) =
    is_valid_lattice(widenlattice(lattice), elem) || isa(elem, MaybeUndef)

"""
    tmeet(lattice, a, b::Type)

Compute the lattice meet of lattice elements `a` and `b` over the lattice
`lattice`. If `lattice` is `JLTypeLattice`, this is equivalent to type
intersection. Note that currently `b` is restricted to being a type (interpreted
as a lattice element in the JLTypeLattice sub-lattice of `lattice`).
"""
function tmeet end

function tmeet(::JLTypeLattice, @nospecialize(a::Type), @nospecialize(b::Type))
    ti = typeintersect(a, b)
    valid_as_lattice(ti) || return Bottom
    return ti
end

"""
    tmerge(lattice, a, b)

Compute a lattice join of elements `a` and `b` over the lattice `lattice`.
Note that the computed element need not be the least upper bound of `a` and
`b`, but rather, we impose additional limitations on the complexity of the
joined element, ideally without losing too much precision in common cases and
remaining mostly associative and commutative.
"""
function tmerge end

"""
    ⊑(lattice, a, b)

Compute the lattice ordering (i.e. less-than-or-equal) relationship between
lattice elements `a` and `b` over the lattice `lattice`. If `lattice` is
`JLTypeLattice`, this is equivalent to subtyping.
"""
function ⊑ end

⊑(::JLTypeLattice, @nospecialize(a::Type), @nospecialize(b::Type)) = a <: b

"""
    ⊏(lattice, a, b) -> Bool

The strict partial order over the type inference lattice.
This is defined as the irreflexive kernel of `⊑`.
"""
⊏(lattice::AbstractLattice, @nospecialize(a), @nospecialize(b)) = ⊑(lattice, a, b) && !⊑(lattice, b, a)

"""
    ⋤(lattice, a, b) -> Bool

This order could be used as a slightly more efficient version of the strict order `⊏`,
where we can safely assume `a ⊑ b` holds.
"""
⋤(lattice::AbstractLattice, @nospecialize(a), @nospecialize(b)) = !⊑(lattice, b, a)

"""
    is_lattice_equal(lattice, a, b) -> Bool

Check if two lattice elements are partial order equivalent.
This is basically `a ⊑ b && b ⊑ a` but (optionally) with extra performance optimizations.
"""
function is_lattice_equal(lattice::AbstractLattice, @nospecialize(a), @nospecialize(b))
    a === b && return true
    ⊑(lattice, a, b) && ⊑(lattice, b, a)
end

"""
    has_nontrivial_const_info(lattice, t) -> Bool

Determine whether the given lattice element `t` of `lattice` has non-trivial
constant information that would not be available from the type itself.
"""
has_nontrivial_const_info(lattice::AbstractLattice, @nospecialize t) =
    has_nontrivial_const_info(widenlattice(lattice), t)
has_nontrivial_const_info(::JLTypeLattice, @nospecialize(t)) = false

# Curried versions
⊑(lattice::AbstractLattice) = (@nospecialize(a), @nospecialize(b)) -> ⊑(lattice, a, b)
⊏(lattice::AbstractLattice) = (@nospecialize(a), @nospecialize(b)) -> ⊏(lattice, a, b)
⋤(lattice::AbstractLattice) = (@nospecialize(a), @nospecialize(b)) -> ⋤(lattice, a, b)

# Fallbacks for external packages using these methods
const fallback_lattice = InferenceLattice(BaseInferenceLattice.instance)
const fallback_ipo_lattice = InferenceLattice(IPOResultLattice.instance)

⊑(@nospecialize(a), @nospecialize(b)) = ⊑(fallback_lattice, a, b)
tmeet(@nospecialize(a), @nospecialize(b)) = tmeet(fallback_lattice, a, b)
tmerge(@nospecialize(a), @nospecialize(b)) = tmerge(fallback_lattice, a, b)
⊏(@nospecialize(a), @nospecialize(b)) = ⊏(fallback_lattice, a, b)
⋤(@nospecialize(a), @nospecialize(b)) = ⋤(fallback_lattice, a, b)
is_lattice_equal(@nospecialize(a), @nospecialize(b)) = is_lattice_equal(fallback_lattice, a, b)

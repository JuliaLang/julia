# TODO add more documentations

function widenlattice end
function is_valid_lattice_norec end

"""
    struct JLTypeLattice <: AbstractLattice

A singleton type representing the lattice of Julia types, without any inference extensions.
"""
struct JLTypeLattice <: AbstractLattice; end
widenlattice(::JLTypeLattice) = error("Type lattice is the least-precise lattice available")
is_valid_lattice_norec(::JLTypeLattice, @nospecialize(elem)) = isa(elem, Type)

"""
    struct ConstsLattice <: AbstractLattice

A lattice extending `JLTypeLattice` and adjoining `Const` and `PartialTypeVar`.
"""
struct ConstsLattice <: AbstractLattice; end
widenlattice(::ConstsLattice) = JLTypeLattice()
is_valid_lattice_norec(::ConstsLattice, @nospecialize(elem)) = isa(elem, Const) || isa(elem, PartialTypeVar)

"""
    struct PartialsLattice{𝕃<:AbstractLattice} <: AbstractLattice

A lattice extending a base lattice `𝕃` and adjoining `PartialStruct` and `PartialOpaque`.
"""
struct PartialsLattice{𝕃<:AbstractLattice} <: AbstractLattice
    parent::𝕃
end
widenlattice(𝕃::PartialsLattice) = 𝕃.parent
is_valid_lattice_norec(::PartialsLattice, @nospecialize(elem)) = isa(elem, PartialStruct) || isa(elem, PartialOpaque)

"""
    struct ConditionalsLattice{𝕃<:AbstractLattice} <: AbstractLattice

A lattice extending a base lattice `𝕃` and adjoining `Conditional`.
"""
struct ConditionalsLattice{𝕃<:AbstractLattice} <: AbstractLattice
    parent::𝕃
end
widenlattice(𝕃::ConditionalsLattice) = 𝕃.parent
is_valid_lattice_norec(::ConditionalsLattice, @nospecialize(elem)) = isa(elem, Conditional)

"""
    struct InterConditionalsLattice{𝕃<:AbstractLattice} <: AbstractLattice

A lattice extending a base lattice `𝕃` and adjoining `InterConditional`.
"""
struct InterConditionalsLattice{𝕃<:AbstractLattice} <: AbstractLattice
    parent::𝕃
end
widenlattice(𝕃::InterConditionalsLattice) = 𝕃.parent
is_valid_lattice_norec(::InterConditionalsLattice, @nospecialize(elem)) = isa(elem, InterConditional)

"""
    struct MustAliasesLattice{𝕃<:AbstractLattice}

A lattice extending lattice `𝕃` and adjoining `MustAlias`.
"""
struct MustAliasesLattice{𝕃<:AbstractLattice} <: AbstractLattice
    parent::𝕃
end
widenlattice(𝕃::MustAliasesLattice) = 𝕃.parent
is_valid_lattice_norec(::MustAliasesLattice, @nospecialize(elem)) = isa(elem, MustAlias)

"""
    struct InterMustAliasesLattice{𝕃<:AbstractLattice}

A lattice extending lattice `𝕃` and adjoining `InterMustAlias`.
"""
struct InterMustAliasesLattice{𝕃<:AbstractLattice} <: AbstractLattice
    parent::𝕃
end
widenlattice(𝕃::InterMustAliasesLattice) = 𝕃.parent
is_valid_lattice_norec(::InterMustAliasesLattice, @nospecialize(elem)) = isa(elem, InterMustAlias)

const AnyConditionalsLattice{𝕃<:AbstractLattice} = Union{ConditionalsLattice{𝕃}, InterConditionalsLattice{𝕃}}
const AnyMustAliasesLattice{𝕃<:AbstractLattice} = Union{MustAliasesLattice{𝕃}, InterMustAliasesLattice{𝕃}}

const SimpleInferenceLattice = typeof(PartialsLattice(ConstsLattice()))
const BaseInferenceLattice = typeof(MustAliasesLattice(ConditionalsLattice(SimpleInferenceLattice.instance)))
const IPOResultLattice = typeof(InterMustAliasesLattice(InterConditionalsLattice(SimpleInferenceLattice.instance)))

"""
    struct InferenceLattice{𝕃<:AbstractLattice} <: AbstractLattice

The full lattice used for abstract interpretation during inference.
Extends a base lattice `𝕃` and adjoins `LimitedAccuracy`.
"""
struct InferenceLattice{𝕃<:AbstractLattice} <: AbstractLattice
    parent::𝕃
end
widenlattice(𝕃::InferenceLattice) = 𝕃.parent
is_valid_lattice_norec(::InferenceLattice, @nospecialize(elem)) = isa(elem, LimitedAccuracy)

"""
    tmeet(𝕃::AbstractLattice, a, b::AnyType)

Compute the lattice meet of lattice elements `a` and `b` over the lattice `𝕃`,
dropping any results that will not be inhabited at runtime.
If `𝕃` is `JLTypeLattice`, this is equivalent to type intersection plus the
elimination of results that have no concrete subtypes.
Note that currently `b` is restricted to being a type
(interpreted as a lattice element in the `JLTypeLattice` sub-lattice of `𝕃`).
"""
function tmeet end

function tmeet(::JLTypeLattice, @nospecialize(a::AnyType), @nospecialize(b::AnyType))
    ti = typeintersect(a, b)
    valid_as_lattice(ti, true) || return Bottom
    return ti
end

"""
    tmerge(𝕃::AbstractLattice, a, b)

Compute a lattice join of elements `a` and `b` over the lattice `𝕃`.
Note that the computed element need not be the least upper bound of `a` and
`b`, but rather, we impose additional limitations on the complexity of the
joined element, ideally without losing too much precision in common cases and
remaining mostly associative and commutative.
"""
function tmerge end

"""
    tmerge_field(𝕃::AbstractLattice, a, b) -> nothing or lattice element

Compute a lattice join of elements `a` and `b` over the lattice `𝕃`,
where `a` and `b` are fields of `PartialStruct` or `Const`.
This is an opt-in interface to allow external lattice implementation to provide its own
field-merge strategy. If it returns `nothing`, `tmerge(::PartialsLattice, ...)`
will use the default aggressive type merge implementation that does not use `tmerge`
recursively to reach convergence.
"""
function tmerge_field end

function tmerge_field(𝕃::AbstractLattice, @nospecialize(a), @nospecialize(b))
    return tmerge_field(widenlattice(𝕃), a, b)
end
tmerge_field(::JLTypeLattice, @nospecialize(a), @nospecialize(b)) = nothing

"""
    ⊑(𝕃::AbstractLattice, a, b)

Compute the lattice ordering (i.e. less-than-or-equal) relationship between
lattice elements `a` and `b` over the lattice `𝕃`.
If `𝕃` is `JLTypeLattice`, this is equivalent to subtyping.
"""
function ⊑ end

@nospecializeinfer ⊑(::JLTypeLattice, @nospecialize(a::AnyType), @nospecialize(b::AnyType)) = a <: b

"""
    ⊏(𝕃::AbstractLattice, a, b)::Bool

The strict partial order over the type inference lattice.
This is defined as the irreflexive kernel of `⊑`.
"""
@nospecializeinfer ⊏(𝕃::AbstractLattice, @nospecialize(a), @nospecialize(b)) = ⊑(𝕃, a, b) && !⊑(𝕃, b, a)

"""
    ⋤(𝕃::AbstractLattice, a, b)::Bool

This order could be used as a slightly more efficient version of the strict order `⊏`,
where we can safely assume `a ⊑ b` holds.
"""
@nospecializeinfer ⋤(𝕃::AbstractLattice, @nospecialize(a), @nospecialize(b)) = !⊑(𝕃, b, a)

"""
    is_lattice_equal(𝕃::AbstractLattice, a, b)::Bool

Check if two lattice elements are partial order equivalent.
This is basically `a ⊑ b && b ⊑ a` in the lattice of `𝕃`
but (optionally) with extra performance optimizations.
"""
@nospecializeinfer function is_lattice_equal(𝕃::AbstractLattice, @nospecialize(a), @nospecialize(b))
    a === b && return true
    return ⊑(𝕃, a, b) && ⊑(𝕃, b, a)
end

"""
    has_nontrivial_extended_info(𝕃::AbstractLattice, t)::Bool

Determine whether the given lattice element `t` of `𝕃` has non-trivial extended lattice
information that would not be available from the type itself.
"""
@nospecializeinfer has_nontrivial_extended_info(𝕃::AbstractLattice, @nospecialize t) =
    has_nontrivial_extended_info(widenlattice(𝕃), t)
@nospecializeinfer function has_nontrivial_extended_info(𝕃::PartialsLattice, @nospecialize t)
    isa(t, PartialStruct) && return true
    isa(t, PartialOpaque) && return true
    return has_nontrivial_extended_info(widenlattice(𝕃), t)
end
@nospecializeinfer function has_nontrivial_extended_info(𝕃::ConstsLattice, @nospecialize t)
    isa(t, PartialTypeVar) && return true
    if isa(t, Const)
        val = t.val
        # a type-valued `Const` may pin `=== val` beyond its widening (which for
        # an open `val` is only the `==`-class `Type{val}`)
        return !issingletontype(typeof(val))
    end
    return has_nontrivial_extended_info(widenlattice(𝕃), t)
end
@nospecializeinfer has_nontrivial_extended_info(::JLTypeLattice, @nospecialize(t)) = false

"""
    is_const_prop_profitable_arg(𝕃::AbstractLattice, t)::Bool

Determine whether the given lattice element `t` of `𝕃` has new extended lattice information
that should be forwarded along with constant propagation.
"""
@nospecializeinfer is_const_prop_profitable_arg(𝕃::AbstractLattice, @nospecialize t) =
    is_const_prop_profitable_arg(widenlattice(𝕃), t)
@nospecializeinfer function is_const_prop_profitable_arg(𝕃::PartialsLattice, @nospecialize t)
    if isa(t, PartialStruct)
        return true # might be a bit aggressive, may want to enable some check like follows:
        # for i = 1:length(t.fields)
        #     fld = t.fields[i]
        #     isconstType(fld) && return true
        #     is_const_prop_profitable_arg(fld) && return true
        #     fld ⊏ fieldtype(t.typ, i) && return true
        # end
        # return false
    end
    isa(t, PartialOpaque) && return true
    return is_const_prop_profitable_arg(widenlattice(𝕃), t)
end
@nospecializeinfer function is_const_prop_profitable_arg(𝕃::ConstsLattice, @nospecialize t)
    if isa(t, Const)
        # don't consider mutable values useful constants unless they have const fields
        val = t.val
        return isa(val, Symbol) || isa(val, Type) || isa(val, Method) || isa(val, CodeInstance) ||
                    !ismutable(val) || (typeof(val).name.constfields != C_NULL)
    end
    isa(t, PartialTypeVar) && return false # this isn't forwardable
    return is_const_prop_profitable_arg(widenlattice(𝕃), t)
end
@nospecializeinfer is_const_prop_profitable_arg(::JLTypeLattice, @nospecialize t) = false

@nospecializeinfer is_forwardable_argtype(𝕃::AbstractLattice, @nospecialize(x)) =
    is_forwardable_argtype(widenlattice(𝕃), x)
@nospecializeinfer function is_forwardable_argtype(𝕃::ConditionalsLattice, @nospecialize x)
    isa(x, Conditional) && return true
    return is_forwardable_argtype(widenlattice(𝕃), x)
end
@nospecializeinfer function is_forwardable_argtype(𝕃::PartialsLattice, @nospecialize x)
    isa(x, PartialStruct) && return true
    isa(x, PartialOpaque) && return true
    return is_forwardable_argtype(widenlattice(𝕃), x)
end
@nospecializeinfer function is_forwardable_argtype(𝕃::ConstsLattice, @nospecialize x)
    isa(x, Const) && return true
    return is_forwardable_argtype(widenlattice(𝕃), x)
end
@nospecializeinfer function is_forwardable_argtype(𝕃::MustAliasesLattice, @nospecialize x)
    isa(x, MustAlias) && return true
    return is_forwardable_argtype(widenlattice(𝕃), x)
end
@nospecializeinfer is_forwardable_argtype(::JLTypeLattice, @nospecialize x) = false

"""
    widenreturn(𝕃ᵢ::AbstractLattice, @nospecialize(rt), info::BestguessInfo) -> new_bestguess
    widenreturn_noslotwrapper(𝕃ᵢ::AbstractLattice, @nospecialize(rt), info::BestguessInfo) -> new_bestguess

Appropriately converts inferred type of a return value `rt` to such a type
that we know we can store in the cache and is valid and good inter-procedurally,
e.g. if `rt isa Conditional` then `rt` should be converted to `InterConditional`
or the other cacheable lattice element.

External lattice `𝕃ᵢ::ExternalLattice` may overload:
- `widenreturn(𝕃ᵢ::ExternalLattice, @nospecialize(rt), info::BestguessInfo)`
- `widenreturn_noslotwrapper(𝕃ᵢ::ExternalLattice, @nospecialize(rt), info::BestguessInfo)`
"""
function widenreturn end, function widenreturn_noslotwrapper end

@nospecializeinfer is_valid_lattice(𝕃::AbstractLattice, @nospecialize(elem)) =
    is_valid_lattice_norec(𝕃, elem) && is_valid_lattice(widenlattice(𝕃), elem)
@nospecializeinfer is_valid_lattice(𝕃::JLTypeLattice, @nospecialize(elem)) = is_valid_lattice_norec(𝕃, elem)

has_conditional(𝕃::AbstractLattice) = has_conditional(widenlattice(𝕃))
has_conditional(::AnyConditionalsLattice) = true
has_conditional(::JLTypeLattice) = false

has_mustalias(𝕃::AbstractLattice) = has_mustalias(widenlattice(𝕃))
has_mustalias(::AnyMustAliasesLattice) = true
has_mustalias(::JLTypeLattice) = false

has_extended_unionsplit(𝕃::AbstractLattice) = has_extended_unionsplit(widenlattice(𝕃))
has_extended_unionsplit(::AnyMustAliasesLattice) = true
has_extended_unionsplit(::JLTypeLattice) = false

# Curried versions
⊑(𝕃::AbstractLattice) = (@nospecialize(a), @nospecialize(b)) -> ⊑(𝕃, a, b)
⊏(𝕃::AbstractLattice) = (@nospecialize(a), @nospecialize(b)) -> ⊏(𝕃, a, b)
⋤(𝕃::AbstractLattice) = (@nospecialize(a), @nospecialize(b)) -> ⋤(𝕃, a, b)
tmerge(𝕃::AbstractLattice) = (@nospecialize(a), @nospecialize(b)) -> tmerge(𝕃, a, b)
tmeet(𝕃::AbstractLattice) = (@nospecialize(a), @nospecialize(b)) -> tmeet(𝕃, a, b)
partialorder(𝕃::AbstractLattice) = ⊑(𝕃)
strictpartialorder(𝕃::AbstractLattice) = ⊏(𝕃)
strictneqpartialorder(𝕃::AbstractLattice) = ⋤(𝕃)
join(𝕃::AbstractLattice) = tmerge(𝕃)
meet(𝕃::AbstractLattice) = tmeet(𝕃)

# Fallbacks for external packages using these methods
const fallback_lattice = InferenceLattice(BaseInferenceLattice.instance)
const fallback_ipo_lattice = InferenceLattice(IPOResultLattice.instance)

@nospecializeinfer @nospecialize(a) ⊑ @nospecialize(b) = ⊑(fallback_lattice, a, b)
@nospecializeinfer @nospecialize(a) ⊏ @nospecialize(b) = ⊏(fallback_lattice, a, b)
@nospecializeinfer @nospecialize(a) ⋤ @nospecialize(b) = ⋤(fallback_lattice, a, b)
@nospecializeinfer tmeet(@nospecialize(a), @nospecialize(b)) = tmeet(fallback_lattice, a, b)
@nospecializeinfer tmerge(@nospecialize(a), @nospecialize(b)) = tmerge(fallback_lattice, a, b)
@nospecializeinfer is_lattice_equal(@nospecialize(a), @nospecialize(b)) = is_lattice_equal(fallback_lattice, a, b)

# Widenlattice with argument
widenlattice(::JLTypeLattice, @nospecialize(t)) = widenconst(t)
function widenlattice(𝕃::AbstractLattice, @nospecialize(t))
    is_valid_lattice_norec(𝕃, t) && return t
    widenlattice(widenlattice(𝕃), t)
end

# This file is a part of Julia. License is MIT: https://julialang.org/license

module Pkg2Types

export VersionInterval, VersionSet
import Base: show, isempty, in, intersect, union!, union, ==, hash, copy, deepcopy_internal

import Pkg3.Types: VersionBound, VersionRange

struct VersionInterval
    lower::VersionNumber
    upper::VersionNumber
end
VersionInterval(lower::VersionNumber) = VersionInterval(lower,typemax(VersionNumber))
VersionInterval() = VersionInterval(typemin(VersionNumber))

show(io::IO, i::VersionInterval) = print(io, "[$(i.lower),$(i.upper))")
isempty(i::VersionInterval) = i.upper <= i.lower
in(v::VersionNumber, i::VersionInterval) = i.lower <= v < i.upper
intersect(a::VersionInterval, b::VersionInterval) = VersionInterval(max(a.lower,b.lower), min(a.upper,b.upper))
==(a::VersionInterval, b::VersionInterval) = a.lower == b.lower && a.upper == b.upper
hash(i::VersionInterval, h::UInt) = hash((i.lower, i.upper), h + (0x0f870a92db508386 % UInt))

# Used to translate from Pkg2 intervals to Pkg3 ranges
function Base.convert(::Type{VersionRange}, a::VersionInterval)
    lower, upper = a.lower, a.upper

    lb = VersionBound(lower.major, lower.minor, lower.patch)

    vb = Int[upper.major, upper.minor, upper.patch]
    i = 3
    while i > 0 && vb[i] == 0
        pop!(vb)
        i -= 1
    end
    # NOTE: an upper bound of 0 could happen in principle, e.g. [v"0.0.0-", v"0.0.0")
    #       but we just ignore this here...
    i > 0 || error("invalid interval upper bound v$upper")
    vb[i] -= 1
    ub = VersionBound(vb...)

    return VersionRange(lb, ub)
end

function normalize!(ivals::Vector{VersionInterval})
    # VersionSet internal normalization:
    # removes empty intervals and fuses intervals without gaps
    # e.g.:
    #     [0.0.0,1.0.0) ∪ [1.0.0,1.5.0) ∪ [1.6.0,1.6.0) ∪ [2.0.0,∞)
    # becomes:
    #     [0.0.0,1.5.0) ∪ [2.0.0,∞)
    # (still assumes that lower bounds are sorted, and intervals do
    # not overlap)
    l = length(ivals)
    l == 0 && return ivals

    lo, up, k0 = ivals[1].lower, ivals[1].upper, 1
    fusing = false
    for k = 2:l
        lo1, up1 = ivals[k].lower, ivals[k].upper
        if lo1 == up
            up = up1
            fusing = true
            continue
        end
        if lo < up
            # The only purpose of the "fusing" check is to avoid
            # extra allocations
            ivals[k0] = fusing ? VersionInterval(lo, up) : ivals[k-1]
            k0 += 1
        end
        fusing = false
        lo, up = lo1, up1
    end
    if lo < up
        ivals[k0] = fusing ? VersionInterval(lo, up) : ivals[l]
        k0 += 1
    end
    resize!(ivals, k0 - 1)
    return ivals
end

struct VersionSet
    intervals::Vector{VersionInterval}
    VersionSet(intervals::Vector{VersionInterval}) = new(normalize!(intervals))
    # copy is defined inside the struct block to call `new` directly
    # without going through `normalize!`
    Base.copy(vset::VersionSet) = new(copy(vset.intervals))
end
function VersionSet(versions::Vector{VersionNumber})
    intervals = VersionInterval[]
    if isempty(versions)
        push!(intervals, VersionInterval())
    else
        isodd(length(versions)) && push!(versions, typemax(VersionNumber))
        while !isempty(versions)
            push!(intervals, VersionInterval(popfirst!(versions), popfirst!(versions)))
        end
    end
    VersionSet(intervals)
end
VersionSet(versions::VersionNumber...) = VersionSet(VersionNumber[versions...])

const empty_versionset = VersionSet(VersionInterval[])

# Windows console doesn't like Unicode
const _empty_symbol = @static Sys.iswindows() ? "empty" : "∅"
const _union_symbol = @static Sys.iswindows() ? " or " : " ∪ "
show(io::IO, s::VersionSet) = isempty(s) ? print(io, _empty_symbol) :
                                           join(io, s.intervals, _union_symbol)
isempty(s::VersionSet) = all(isempty, s.intervals)
in(v::VersionNumber, s::VersionSet) = any(i->in(v,i), s.intervals)
function intersect(A::VersionSet, B::VersionSet)
    (isempty(A) || isempty(B)) && return copy(empty_versionset)
    ivals = [intersect(a,b) for a in A.intervals for b in B.intervals]
    sort!(ivals, by=i->i.lower)
    VersionSet(ivals)
end

union(A::VersionSet, B::VersionSet) = union!(copy(A), B)
function union!(A::VersionSet, B::VersionSet)
    A == B && return A
    ivals = A.intervals
    for intB in B.intervals
        lB, uB = intB.lower, intB.upper
        k0 = findfirst(i->(i.upper > lB), ivals)
        if k0 == 0
            push!(ivals, intB)
            continue
        end
        lB = min(lB, ivals[k0].lower)
        for k1 = k0:length(ivals)
            intA = ivals[k1]
            if uB < intA.lower
                splice!(ivals, k0:(k1-1), (VersionInterval(lB, uB),))
                break
            elseif uB ∈ intA || k1 == length(ivals)
                splice!(ivals, k0:k1, (VersionInterval(lB, max(uB, intA.upper)),))
                break
            end
        end
    end
    normalize!(ivals)
    return A
end

==(A::VersionSet, B::VersionSet) = A.intervals == B.intervals
hash(s::VersionSet, h::UInt) = hash(s.intervals, h + (0x2fd2ca6efa023f44 % UInt))
deepcopy_internal(vs::VersionSet, ::IdDict) = copy(vs)

end # module
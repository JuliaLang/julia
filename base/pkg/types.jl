# This file is a part of Julia. License is MIT: http://julialang.org/license

module Types

export VersionInterval, VersionSet, Requires, Available, Fixed, merge_requires!, satisfies,
       ResolveBacktraceItem, ResolveBacktrace
import Base: show, isempty, in, intersect, union!, union, ==, hash, copy, deepcopy_internal, push!

immutable VersionInterval
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

immutable VersionSet
    intervals::Vector{VersionInterval}
end
function VersionSet(versions::Vector{VersionNumber})
    intervals = VersionInterval[]
    if isempty(versions)
        push!(intervals, VersionInterval())
    else
        isodd(length(versions)) && push!(versions, typemax(VersionNumber))
        while !isempty(versions)
            push!(intervals, VersionInterval(shift!(versions), shift!(versions)))
        end
    end
    VersionSet(intervals)
end
VersionSet(versions::VersionNumber...) = VersionSet(VersionNumber[versions...])

show(io::IO, s::VersionSet) = join(io, s.intervals, " ∪ ")
isempty(s::VersionSet) = all(isempty, s.intervals)
in(v::VersionNumber, s::VersionSet) = any(i->in(v,i), s.intervals)
function intersect(A::VersionSet, B::VersionSet)
    ivals = [intersect(a,b) for a in A.intervals for b in B.intervals]
    filter!(i->!isempty(i), ivals)
    sort!(ivals, by=i->i.lower)
    VersionSet(ivals)
end
copy(A::VersionSet) = VersionSet(copy(A.intervals))

function normalize!(A::VersionSet)
    # removes empty intervals and fuses intervals without gaps
    # e.g.:
    #     [0.0.0,1.0.0) ∪ [1.0.0,1.5.0) ∪ [1.6.0,1.6.0) ∪ [2.0.0,∞)
    # becomes:
    #     [0.0.0,1.5.0) ∪ [2.0.0,∞)
    # (still assumes that intervals are properly sorted)
    ivals = A.intervals
    l = length(ivals)
    for k = l:-1:1
        isempty(ivals[k]) && (splice!(ivals, k); l -= 1)
    end
    l > 1 || return A
    lo, up, k0 = ivals[l].lower, ivals[l].upper, l
    fuse = false
    for k = (l-1):-1:1
        lo1, up1 = ivals[k].lower, ivals[k].upper
        if up1 == lo
            fuse = true
            lo = lo1
            continue
        end
        fuse && splice!(ivals, (k+1):k0, (VersionInterval(lo, up),))
        fuse = false
        lo, up = lo1, up1
        k0 = k
    end
    fuse && splice!(ivals, 1:k0, (VersionInterval(lo, up),))
    return A
end

union(A::VersionSet, B::VersionSet) = union!(copy(A), B)
function union!(A::VersionSet, B::VersionSet)
    A == B && return normalize!(A)
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
    return normalize!(VersionSet(ivals))
end

==(A::VersionSet, B::VersionSet) = A.intervals == B.intervals
hash(s::VersionSet, h::UInt) = hash(s.intervals, h + (0x2fd2ca6efa023f44 % UInt))
deepcopy_internal(vs::VersionSet, ::ObjectIdDict) = copy(vs)

typealias Requires Dict{String,VersionSet}

function merge_requires!(A::Requires, B::Requires)
    for (pkg,vers) in B
        A[pkg] = haskey(A,pkg) ? intersect(A[pkg],vers) : vers
    end
    return A
end

satisfies(pkg::AbstractString, ver::VersionNumber, reqs::Requires) =
    !haskey(reqs, pkg) || in(ver, reqs[pkg])

immutable Available
    sha1::String
    requires::Requires
end

==(a::Available, b::Available) = a.sha1 == b.sha1 && a.requires == b.requires
hash(a::Available, h::UInt) = hash((a.sha1, a.requires), h + (0xbc8ae0de9d11d972 % UInt))
copy(a::Available) = Available(a.sha1, copy(a.requires))

show(io::IO, a::Available) = isempty(a.requires) ?
    print(io, "Available(", repr(a.sha1), ")") :
    print(io, "Available(", repr(a.sha1), ",", a.requires, ")")

immutable Fixed
    version::VersionNumber
    requires::Requires
end
Fixed(v::VersionNumber) = Fixed(v,Requires())

==(a::Fixed, b::Fixed) = a.version == b.version && a.requires == b.requires
hash(f::Fixed, h::UInt) = hash((f.version, f.requires), h + (0x68628b809fd417ca % UInt))

show(io::IO, f::Fixed) = isempty(f.requires) ?
    print(io, "Fixed(", repr(f.version), ")") :
    print(io, "Fixed(", repr(f.version), ",", f.requires, ")")

# TODO: Available & Fixed are almost the same – merge them?
# Free could include the same information too, it just isn't
# required by anything that processes these things.


typealias VersionReq Union{VersionNumber,VersionSet}
typealias WhyReq Tuple{VersionReq,Any}

# This is used to keep track of dependency relations when propagating
# requirements, so as to emit useful information in case of unsatisfiable
# conditions.
# The `versionreq` field keeps track of the remaining allowed versions,
# intersecting all requirements.
# The `why` field is a Vector which keeps track of the requirements. Each
# entry is a Tuple of two elements:
# 1) the first element is the version requirement (can be a single VersionNumber
#    or a VersionSet).
# 2) the second element can be either :fixed (for requirements induced by
#    fixed packages), :required (for requirements induced by explicitly
#    required packages), or a Pair p=>backtrace_item (for requirements induced
#    indirectly, where `p` is the package name and `backtrace_item` is
#    another ResolveBacktraceItem.
type ResolveBacktraceItem
    versionreq::VersionReq
    why::Vector{WhyReq}
    ResolveBacktraceItem() = new(VersionSet(), WhyReq[])
    ResolveBacktraceItem(reason, versionreq::VersionReq) = new(versionreq, WhyReq[(versionreq,reason)])
end

const empty_versionset = VersionSet([v"0.0",v"0.0"])

function push!(ritem::ResolveBacktraceItem, reason, versionset::VersionSet)
    if isa(ritem.versionreq, VersionSet)
        ritem.versionreq = ritem.versionreq ∩ versionset
    elseif ritem.versionreq ∉ versionset
        ritem.versionreq = empty_versionset
    end
    push!(ritem.why, (versionset,reason))
end

function push!(ritem::ResolveBacktraceItem, reason, version::VersionNumber)
    if isa(ritem.versionreq, VersionSet)
        if version ∈ ritem.versionreq
            ritem.versionreq = version
        else
            ritem.versionreq = empty_versionset
        end
    elseif ritem.versionreq ≠ version
        ritem.versionreq = empty_versionset
    end
    push!(ritem.why, (version,reason))
end


show(io::IO, ritem::ResolveBacktraceItem) = _show(io, ritem, "", Set{ResolveBacktraceItem}([ritem]))

function _show(io::IO, ritem::ResolveBacktraceItem, indent::String, seen::Set{ResolveBacktraceItem})
    l = length(ritem.why)
    for (i,(vs,w)) in enumerate(ritem.why)
        print(io, indent, (i==l ? '└' : '├'), '─')
        if w ≡ :fixed
            @assert isa(vs, VersionNumber)
            println(io, "version $vs set by fixed requirement (package is checked out, dirty or pinned)")
        elseif w ≡ :required
            @assert isa(vs, VersionSet)
            println(io, "version range $vs set by an explicit requirement")
        else
            @assert isa(w, Pair{<:AbstractString,ResolveBacktraceItem})
            if isa(vs, VersionNumber)
                print(io, "version $vs ")
            else
                print(io, "version range $vs ")
            end
            print(io, "required by package $(w[1]), ")
            if isa(w[2].versionreq, VersionSet)
                if !isempty(w[2].versionreq)
                    println(io, "whose allowed version range is $(w[2].versionreq):")
                else
                    println(io, "whose allowed version range is empty:")
                end
            else
                println(io, "whose only allowed version is $(w[2].versionreq):")
            end
            if w[2] ∈ seen
                println(io, (i==l ? "  " : "│ ") * indent, "└─[see above for $(w[1]) backtrace]")
                continue
            end
            push!(seen, w[2])
            _show(io, w[2], (i==l ? "  " : "│ ") * indent, seen)
        end
    end
end

typealias ResolveBacktrace Dict{AbstractString,ResolveBacktraceItem}

end # module

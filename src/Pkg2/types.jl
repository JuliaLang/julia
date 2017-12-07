# This file is a part of Julia. License is MIT: https://julialang.org/license

module Pkg2Types

export Requires, Available, Fixed, merge_requires!, satisfies,
       ResolveBacktraceItem, ResolveBacktrace
import Base: show, isempty, in, intersect, union!, union, ==, hash, copy, deepcopy_internal, push!

import Pkg3.equalto
import ...iswindows

import ..Types: VersionSpec

const Requires = Dict{String,VersionSpec}

function merge_requires!(A::Requires, B::Requires)
    for (pkg,vers) in B
        A[pkg] = haskey(A,pkg) ? intersect(A[pkg],vers) : vers
    end
    return A
end

satisfies(pkg::AbstractString, ver::VersionNumber, reqs::Requires) =
    !haskey(reqs, pkg) || in(ver, reqs[pkg])

struct Available
    sha1::String
    requires::Requires
end

==(a::Available, b::Available) = a.sha1 == b.sha1 && a.requires == b.requires
hash(a::Available, h::UInt) = hash((a.sha1, a.requires), h + (0xbc8ae0de9d11d972 % UInt))
copy(a::Available) = Available(a.sha1, copy(a.requires))

show(io::IO, a::Available) = isempty(a.requires) ?
    print(io, "Available(", repr(a.sha1), ")") :
    print(io, "Available(", repr(a.sha1), ",", a.requires, ")")

struct Fixed
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


const VersionReq = Union{VersionNumber,VersionSpec}
const WhyReq = Tuple{VersionReq,Any}

# This is used to keep track of dependency relations when propagating
# requirements, so as to emit useful information in case of unsatisfiable
# conditions.
# The `versionreq` field keeps track of the remaining allowed versions,
# intersecting all requirements.
# The `why` field is a Vector which keeps track of the requirements. Each
# entry is a Tuple of two elements:
# 1) the first element is the version requirement (can be a single VersionNumber
#    or a VersionSpec).
# 2) the second element can be either :fixed (for requirements induced by
#    fixed packages), :required (for requirements induced by explicitly
#    required packages), or a Pair p=>backtrace_item (for requirements induced
#    indirectly, where `p` is the package name and `backtrace_item` is
#    another ResolveBacktraceItem.
mutable struct ResolveBacktraceItem
    versionreq::VersionReq
    why::Vector{WhyReq}
    ResolveBacktraceItem() = new(VersionSpec(), WhyReq[])
    ResolveBacktraceItem(reason, versionreq::VersionReq) = new(versionreq, WhyReq[(versionreq,reason)])
end

function push!(ritem::ResolveBacktraceItem, reason, versionspec::VersionSpec)
    if isa(ritem.versionreq, VersionSpec)
        ritem.versionreq = ritem.versionreq ∩ versionspec
    elseif ritem.versionreq ∉ versionspec
        ritem.versionreq = copy(empty_versionspec)
    end
    push!(ritem.why, (versionspec,reason))
end

function push!(ritem::ResolveBacktraceItem, reason, version::VersionNumber)
    if isa(ritem.versionreq, VersionSpec)
        if version ∈ ritem.versionreq
            ritem.versionreq = version
        else
            ritem.versionreq = copy(empty_versionspec)
        end
    elseif ritem.versionreq ≠ version
        ritem.versionreq = copy(empty_versionspec)
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
            @assert isa(vs, VersionSpec)
            println(io, "version range $vs set by an explicit requirement")
        else
            @assert isa(w, Pair{<:AbstractString,ResolveBacktraceItem})
            if isa(vs, VersionNumber)
                print(io, "version $vs ")
            else
                print(io, "version range $vs ")
            end
            print(io, "required by package $(w[1]), ")
            if isa(w[2].versionreq, VersionSpec)
                println(io, "whose allowed version range is $(w[2].versionreq):")
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

const ResolveBacktrace = Dict{AbstractString,ResolveBacktraceItem}

end # module

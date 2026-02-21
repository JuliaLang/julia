# This file is a part of Julia. License is MIT: https://julialang.org/license

## semantic version numbers (https://semver.org/)

const VerTuple = Tuple{Vararg{Union{UInt64,String}}}

const VInt = UInt32
"""
    VersionNumber

Version number type which follows the specifications of
[semantic versioning (semver)](https://semver.org/spec/v2.0.0-rc.2.html), composed of major, minor
and patch numeric values, followed by pre-release and build
alphanumeric annotations.
As an extension to this standard, Julia also allows a single, empty prerelease annotation
if there is no build identifier present (e.g. `1.0-`), or a single, empty build annotation (e.g. `1.0+`).

`VersionNumber` objects can be compared with all of the standard comparison
operators (`==`, `<`, `<=`, etc.), with the result following semver v2.0.0-rc.2 rules.
Different from the semver standard, build annotations are not ignored when comparing version numbers.

`VersionNumber` has the following public fields:
- `v.major::Integer`
- `v.minor::Integer`
- `v.patch::Integer`
- `v.prerelease::Tuple{Vararg{Union{Integer, AbstractString}}}`
- `v.build::Tuple{Vararg{Union{Integer, AbstractString}}}`

See also [`@v_str`](@ref) to efficiently construct `VersionNumber` objects
from semver-format literal strings, [`VERSION`](@ref) for the `VersionNumber`
of Julia itself, and [Version Number Literals](@ref man-version-number-literals)
in the manual.

# Examples
```jldoctest
julia> a = VersionNumber(1, 2, 3)
v"1.2.3"

julia> a >= v"1.2"
true

julia> b = VersionNumber("2.0.1-rc1")
v"2.0.1-rc1"

julia> b >= v"2.0.1"
false
```
"""
struct VersionNumber
    major::VInt
    minor::VInt
    patch::VInt
    prerelease::VerTuple
    build::VerTuple

    function VersionNumber(major::VInt, minor::VInt, patch::VInt,
                           @nospecialize(pre::VerTuple), @nospecialize(bld::VerTuple))
        major >= 0 || throw(ArgumentError("invalid negative major version: $major"))
        minor >= 0 || throw(ArgumentError("invalid negative minor version: $minor"))
        patch >= 0 || throw(ArgumentError("invalid negative patch version: $patch"))
        for ident in pre
            if ident isa Integer
                ident >= 0 || throw(ArgumentError("invalid negative pre-release identifier: $ident"))
            else
                if !occursin(r"^(?:|[0-9a-z-]*[a-z-][0-9a-z-]*)$"i, ident) ||
                    isempty(ident) && !(length(pre)==1 && isempty(bld))
                    throw(ArgumentError("invalid pre-release identifier: $(repr(ident))"))
                end
            end
        end
        for ident in bld
            if ident isa Integer
                ident >= 0 || throw(ArgumentError("invalid negative build identifier: $ident"))
            else
                if !occursin(r"^(?:|[0-9a-z-]*[a-z-][0-9a-z-]*)$"i, ident) ||
                    isempty(ident) && length(bld)!=1
                    throw(ArgumentError("invalid build identifier: $(repr(ident))"))
                end
            end
        end
        new(major, minor, patch, pre, bld)
    end
end
VersionNumber(major::Integer, minor::Integer = 0, patch::Integer = 0,
        pre::Tuple{Vararg{Union{Integer,AbstractString}}} = (),
        bld::Tuple{Vararg{Union{Integer,AbstractString}}} = ()) =
    VersionNumber(VInt(major), VInt(minor), VInt(patch),
        map(x->x isa Integer ? UInt64(x) : String(x), pre),
        map(x->x isa Integer ? UInt64(x) : String(x), bld))

VersionNumber(v::Tuple) = VersionNumber(v...)
VersionNumber(v::VersionNumber) = v

function print(io::IO, v::VersionNumber)
    v == typemax(VersionNumber) && return print(io, "∞")
    print(io, v.major)
    print(io, '.')
    print(io, v.minor)
    print(io, '.')
    print(io, v.patch)
    if !isempty(v.prerelease)
        print(io, '-')
        join(io, v.prerelease,'.')
    end
    if !isempty(v.build)
        print(io, '+')
        join(io, v.build,'.')
    end
end
show(io::IO, v::VersionNumber) = print(io, "v\"", v, "\"")

Broadcast.broadcastable(v::VersionNumber) = Ref(v)

const VERSION_REGEX = r"^
    v?                                      # prefix        (optional)
    (\d+)                                   # major         (required)
    (?:\.(\d+))?                            # minor         (optional)
    (?:\.(\d+))?                            # patch         (optional)
    (?:(-)|                                 # pre-release   (optional)
    ([a-z][0-9a-z-]*(?:\.[0-9a-z-]+)*|-(?:[0-9a-z-]+\.)*[0-9a-z-]+)?
    (?:(\+)|
    (?:\+((?:[0-9a-z-]+\.)*[0-9a-z-]+))?    # build         (optional)
    ))
$"ix

function split_idents(s::AbstractString)
    idents = eachsplit(s, '.')
    pidents = Union{UInt64,String}[occursin(r"^\d+$", ident) ? parse(UInt64, ident) : String(ident) for ident in idents]
    return tuple(pidents...)::VerTuple
end

function tryparse(::Type{VersionNumber}, v::AbstractString)
    v == "∞" && return typemax(VersionNumber)
    m = match(VERSION_REGEX, String(v)::String)
    m === nothing && return nothing
    major, minor, patch, minus, prerl, plus, build = m.captures
    major = parse(VInt, major::AbstractString)
    minor = minor !== nothing ? parse(VInt, minor) : VInt(0)
    patch = patch !== nothing ? parse(VInt, patch) : VInt(0)
    if prerl !== nothing && !isempty(prerl) && prerl[1] == '-'
        prerl = prerl[2:end] # strip leading '-'
    end
    prerl = prerl !== nothing ? split_idents(prerl) : minus !== nothing ? ("",) : ()
    build = build !== nothing ? split_idents(build) : plus  !== nothing ? ("",) : ()
    return VersionNumber(major, minor, patch, prerl::VerTuple, build::VerTuple)
end

function parse(::Type{VersionNumber}, v::AbstractString)
    ver = tryparse(VersionNumber, v)
    ver === nothing && throw(ArgumentError("invalid version string: $v"))
    return ver
end

VersionNumber(v::AbstractString) = parse(VersionNumber, v)

"""
    @v_str

String macro used to parse a string to a [`VersionNumber`](@ref).

# Examples
```jldoctest
julia> v"1.2.3"
v"1.2.3"

julia> v"2.0.1-rc1"
v"2.0.1-rc1"
```
"""
macro v_str(v); VersionNumber(v); end

function typemax(::Type{VersionNumber})
    ∞ = typemax(VInt)
    VersionNumber(∞, ∞, ∞, (), ("",))
end

typemin(::Type{VersionNumber}) = v"0-"

ident_cmp(a::Integer, b::Integer) = cmp(a, b)
ident_cmp(a::Integer, b::String ) = isempty(b) ? +1 : -1
ident_cmp(a::String,  b::Integer) = isempty(a) ? -1 : +1
ident_cmp(a::String,  b::String ) = cmp(a, b)

function ident_cmp(@nospecialize(A::VerTuple), @nospecialize(B::VerTuple))
    for (a, b) in Iterators.Zip{Tuple{VerTuple,VerTuple}}((A, B))
        c = ident_cmp(a, b)
        (c != 0) && return c
    end
    length(A) < length(B) ? -1 :
    length(B) < length(A) ? +1 : 0
end

function ==(a::VersionNumber, b::VersionNumber)
    (a.major != b.major) && return false
    (a.minor != b.minor) && return false
    (a.patch != b.patch) && return false
    (ident_cmp(a.prerelease, b.prerelease) != 0) && return false
    (ident_cmp(a.build, b.build) != 0) && return false
    return true
end

issupbuild(v::VersionNumber) = length(v.build)==1 && isempty(v.build[1])

function isless(a::VersionNumber, b::VersionNumber)
    (a.major < b.major) && return true
    (a.major > b.major) && return false
    (a.minor < b.minor) && return true
    (a.minor > b.minor) && return false
    (a.patch < b.patch) && return true
    (a.patch > b.patch) && return false
    (!isempty(a.prerelease) && isempty(b.prerelease)) && return true
    (isempty(a.prerelease) && !isempty(b.prerelease)) && return false
    c = ident_cmp(a.prerelease,b.prerelease)
    (c < 0) && return true
    (c > 0) && return false
    (!issupbuild(a) && issupbuild(b)) && return true
    (issupbuild(a) && !issupbuild(b)) && return false
    c = ident_cmp(a.build,b.build)
    (c < 0) && return true
    return false
end

function hash(v::VersionNumber, h::UInt)
    h ⊻= 0x8ff4ffdb75f9fede % UInt
    h = hash(v.major, h)
    h = hash(v.minor, h)
    h = hash(v.patch, h)
    h = hash(v.prerelease, ~h)
    h = hash(v.build, ~h)
end

lowerbound(v::VersionNumber) = VersionNumber(v.major, v.minor, v.patch, ("",), ())
upperbound(v::VersionNumber) = VersionNumber(v.major, v.minor, v.patch, (), ("",))

thispatch(v::VersionNumber) = VersionNumber(v.major, v.minor, v.patch)
thisminor(v::VersionNumber) = VersionNumber(v.major, v.minor, 0)
thismajor(v::VersionNumber) = VersionNumber(v.major, 0, 0)

nextpatch(v::VersionNumber) = v < thispatch(v) ? thispatch(v) : VersionNumber(v.major, v.minor, v.patch+1)
nextminor(v::VersionNumber) = v < thisminor(v) ? thisminor(v) : VersionNumber(v.major, v.minor+1, 0)
nextmajor(v::VersionNumber) = v < thismajor(v) ? thismajor(v) : VersionNumber(v.major+1, 0, 0)

## julia version info

"""
    VERSION

A [`VersionNumber`](@ref) object describing which version of Julia is in use. See also
[Version Number Literals](@ref man-version-number-literals).
"""
const VERSION = try
    ver = VersionNumber(VERSION_STRING)
    if !isempty(ver.prerelease) && !GIT_VERSION_INFO.tagged_commit
        if GIT_VERSION_INFO.build_number >= 0
            ver = VersionNumber(ver.major, ver.minor, ver.patch, (ver.prerelease..., GIT_VERSION_INFO.build_number), ver.build)
        else
            println("WARNING: no build number found for pre-release version")
            ver = VersionNumber(ver.major, ver.minor, ver.patch, (ver.prerelease..., "unknown"), ver.build)
        end
    elseif GIT_VERSION_INFO.build_number > 0
        println("WARNING: ignoring non-zero build number for VERSION")
    end
    ver
catch e
    println("while creating Base.VERSION, ignoring error $e")
    VersionNumber(0)
end

const libllvm_version = if endswith(libllvm_version_string, "jl")
    # strip the "jl" SONAME suffix (JuliaLang/julia#33058)
    # (LLVM does never report a prerelease version anyway)
    VersionNumber(libllvm_version_string[1:end-2])
else
    VersionNumber(libllvm_version_string)
end

libllvm_path() = ccall(:jl_get_libllvm, Any, ())


################
# VersionBound #
################
struct VersionBound
    t::NTuple{3, UInt32}
    n::Int
    function VersionBound(tin::NTuple{n, Integer}) where {n}
        n <= 3 || throw(ArgumentError("VersionBound: you can only specify major, minor and patch versions"))
        n == 0 && return new((0, 0, 0), n)
        n == 1 && return new((tin[1], 0, 0), n)
        n == 2 && return new((tin[1], tin[2], 0), n)
        n == 3 && return new((tin[1], tin[2], tin[3]), n)
        error("invalid $n")
    end
end
VersionBound(t::Integer...) = VersionBound(t)
VersionBound(v::VersionNumber) = VersionBound(v.major, v.minor, v.patch)

Base.getindex(b::VersionBound, i::Int) = b.t[i]

function ≲(v::VersionNumber, b::VersionBound)
    b.n == 0 && return true
    b.n == 1 && return v.major <= b[1]
    b.n == 2 && return (v.major, v.minor) <= (b[1], b[2])
    return (v.major, v.minor, v.patch) <= (b[1], b[2], b[3])
end

function ≲(b::VersionBound, v::VersionNumber)
    b.n == 0 && return true
    b.n == 1 && return v.major >= b[1]
    b.n == 2 && return (v.major, v.minor) >= (b[1], b[2])
    return (v.major, v.minor, v.patch) >= (b[1], b[2], b[3])
end

function isless_ll(a::VersionBound, b::VersionBound)
    m, n = a.n, b.n
    for i in 1:min(m, n)
        a[i] < b[i] && return true
        a[i] > b[i] && return false
    end
    return m < n
end

stricterlower(a::VersionBound, b::VersionBound) = isless_ll(a, b) ? b : a

# Comparison between two upper bounds
function isless_uu(a::VersionBound, b::VersionBound)
    m, n = a.n, b.n
    for i in 1:min(m, n)
        a[i] < b[i] && return true
        a[i] > b[i] && return false
    end
    return m > n
end

stricterupper(a::VersionBound, b::VersionBound) = isless_uu(a, b) ? a : b

# `isjoinable` compares an upper bound of a range with the lower bound of the next range
# to determine if they can be joined, as in [1.5-2.8, 2.5-3] -> [1.5-3]. Used by `union!`.
# The equal-length-bounds case is special since e.g. `1.5` can be joined with `1.6`,
# `2.3.4` can be joined with `2.3.5` etc.

function isjoinable(up::VersionBound, lo::VersionBound)
    up.n == 0 && lo.n == 0 && return true
    if up.n == lo.n
        n = up.n
        for i in 1:(n - 1)
            up[i] > lo[i] && return true
            up[i] < lo[i] && return false
        end
        up[n] < lo[n] - 1 && return false
        return true
    else
        l = min(up.n, lo.n)
        for i in 1:l
            up[i] > lo[i] && return true
            up[i] < lo[i] && return false
        end
    end
    return true
end

Base.hash(r::VersionBound, h::UInt) = hash(r.t, hash(r.n, h))

# Hot code
function VersionBound(s::AbstractString)
    s = strip(s)
    s == "*" && return VersionBound()
    first(s) == 'v' && (s = SubString(s, 2))
    l = lastindex(s)

    p = findnext('.', s, 1)
    b = p === nothing ? l : (p - 1)
    i = parse(Int64, SubString(s, 1, b))
    p === nothing && return VersionBound(i)

    a = p + 1
    p = findnext('.', s, a)
    b = p === nothing ? l : (p - 1)
    j = parse(Int64, SubString(s, a, b))
    p === nothing && return VersionBound(i, j)

    a = p + 1
    p = findnext('.', s, a)
    b = p === nothing ? l : (p - 1)
    k = parse(Int64, SubString(s, a, b))
    p === nothing && return VersionBound(i, j, k)

    error("invalid VersionBound string $(repr(s))")
end

################
# VersionRange #
################
struct VersionRange
    lower::VersionBound
    upper::VersionBound
    # NOTE: ranges are allowed to be empty; they are ignored by VersionSpec anyway
    function VersionRange(lo::VersionBound, hi::VersionBound)
        # lo.t == hi.t implies that digits past min(lo.n, hi.n) are zero
        # lo.n < hi.n example: 1.2-1.2.0 => 1.2.0
        # lo.n > hi.n example: 1.2.0-1.2 => 1.2
        lo.t == hi.t && (lo = hi)
        return new(lo, hi)
    end
end
VersionRange(b::VersionBound = VersionBound()) = VersionRange(b, b)
VersionRange(t::Integer...) = VersionRange(VersionBound(t...))
VersionRange(v::VersionNumber) = VersionRange(VersionBound(v))
VersionRange(lo::VersionNumber, hi::VersionNumber) = VersionRange(VersionBound(lo), VersionBound(hi))

# The vast majority of VersionRanges are in practice equal to "1"
const VersionRange_1 = VersionRange(VersionBound("1"), VersionBound("1"))
function VersionRange(s::AbstractString)
    s == "1" && return VersionRange_1
    p = split(s, "-")
    if length(p) != 1 && length(p) != 2
        throw(ArgumentError("invalid version range: $(repr(s))"))
    end
    lower = VersionBound(p[1])
    upper = length(p) == 1 ? lower : VersionBound(p[2])
    return VersionRange(lower, upper)
end

function Base.isempty(r::VersionRange)
    for i in 1:min(r.lower.n, r.upper.n)
        r.lower[i] > r.upper[i] && return true
        r.lower[i] < r.upper[i] && return false
    end
    return false
end

function Base.print(io::IO, r::VersionRange)
    m, n = r.lower.n, r.upper.n
    return if (m, n) == (0, 0)
        print(io, '*')
    elseif m == 0
        print(io, "0 -")
        join(io, r.upper.t, '.')
    elseif n == 0
        join(io, r.lower.t, '.')
        print(io, " - *")
    else
        join(io, r.lower.t[1:m], '.')
        if r.lower != r.upper
            print(io, " - ")
            join(io, r.upper.t[1:n], '.')
        end
    end
end
Base.show(io::IO, r::VersionRange) = print(io, "VersionRange(\"", r, "\")")

Base.in(v::VersionNumber, r::VersionRange) = r.lower ≲ v ≲ r.upper

Base.intersect(a::VersionRange, b::VersionRange) = VersionRange(stricterlower(a.lower, b.lower), stricterupper(a.upper, b.upper))

function Base.union!(ranges::Vector{<:VersionRange})
    l = length(ranges)
    l == 0 && return ranges

    sort!(ranges, lt = (a, b) -> (isless_ll(a.lower, b.lower) || (a.lower == b.lower && isless_uu(a.upper, b.upper))))

    k0 = 1
    ks = findfirst(!isempty, ranges)
    ks === nothing && return empty!(ranges)

    lo, up, k0 = ranges[ks].lower, ranges[ks].upper, 1
    for k in (ks + 1):l
        isempty(ranges[k]) && continue
        lo1, up1 = ranges[k].lower, ranges[k].upper
        if isjoinable(up, lo1)
            isless_uu(up, up1) && (up = up1)
            continue
        end
        vr = VersionRange(lo, up)
        @assert !isempty(vr)
        ranges[k0] = vr
        k0 += 1
        lo, up = lo1, up1
    end
    vr = VersionRange(lo, up)
    if !isempty(vr)
        ranges[k0] = vr
        k0 += 1
    end
    resize!(ranges, k0 - 1)
    return ranges
end

Base.minimum(r::VersionRange) = r.lower

###############
# VersionSpec #
###############
struct VersionSpec
    ranges::Vector{VersionRange}
    VersionSpec(r::Vector{<:VersionRange}) = new(length(r) == 1 ? r : union!(r))
    VersionSpec(vs::VersionSpec) = vs
end

VersionSpec(r::VersionRange) = VersionSpec(VersionRange[r])
VersionSpec(v::VersionNumber) = VersionSpec(VersionRange(v))
const _all_versionsspec = VersionSpec(VersionRange())
VersionSpec() = _all_versionsspec
VersionSpec(s::AbstractString) = VersionSpec(VersionRange(s))
VersionSpec(v::AbstractVector) = VersionSpec(map(VersionRange, v))

# Hot code
function Base.in(v::VersionNumber, s::VersionSpec)
    for r in s.ranges
        v in r && return true
    end
    return false
end

# Optimized batch version check for version lists
# Fills dest[1:n] indicating which versions are in the VersionSpec
# Optimized for sorted version lists (but works correctly even if unsorted)
# Note: Only fills indices 1:n, leaves rest of dest unchanged
function matches_spec_range!(dest::BitVector, versions::AbstractVector{VersionNumber}, spec::VersionSpec, n::Int)
    @assert length(versions) == n
    @assert length(dest) >= n

    # Initialize to false
    dest[1:n] .= false

    isempty(spec.ranges) && return dest

    # Assumes versions are sorted (as created in Operations.jl:1002)
    # If sorted, this avoids O(n*m) comparisons by scanning linearly
    @inbounds for range in spec.ranges
        # Find first version that could be in range
        i = 1
        while i <= n && !(range.lower ≲ versions[i])
            i += 1
        end

        # Mark all versions in range
        while i <= n && versions[i] ≲ range.upper
            dest[i] = true
            i += 1
        end
    end

    return dest
end

Base.copy(vs::VersionSpec) = VersionSpec(vs)

const empty_versionspec = VersionSpec(VersionRange[])
const _empty_symbol = "∅"

Base.isempty(s::VersionSpec) = all(isempty, s.ranges)
@assert isempty(empty_versionspec)
# Hot code, measure performance before changing
function Base.intersect(A::VersionSpec, B::VersionSpec)
    (isempty(A) || isempty(B)) && return copy(empty_versionspec)
    ranges = Vector{VersionRange}(undef, length(A.ranges) * length(B.ranges))
    i = 1
    @inbounds for a in A.ranges, b in B.ranges
        ranges[i] = intersect(a, b)
        i += 1
    end
    return VersionSpec(ranges)
end
Base.intersect(a::VersionNumber, B::VersionSpec) = a in B ? VersionSpec(a) : empty_versionspec
Base.intersect(A::VersionSpec, b::VersionNumber) = intersect(b, A)

function Base.union(A::VersionSpec, B::VersionSpec)
    A == B && return A
    Ar = copy(A.ranges)
    append!(Ar, B.ranges)
    union!(Ar)
    return VersionSpec(Ar)
end

Base.:(==)(A::VersionSpec, B::VersionSpec) = A.ranges == B.ranges
Base.hash(s::VersionSpec, h::UInt) = hash(s.ranges, h + (0x2fd2ca6efa023f44 % UInt))

function Base.print(io::IO, s::VersionSpec)
    isempty(s) && return print(io, _empty_symbol)
    length(s.ranges) == 1 && return print(io, s.ranges[1])
    print(io, '[')
    for i in 1:length(s.ranges)
        1 < i && print(io, ", ")
        print(io, s.ranges[i])
    end
    return print(io, ']')
end

function Base.show(io::IO, s::VersionSpec)
    print(io, "VersionSpec(")
    if length(s.ranges) == 1
        print(io, '"', s.ranges[1], '"')
    else
        print(io, "[")
        for i in 1:length(s.ranges)
            1 < i && print(io, ", ")
            print(io, '"', s.ranges[i], '"')
        end
        print(io, ']')
    end
    return print(io, ")")
end

Base.minimum(v::VersionSpec) = minimum(v.ranges[1])

###################
# Semver notation #
###################

function semver_spec(s::String; throw = true)
    ranges = VersionRange[]
    for ver in strip.(split(strip(s), ','))
        range = nothing
        found_match = false
        for (ver_reg, f) in ver_regs
            if occursin(ver_reg, ver)
                range = f(match(ver_reg, ver))
                found_match = true
                break
            end
        end
        if !found_match
            if throw
                error("invalid version specifier: \"$s\"")
            else
                return nothing
            end
        end
        push!(ranges, range)
    end
    return VersionSpec(ranges)
end

function semver_interval(m::RegexMatch)
    @assert length(m.captures) == 4
    n_significant = count(x -> x !== nothing, m.captures) - 1
    typ, _major, _minor, _patch = m.captures
    major = parse(Int, _major)
    minor = (n_significant < 2) ? 0 : parse(Int, _minor)
    patch = (n_significant < 3) ? 0 : parse(Int, _patch)
    if n_significant == 3 && major == 0 && minor == 0 && patch == 0
        error("invalid version: \"0.0.0\"")
    end
    # Default type is :caret
    vertyp = (typ == "" || typ == "^") ? :caret : :tilde
    v0 = VersionBound((major, minor, patch))
    return if vertyp === :caret
        if major != 0
            return VersionRange(v0, VersionBound((v0[1],)))
        elseif minor != 0
            return VersionRange(v0, VersionBound((v0[1], v0[2])))
        else
            if n_significant == 1
                return VersionRange(v0, VersionBound((0,)))
            elseif n_significant == 2
                return VersionRange(v0, VersionBound((0, 0)))
            else
                return VersionRange(v0, VersionBound((0, 0, v0[3])))
            end
        end
    else
        if n_significant == 3 || n_significant == 2
            return VersionRange(v0, VersionBound((v0[1], v0[2])))
        else
            return VersionRange(v0, VersionBound((v0[1],)))
        end
    end
end

const _inf = VersionBound("*")
function inequality_interval(m::RegexMatch)
    @assert length(m.captures) == 4
    typ, _major, _minor, _patch = m.captures
    n_significant = count(x -> x !== nothing, m.captures) - 1
    major = parse(Int, _major)
    minor = (n_significant < 2) ? 0 : parse(Int, _minor)
    patch = (n_significant < 3) ? 0 : parse(Int, _patch)
    if n_significant == 3 && major == 0 && minor == 0 && patch == 0
        error("invalid version: 0.0.0")
    end
    v = VersionBound(major, minor, patch)
    if occursin(r"^<\s*$", typ)
        nil = VersionBound(0, 0, 0)
        if v[3] == 0
            if v[2] == 0
                v1 = VersionBound(v[1] - 1)
            else
                v1 = VersionBound(v[1], v[2] - 1)
            end
        else
            v1 = VersionBound(v[1], v[2], v[3] - 1)
        end
        return VersionRange(nil, v1)
    elseif occursin(r"^=\s*$", typ)
        return VersionRange(v)
    elseif occursin(r"^>=\s*$", typ) || occursin(r"^≥\s*$", typ)
        return VersionRange(v, _inf)
    else
        error("invalid prefix $typ")
    end
end

function hyphen_interval(m::RegexMatch)
    @assert length(m.captures) == 6
    _lower_major, _lower_minor, _lower_patch, _upper_major, _upper_minor, _upper_patch = m.captures
    if isnothing(_lower_minor)
        lower_bound = VersionBound(parse(Int, _lower_major))
    elseif isnothing(_lower_patch)
        lower_bound = VersionBound(
            parse(Int, _lower_major),
            parse(Int, _lower_minor)
        )
    else
        lower_bound = VersionBound(
            parse(Int, _lower_major),
            parse(Int, _lower_minor),
            parse(Int, _lower_patch)
        )
    end
    if isnothing(_upper_minor)
        upper_bound = VersionBound(parse(Int, _upper_major))
    elseif isnothing(_upper_patch)
        upper_bound = VersionBound(
            parse(Int, _upper_major),
            parse(Int, _upper_minor)
        )
    else
        upper_bound = VersionBound(
            parse(Int, _upper_major),
            parse(Int, _upper_minor),
            parse(Int, _upper_patch)
        )
    end
    return VersionRange(lower_bound, upper_bound)
end

const version = "v?([0-9]+?)(?:\\.([0-9]+?))?(?:\\.([0-9]+?))?"
const ver_regs =
    Pair{Regex, Any}[
    Regex("^([~^]?)?$version\$") => semver_interval, # 0.5 ^0.4 ~0.3.2
    Regex("^((?:≥\\s*)|(?:>=\\s*)|(?:=\\s*)|(?:<\\s*)|(?:=\\s*))v?$version\$") => inequality_interval, # < 0.2 >= 0.5,2
    Regex("^[\\s]*$version[\\s]*?\\s-\\s[\\s]*?$version[\\s]*\$") => hyphen_interval, # 0.7 - 1.3
]

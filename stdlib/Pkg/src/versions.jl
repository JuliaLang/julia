# This file is a part of Julia. License is MIT: https://julialang.org/license

################
# VersionBound #
################
struct VersionBound
    t::NTuple{3,UInt32}
    n::Int
    function VersionBound(tin::NTuple{n,Integer}) where n
        n <= 3 || throw(ArgumentError("VersionBound: you can only specify major, minor and patch versions"))
        n == 0 && return new((0,           0,      0), n)
        n == 1 && return new((tin[1],      0,      0), n)
        n == 2 && return new((tin[1], tin[2],      0), n)
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

≳(v::VersionNumber, b::VersionBound) = v ≲ b
≳(b::VersionBound, v::VersionNumber) = b ≲ v

function isless_ll(a::VersionBound, b::VersionBound)
    m, n = a.n, b.n
    for i = 1:min(m, n)
        a[i] < b[i] && return true
        a[i] > b[i] && return false
    end
    return m < n
end

stricterlower(a::VersionBound, b::VersionBound) = isless_ll(a, b) ? b : a

# Comparison between two upper bounds
function isless_uu(a::VersionBound, b::VersionBound)
    m, n = a.n, b.n
    for i = 1:min(m, n)
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
        for i = 1:(n - 1)
            up[i] > lo[i] && return true
            up[i] < lo[i] && return false
        end
        up[n] < lo[n] - 1 && return false
        return true
    else
        l = min(up.n, lo.n)
        for i = 1:l
            up[i] > lo[i] && return true
            up[i] < lo[i] && return false
        end
    end
    return true
end

Base.hash(r::VersionBound, h::UInt) = hash(hash(r.t, h), r.n)

VersionBound(s::AbstractString) =
    s == "*" ? VersionBound() : VersionBound(map(x -> parse(Int, x), split(s, '.'))...)

################
# VersionRange #
################
struct VersionRange
    lower::VersionBound
    upper::VersionBound
    # NOTE: ranges are allowed to be empty; they are ignored by VersionSpec anyway
end
VersionRange(b::VersionBound=VersionBound()) = VersionRange(b, b)
VersionRange(t::Integer...)                  = VersionRange(VersionBound(t...))
VersionRange(v::VersionNumber)               = VersionRange(VersionBound(v))
function VersionRange(s::AbstractString)
    m = match(r"^\s*v?((?:\d+(?:\.\d+)?(?:\.\d+)?)|\*)(?:\s*-\s*v?((?:\d+(?:\.\d+)?(?:\.\d+)?)|\*))?\s*$", s)
    m == nothing && throw(ArgumentError("invalid version range: $(repr(s))"))
    lower = VersionBound(m.captures[1])
    upper = m.captures[2] != nothing ? VersionBound(m.captures[2]) : lower
    return VersionRange(lower, upper)
end

function Base.isempty(r::VersionRange)
    for i = 1:min(r.lower.n, r.upper.n)
        r.lower[i] > r.upper[i] && return true
        r.lower[i] < r.upper[i] && return false
    end
    return false
end

function Base.print(io::IO, r::VersionRange)
    m, n = r.lower.n, r.upper.n
    if (m, n) == (0, 0)
        print(io, '*')
    elseif m == 0
        print(io, "0-")
        join(io, r.upper.t, '.')
    elseif n == 0
        join(io, r.lower.t, '.')
        print(io, "-*")
    else
        join(io, r.lower.t[1:m], '.')
        if r.lower != r.upper
            print(io, '-')
            join(io, r.upper.t[1:n], '.')
        end
    end
end
Base.show(io::IO, r::VersionRange) = print(io, "VersionRange(\"", r, "\")")

Base.in(v::VersionNumber, r::VersionRange) = r.lower ≲ v ≲ r.upper
Base.in(v::VersionNumber, r::VersionNumber) = v == r

Base.intersect(a::VersionRange, b::VersionRange) = VersionRange(stricterlower(a.lower, b.lower), stricterupper(a.upper, b.upper))

function Base.union!(ranges::Vector{<:VersionRange})
    l = length(ranges)
    l == 0 && return ranges

    sort!(ranges, lt=(a, b) -> (isless_ll(a.lower, b.lower) || (a.lower == b.lower && isless_uu(a.upper, b.upper))))

    k0 = 1
    ks = findfirst(!isempty, ranges)
    ks == nothing && return empty!(ranges)

    lo, up, k0 = ranges[ks].lower, ranges[ks].upper, 1
    for k = (ks + 1):l
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

###############
# VersionSpec #
###############
struct VersionSpec
    ranges::Vector{VersionRange}
    VersionSpec(r::Vector{<:VersionRange}) = new(union!(r))
    VersionSpec(vs::VersionSpec) = new(copy(vs.ranges))
end

VersionSpec() = VersionSpec(VersionRange())
VersionSpec(v::VersionNumber) = VersionSpec(VersionRange(v))
VersionSpec(r::VersionRange) = VersionSpec(VersionRange[r])
VersionSpec(s::AbstractString) = VersionSpec(VersionRange(s))
VersionSpec(v::AbstractVector) = VersionSpec(map(VersionRange, v))

# Hot code
function Base.in(v::VersionNumber, s::VersionSpec)
    for r in s.ranges
        v in r && return true
    end
    return false
end

Base.copy(vs::VersionSpec) = VersionSpec(vs)

const empty_versionspec = VersionSpec(VersionRange[])
# Windows console doesn't like Unicode
const _empty_symbol = @static Sys.iswindows() ? "empty" : "∅"

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
    VersionSpec(ranges)
end
Base.intersect(a::VersionNumber, B::VersionSpec) = a in B ? VersionSpec(a) : empty_versionspec
Base.intersect(A::VersionSpec, b::VersionNumber) = intersect(b, A)

Base.union(A::VersionSpec, B::VersionSpec) = union!(copy(A), B)
function Base.union!(A::VersionSpec, B::VersionSpec)
    A == B && return A
    append!(A.ranges, B.ranges)
    union!(A.ranges)
    return A
end

Base.:(==)(A::VersionSpec, B::VersionSpec) = A.ranges == B.ranges
Base.hash(s::VersionSpec, h::UInt) = hash(s.ranges, h + (0x2fd2ca6efa023f44 % UInt))

function Base.print(io::IO, s::VersionSpec)
    isempty(s) && return print(io, _empty_symbol)
    length(s.ranges) == 1 && return print(io, s.ranges[1])
    print(io, '[')
    for i = 1:length(s.ranges)
        1 < i && print(io, ", ")
        print(io, s.ranges[i])
    end
    print(io, ']')
end
Base.show(io::IO, s::VersionSpec) = print(io, "VersionSpec(\"", s, "\")")


###################
# Semver notation #
###################


function semver_spec(s::String)
    s = replace(s, " " => "")
    ranges = VersionRange[]
    for ver in split(s, ',')
        range = nothing
        found_match = false
        for (ver_reg, f) in ver_regs
            if occursin(ver_reg, ver)
                range = f(match(ver_reg, ver))
                found_match = true
                break
            end
        end
        found_match || error("invalid version specifier: $s")
        push!(ranges, range)
    end
    return VersionSpec(ranges)
end

function semver_interval(m::RegexMatch)
    @assert length(m.captures) == 4
    n_significant = count(x -> x !== nothing, m.captures) - 1
    typ, _major, _minor, _patch = m.captures
    major =                           parse(Int, _major)
    minor = (n_significant < 2) ? 0 : parse(Int, _minor)
    patch = (n_significant < 3) ? 0 : parse(Int, _patch)
    if n_significant == 3 && major == 0 && minor == 0 && patch == 0
        error("invalid version: \"0.0.0\"")
    end
    # Default type is :caret
    vertyp = (typ == "" || typ == "^") ? :caret : :tilde
    v0 = VersionBound((major, minor, patch))
    if vertyp == :caret
        if major != 0
            return VersionRange(v0, VersionBound((v0[1],)))
        elseif minor != 0
            return VersionRange(v0, VersionBound((v0[1], v0[2])))
        else
            if n_significant == 1
                return VersionRange(v0, VersionBound((0,)))
            elseif n_significant == 2
                return VersionRange(v0, VersionBound((0, 0,)))
            else
                return VersionRange(v0, VersionBound((0, 0, v0[3])))
            end
        end
    else
        if n_significant == 3 || n_significant == 2
            return VersionRange(v0, VersionBound((v0[1], v0[2],)))
        else
            return VersionRange(v0, VersionBound((v0[1],)))
        end
    end
end

const _inf = Pkg.Types.VersionBound("*")
function inequality_interval(m::RegexMatch)
    @assert length(m.captures) == 4
    typ, _major, _minor, _patch = m.captures
    n_significant = count(x -> x !== nothing, m.captures) - 1
    major =                           parse(Int, _major)
    minor = (n_significant < 2) ? 0 : parse(Int, _minor)
    patch = (n_significant < 3) ? 0 : parse(Int, _patch)
    if n_significant == 3 && major == 0 && minor == 0 && patch == 0
        error("invalid version: $s")
    end
    v = VersionBound(major, minor, patch)
    if typ == "<"
        nil = VersionBound(0, 0, 0)
        if v[3] == 0
            if v[2] == 0
                v1 = VersionBound(v[1]-1)
            else
                v1 = VersionBound(v[1], v[2]-1)
            end
        else
            v1 = VersionBound(v[1], v[2], v[3]-1)
        end
        return VersionRange(nil, v1)
    elseif typ == "="
        return VersionRange(v)
    elseif typ == ">=" || typ == "≥"
           return VersionRange(v, _inf)
    else
        error("invalid prefix $typ")
    end
end

const version = "v?([0-9]+?)(?:\\.([0-9]+?))?(?:\\.([0-9]+?))?"
const ver_regs =
[
    Regex("^([~^]?)?$version\$") => semver_interval, # 0.5 ^0.4 ~0.3.2
    Regex("^((?:≥)|(?:>=)|(?:=)|(?:<)|(?:=))v?$version\$")  => inequality_interval,# < 0.2 >= 0.5,2
]

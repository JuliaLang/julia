#!/usr/bin/env julia

function packagelt(a::String, b::String)
    a == "julia" && b != "julia" && return true
    a != "julia" && b == "julia" && return false
    return lowercase(a) < lowercase(b)
end

≲(v::VersionNumber, t::NTuple{0,Integer}) = true
≲(v::VersionNumber, t::NTuple{1,Integer}) = v.major ≤ t[1]
≲(v::VersionNumber, t::NTuple{2,Integer}) = v.major < t[1] ||
                                            v.major ≤ t[1] && v.minor ≤ t[2]
≲(v::VersionNumber, t::NTuple{3,Integer}) = v.major < t[1] ||
                                            v.major ≤ t[1] && v.minor < t[2] ||
                                            v.major ≤ t[1] && v.minor ≤ t[2] && v.patch ≤ t[3]

≲(t::NTuple{0,Integer}, v::VersionNumber) = true
≲(t::NTuple{1,Integer}, v::VersionNumber) = t[1] ≤ v.major
≲(t::NTuple{2,Integer}, v::VersionNumber) = t[1] < v.major ||
                                            t[1] ≤ v.major && t[2] ≤ v.minor
≲(t::NTuple{3,Integer}, v::VersionNumber) = t[1] < v.major ||
                                            t[1] ≤ v.major && t[2] < v.minor ||
                                            t[1] ≤ v.major && t[2] ≤ v.minor && t[3] ≤ v.patch

function compress_versions(inc::Vector{VersionNumber}, from::Vector{VersionNumber})
    issorted(inc) || (inc = sort(inc))
    exc = sort!(setdiff(from, inc))
    pairs = []
    if isempty(exc)
        lo, hi = first(inc), last(inc)
        push!(pairs, (lo.major, lo.minor) => (hi.major, hi.minor))
    else
        for v in inc
            t = (v.major, v.minor)
            if any(t ≲ w ≲ t for w in exc)
                t = (v.major, v.minor, v.patch)
            end
            if isempty(pairs) || any(pairs[end][1] ≲ w ≲ t for w in exc)
                push!(pairs, t => t) # need a new interval
            else
                pairs[end] = pairs[end][1] => t # can be merged with last
            end
        end
    end
    @assert all(any(p[1] ≲ v ≲ p[2] for p ∈ pairs) for v ∈ inc)
    @assert all(!any(p[1] ≲ v ≲ p[2] for p ∈ pairs) for v ∈ exc)
    return pairs
end
compress_versions(f::Function, from::Vector{VersionNumber}) =
    compress_versions(filter(f, from), from)
compress_versions(vi::VersionInterval, from::Vector{VersionNumber}) =
    compress_versions(v->v in vi, from)
compress_versions(inc, from) = compress_versions(inc, collect(from))

versions_string(p::Pair) = versions_string(p...)
versions_string(a::Tuple{}, b::Tuple{}) = "*"
versions_string(a::NTuple{m,Integer}, b::NTuple{n,Integer}) where {m,n} =
    a == b ? join(a, '.') : "$(join(a, '.'))-$(join(b, '.'))"

versions_repr(x) = repr(versions_string(x))
versions_repr(v::Vector) = length(v) == 1 ? repr(versions_string(v[1])) :
    "[" * join(map(repr∘versions_string, v), ", ") * "]"

## compress per-version data ##

function compress_versions_data(
    d::Dict{VersionNumber,Dict{String,String}},
    versions::Vector{VersionNumber},
)
    kvs = Dict{Pair{String,String},Vector{VersionNumber}}()
    for (ver, x) in d, (k, v) in x
        push!(get!(kvs, k => v, VersionNumber[]), ver)
    end
    tx = Tuple{Any,String,String}[]
    for (kv, vs) in kvs
        for v in compress_versions(sort!(vs), versions)
            push!(tx, (v, kv...))
        end
    end
    sort!(tx, by=t->(t[1][1], t[1][2], t[2], t[3]))
    return Tuple{String,String,String}[
        (versions_string(t[1]), t[2], t[3]) for t in tx
    ]
end

## dict utility functions ##

function invert_map(fwd::Dict{K,V}) where {K,V}
    rev = Dict{V,Vector{K}}()
    for (k, v) in fwd
        push!(get!(rev, v, K[]), k)
    end
    return rev
end

function invert_map(fwd::Dict{Vector{K},V}) where {K,V}
    rev = Dict{V,Vector{K}}()
    for (k, v) in fwd
        append!(get!(rev, v, K[]), k)
    end
    return rev
end

flatten_keys(d::Dict{Vector{K},V}) where {K,V} =
    isempty(d) ? Dict{K,V}() : Dict{K,V}(k => v for (ks, v) in d for k in ks)

## TOML generation functions ##

function write_toml(f::Function, names::String...)
    path = joinpath(names...) * ".toml"
    mkpath(dirname(path))
    open(path, "w") do io
        f(io)
    end
end

toml_key(str::String) = contains(str, r"[^\w-]") ? repr(str) : str
toml_key(strs::String...) = join(map(toml_key, [strs...]), '.')

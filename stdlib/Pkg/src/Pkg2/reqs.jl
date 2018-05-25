# This file is a part of Julia. License is MIT: https://julialang.org/license

module Reqs

import Base: ==
using ..Pkg2Types

# representing lines of REQUIRE files

abstract type Line end
struct Comment <: Line
    content::AbstractString
end
struct Requirement <: Line
    content::AbstractString
    package::AbstractString
    versions::VersionSet
    system::Vector{AbstractString}

    function Requirement(content::AbstractString)
        fields = split(replace(content, r"#.*$" => ""))
        system = AbstractString[]
        while !isempty(fields) && fields[1][1] == '@'
            push!(system, popfirst!(fields)[2:end])
        end
        isempty(fields) && throw(PkgError("invalid requires entry: $content"))
        package = popfirst!(fields)
        all(field->occursin(Base.VERSION_REGEX, field), fields) ||
            throw(PkgError("invalid requires entry for $package: $content"))
        versions = VersionNumber.(fields)
        issorted(versions) || throw(PkgError("invalid requires entry for $package: $content"))
        new(content, package, VersionSet(versions), system)
    end
end

function read(readable::Union{IO,Base.AbstractCmd})
    lines = Line[]
    for line in eachline(readable)
        push!(lines, occursin(r"^\s*(?:#|$)", line) ? Comment(line) : Requirement(line))
    end
    return lines
end
read(file::AbstractString) = isfile(file) ? open(read,file) : Line[]


end # module

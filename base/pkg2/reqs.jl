module Reqs

using ..Types

# representing lines of REQUIRE files

abstract Line
immutable Comment <: Line
    content::String
end
immutable Requirement <: Line
    content::String
    package::String
    versions::VersionSet

    function Requirement(content::String)
        fields = split(replace(content, r"#.*$", ""))
        package = shift!(fields)
        versions = VersionNumber[]
        for field in fields
            m = match(Base.VERSION_REGEX, field)
            m == nothing && error("invalid requires entry field for $package: $field")
            major, minor, patch, minus, prerl, plus, build = m.captures
            major = int(major)
            minor = minor != nothing ? int(minor) : 0
            patch = patch != nothing ? int(patch) : 0
            v = VersionNumber(major, minor, patch)
            (prerl != nothing || build != nothing || plus == "+") &&
                warn("in requires entry for $package: field $field will be treated as $v")
            push!(versions, v)
        end
        issorted(versions) || error("invalid requires entry for $package: $versions")
        new(content, package, VersionSet(versions))
    end
    function Requirement(package::String, versions::VersionSet)
        content = package
        if versions != VersionSet()
            for ival in versions.intervals
                (content *= " $(ival.lower)")
                ival.upper < typemax(VersionNumber) &&
                (content *= " $(ival.upper)")
            end
        end
        new(content, package, versions)
    end
end

# general machinery for parsing REQUIRE files

function read(io::IO)
    lines = Line[]
    for line in eachline(io)
        line = chomp(line)
        push!(lines, ismatch(r"^\s*(?:#|$)", line) ? Comment(line) : Requirement(line))
    end
    return lines
end
read(file::String) = isfile(file) ? open(read,file) : Line[]

function write(io::IO, lines::Vector{Line})
    for line in lines
        println(io, line.content)
    end
end
write(file::String, lines::Vector{Line}) = open(file, "w") do io
    write(io, lines)
end

function parse(lines::Vector{Line})
    reqs = Requires()
    for line in lines
        if isa(line,Requirement)
            reqs[line.package] = haskey(reqs, line.package) ?
                intersect(reqs[line.package], line.versions) : line.versions
        end
    end
    return reqs
end
parse(x) = parse(read(x))

# add & rm: intended to be used with Write.update_file

function add(lines::Vector{Line}, pkg::String, versions::VersionSet=VersionSet())
    v = VersionSet[]
    filtered = filter(lines) do line
        (isa(line,Comment) || line.package != pkg) && return true
        push!(v, line.versions)
        return false
    end
    length(v) == 1 && v[1] == intersect(v[1],versions) && return copy(lines)
    versions = reduce(intersect, versions, v)
    push!(filtered, Requirement(pkg, versions))
end

rm(lines::Vector{Line}, pkg::String) = filter(lines) do line
    isa(line,Comment) || line.package != pkg
end

end # module

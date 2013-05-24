module Reqs

using ..Types

abstract Line
immutable Comment <: Line
    content::String
end
immutable Requirement <: Line
    content::String
    package::String
    versions::VersionSet
end

process(io::IO) = @task begin
    for line in eachline(io)
        line = chomp(line)
        if ismatch(r"^\s*(?:#|$)", line)
            produce(Comment(line))
        else
            fields = split(replace(line, r"#.*$", ""))
            pkg = shift!(fields)
            all(field->ismatch(Base.VERSION_REGEX, field), fields) ||
                error("invalid requires entry for $pkg: $fields")
            versions = [ convert(VersionNumber, field) for field in fields ]
            issorted(versions) || error("invalid requires entry for $pkg: $versions")
            produce(Requirement(line, pkg, VersionSet(versions)))
        end
    end
end

function parse(io::IO)
    reqs = Requires()
    for r in process(io)
        if isa(r,Requirement)
            reqs[r.package] = haskey(reqs,r.package) ?
                intersect(reqs[r.package], r.versions) : r.versions
        end
    end
    return reqs
end
parse(file::String) = isfile(file) ? open(parse,file) : Requires()

function add(input::IO, output::IO, pkg::String, versions::VersionSet=VersionSet())
    existed = false
    for r in process(input)
        if isa(r,Requirement) && r.package == pkg
            versions = intersect(versions, r.versions)
            existed = true
        else
            println(output, r.content)
        end
    end
    if versions == VersionSet()
        println(output, pkg)
    else
        print(output, pkg)
        for ival in versions.intervals
            print(output, "\t", ival.lower)
            ival.upper < typemax(VersionNumber) &&
            print(output, "\t", ival.upper)
        end
        println(output)
    end
    return existed
end

function rm(input::IO, output::IO, pkg::String)
    existed = false
    for r in process(input)
        if isa(r,Requirement) && r.package == pkg
            existed = true
        else
            println(output, r.content)
        end
    end
    return existed
end

end # module

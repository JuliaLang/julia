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
        if ismatch(r"^\s*(?:#|$)", line)
            produce(Comment(line))
        else
            fields = split(replace(line, r"#.*$", ""))
            pkg = shift!(fields)
            all(_->ismatch(Base.VERSION_REGEX,_), fields) ||
                error("invalid requires entry for $pkg: $fields")
            vers = [ convert(VersionNumber, field) for field in fields ]
            issorted(vers) || error("invalid requires entry for $pkg: $vers")
            intervals = VersionInterval[]
            if isempty(vers)
                push!(intervals, VersionInterval(typemin(VersionNumber),typemax(VersionNumber)))
            else
                isodd(length(vers)) && push!(vers, typemax(VersionNumber))
                while !isempty(vers)
                    push!(intervals, VersionInterval(shift!(vers), shift!(vers)))
                end
            end
            produce(Requirement(line,pkg,VersionSet(intervals)))
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
parse(file::String="REQUIRE") = isfile(file) ? open(parse,file) : Requires()

end # module

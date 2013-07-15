module ExtDeps

import Base: isequal
import ..Reqs: Line, Comment

immutable Record <: Line
	content::String
end

isequal(a::Record,b::Record) = isequal(a.content,b.content)

# Reading and ExtDeps file
function read(io::IO)
    lines = Line[]
    for line in eachline(io)
        # workaround for https://github.com/JuliaLang/julia/issues/3473
        isempty(line) && continue
        line = chomp(line)
        push!(lines, ismatch(r"^\s*(?:#|$)", line) ? Comment(line) : Record(line))
    end
    return lines
end
read(file::String) = isfile(file) ? open(read,file) : Line[]

function add(lines::Vector{Line},pkg::String)
	lines = copy(lines)
	if !contains(lines,Record(pkg))
		push!(lines,Record(pkg))
	end
	lines
end

rm(lines::Vector{Line},pkg::String) = filter(x->x.content!=pkg,lines)

end
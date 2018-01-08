"Identify if character in subset of bare key symbols"
isbare(c::Char) = 'A' <= c <= 'Z' || 'a' <= c <= 'z' || isdigit(c) || c == '-' || c == '_'

function printkey(io::IO, keys::Vector{String})
    for (i, k) in enumerate(keys)
        i != 1 && Base.print(io, ".")
        if length(k) == 0
            # empty key
            Base.print(io, "\"\"")
        elseif !all([isbare(c) for c in k])
            # quoted key
            Base.print(io, "\"$(escape_string(k))\"")
        else
            Base.print(io, k)
        end
    end
end

function printvalue(io::IO, value; sorted=false)
    if isa(value, AbstractDict)
        _print(io, value, sorted=sorted)
    elseif isa(value, Array)
        Base.print(io, "[")
        for (i, x) in enumerate(value)
            i != 1 && Base.print(io, ", ")
            if isa(x, AbstractDict)
                _print(io, x, sorted=sorted)
            else
                printvalue(io, x, sorted=sorted)
            end
        end
        Base.print(io, "]")
    elseif isa(value, AbstractString)
        Base.print(io, "\"$(escape_string(value))\"")
    elseif isa(value, DateTime)
        Base.print(io, Dates.format(value, "YYYY-mm-ddTHH:MM:SS.sssZ"))
    else
        Base.print(io, value)
    end
end

function _print(io::IO, a::AbstractDict, ks=String[]; sorted=false)
    akeys = keys(a)
    if sorted
        akeys = sort!(collect(akeys))
    end
    first_block = true

    for key in akeys
        value = a[key]
        # skip tables
        isa(value, AbstractDict) && continue # skip tables
        # skip arrays of tabels
        isa(value, Array) && length(value)>0 && isa(value[1], AbstractDict) && continue

        Base.print(io, repeat("    ", max(0, length(ks)-1)))
        printkey(io, [key])
        Base.print(io, " = ") # print separator
        printvalue(io, value, sorted=sorted)
        Base.print(io, "\n")  # new line?
        first_block = false
    end

    indent = repeat("    ", length(ks))
    for key in akeys
        value = a[key]
        if isa(value, AbstractDict)
            # print table
            first_block || println(io)
            first_block = false
            push!(ks, key)
            Base.print(io, indent)
            Base.print(io,"[")
            printkey(io, ks)
            Base.print(io,"]\n")
            _print(io, value, ks, sorted=sorted)
            pop!(ks)
        elseif isa(value, Array) && length(value)>0 && isa(value[1], AbstractDict)
            # print array of tables
            first_block || println(io)
            first_block = false
            push!(ks, key)
            for v in value
                Base.print(io, indent)
                Base.print(io,"[[")
                printkey(io, ks)
                Base.print(io,"]]\n")
                !isa(v, AbstractDict) && error("array should contain only tables")
                _print(io, v, ks, sorted=sorted)
            end
            pop!(ks)
        end
    end
end

print(io::IO, a::AbstractDict; sorted=false) = _print(io, a, sorted=sorted)
print(a::AbstractDict; sorted=false) = print(STDOUT, a, sorted=sorted)

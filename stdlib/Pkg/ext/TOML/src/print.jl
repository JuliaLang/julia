# This file is a part of Julia. License is MIT: https://julialang.org/license

"Identify if character in subset of bare key symbols"
isbare(c::AbstractChar) = 'A' <= c <= 'Z' || 'a' <= c <= 'z' || isdigit(c) || c == '-' || c == '_'

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

is_table(value)           = isa(value, AbstractDict)
is_array_of_tables(value) = isa(value, Array) && length(value) > 0 && isa(value[1], AbstractDict)
is_tabular(value)         = is_table(value) || is_array_of_tables(value)

function _print(io::IO, a::AbstractDict,
    ks::Vector{String} = String[];
    indent::Int = 0,
    first_block::Bool = true,
    sorted::Bool = false,
    by::Function = identity,
)
    akeys = keys(a)
    if sorted
        akeys = sort!(collect(akeys), by = by)
    end

    for key in akeys
        value = a[key]
        is_tabular(value) && continue
        Base.print(io, ' '^4max(0,indent-1))
        printkey(io, [key])
        Base.print(io, " = ") # print separator
        printvalue(io, value, sorted = sorted)
        Base.print(io, "\n")  # new line?
        first_block = false
    end

    for key in akeys
        value = a[key]
        if is_table(value)
            push!(ks, key)
            header = !all(is_tabular(v) for v in values(value))
            if header
                # print table
                first_block || println(io)
                first_block = false
                Base.print(io, ' '^4indent)
                Base.print(io,"[")
                printkey(io, ks)
                Base.print(io,"]\n")
            end
            _print(io, value, ks,
                indent = indent + header, first_block = header, sorted = sorted, by = by)
            pop!(ks)
        elseif is_array_of_tables(value)
            # print array of tables
            first_block || println(io)
            first_block = false
            push!(ks, key)
            for v in value
                Base.print(io, ' '^4indent)
                Base.print(io,"[[")
                printkey(io, ks)
                Base.print(io,"]]\n")
                !isa(v, AbstractDict) && error("array should contain only tables")
                _print(io, v, ks, indent = indent + 1, sorted = sorted, by = by)
            end
            pop!(ks)
        end
    end
end

print(io::IO, a::AbstractDict; kwargs...) = _print(io, a; kwargs...)
print(a::AbstractDict; kwargs...) = print(stdout, a; kwargs...)

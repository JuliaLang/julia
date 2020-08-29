# This file is a part of Julia. License is MIT: https://julialang.org/license

import Dates

import ..isvalid_barekey_char

function printkey(io::IO, keys::Vector{String})
    for (i, k) in enumerate(keys)
        i != 1 && Base.print(io, ".")
        if length(k) == 0
            # empty key
            Base.print(io, "\"\"")
        elseif any(!isvalid_barekey_char, k)
            # quoted key
            Base.print(io, "\"", escape_string(k) ,"\"")
        else
            Base.print(io, k)
        end
    end
end

const MbyFunc = Union{Function, Nothing}
const TOMLValue = Union{AbstractVector, AbstractDict, Dates.DateTime, Dates.Time, Dates.Date, Bool, Integer, AbstractFloat, String}
function printvalue(f::MbyFunc, io::IO, value::AbstractVector; sorted=false)
    Base.print(io, "[")
    for (i, x) in enumerate(value)
        i != 1 && Base.print(io, ", ")
        if isa(x, AbstractDict)
            _print(f, io, x; sorted)
        else
            printvalue(f, io, x; sorted)
        end
    end
    Base.print(io, "]")
end
function printvalue(f::MbyFunc, io::IO, value; sorted)
    if f === nothing
        error("type `$(typeof(value))` is not a valid TOML type, pass a conversion function to `TOML.print`")
    end
    toml_value = f(value)
    if !(toml_value isa TOMLValue)
        error("TOML syntax function for type `$(typeof(value))` did not return a valid TOML type but a `$(typeof(toml_value))`")
    end
    Base.invokelatest(printvalue, f, io, toml_value; sorted)
end
printvalue(f::MbyFunc, io::IO, value::AbstractDict; sorted) =
    _print(f, io, value; sorted)
printvalue(f::MbyFunc, io::IO, value::Dates.DateTime; sorted) =
    Base.print(io, Dates.format(value, Dates.dateformat"YYYY-mm-dd\THH:MM:SS.sss\Z"))
printvalue(f::MbyFunc, io::IO, value::Dates.Time; sorted) =
    Base.print(io, Dates.format(value, Dates.dateformat"HH:MM:SS.sss"))
printvalue(f::MbyFunc, io::IO, value::Dates.Date; sorted) =
    Base.print(io, Dates.format(value, Dates.dateformat"YYYY-mm-dd"))
printvalue(f::MbyFunc, io::IO, value::Bool; sorted) =
    Base.print(io, value ? "true" : "false")
printvalue(f::MbyFunc, io::IO, value::Integer; sorted) =
    Base.print(io, Int64(value))  # TOML specifies 64-bit signed long range for integer
printvalue(f::MbyFunc, io::IO, value::AbstractFloat; sorted) =
    Base.print(io, isnan(value) ? "nan" :
                   isinf(value) ? string(value > 0 ? "+" : "-", "inf") :
                   Float64(value))  # TOML specifies IEEE 754 binary64 for float
printvalue(f::MbyFunc, io::IO, value::AbstractString; sorted) = Base.print(io, "\"", escape_string(value), "\"")

is_table(value)           = isa(value, AbstractDict)
is_array_of_tables(value) = isa(value, AbstractArray) &&
                            length(value) > 0 && isa(value[1], AbstractDict)
is_tabular(value)         = is_table(value) || is_array_of_tables(value)

function _print(f::MbyFunc, io::IO, a::AbstractDict,
    ks::Vector{String} = String[];
    indent::Int = 0,
    first_block::Bool = true,
    sorted::Bool,
    by::Function,
)
    akeys = keys(a)
    if sorted
        akeys = sort!(collect(akeys); by)
    end

    # First print non-tabular entries
    for key in akeys
        value = a[key]
        is_tabular(value) && continue
        Base.print(io, ' '^4max(0,indent-1))
        printkey(io, [String(key)])
        Base.print(io, " = ") # print separator
        printvalue(f, io, value; sorted)
        Base.print(io, "\n")  # new line?
        first_block = false
    end

    for key in akeys
        value = a[key]
        if is_table(value)
            push!(ks, String(key))
            header = !all(is_tabular(v) for v in values(value))::Bool
            if header
                # print table
                first_block || println(io)
                first_block = false
                Base.print(io, ' '^4indent)
                Base.print(io,"[")
                printkey(io, ks)
                Base.print(io,"]\n")
            end
            # Use runtime dispatch here since the type of value seems not to be enforced other than as AbstractDict
            Base.invokelatest(_print, f, io, value, ks; indent = indent + header, first_block = header, sorted, by)
            pop!(ks)
        elseif is_array_of_tables(value)
            # print array of tables
            first_block || println(io)
            first_block = false
            push!(ks, String(key))
            for v in value
                Base.print(io, ' '^4indent)
                Base.print(io,"[[")
                printkey(io, ks)
                Base.print(io,"]]\n")
                # TODO, nicer error here
                !isa(v, AbstractDict) && error("array should contain only tables")
                Base.invokelatest(_print, f, io, v, ks; indent = indent + 1, sorted, by)
            end
            pop!(ks)
        end
    end
end

print(f::MbyFunc, io::IO, a::AbstractDict; sorted::Bool=false, by=identity) = _print(f, io, a; sorted, by)
print(f::MbyFunc, a::AbstractDict; sorted::Bool=false, by=identity) = print(f, stdout, a; sorted, by)
print(io::IO, a::AbstractDict; sorted::Bool=false, by=identity) = _print(nothing, io, a; sorted, by)
print(a::AbstractDict; sorted::Bool=false, by=identity) = print(nothing, stdout, a; sorted, by)

# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base: @invokelatest
import ..isvalid_barekey_char # from Parser

function print_toml_escaped(io::IO, s::AbstractString)
    for c::AbstractChar in s
        if !isvalid(c)
            error("TOML print: invalid character $(repr(c)) encountered when printing string")
        end
        if c == '\b'
            Base.print(io, '\\', 'b')
        elseif c == '\t'
            Base.print(io, '\\', 't')
        elseif c == '\n'
            Base.print(io, '\\', 'n')
        elseif c == '\f'
            Base.print(io, '\\', 'f')
        elseif c == '\r'
            Base.print(io, '\\', 'r')
        elseif c == '"'
            Base.print(io, '\\', '"')
        elseif c == '\\'
            Base.print(io, "\\", '\\')
        elseif Base.iscntrl(c)
            Base.print(io, "\\u")
            Base.print(io, string(UInt32(c), base=16, pad=4))
        else
            Base.print(io, c)
        end
    end
end

const BaseTOMLValue = Union{AbstractVector, AbstractDict, Bool, Integer, AbstractFloat, AbstractString,
                            Base.TOML.DateTime, Base.TOML.Time, Base.TOML.Date}

is_valid_toml_value(@nospecialize(::Any)) = false
is_valid_toml_value(@nospecialize(::BaseTOMLValue)) = true

########
# Keys #
########

function printkey(io::IO, keys::Vector{String})
    for (i, k) in enumerate(keys)
        i != 1 && Base.print(io, ".")
        if length(k) == 0
            # empty key
            Base.print(io, "\"\"")
        elseif any(!isvalid_barekey_char, k)
            # quoted key
            Base.print(io, "\"")
            print_toml_escaped(io, k)
            Base.print(io, "\"")
        else
            Base.print(io, k)
        end
    end
end

function to_toml_value(@nospecialize(f::Function), value)
    if f === identity
        error("type `$(typeof(value))` is not a valid TOML type, pass a conversion function to `TOML.print`")
    end
    toml_value = f(value)
    if !is_valid_toml_value(toml_value)
        error("TOML syntax function for type `$(typeof(value))` did not return a valid TOML type but a `$(typeof(toml_value))`")
    end
    return toml_value
end

##########
# Values #
##########

# Fallback
function printvalue(f::Function, io::IO, value, sorted::Bool)
    toml_value = to_toml_value(f, value)
    @invokelatest printvalue(f, io, toml_value, sorted)
end

function printvalue(f::Function, io::IO, value::AbstractVector, sorted::Bool)
    Base.print(io, "[")
    for (i, x) in enumerate(value)
        i != 1 && Base.print(io, ", ")
        printvalue(f, io, x, sorted)
    end
    Base.print(io, "]")
end

function printvalue(f::Function, io::IO, value::Base.TOML.DateTime, sorted::Bool)
    printvalue(f, io, value.date, sorted)
    Base.print(io, "T")
    printvalue(f, io, value.time, sorted)
    Base.print(io, "Z")
end

function printvalue(f::Function, io::IO, value::Base.TOML.Time, sorted::Bool)
    Base.print(io, string(value.hour, pad=2))
    Base.print(io, ":")
    Base.print(io, string(value.minute, pad=2))
    Base.print(io, ":")
    Base.print(io, string(value.second, pad=2))
    if value.ms != 0
        Base.print(io, ".")
        Base.print(io, string(value.ms, pad=3))
    end
end

function printvalue(f::Function, io::IO, value::Base.TOML.Date, sorted::Bool)
    Base.print(io, string(value.year, pad=4))
    Base.print(io, "-")
    Base.print(io, string(value.month, pad=2))
    Base.print(io, "-")
    Base.print(io, string(value.day, pad=2))
end

function printvalue(f::Function, io::IO, value::Bool, sorted::Bool)
    Base.print(io, value ? "true" : "false")
end

function printvalue(f::Function, io::IO, value::Integer, sorted::Bool)
    value isa Signed && return Base.show(io, value)
    # unsigned integers are printed as hex
    n = 2 * ndigits(value, base=256)
    Base.print(io, "0x", string(value, base=16, pad=n))
    return
end

function printvalue(f::Function, io::IO, value::AbstractFloat, sorted::Bool)
    # The early conversion here avoids invalidations from isnan/isinf
    value = Float64(value)

    if isnan(value)
        Base.print(io, "nan")
    elseif isinf(value)
        Base.print(io, value > 0 ? "+inf" : "-inf")
    else
        Base.print(io, value) # TOML specifies IEEE 754 binary64 for float
    end
end

function printvalue(f::Function, io::IO, value::AbstractString, sorted::Bool)
    qmark = Base.contains(value, "\n") ? "\"\"\"" : "\""
    Base.print(io, qmark)
    print_toml_escaped(io, value)
    Base.print(io, qmark)
end

function printvalue(f::Function, io::IO, value::AbstractDict, sorted::Bool)
    print_inline_table(f, io, value, sorted)
end

function print_inline_table(f::Function, io::IO, value::AbstractDict, sorted::Bool)
    vkeys = collect(keys(value))::AbstractArray
    if sorted
        sort!(vkeys)
    end
    Base.print(io, "{")
    for (i, k) in enumerate(vkeys)
        v = value[k]
        i != 1 && Base.print(io, ", ")
        printkey(io, [String(k)])
        Base.print(io, " = ")
        printvalue(f, io, v, sorted)
    end
    Base.print(io, "}")
end


##########
# Tables #
##########

is_table(@nospecialize(value)) = isa(value, AbstractDict)
is_array_of_tables(@nospecialize(value)) =
    isa(value, AbstractArray) &&
    length(value) > 0 && (isa(value, AbstractArray{<:AbstractDict}) ||
                          all(v -> isa(v, AbstractDict), value))
is_tabular(@nospecialize(value)) = is_table(value) || @invokelatest(is_array_of_tables(value))

function print_table(f::Function, io::IO, a::AbstractDict,
    ks::Vector{String} = String[];
    indent::Int = 0,
    first_block::Bool = true,
    sorted::Bool = false,
    inline_tables::IdSet,
    by::Function = identity,
)

    if a in inline_tables
        @invokelatest print_inline_table(f, io, a, sorted)
        return
    end

    akeys = keys(a)
    if sorted
        akeys = sort!(collect(akeys); by)
    end

    # First print non-tabular entries
    for key in akeys
        value = a[key]
        if !is_valid_toml_value(value)
            value = to_toml_value(f, value)
        end
        if is_tabular(value) && !(value in inline_tables)
            continue
        end

        Base.print(io, ' '^4max(0,indent-1))
        printkey(io, [String(key)])
        Base.print(io, " = ") # print separator
        printvalue(f, io, value, sorted)
        Base.print(io, "\n")  # new line?
        first_block = false
    end

    for key in akeys
        value = a[key]
        if !is_valid_toml_value(value)
            value = to_toml_value(f, value)
        end
        if is_table(value) && !(value in inline_tables)
            push!(ks, String(key))
            _values = @invokelatest values(value)
            header = isempty(value) || !all(is_tabular(v) for v in _values)::Bool || any(v in inline_tables for v in _values)::Bool
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
            @invokelatest print_table(f, io, value, ks; indent = indent + header, first_block = header, sorted, by, inline_tables)
            pop!(ks)
        elseif @invokelatest(is_array_of_tables(value))
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
                @invokelatest print_table(f, io, v, ks; indent = indent + 1, sorted, by, inline_tables)
            end
            pop!(ks)
        end
    end
end


#######
# API #
#######

print(f::Function, io::IO, a::AbstractDict; sorted::Bool=false, by=identity, inline_tables::IdSet{<:AbstractDict}=IdSet{Dict{String}}()) =
    print_table(f, io, a; sorted, by, inline_tables)
print(f::Function, a::AbstractDict; sorted::Bool=false, by=identity, inline_tables::IdSet{<:AbstractDict}=IdSet{Dict{String}}()) =
    print(f, stdout, a; sorted, by, inline_tables)
print(io::IO, a::AbstractDict; sorted::Bool=false, by=identity, inline_tables::IdSet{<:AbstractDict}=IdSet{Dict{String}}()) =
    print_table(identity, io, a; sorted, by, inline_tables)
print(a::AbstractDict; sorted::Bool=false, by=identity, inline_tables::IdSet{<:AbstractDict}=IdSet{Dict{String}}()) =
    print(identity, stdout, a; sorted, by, inline_tables)

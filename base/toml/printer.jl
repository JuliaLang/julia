# This file is a part of Julia. License is MIT: https://julialang.org/license

import Base: @invokelatest
import ..isvalid_barekey_char # from Parser
import ..Comments, ..CommentBlock, ..CommentPath # from Parser

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

# Dynamic dispatch, for values of types that the printer does not handle statically.
# Kept in one place so that it can be overridden for `juliac --trim`.
@noinline _invoke_generic(@nospecialize(f), @nospecialize(args...)) = @invokelatest f(args...)

# Packages (e.g. Dates) extend `is_valid_toml_value`. Call it dynamically, so that doing
# so does not invalidate the printer, which also runs while loading packages.
_is_valid_toml_value(@nospecialize(x)) =
    x isa BaseTOMLValue || _invoke_generic(is_valid_toml_value, x)::Bool

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

function to_toml_value(f::F, @nospecialize(value)) where {F<:Function}
    if f === identity
        error(lazy"type `$(typeof(value))` is not a valid TOML type, pass a conversion function to `TOML.print`")
    end
    toml_value = f(value)
    if !_is_valid_toml_value(toml_value)
        error(lazy"TOML syntax function for type `$(typeof(value))` did not return a valid TOML type but a `$(typeof(toml_value))`")
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

# Print a value of any type. Values of the common TOML types (e.g. as returned by `TOML.parse`)
# are printed without dynamic dispatch, so that printing is inferable (needed for `--trim`).
function printvalue_any(f::F, io::IO, @nospecialize(value), sorted::Bool) where {F<:Function}
    value isa String && return printvalue(f, io, value, sorted)
    value isa Int64 && return printvalue(f, io, value, sorted)
    value isa UInt64 && return printvalue(f, io, value, sorted)
    value isa Float64 && return printvalue(f, io, value, sorted)
    value isa Bool && return printvalue(f, io, value, sorted)
    value isa Dict{String, Any} && return printvalue(f, io, value, sorted)
    value isa Vector{Any} && return printvalue(f, io, value, sorted)
    value isa Vector{String} && return printvalue(f, io, value, sorted)
    value isa Vector{Int64} && return printvalue(f, io, value, sorted)
    value isa Vector{UInt64} && return printvalue(f, io, value, sorted)
    value isa Vector{Float64} && return printvalue(f, io, value, sorted)
    value isa Vector{Bool} && return printvalue(f, io, value, sorted)
    value isa Vector{Dict{String, Any}} && return printvalue(f, io, value, sorted)
    value isa Base.TOML.DateTime && return printvalue(f, io, value, sorted)
    value isa Base.TOML.Date && return printvalue(f, io, value, sorted)
    value isa Base.TOML.Time && return printvalue(f, io, value, sorted)
    return printvalue_generic(f, io, value, sorted)
end

# Values of other types, e.g. from Dates (which extends `printvalue`)
printvalue_generic(f::Function, io::IO, @nospecialize(value), sorted::Bool) =
    _invoke_generic(printvalue, f, io, value, sorted)

function printvalue(f::F, io::IO, value::AbstractVector, sorted::Bool) where {F<:Function}
    Base.print(io, "[")
    for (i, x) in enumerate(value)
        i != 1 && Base.print(io, ", ")
        printvalue_any(f, io, x, sorted)
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

function printvalue(f::F, io::IO, value::AbstractDict, sorted::Bool) where {F<:Function}
    print_inline_table(f, io, value, sorted)
end

function print_inline_table(f::F, io::IO, value::AbstractDict, sorted::Bool) where {F<:Function}
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
        printvalue_any(f, io, v, sorted)
    end
    Base.print(io, "}")
end


############
# Comments #
############

# Sanitize comment text so it cannot introduce TOML syntax.

function print_comment_line(io::IO, indentstr::String, text::AbstractString)
    Base.print(io, indentstr, '#')
    # Preserve captured whitespace, but format programmatic comments as `# text`.
    if !(isempty(text) || first(text) == ' ' || first(text) == '\t')
        Base.print(io, ' ')
    end
    for c::AbstractChar in text
        if c == '\n'
            Base.print(io, '\n', indentstr, "# ")
        elseif c == '\t' || !Base.iscntrl(c)
            Base.print(io, c)
        end
    end
    Base.print(io, '\n')
end

function print_comments_above(io::IO, comments::Comments, path::CommentPath, indentstr::String)
    block = get(comments.items, path, nothing)
    block === nothing && return
    for line in block.above
        print_comment_line(io, indentstr, line)
    end
end

function print_comment_inline(io::IO, comments::Comments, path::CommentPath)
    block = get(comments.items, path, nothing)
    block === nothing && return
    text = block.inline
    text === nothing && return
    Base.print(io, " #")
    if !(isempty(text) || first(text) == ' ' || first(text) == '\t')
        Base.print(io, ' ')
    end
    for c::AbstractChar in text
        if c == '\t' || (c != '\n' && !Base.iscntrl(c))
            Base.print(io, c)
        else
            Base.print(io, ' ')
        end
    end
end

has_comments(comments::Comments, path::CommentPath) =
    haskey(comments.items, path) || haskey(comments.floating, path)

# Hoist nested comments to an inline table's owning entry.
function print_hoisted_inline_comments(io::IO, comments::Comments, a::AbstractDict,
                                       path::CommentPath, indentstr::String, sorted::Bool)
    floating = get(comments.floating, path, nothing)
    if floating !== nothing
        for line in floating
            print_comment_line(io, indentstr, line)
        end
    end
    vkeys = collect(keys(a))
    sorted && sort!(vkeys)
    for k in vkeys
        kpath = [path; String(k)]
        block = get(comments.items, kpath, nothing)
        if block !== nothing
            for line in block.above
                print_comment_line(io, indentstr, line)
            end
            block.inline === nothing || print_comment_line(io, indentstr, block.inline)
        end
        v = a[k]
        if v isa Dict{String, Any}
            print_hoisted_inline_comments(io, comments, v, kpath, indentstr, sorted)
        elseif v isa AbstractDict
            _invoke_generic(print_hoisted_inline_comments, io, comments, v, kpath, indentstr, sorted)
        end
    end
end

function print_comments_floating(io::IO, comments::Comments, path::CommentPath, indentstr::String)
    lines = get(comments.floating, path, nothing)
    lines === nothing && return false
    for line in lines
        print_comment_line(io, indentstr, line)
    end
    return !isempty(lines)
end

##########
# Tables #
##########

# `value` if it is a valid TOML value, otherwise `value` converted by `f`
toml_value(f::F, @nospecialize(value)) where {F<:Function} =
    _is_valid_toml_value(value) ? value : to_toml_value(f, value)

is_table(@nospecialize(value)) = isa(value, AbstractDict)
function is_array_of_tables(@nospecialize(value))
    value isa AbstractArray || return false
    value isa AbstractArray{<:Union{AbstractString, Number}} && return false
    value isa Vector{Any} && return _is_array_of_tables(value)
    value isa Vector{Dict{String, Any}} && return _is_array_of_tables(value)
    return _invoke_generic(_is_array_of_tables, value)::Bool
end
_is_array_of_tables(value::AbstractArray) =
    length(value) > 0 && (isa(value, AbstractArray{<:AbstractDict}) ||
                          all(v -> isa(v, AbstractDict), value))
is_tabular(@nospecialize(value)) = is_table(value) || is_array_of_tables(value)

print_table(f::F, io::IO, a::AbstractDict, ks::Vector{String} = String[];
            indent::Int = 0, first_block::Bool = true, sorted::Bool = false, inline_tables::IdSet,
            by::B = identity, comments::Union{Comments, Nothing} = nothing) where {F<:Function, B<:Function} =
    _print_table(f, io, a, ks, indent, first_block, sorted, inline_tables, by, comments)

function _print_table(f::F, io::IO, a::AbstractDict, ks::Vector{String}, indent::Int,
                      first_block::Bool, sorted::Bool, inline_tables::IdSet, by::B,
                      comments::Union{Comments, Nothing}) where {F<:Function, B<:Function}

    if a in inline_tables
        print_inline_table(f, io, a, sorted)
        return
    end

    entry_indentstr = ' '^4max(0, indent-1)
    if comments !== nothing
        if print_comments_floating(io, comments, ks, entry_indentstr)
            println(io)
        end
    end

    akeys = keys(a)
    if sorted
        akeys = sort!(collect(akeys); by)
    end

    # First print non-tabular entries
    for key in akeys
        value = toml_value(f, a[key])
        if is_tabular(value) && !(value in inline_tables)
            continue
        end

        if comments !== nothing
            entry_path = [ks; String(key)]
            print_comments_above(io, comments, entry_path, entry_indentstr)
            if value isa AbstractDict && value in inline_tables
                if value isa Dict{String, Any}
                    print_hoisted_inline_comments(io, comments, value, entry_path, entry_indentstr, sorted)
                else
                    _invoke_generic(print_hoisted_inline_comments, io, comments, value, entry_path, entry_indentstr, sorted)
                end
            end
        end
        Base.print(io, entry_indentstr)
        printkey(io, [String(key)])
        Base.print(io, " = ") # print separator
        printvalue_any(f, io, value, sorted)
        if comments !== nothing
            print_comment_inline(io, comments, [ks; String(key)])
        end
        Base.print(io, "\n")  # new line?
        first_block = false
    end

    for key in akeys
        value = toml_value(f, a[key])
        if is_table(value) && !(value in inline_tables)
            push!(ks, String(key))
            first_block = if value isa Dict{String, Any}
                print_subtable(f, io, value, ks, indent, first_block, sorted, inline_tables, by, comments)
            else
                _invoke_generic(print_subtable, f, io, value, ks, indent, first_block, sorted, inline_tables, by, comments)::Bool
            end
            pop!(ks)
        elseif is_array_of_tables(value)
            # print array of tables
            first_block || println(io)
            first_block = false
            push!(ks, String(key))
            if comments !== nothing
                print_comments_above(io, comments, ks, ' '^4indent)
            end
            if value isa Vector{Any}
                print_array_of_tables(f, io, value, ks, indent, sorted, inline_tables, by, comments)
            elseif value isa Vector{Dict{String, Any}}
                print_array_of_tables(f, io, value, ks, indent, sorted, inline_tables, by, comments)
            else
                _invoke_generic(print_array_of_tables, f, io, value, ks, indent, sorted, inline_tables, by, comments)
            end
            pop!(ks)
        end
    end
end

# Print the table `value` at path `ks`, return the new `first_block`.
function print_subtable(f::F, io::IO, value::AbstractDict, ks::Vector{String}, indent::Int,
                        first_block::Bool, sorted::Bool, inline_tables::IdSet, by::B,
                        comments::Union{Comments, Nothing}) where {F<:Function, B<:Function}
    _values = values(value)
    header = isempty(value) || !all(is_tabular(v) for v in _values)::Bool || any(v in inline_tables for v in _values)::Bool
    if !header && comments !== nothing
        # Preserve headers that anchor comments.
        header = has_comments(comments, ks)
    end
    if header
        # print table
        first_block || println(io)
        first_block = false
        if comments !== nothing
            print_comments_above(io, comments, ks, ' '^4indent)
        end
        Base.print(io, ' '^4indent)
        Base.print(io,"[")
        printkey(io, ks)
        Base.print(io,"]")
        if comments !== nothing
            print_comment_inline(io, comments, ks)
        end
        Base.print(io,"\n")
    end
    _print_table(f, io, value, ks, indent + header, header, sorted, inline_tables, by, comments)
    return first_block
end

function print_array_of_tables(f::F, io::IO, value::AbstractArray, ks::Vector{String}, indent::Int,
                               sorted::Bool, inline_tables::IdSet, by::B,
                               comments::Union{Comments, Nothing}) where {F<:Function, B<:Function}
    for v in value
        Base.print(io, ' '^4indent)
        Base.print(io,"[[")
        printkey(io, ks)
        Base.print(io,"]]\n")
        # TODO, nicer error here
        !isa(v, AbstractDict) && error("array should contain only tables")
        if v isa Dict{String, Any}
            _print_table(f, io, v, ks, indent + 1, true, sorted, inline_tables, by, comments)
        else
            _invoke_generic(_print_table, f, io, v, ks, indent + 1, true, sorted, inline_tables, by, comments)
        end
    end
end


#######
# API #
#######

print(f::F, io::IO, a::AbstractDict; sorted::Bool=false, by=identity, inline_tables::IdSet{<:AbstractDict}=IdSet{Dict{String}}(), comments::Union{Comments, Nothing}=nothing) where {F<:Function} =
    print_table(f, io, a; sorted, by, inline_tables, comments)
print(f::F, a::AbstractDict; sorted::Bool=false, by=identity, inline_tables::IdSet{<:AbstractDict}=IdSet{Dict{String}}(), comments::Union{Comments, Nothing}=nothing) where {F<:Function} =
    print(f, stdout, a; sorted, by, inline_tables, comments)
print(io::IO, a::AbstractDict; sorted::Bool=false, by=identity, inline_tables::IdSet{<:AbstractDict}=IdSet{Dict{String}}(), comments::Union{Comments, Nothing}=nothing) =
    print_table(identity, io, a; sorted, by, inline_tables, comments)
print(a::AbstractDict; sorted::Bool=false, by=identity, inline_tables::IdSet{<:AbstractDict}=IdSet{Dict{String}}(), comments::Union{Comments, Nothing}=nothing) =
    print(identity, stdout, a; sorted, by, inline_tables, comments)

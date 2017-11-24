# This file is a part of Julia. License is MIT: https://julialang.org/license

### Parsing utilities

_directives(::Type{DateFormat{S,T}}) where {S,T} = T.parameters

character_codes(df::Type{DateFormat{S,T}}) where {S,T} = character_codes(_directives(df))
function character_codes(directives::SimpleVector)
    letters = sizehint!(Char[], length(directives))
    for (i, directive) in enumerate(directives)
        if directive <: DatePart
            letter = first(directive.parameters)
            push!(letters, letter)
        end
    end
    return letters
end

genvar(t::DataType) = Symbol(lowercase(string(Base.datatype_name(t))))

"""
    tryparsenext_core(str::AbstractString, pos::Int, len::Int, df::DateFormat, raise=false)

Parses the string according to the directives within the DateFormat. Parsing will start at
character index `pos` and will stop when all directives are used or we have parsed up to
the end of the string, `len`. When a directive cannot be parsed the returned value tuple
will be null if `raise` is false otherwise an exception will be thrown.

Returns a 3-element tuple `(values, pos, num_parsed)`:
* `values::Nullable{Tuple}`: A tuple which contains a value for each `DatePart` within the
  `DateFormat` in the order in which they occur. If the string ends before we finish parsing
  all the directives the missing values will be filled in with default values.
* `pos::Int`: The character index at which parsing stopped.
* `num_parsed::Int`: The number of values which were parsed and stored within `values`.
  Useful for distinguishing parsed values from default values.
"""
@generated function tryparsenext_core(str::AbstractString, pos::Int, len::Int,
                                      df::DateFormat, raise::Bool=false)
    directives = _directives(df)
    letters = character_codes(directives)

    tokens = Type[CONVERSION_SPECIFIERS[letter] for letter in letters]
    value_names = Symbol[genvar(t) for t in tokens]
    value_defaults = Tuple(CONVERSION_DEFAULTS[t] for t in tokens)
    R = typeof(value_defaults)

    # Pre-assign variables to defaults. Allows us to use `@goto done` without worrying about
    # unassigned variables.
    assign_defaults = Expr[
        quote
            $name = $default
        end
        for (name, default) in zip(value_names, value_defaults)
    ]

    vi = 1
    parsers = Expr[
        begin
            if directives[i] <: DatePart
                name = value_names[vi]
                nullable = Symbol(:nullable_, name)
                vi += 1
                quote
                    pos > len && @goto done
                    $nullable, next_pos = tryparsenext(directives[$i], str, pos, len, locale)
                    isnull($nullable) && @goto error
                    $name = unsafe_get($nullable)
                    pos = next_pos
                    num_parsed += 1
                    directive_index += 1
                end
            else
                quote
                    pos > len && @goto done
                    nullable_delim, next_pos = tryparsenext(directives[$i], str, pos, len, locale)
                    isnull(nullable_delim) && @goto error
                    pos = next_pos
                    directive_index += 1
                end
            end
        end
        for i in 1:length(directives)
    ]

    quote
        directives = df.tokens
        locale::DateLocale = df.locale

        num_parsed = 0
        directive_index = 1

        $(assign_defaults...)
        $(parsers...)

        pos > len || @goto error

        @label done
        return Nullable{$R}($(Expr(:tuple, value_names...))), pos, num_parsed

        @label error
        if raise
            if directive_index > length(directives)
                throw(ArgumentError("Found extra characters at the end of date time string"))
            else
                d = directives[directive_index]
                throw(ArgumentError("Unable to parse date time. Expected directive $d at char $pos"))
            end
        end
        return Nullable{$R}(), pos, 0
    end
end

"""
    tryparsenext_internal(::Type{<:TimeType}, str, pos, len, df::DateFormat, raise=false)

Parses the string according to the directives within the DateFormat. The specified TimeType
type determines the type of and order of tokens returned. If the given DateFormat or string
does not provide a required token a default value will be used. When the string cannot be
parsed the returned value tuple will be null if `raise` is false otherwise an exception will
be thrown.

Returns a 2-element tuple `(values, pos)`:
* `values::Nullable{Tuple}`: A tuple which contains a value for each token as specified by
  the passed in type.
* `pos::Int`: The character index at which parsing stopped.
"""
@generated function tryparsenext_internal(::Type{T}, str::AbstractString, pos::Int, len::Int,
                                          df::DateFormat, raise::Bool=false) where T<:TimeType
    letters = character_codes(df)

    tokens = Type[CONVERSION_SPECIFIERS[letter] for letter in letters]
    value_names = Symbol[genvar(t) for t in tokens]

    output_tokens = CONVERSION_TRANSLATIONS[T]
    output_names = Symbol[genvar(t) for t in output_tokens]
    output_defaults = Tuple(CONVERSION_DEFAULTS[t] for t in output_tokens)
    R = typeof(output_defaults)

    # Pre-assign output variables to defaults. Ensures that all output variables are
    # assigned as the value tuple returned from `tryparsenext_core` may not include all
    # of the required variables.
    assign_defaults = Expr[
        quote
            $name = $default
        end
        for (name, default) in zip(output_names, output_defaults)
    ]

    # Unpacks the value tuple returned by `tryparsenext_core` into separate variables.
    value_tuple = Expr(:tuple, value_names...)

    quote
        values, pos, num_parsed = tryparsenext_core(str, pos, len, df, raise)
        isnull(values) && return Nullable{$R}(), pos
        $(assign_defaults...)
        $value_tuple = unsafe_get(values)
        return Nullable{$R}($(Expr(:tuple, output_names...))), pos
    end
end

@inline function tryparsenext_base10(str::AbstractString, i::Int, len::Int, min_width::Int=1, max_width::Int=0)
    i > len && (return Nullable{Int64}(), i)
    min_pos = min_width <= 0 ? i : i + min_width - 1
    max_pos = max_width <= 0 ? len : min(i + max_width - 1, len)
    d::Int64 = 0
    @inbounds while i <= max_pos
        c, ii = next(str, i)
        if '0' <= c <= '9'
            d = d * 10 + (c - '0')
        else
            break
        end
        i = ii
    end
    if i <= min_pos
        return Nullable{Int64}(), i
    else
        return Nullable{Int64}(d), i
    end
end

@inline function tryparsenext_word(str::AbstractString, i, len, locale, maxchars=0)
    word_start, word_end = i, 0
    max_pos = maxchars <= 0 ? len : min(chr2ind(str, ind2chr(str,i) + maxchars - 1), len)
    @inbounds while i <= max_pos
        c, ii = next(str, i)
        if isalpha(c)
            word_end = i
        else
            break
        end
        i = ii
    end
    if word_end == 0
        return Nullable{SubString}(), i
    else
        return Nullable{SubString}(SubString(str, word_start, word_end)), i
    end
end

function Base.parse(::Type{DateTime}, s::AbstractString, df::typeof(ISODateTimeFormat))
    i, end_pos = start(s), endof(s)

    dm = dd = Int64(1)
    th = tm = ts = tms = Int64(0)

    nv, i = tryparsenext_base10(s, i, end_pos, 1)
    dy = isnull(nv) ? (@goto error) : unsafe_get(nv)
    i > end_pos && @goto error

    c, i = next(s, i)
    c != '-' && @goto error
    i > end_pos && @goto done

    nv, i = tryparsenext_base10(s, i, end_pos, 1, 2)
    dm = isnull(nv) ? (@goto error) : unsafe_get(nv)
    i > end_pos && @goto done

    c, i = next(s, i)
    c != '-' && @goto error
    i > end_pos && @goto done

    nv, i = tryparsenext_base10(s, i, end_pos, 1, 2)
    dd = isnull(nv) ? (@goto error) : unsafe_get(nv)
    i > end_pos && @goto done

    c, i = next(s, i)
    c != 'T' && @goto error
    i > end_pos && @goto done

    nv, i = tryparsenext_base10(s, i, end_pos, 1, 2)
    th = isnull(nv) ? (@goto error) : unsafe_get(nv)
    i > end_pos && @goto done

    c, i = next(s, i)
    c != ':' && @goto error
    i > end_pos && @goto done

    nv, i = tryparsenext_base10(s, i, end_pos, 1, 2)
    tm = isnull(nv) ? (@goto error) : unsafe_get(nv)
    i > end_pos && @goto done

    c, i = next(s, i)
    c != ':' && @goto error
    i > end_pos && @goto done

    nv, i = tryparsenext_base10(s, i, end_pos, 1, 2)
    ts = isnull(nv) ? (@goto error) : unsafe_get(nv)
    i > end_pos && @goto done

    c, i = next(s, i)
    c != '.' && @goto error
    i > end_pos && @goto done

    nv, j = tryparsenext_base10(s, i, end_pos, 1, 3)
    tms = isnull(nv) ? (@goto error) : unsafe_get(nv)
    tms *= 10 ^ (3 - (j - i))

    j > end_pos || @goto error

    @label done
    return DateTime(dy, dm, dd, th, tm, ts, tms)

    @label error
    throw(ArgumentError("Invalid DateTime string"))
end

function Base.parse(::Type{T}, str::AbstractString, df::DateFormat=default_format(T)) where T<:TimeType
    pos, len = start(str), endof(str)
    values, pos = tryparsenext_internal(T, str, pos, len, df, true)
    T(unsafe_get(values)...)
end

function Base.tryparse(::Type{T}, str::AbstractString, df::DateFormat=default_format(T)) where T<:TimeType
    pos, len = start(str), endof(str)
    values, pos = tryparsenext_internal(T, str, pos, len, df, false)
    if isnull(values)
        Nullable{T}()
    elseif isnull(validargs(T, unsafe_get(values)...))
        # TODO: validargs gets called twice, since it's called again in the T constructor
        Nullable{T}(T(unsafe_get(values)...))
    else
        Nullable{T}()
    end
end

"""
    parse_components(str::AbstractString, df::DateFormat) -> Array{Any}

Parse the string into its components according to the directives in the DateFormat.
Each component will be a distinct type, typically a subtype of Period. The order of the
components will match the order of the `DatePart` directives within the DateFormat. The
number of components may be less than the total number of `DatePart`.
"""
@generated function parse_components(str::AbstractString, df::DateFormat)
    letters = character_codes(df)
    tokens = Type[CONVERSION_SPECIFIERS[letter] for letter in letters]

    quote
        pos, len = start(str), endof(str)
        values, pos, num_parsed = tryparsenext_core(str, pos, len, df, true)
        t = unsafe_get(values)
        types = $(Expr(:tuple, tokens...))
        result = Vector{Any}(uninitialized, num_parsed)
        for (i, typ) in enumerate(types)
            i > num_parsed && break
            result[i] = typ(t[i])  # Constructing types takes most of the time
        end
        return result
    end
end

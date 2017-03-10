### Parsing utilities

function directives{S,F}(::Type{DateFormat{S,F}})
    tokens = F.parameters
    di = 1
    directive_index = zeros(Int, length(tokens))
    directive_letters = sizehint!(Char[], length(tokens))
    for (i, token) in enumerate(tokens)
        if token <: DatePart
            directive_index[i] = di

            letter = first(token.parameters)
            push!(directive_letters, letter)

            di += 1
        end
    end
    return tokens, directive_index, directive_letters
end

genvar(t::DataType) = Symbol(lowercase(string(Base.datatype_name(t))))


@generated function tryparse_core(str::AbstractString, df::DateFormat, raise::Bool=false)
    token_types, directive_index, directive_letters = directives(df)

    directive_types = Type[FORMAT_SPECIFIERS[letter] for letter in directive_letters]
    directive_names = Symbol[genvar(t) for t in directive_types]
    directive_defaults = Tuple(FORMAT_DEFAULTS[t] for t in directive_types)
    R = typeof(directive_defaults)

    # Pre-assign output variables to default values. Allows us to use `@goto done` without
    # worrying about unassigned variables.
    assign_defaults = Expr[
        quote
            $name = $default
        end
        for (name, default) in zip(directive_names, directive_defaults)
    ]

    parsers = Expr[
        begin
            di = directive_index[i]
            if di != 0
                name = directive_names[di]
                nullable = Symbol(:nullable_, name)
                quote
                    pos > len && @goto done
                    $nullable, next_pos = tryparsenext(tokens[$i], str, pos, len, locale)
                    isnull($nullable) && @goto error
                    $name = unsafe_get($nullable)
                    pos = next_pos
                    directive_idx += 1
                    token_idx += 1
                end
            else
                quote
                    pos > len && @goto done
                    nullable_delim, next_pos = tryparsenext(tokens[$i], str, pos, len, locale)
                    isnull(nullable_delim) && @goto error
                    pos = next_pos
                    token_idx += 1
                end
            end
        end
        for i in 1:length(token_types)
    ]

    quote
        tokens = df.tokens
        locale::DateLocale = df.locale
        pos, len = start(str), endof(str)
        directive_idx = 0
        token_idx = 1

        $(assign_defaults...)
        $(parsers...)

        pos > len || @goto error

        @label done
        return Nullable{$R}($(Expr(:tuple, directive_names...))), directive_idx

        @label error
        # Note: Keeping exception generation in separate function helps with performance
        if raise
            if token_idx > length(tokens)
                throw(ArgumentError("Found extra characters at the end of date time string"))
            else
                throw(ArgumentError("Unable to parse date time. Expected token $(tokens[token_idx]) at char $pos"))
            end
        end
        return Nullable{$R}(), 0
    end
end


@generated function tryparse_internal{T<:TimeType}(
    ::Type{T}, str::AbstractString, df::DateFormat, raise::Bool=false,
)
    token_types, directive_index, directive_letters = directives(df)

    directive_types = Type[FORMAT_SPECIFIERS[letter] for letter in directive_letters]
    directive_names = Symbol[genvar(t) for t in directive_types]

    output_types = FORMAT_TRANSLATIONS[T]
    output_names = Symbol[genvar(t) for t in output_types]
    output_defaults = Tuple(FORMAT_DEFAULTS[t] for t in output_types)
    R = typeof(output_defaults)

    # Pre-assign output variables to default values. Ensures that all output variables are
    # assigned as the format directives may not include all of the required variables.
    assign_defaults = Expr[
        quote
            $name = $default
        end
        for (name, default) in zip(output_names, output_defaults)
    ]

    # Unpacks the tuple into various directive variables.
    directive_tuple = Expr(:tuple, directive_names...)

    quote
        values, index = tryparse_core(str, df, raise)
        isnull(values) && return Nullable{$R}()
        $(assign_defaults...)
        $directive_tuple = unsafe_get(values)
        Nullable{$R}($(Expr(:tuple, output_names...)))
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

function Base.parse{T<:TimeType}(
    ::Type{T}, str::AbstractString, df::DateFormat=default_format(T),
)
    nt = tryparse_internal(T, str, df, true)
    T(unsafe_get(nt)...)
end

function Base.tryparse{T<:TimeType}(
    ::Type{T}, str::AbstractString, df::DateFormat=default_format(T),
)
    nt = tryparse_internal(T, str, df, false)
    if isnull(nt)
        Nullable{T}()
    else
        Nullable{T}(T(unsafe_get(nt)...))
    end
end

@generated function Base.parse(::Type{Vector}, str::AbstractString, df::DateFormat)
    token_types, directive_index, directive_letters = directives(df)
    directive_types = Type[FORMAT_SPECIFIERS[letter] for letter in directive_letters]

    quote
        nt, num_parsed = tryparse_core(str, df, true)
        t = unsafe_get(nt)
        directive_types = $(Expr(:tuple, directive_types...))
        result = Vector{Any}(num_parsed)
        for (i, typ) in enumerate(directive_types)
            i > num_parsed && break
            result[i] = typ(t[i])  # Constructing types takes most of the time
        end
        return result
    end
end

# This file is a part of Julia. License is MIT: https://julialang.org/license

### Parsing utilities

_directives(::Type{DateFormat{S,T}}) where {S,T} = T.parameters

character_codes(df::Type{DateFormat{S,T}}) where {S,T} = character_codes(_directives(df))
function character_codes(directives::Core.SimpleVector)
    letters = sizehint!(Char[], length(directives))
    for (i, directive) in enumerate(directives)
        if directive <: DatePart
            letter = first(directive.parameters)
            push!(letters, letter)
        end
    end
    return letters
end

genvar(t::DataType) = Symbol(lowercase(string(nameof(t))))

# Token types with a reader for UTF-8 bytes. Bytes are read for other tokens as characters,
# with `tryparsenext_chars`.
hasbytereader(D::Type) =
    D <: Union{Delim{Char}, Delim{String}} || D <: DatePart && first(D.parameters) in "yYmdHIMSsnp"

"""
    tryparsenext_core(str, pos::Int, len::Int, df::DateFormat, raise=false)

Parse the string according to the directives within the `DateFormat`. `str` is a string,
or the bytes of UTF-8 text, such as `codeunits(s)`, read at byte indices with `len` their
length. Parsing will start at index `pos` and will stop when all directives are used or we
have parsed up to the end of the string, `len`. When a directive cannot be parsed the returned value
will be `nothing` if `raise` is false otherwise an exception will be thrown.

If successful, return a 3-element tuple `(values, pos, num_parsed)`:
* `values::Tuple`: A tuple which contains a value
  for each `DatePart` within the `DateFormat` in the order
  in which they occur. If the string ends before we finish parsing all the directives
  the missing values will be filled in with default values.
* `pos::Int`: The character index at which parsing stopped.
* `num_parsed::Int`: The number of values which were parsed and stored within `values`.
  Useful for distinguishing parsed values from default values.
"""
@generated function tryparsenext_core(str::Union{AbstractString, AbstractVector{UInt8}}, pos::Int, len::Int,
                                      df::DateFormat, raise::Bool=false)
    directives = _directives(df)
    letters = character_codes(directives)

    tokens = Type[CONVERSION_SPECIFIERS[letter] for letter in letters]
    value_names = Symbol[genvar(t) for t in tokens]
    value_defaults = Any[CONVERSION_DEFAULTS[t] for t in tokens]

    # Pre-assign variables to defaults. Allows us to use `@goto done` without worrying about
    # unassigned variables.
    assign_defaults = Expr[]
    for (name, default) in zip(value_names, value_defaults)
        push!(assign_defaults, quote
            $name = $default
        end)
    end

    reader(D) = str <: AbstractString || hasbytereader(D) ? :tryparsenext : :tryparsenext_chars

    vi = 1
    parsers = Expr[]
    for i = 1:length(directives)
        read = reader(directives[i])
        if directives[i] <: DatePart
            name = value_names[vi]
            vi += 1
            push!(parsers, quote
                pos > len && @goto done
                let val = $read(directives[$i], str, pos, len, locale)
                    val === nothing && @goto error
                    $name, pos = val
                end
                num_parsed += 1
                directive_index += 1
            end)
        else
            push!(parsers, quote
                pos > len && @goto done
                let val = $read(directives[$i], str, pos, len, locale)
                    val === nothing && @goto error
                    delim, pos = val
                end
                directive_index += 1
            end)
        end
    end

    # The error message prints the directive that failed. Each one is printed at its own
    # type: indexing the mixed-type tuple at a runtime position would need dynamic dispatch.
    directive_strings = Expr(:tuple, (:(string(directives[$i])) for i in 1:length(directives))...)

    return quote
        $(Expr(:meta, :inline))
        directives = df.tokens
        locale::DateLocale = df.locale

        num_parsed = 0
        directive_index = 1

        $(assign_defaults...)
        $(parsers...)

        pos > len || @goto error

        @label done
        return $(Expr(:tuple, value_names...)), pos, num_parsed

        @label error
        if raise
            if directive_index > length(directives)
                throw(ArgumentError("Found extra characters at the end of date time string"))
            else
                d = $directive_strings[directive_index]
                throw(ArgumentError("Unable to parse date time. Expected directive $d at char $pos"))
            end
        end
        return nothing
    end
end

# The width of each DatePart in the format string `S`, found as `DateFormat` finds them: a
# run of one code letter that a backslash does not precede. Returns `nothing` if `S` does
# not have the DateParts of `directives` in that order.
function format_widths(S::Symbol, directives::Core.SimpleVector)
    f = String(S)
    widths = Int[]
    k = firstindex(f)
    for D in directives
        D <: DatePart || continue
        c = first(D.parameters)
        while k <= lastindex(f) && !(f[k] == c && (k == firstindex(f) || f[prevind(f, k)] != '\\'))
            k = nextind(f, k)
        end
        k <= lastindex(f) || return nothing
        width = 0
        while k <= lastindex(f) && f[k] == c
            width += 1
            k = nextind(f, k)
        end
        push!(widths, width)
    end
    return widths
end

# 10^k for k in 0:9
const POWERS_OF_TEN = ntuple(k -> Int64(10)^(k - 1), 10)

# A fractional second that ends the text: all of `bytes[i:len]`, at least one byte, read
# as `tryparsenext_fraction` reads a field without a fixed width, with a precision of at
# most 9
@inline function tryparsenext_lastfraction(bytes::AbstractVector{UInt8}, i::Int, len::Int, precision::Int)
    ndigits = min(len - i + 1, precision)
    frac = Int64(0)
    ok = true
    first = i
    # Eight digits at a time, as `Base.Parsers` reads integers
    if ndigits >= 8
        w = Base.Parsers.load8(bytes, i)
        ok = Base.Parsers.isdigits8(w)
        frac = Int64(Base.Parsers.digits8(w))
        first = i + 8
    end
    @inbounds for k in first:(i + ndigits - 1)
        digit = bytes[k] - UInt8('0')
        ok &= digit <= 0x09
        frac = 10frac + digit
    end
    # Digits past the precision must be zero
    @inbounds for k in (i + ndigits):len
        ok &= bytes[k] == UInt8('0')
    end
    ok || return nothing
    return frac * POWERS_OF_TEN[precision - ndigits + 1]
end

"""
    tryparsenext_fixed(bytes, pos, len, df::DateFormat)

Read `bytes` when each numeric field has exactly the width that `df` writes it with, as in
`2024-02-13T05:28:17.123` for ISO formats, at offsets fixed when the method is compiled.
`df` must consist of numeric fields and ASCII delimiters, optionally followed by a
delimiter and a fractional second. Returns what `tryparsenext_core` returns for such text,
or `nothing` when the text has another shape, such as a one-digit month, a signed year, or
a missing field. The caller then reads it with `tryparsenext_core`, which also reports any
error.
"""
@generated function tryparsenext_fixed(bytes::AbstractVector{UInt8}, pos::Int, len::Int, df::DateFormat)
    directives = _directives(df)
    n = length(directives)
    isnumberpart(D) = D <: DatePart && first(D.parameters) in "yYmdHIMS"
    isfractionpart(D) = D <: DatePart && first(D.parameters) in "sn"
    isdelimiter(D) = D <: Union{Delim{Char, 1}, Delim{String}}
    hasfraction = n >= 2 && isfractionpart(directives[n]) && isdelimiter(directives[n - 1])
    bodyend = hasfraction ? n - 2 : n
    (bodyend > 0 && all(k -> isnumberpart(directives[k]) || isdelimiter(directives[k]), 1:bodyend)) || return :(nothing)
    # The offsets come from the widths in the format string, and the generated code checks
    # that the tokens have them
    widths = format_widths(df.parameters[1], directives)
    widths === nothing && return :(nothing)

    letters = character_codes(directives)
    tokens = Type[CONVERSION_SPECIFIERS[letter] for letter in letters]
    value_names = Symbol[genvar(t) for t in tokens]

    guards = Expr[]
    reads = Expr[]
    checks = Expr[]
    # Check that delimiter `k` is at byte `at` of the text, in ASCII, and return its length
    function delimiter!(checks, k, at)
        if directives[k] <: Delim{Char}
            push!(checks, :(let b = @inbounds bytes[$at]
                (b < 0x80) & (UInt32(b) << 24 == reinterpret(UInt32, directives[$k].d))
            end))
            return 1
        end
        N = directives[k].parameters[2]
        push!(guards, :(ncodeunits(directives[$k].d) == $N))
        for j in 1:N
            push!(checks, :(let b = @inbounds bytes[$at + $(j - 1)]
                (b < 0x80) & (b == @inbounds codeunit(directives[$k].d, $j))
            end))
        end
        return N
    end
    offset = 0
    vi = 0
    for k in 1:bodyend
        D = directives[k]
        if D <: DatePart
            vi += 1
            width = widths[vi]
            width <= 18 || return :(nothing)
            fixed = k < n && directives[k + 1] <: DatePart
            # A field without a fixed width reads every digit, whatever its width
            push!(guards, fixed ? :(directives[$k].fixed & (directives[$k].width == $width)) : :(!directives[$k].fixed))
            digits = [gensym(:digit) for j in 1:width]
            for j in 1:width
                push!(reads, :($(digits[j]) = (@inbounds bytes[pos + $(offset + j - 1)]) - UInt8('0')))
                push!(checks, :($(digits[j]) <= 0x09))
            end
            push!(reads, :($(value_names[vi]) = +($((:(Int64($(digits[j])) * $(Int64(10)^(width - j))) for j in 1:width)...))))
            offset += width
            # A field without a fixed width takes every digit, and a delimiter can be a digit.
            # After the last field, the text ends or the fraction's delimiter follows.
            fixed || k == bodyend || push!(checks, :((@inbounds bytes[pos + $offset]) - UInt8('0') > 0x09))
        else
            offset += delimiter!(checks, k, :(pos + $offset))
        end
    end

    # Each check is evaluated, so that a single branch rejects the text
    all_of(conditions) = foldl((a, b) -> :($a & $b), conditions; init = true)

    fraction = if hasfraction
        name = value_names[end]
        lastisfield = directives[bodyend] <: DatePart
        precision = first(directives[n].parameters) == 's' ? 3 : 9
        # The fraction ends the text, so it takes every digit to the end
        push!(guards, :(!directives[$n].fixed))
        separator = Expr[]
        # The last field takes every digit, and a delimiter can be a digit
        lastisfield && push!(separator, :((@inbounds bytes[i]) - UInt8('0') > 0x09))
        seplen = delimiter!(separator, n - 1, :i)
        quote
            $name = $(CONVERSION_DEFAULTS[tokens[end]])
            # The text may stop before the delimiter or right after it, as tryparsenext_core allows
            if i <= len
                i + $(seplen - 1) <= len && $(all_of(separator)) || return nothing
                i += $seplen
                if i <= len
                    frac = tryparsenext_lastfraction(bytes, i, len, $precision)
                    frac === nothing && return nothing
                    $name = frac
                    i = len + 1
                    num_parsed += 1
                end
            end
        end
    else
        nothing
    end

    return quote
        $(Expr(:meta, :inline))
        directives = df.tokens
        $(all_of(guards)) && pos + $(offset - 1) <= len || return nothing
        $(reads...)
        $(all_of(checks)) || return nothing
        i = pos + $offset
        num_parsed = $vi
        $fraction
        i > len || return nothing
        return $(Expr(:tuple, value_names...)), i, num_parsed
    end
end

# Every Timestamp{P} parses the same fields
conversion_translations(::Type{T}) where {T<:TimeType} = CONVERSION_TRANSLATIONS[T]
conversion_translations(::Type{<:Timestamp}) = CONVERSION_TRANSLATIONS[Timestamp]

"""
    tryparsenext_internal(::Type{<:TimeType}, str, pos, len, df::DateFormat, raise=false)

Parse the string according to the directives within the `DateFormat`. The specified `TimeType`
type determines the type of and order of tokens returned. If the given `DateFormat` or string
does not provide a required token a default value will be used. When the string cannot be
parsed the returned value will be `nothing` if `raise` is false otherwise an exception will
be thrown.

If successful, returns a 2-element tuple `(values, pos)`:
* `values::Tuple`: A tuple which contains a value
  for each token as specified by the passed in type.
* `pos::Int`: The character index at which parsing stopped.
"""
@inline function tryparsenext_internal(::Type{T}, str::Union{AbstractString, AbstractVector{UInt8}}, pos::Int, len::Int,
                                       df::DateFormat, raise::Bool=false) where T<:TimeType
    val = tryparsenext_core(str, pos, len, df, raise)
    val === nothing && return nothing
    values, pos, num_parsed = val
    fields = translatevalues(T, values, df, raise)
    fields === nothing && return nothing
    return fields, pos
end

# The fields of a `T` from the `values` that `df` reads, with defaults for those `df` lacks,
# or `nothing` (or an error if `raise`) for a `DateTime` fraction that is not whole
# milliseconds
@generated function translatevalues(::Type{T}, values::Tuple, df::DateFormat, raise::Bool) where T<:TimeType
    letters = character_codes(df)

    tokens = Type[CONVERSION_SPECIFIERS[letter] for letter in letters]
    value_names = Symbol[genvar(t) for t in tokens]

    output_tokens = conversion_translations(T)
    output_names = Symbol[genvar(t) for t in output_tokens]
    output_defaults = Any[CONVERSION_DEFAULTS[t] for t in output_tokens]

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

    # DateTime has no nanosecond field, so add an `n` field to the milliseconds. It must
    # be a whole number of milliseconds.
    normalize_fraction = if T === DateTime && Nanosecond in tokens
        quote
            millisecond_from_nanoseconds, nanosecond_remainder =
                divrem(nanosecond, Int64(1000000))
            if nanosecond_remainder != 0
                raise && throw(ArgumentError("Fractional second is not exactly representable as a DateTime"))
                return nothing
            end
            millisecond += millisecond_from_nanoseconds
        end
    else
        nothing
    end

    return quote
        $(Expr(:meta, :inline))
        $(assign_defaults...)
        $value_tuple = values
        $normalize_fraction
        return $(Expr(:tuple, output_names...))
    end
end

@inline function tryparsenext_sign(str::AbstractString, i::Int, len::Int)
    i > len && return nothing
    c, ii = iterate(str, i)::Tuple{Char, Int}
    if c == '+'
        return 1, ii
    elseif c == '-'
        return -1, ii
    else
        return nothing
    end
end

@inline function tryparsenext_base10(str::AbstractString, i::Int, len::Int, min_width::Int=1, max_width::Int=0)
    i > len && return nothing
    min_pos = min_width <= 0 ? i : i + min_width - 1
    max_pos = max_width <= 0 ? len : min(i + max_width - 1, len)
    d::Int64 = 0
    @inbounds while i <= max_pos
        c, ii = iterate(str, i)::Tuple{Char, Int}
        if '0' <= c <= '9'
            digit = Int64(c - '0')
            d > div(typemax(Int64) - digit, 10) && return nothing
            d = d * 10 + digit
        else
            break
        end
        i = ii
    end
    if i <= min_pos
        return nothing
    else
        return d, i
    end
end

# Signs and digits are ASCII, and a non-ASCII character's bytes never match one
@inline function tryparsenext_sign(bytes::AbstractVector{UInt8}, i::Int, len::Int)
    i > len && return nothing
    b = @inbounds bytes[i]
    b == UInt8('+') && return 1, i + 1
    b == UInt8('-') && return -1, i + 1
    return nothing
end

@inline function tryparsenext_base10(bytes::AbstractVector{UInt8}, i::Int, len::Int, min_width::Int=1, max_width::Int=0)
    i > len && return nothing
    min_pos = min_width <= 0 ? i : i + min_width - 1
    max_pos = max_width <= 0 ? len : min(i + max_width - 1, len)
    # Eighteen digits cannot overflow
    safe_pos = i + 17
    d::Int64 = 0
    @inbounds while i <= max_pos
        digit = bytes[i] - UInt8('0')
        digit <= 0x09 || break
        i > safe_pos && d > div(typemax(Int64) - digit, 10) && return nothing
        d = d * 10 + digit
        i += 1
    end
    return i <= min_pos ? nothing : (d, i)
end

@inline function tryparsenext_word(str::AbstractString, i, len, locale, maxchars=0)
    word_start, word_end = i, 0
    max_pos = maxchars <= 0 ? len : min(len, nextind(str, i, maxchars-1))
    @inbounds while i <= max_pos
        c, ii = iterate(str, i)::Tuple{Char, Int}
        if isletter(c)
            word_end = i
        else
            break
        end
        i = ii
    end
    if word_end == 0
        return nothing
    else
        return SubString(str, word_start, word_end), i
    end
end

@inline function Base.parse(::Type{DateTime}, s::AbstractString, df::typeof(ISODateTimeFormat))
    # Read text with every field at full width at fixed offsets. Below 24 bytes it has at
    # most three fraction digits, which is all this method accepts.
    if s isa Union{String, SubString{String}} && ncodeunits(s) <= 23
        dt = parsefixed(DateTime, codeunits(s), ncodeunits(s), df)
        dt isa DateTime && return dt
    end
    return parse_isodatetime(s)
end

# The rest of `parse(DateTime, s, ISODateTimeFormat)`, out of line so that only the fast
# path is inlined into callers
@noinline function parse_isodatetime(s::AbstractString)
    i, end_pos = firstindex(s), lastindex(s)
    i > end_pos && throw(ArgumentError("Cannot parse an empty string as a DateTime"))

    coefficient = 1
    local dy
    dm = dd = Int64(1)
    th = tm = ts = tms = Int64(0)
    @label error begin
        @label done begin
            # Optional sign
            let val = tryparsenext_sign(s, i, end_pos)
                if val !== nothing
                    coefficient, i = val
                end
            end

            let val = tryparsenext_base10(s, i, end_pos, 1)
                val === nothing && break error
                dy, i = val
                i > end_pos && break done
            end

            c, i = iterate(s, i)::Tuple{Char, Int}
            c != '-' && break error
            i > end_pos && break done

            let val = tryparsenext_base10(s, i, end_pos, 1, 2)
                val === nothing && break error
                dm, i = val
                i > end_pos && break done
            end

            c, i = iterate(s, i)::Tuple{Char, Int}
            c != '-' && break error
            i > end_pos && break done

            let val = tryparsenext_base10(s, i, end_pos, 1, 2)
                val === nothing && break error
                dd, i = val
                i > end_pos && break done
            end

            c, i = iterate(s, i)::Tuple{Char, Int}
            c != 'T' && break error
            i > end_pos && break done

            let val = tryparsenext_base10(s, i, end_pos, 1, 2)
                val === nothing && break error
                th, i = val
                i > end_pos && break done
            end

            c, i = iterate(s, i)::Tuple{Char, Int}
            c != ':' && break error
            i > end_pos && break done

            let val = tryparsenext_base10(s, i, end_pos, 1, 2)
                val === nothing && break error
                tm, i = val
                i > end_pos && break done
            end

            c, i = iterate(s, i)::Tuple{Char, Int}
            c != ':' && break error
            i > end_pos && break done

            let val = tryparsenext_base10(s, i, end_pos, 1, 2)
                val === nothing && break error
                ts, i = val
                i > end_pos && break done
            end

            c, i = iterate(s, i)::Tuple{Char, Int}
            c != '.' && break error
            i > end_pos && break done

            let val = tryparsenext_base10(s, i, end_pos, 1, 3)
                val === nothing && break error
                tms, j = val
                tms *= 10 ^ (3 - (j - i))
                j > end_pos || break error
            end
        end

        return DateTime(dy * coefficient, dm, dd, th, tm, ts, tms)
    end
    throw(ArgumentError("Invalid DateTime string"))
end

# The value of the parsed fields of a `T`, or a `ParseFailure`: `INVALID` where `validargs`
# would return an error, or `OVERFLOW` for a `Timestamp` outside the range of its type.
# These make the checks of `validargs` and the arithmetic of the constructors, without
# building an error.
@inline function fromparts(::Type{Date}, y::Int64, m::Int64, d::Int64)
    0 < m < 13 && 0 < d <= daysinmonth(y, m) || return INVALID
    return Date(UTD(totaldays(y, m, d)))
end

# A 24-hour clock also allows 24:00 when the rest of the time is zero
@inline validhour(h::Int64, ampm::AMPM, restzero::Bool) =
    ampm == TWENTYFOURHOUR ? -1 < h < 24 || (h == 24 && restzero) : 0 < h < 13

@inline function fromparts(::Type{DateTime}, y::Int64, m::Int64, d::Int64, h::Int64, mi::Int64,
                           s::Int64, ms::Int64, ampm::AMPM)
    0 < m < 13 && 0 < d <= daysinmonth(y, m) || return INVALID
    validhour(h, ampm, mi == s == ms == 0) && -1 < mi < 60 && -1 < s < 60 && -1 < ms < 1000 ||
        return INVALID
    return DateTime(UTM(ms + 1000 * (s + 60mi + 3600adjusthour(h, ampm) + 86400 * totaldays(y, m, d))))
end

@inline function fromparts(::Type{Time}, h::Int64, mi::Int64, s::Int64, ms::Int64, us::Int64,
                           ns::Int64, ampm::AMPM)
    validhour(h, ampm, false) && -1 < mi < 60 && -1 < s < 60 && -1 < ms < 1000 &&
        -1 < us < 1000 && -1 < ns < 1000000000 && 1000000ms + 1000us + ns < 1000000000 ||
        return INVALID
    return Time(Nanosecond(ns + 1000us + 1000000ms + 1000000000s + 60000000000mi + 3600000000000adjusthour(h, ampm)))
end

fromparts(::Type{Timestamp}, values...) = fromparts(Timestamp{Nanosecond}, values...)

# Fields of other types, which tokens that packages define can read, are left to the constructors
fromparts(::Type, values...) = nothing

@inline function fromparts(::Type{Timestamp{P}}, y::Int64, m::Int64, d::Int64, h::Int64, mi::Int64,
                           s::Int64, ms::Int64, us::Int64, ns::Int64, ampm::AMPM) where {P}
    # Throws for a resolution that does not divide a second, as the type's range does
    scale = timestamp_scale(P)
    0 < m < 13 && 0 < d <= daysinmonth(y, m) || return INVALID
    validhour(h, ampm, mi == s == ms == us == ns == 0) && -1 < mi < 60 && -1 < s < 60 &&
        -1 < ms < 1000 && -1 < us < 1000 && -1 < ns < 1000000000 &&
        1000000ms + 1000us + ns < 1000000000 || return INVALID
    nsofday = ns + 1000us + 1000000ms + 1000000000 * (s + 60mi + 3600adjusthour(h, ampm))
    ticks, remainder = divrem(nsofday, scale)
    iszero(remainder) || return INVALID
    year(typemin(Timestamp{P})) <= y <= year(typemax(Timestamp{P})) || return OVERFLOW
    epochdays = timestamp_totaldays(P, y, m, d) - UNIXEPOCHDAYS
    perday = timestamp_ticks_per_day(P)
    fldmod(value(typemin(P)), perday) <= (epochdays, ticks) <= fldmod(value(typemax(P)), perday) ||
        return OVERFLOW
    # A count in range is a whole number of ticks
    timestamp_count_type(P) === Int64 && ticks isa Int64 && epochdays isa Int64 &&
        return Timestamp{P}(UTInstant(P(epochdays * perday + ticks)))
    return timestamp_from_day(Timestamp{P}, timestamp_totaldays(P, y, m, d), nsofday)
end

# The text that `parse` and `tryparse` read: the bytes of a `String` or `SubString{String}`,
# at byte indices, or other strings as characters
parsetext(str::Union{String, SubString{String}}) = codeunits(str), ncodeunits(str)
parsetext(str::AbstractString) = str, lastindex(str)

"""
    parsefixed(T, bytes, len, df::DateFormat)

The `T` of the text in `bytes[1:len]` when `tryparsenext_fixed` reads it, or a
`ParseFailure` for its fields out of range, or `nothing` for text it does not read, which
the general reader then reads.
"""
@inline function parsefixed(::Type{T}, bytes::AbstractVector{UInt8}, len::Int, df::DateFormat) where T<:TimeType
    val = tryparsenext_fixed(bytes, 1, len, df)
    val === nothing && return nothing
    fields = translatevalues(T, first(val), df, false)
    fields === nothing && return nothing
    return fromparts(T, fields...)
end
@inline parsefixed(::Type{T}, str::Union{String, SubString{String}}, df::DateFormat) where {T<:Union{Date, DateTime, Time, Timestamp}} =
    parsefixed(T, codeunits(str), ncodeunits(str), df)
parsefixed(::Type, str::AbstractString, df::DateFormat) = nothing

# `parse`, `tryparse`, and `parsevalue` inline the fixed-offset reader into callers, where
# a constant format folds its checks, and call the general reader out of line
@inline function Base.parse(::Type{T}, str::AbstractString, df::DateFormat=default_format(T)) where T<:TimeType
    x = parsefixed(T, str, df)
    x isa T && return x
    return parse_general(T, str, df)
end

@noinline function parse_general(::Type{T}, str::AbstractString, df::DateFormat) where T<:TimeType
    text, len = parsetext(str)
    pos = firstindex(str)
    pos > len && throw(ArgumentError("Cannot parse an empty string as a Date or Time"))
    val = tryparsenext_internal(T, text, pos, len, df, true)
    @assert val !== nothing
    values, endpos = val
    if T <: Union{Date, DateTime, Time, Timestamp}
        x = fromparts(T, values...)
        x isa T && return x
    end
    # Throws the error for the fields
    return T(values...)::T
end

@inline function Base.tryparse(::Type{T}, str::AbstractString, df::DateFormat=default_format(T)) where T<:TimeType
    x = parsefixed(T, str, df)
    # The general reader reads the same fields from text that the fixed offsets read
    x === nothing || return x isa ParseFailure ? nothing : x
    return tryparse_general(T, str, df)
end

@noinline function tryparse_general(::Type{T}, str::AbstractString, df::DateFormat) where T<:TimeType
    text, len = parsetext(str)
    pos = firstindex(str)
    pos > len && return nothing
    res = tryparsenext_internal(T, text, pos, len, df, false)
    res === nothing && return nothing
    values, endpos = res
    if T <: Union{Date, DateTime, Time, Timestamp}
        x = fromparts(T, values...)
        x === nothing || return x isa ParseFailure ? nothing : x
    end
    if validargs(T, values...) === nothing
        # TODO: validargs gets called twice, since it's called again in the T constructor
        return T(values...)::T
    end
    return nothing
end

"""
    Base.Parsers.parsevalue(T, bytes; dateformat = Dates.default_format(T))

For a `Date`, `DateTime`, `Time` or `Timestamp`, parse all of `bytes` as UTF-8 text in
the [`DateFormat`](@ref) `dateformat`, by default the ISO 8601 format of `T`. Returns
the value that [`tryparse`](@ref) returns for the text, or `Base.Parsers.INVALID` where
`tryparse` returns `nothing`, except that a `Timestamp` outside the range of its type is
`Base.Parsers.OVERFLOW`.

# Examples
```jldoctest
julia> Base.Parsers.parsevalue(Date, codeunits("2024-02-13"))
2024-02-13

julia> Base.Parsers.parsevalue(DateTime, view(codeunits("x,02/13/2024 5:28 PM,y"), 3:20);
                               dateformat = dateformat"mm/dd/yyyy I:MM p")
2024-02-13T17:28:00

julia> Base.Parsers.parsevalue(Date, codeunits("2024-02-30"))
Base.Parsers.INVALID
```

!!! compat "Julia 1.15"
    These methods require Julia 1.15 or later.
"""
@inline function Base.Parsers.parsevalue(::Type{T}, bytes::AbstractVector{UInt8};
                                 dateformat::DateFormat = default_format(T)) where {T<:Union{Date, DateTime, Time, Timestamp}}
    require_one_based_indexing(bytes)
    len = length(bytes)
    x = parsefixed(T, bytes, len, dateformat)
    x === nothing || return x
    return parsevalue_general(T, bytes, len, dateformat)
end

@noinline function parsevalue_general(::Type{T}, bytes::AbstractVector{UInt8}, len::Int, df::DateFormat) where T<:TimeType
    val = len > 0 ? tryparsenext_internal(T, bytes, 1, len, df, false) : nothing
    val === nothing && return INVALID
    values = first(val)
    x = fromparts(T, values...)
    x === nothing || return x
    return validargs(T, values...) === nothing ? T(values...)::T : INVALID
end

"""
    parse_components(str::AbstractString, df::DateFormat)::Array{Any}

Parse the string into its components according to the directives in the `DateFormat`.
Each component will be a distinct type, typically a subtype of Period. The order of the
components will match the order of the `DatePart` directives within the `DateFormat`. The
number of components may be less than the total number of `DatePart` directives.
"""
@generated function parse_components(str::AbstractString, df::DateFormat)
    letters = character_codes(df)
    tokens = Type[CONVERSION_SPECIFIERS[letter] for letter in letters]

    return quote
        pos, len = firstindex(str), lastindex(str)
        val = tryparsenext_core(str, pos, len, df, #=raise=#true)
        @assert val !== nothing
        values, pos, num_parsed = val
        types = $(Expr(:tuple, tokens...))
        result = Vector{Any}(undef, num_parsed)
        for (i, typ) in enumerate(types)
            i > num_parsed && break
            result[i] = typ(values[i])  # Constructing types takes most of the time
        end
        return result
    end
end

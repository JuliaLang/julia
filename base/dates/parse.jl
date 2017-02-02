### Parsing utilities

@generated function tryparse_internal{T<:TimeType, S, F}(::Type{T}, str::AbstractString, df::DateFormat{S, F}, raise::Bool=false)
    token_types = Type[dp <: DatePart ? SLOT_RULE[first(dp.parameters)] : Void for dp in F.parameters]
    N = length(F.parameters)

    types = slot_order(T)
    num_types = length(types)
    order = Vector{Int}(num_types)
    for i = 1:num_types
        order[i] = findfirst(token_types, types[i])
    end

    field_defaults = slot_defaults(T)
    field_order = tuple(order...)
    tuple_type = slot_types(T)

    # `slot_order`, `slot_defaults`, and `slot_types` return tuples of the same length
    assert(num_types == length(field_order) == length(field_defaults))

    quote
        R = Nullable{$tuple_type}
        t = df.tokens
        l = df.locale
        pos, len = start(str), endof(str)

        err_idx = 1
        Base.@nexprs $N i->val_i = 0
        Base.@nexprs $N i->(begin
            pos > len && @goto done
            nv, next_pos = tryparsenext(t[i], str, pos, len, l)
            isnull(nv) && @goto error
            val_i, pos = unsafe_get(nv), next_pos
            err_idx += 1
        end)
        pos <= len && @goto error

        @label done
        parts = Base.@ntuple $N val
        return R(reorder_args(parts, $field_order, $field_defaults, err_idx)::$tuple_type)

        @label error
        # Note: Keeping exception generation in separate function helps with performance
        raise && throw(gen_exception(t, err_idx, pos))
        return R()
    end
end

function gen_exception(tokens, err_idx, pos)
    if err_idx > length(tokens)
        ArgumentError("Found extra characters at the end of date time string")
    else
        ArgumentError("Unable to parse date time. Expected token $(tokens[err_idx]) at char $pos")
    end
end

#    reorder_args(val, idx, default, default_from)
#
# reorder elements of `val` tuple according to `idx` tuple. Use `default[i]`
# when `idx[i] == 0` or i >= default_from
#
# returns a tuple `xs` of the same length as `idx` where `xs[i]` is
# `val[idx[i]]` if `idx[i]` is non zero, `default[i]` if `idx[i]` is zero.
#
# `xs[i]` is `default[i]` for all i >= `default_from`.
#
#
function reorder_args{N}(val::Tuple, idx::NTuple{N}, default::Tuple, default_from::Integer)
    ntuple(Val{N}) do i
        if idx[i] == 0 || idx[i] >= default_from
            default[i]
        else
            val[idx[i]]
        end
    end
end

function Base.tryparse{T<:TimeType}(::Type{T}, str::AbstractString, df::DateFormat)
    nt = tryparse_internal(T, str, df, false)
    if isnull(nt)
        return Nullable{T}()
    else
        return Nullable{T}(T(unsafe_get(nt)...))
    end
end

default_format(::Type{Date}) = ISODateFormat
default_format(::Type{DateTime}) = ISODateTimeFormat

function Base.parse{T<:TimeType}(::Type{T},
                                 str::AbstractString,
                                 df::DateFormat=default_format(T))
    nt = tryparse_internal(T, str, df, true)
    T(unsafe_get(nt)...)
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

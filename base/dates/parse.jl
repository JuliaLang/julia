### Parsing utilities

@generated function tryparse_internal{T<:TimeType, N}(::Type{T}, str::AbstractString, df::DateFormat{NTuple{N}}, raise::Bool=false)
    token_types = Type[dp <: DatePart ? SLOT_RULE[first(dp.parameters)] : Void for dp in df.parameters[1].parameters]

    types = slot_order(T)
    num_types = length(types)
    order = Vector{Int}(num_types)
    for i = 1:num_types
        order[i] = findfirst(token_types, types[i])
    end

    field_defaults = slot_defaults(T)
    field_order = tuple(order...)
    tuple_type = slot_types(T)

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
            val_i, pos = get(nv), next_pos
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

function reorder_args{Nv, Ni}(val::NTuple{Nv}, idx::NTuple{Ni}, default::NTuple{Ni}, valid_till)
    ntuple(Val{Ni}) do i
        if idx[i] == 0 || idx[i] > valid_till
            default[i]
        else
            val[idx[i]]
        end
    end
end

function Base.tryparse{T<:TimeType}(::Type{T}, str::AbstractString, df::DateFormat)
    nt = tryparse_internal(T, str, df, false)
    Nullable{T}(isnull(nt) ? nothing : T(nt.value...))
end

function Base.parse{T<:TimeType}(::Type{T}, str::AbstractString, df::DateFormat)
    nt = tryparse_internal(T, str, df, true)
    T(nt.value...)
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
    i > min_pos || (return Nullable{Int64}(), i)
    return Nullable{Int64}(d), i
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
    return Nullable{SubString}(word_end == 0 ? nothing : SubString(str, word_start, word_end)), i
end

function minwidth(num, n)
    s = string(abs(num))
    s = length(s) < n ? lpad(s, n, 0) : s
    num < 0 ? string('-', s) : s
end

function rfixwidth(num, n)
    assert(num >= 0)
    s = string(num)
    length(s) < n ? lpad(s, n, 0) : s[end - n + 1:end]
end


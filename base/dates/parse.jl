### Parsing utilities

@generated function tryparse_internal{N}(df::DateFormat{NTuple{N}}, str::AbstractString, raise::Bool=false)
    quote
        R = Nullable{NTuple{7,Int64}}
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
        return R(reorder_args(parts, df.field_order, df.field_defaults, err_idx)::NTuple{7,Int64})

        @label error
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

@inline _create_timeobj(tup, T::Type{DateTime}) = T(tup...)
@inline _create_timeobj(tup, T::Type{Date}) = T(tup[1:3]...)

function Base.tryparse{T<:TimeType}(::Type{T}, str::AbstractString, df::DateFormat)
    R = Nullable{T}
    nt = tryparse_internal(df, str, false)
    Nullable{T}(isnull(nt) ? nothing : _create_timeobj(nt.value, T))
end

function Base.parse{T<:TimeType}(::Type{T}, str::AbstractString, df::DateFormat)
    nt = tryparse_internal(df, str, true)
    _create_timeobj(nt.value, T)
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

@inline function tryparsenext_base10(str::AbstractString, i::Int, len::Int, min_width::Int=1, max_width::Int=0)
    i > len && (return Nullable{Int}(), i)
    min_pos = min_width <= 0 ? i : i + min_width - 1
    max_pos = max_width <= 0 ? len : min(i + max_width - 1, len)
    d::Int = 0
    @inbounds while i <= max_pos
        c, j = next(str, i)
        if '0' <= c <= '9'
            d = d * 10 + (c - '0')
        else
            break
        end
        i = j
    end
    i > min_pos || (return Nullable{Int}(), i)
    return Nullable{Int}(d), i
end

@inline function tryparsenext_word(str, i, len, locale, maxchars=0)
    max_pos = maxchars <= 0 ? len : min(chr2ind(str, ind2chr(str,i) + maxchars - 1), len)
    while i <= max_pos
        c, ii = next(str, i)
        !isalpha(c) && break
        i = ii
    end
    return Nullable{Int}(0), i
end

function minwidth(num, n)
    s = string(abs(num))
    s = length(s) < n ?  lpad(s, n, 0) : s
    num < 0 ? string('-', s) : s
end

function rfixwidth(num, n)
    s = string(num)
    length(s) > n ? s[end-(n-1):end] : lpad(s, n, 0)
end


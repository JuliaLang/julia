import Base.tryparse

function DateTime(str::AbstractString)
    nd = tryparse(DateTime, str)
    isnull(nd) && throw(ParseError("Invalid DateTime string"))
    get(nd)
end
function Date(str::AbstractString)
    nd = tryparse(Date, str)
    isnull(nd) && throw(ParseError("Invalid Date string"))
    get(nd)
end



macro chk1(expr,label=:error)
    quote
        x,i = $(esc(expr))
        if isnull(x)
            @goto $label
        else
            get(x),i
        end
    end
end

function tryparse{T<:Union{Date,DateTime}}(::Type{T}, str::AbstractString)
    i = start(str)
    i = skipwhitespace(str,i)
    nd, i = tryparsenext(T, str, i)
    i = skipwhitespace(str,i)
    if !done(str,i)
        return Nullable{T}()
    else
        return nd
    end
end

@inline function skipwhitespace(str,i)
    while !done(str,i)
        c,ii = next(str,i)
        if !isspace(c)
            break
        end
        i = ii
    end
    return i
end

@inline function tryparsenext(::Type{Date},str,i)
    R = Nullable{Date}
    dy, i = @chk1 tryparsenext_base10(str,i,10)
    c,  i = @chk1 tryparsenext_char(str,i,'-')
    dm, i = @chk1 tryparsenext_base10(str,i,2)
    c,  i = @chk1 tryparsenext_char(str,i,'-')
    dd, i = @chk1 tryparsenext_base10(str,i,2)
    d = Date(dy,dm,dd)
    return R(d), i

    @label error
    return R(), i
end


@inline function tryparsenext(::Type{DateTime},str,i)
    R = Nullable{DateTime}
    dy, i = @chk1 tryparsenext_base10(str,i,10)
    c,  i = @chk1 tryparsenext_char(str,i,'-')
    dm, i = @chk1 tryparsenext_base10(str,i,2)
    c,  i = @chk1 tryparsenext_char(str,i,'-')
    dd, i = @chk1 tryparsenext_base10(str,i,2)
    c,  i = @chk1 tryparsenext_char(str,i,('T',' '))
    th, i = @chk1 tryparsenext_base10(str,i,2)
    c,  i = @chk1 tryparsenext_char(str,i,':')
    tm, i = @chk1 tryparsenext_base10(str,i,2)
    c,  i = @chk1 tryparsenext_char(str,i,':')
    ts, i = @chk1 tryparsenext_base10(str,i,2)

    nc, i = tryparsenext_char(str,i,'.')
    if isnull(nc)
        d = DateTime(dy,dm,dd,th,tm,ts)
    else
        tms,i = @chk1 tryparsenext_base10_frac(str,i,3)
        d = DateTime(dy,dm,dd,th,tm,ts,tms)
    end
    return R(d), i

    @label error
    return R(), i
end

@inline function tryparsenext_base10_digit(str,i)
    R = Nullable{Int}
    done(str,i) && @goto error
    c,ii = next(str,i)
    '0' <= c <= '9' || @goto error
    return R(c-'0'), ii

    @label error
    return R(), i
end

@inline function tryparsenext_base10(str,i,maxdig)
    R = Nullable{Int}
    r,i = @chk1 tryparsenext_base10_digit(str,i)
    for j = 2:maxdig
        d,i = @chk1 tryparsenext_base10_digit(str,i) done
        r = r*10 + d
    end
    @label done
    return R(r), i

    @label error
    return R(), i
end

@inline function tryparsenext_base10_frac(str,i,maxdig)
    R = Nullable{Int}
    r,i = @chk1 tryparsenext_base10_digit(str,i)
    local j
    for j = 2:maxdig
        d,i = @chk1 tryparsenext_base10_digit(str,i) done
        r = 10*r + d
    end
    @label done
    for k = j+1:maxdig
        r *= 10
    end
    return R(r), i

    @label error
    return R(), i
end


@inline function tryparsenext_char(str,i,cc::Char)
    R = Nullable{Char}
    done(str,i) && @goto error
    c,ii = next(str,i)
    c == cc || @goto error
    return R(c), ii

    @label error
    return R(), i
end
@inline function tryparsenext_char(str,i,CC::Tuple{Char,Char})
    R = Nullable{Char}
    done(str,i) && @goto error
    c,ii = next(str,i)
    c == CC[1] || c == CC[2] || @goto error
    return R(c), ii

    @label error
    return R(), i
end

import Base.tryparse


function DateTime(str::AbstractString,::FastDateFormat{:ISODateTime})
    nd = tryparse(DateTime, str)
    isnull(nd) && throw(ArgumentError("Invalid DateTime string"))
    get(nd)
end
function Date(str::AbstractString,::FastDateFormat{:ISODate})
    nd = tryparse(Date, str)
    isnull(nd) && throw(ArgumentError("Invalid Date string"))
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
    dm = dd = 1
    dy, i = @chk1 tryparsenext_base10(str,i,10)
    c,  i = @chk1 tryparsenext_char(str,i,'-')
    dm, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,'-') done
    dd, i = @chk1 tryparsenext_base10(str,i,2) done

    @label done
    d = Date(dy,dm,dd)
    return R(d), i

    @label error
    return R(), i
end


@inline function tryparsenext(::Type{DateTime},str,i)
    R = Nullable{DateTime}
    dm = dd = 1
    th = tm = ts = tms = 0
    dy, i = @chk1 tryparsenext_base10(str,i,10)
    c,  i = @chk1 tryparsenext_char(str,i,'-')
    dm, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,'-') done
    dd, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,'T') done
    th, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,':') done
    tm, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,':') done
    ts, i = @chk1 tryparsenext_base10(str,i,2) done
    c,  i = @chk1 tryparsenext_char(str,i,'.') done
    tms,i = @chk1 tryparsenext_base10_frac(str,i,3) done

    @label done
    d = DateTime(dy,dm,dd,th,tm,ts,tms)
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
    for j = 2:maxdig
        nd,i = tryparsenext_base10_digit(str,i)
        if isnull(nd)
            for k = j:maxdig
                r *= 10
            end
            break
        end
        d = get(nd)
        r = 10*r + d
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

# TODO: optimize this
function format(dt::DateTime, ::FastDateFormat{:ISODateTime})
    y,m,d = yearmonthday(days(dt))
    h,mi,s = hour(dt),minute(dt),second(dt)
    yy = y < 0 ? @sprintf("%05i",y) : lpad(y,4,"0")
    mm = lpad(m,2,"0")
    dd = lpad(d,2,"0")
    hh = lpad(h,2,"0")
    mii = lpad(mi,2,"0")
    ss = lpad(s,2,"0")
    ms = millisecond(dt) == 0 ? "" : string(millisecond(dt)/1000.0)[2:end]
    return "$yy-$mm-$(dd)T$hh:$mii:$ss$(ms)"
end

function format(dt::Date, ::FastDateFormat{:ISODate})
    y,m,d = yearmonthday(value(dt))
    yy = y < 0 ? @sprintf("%05i",y) : lpad(y,4,"0")
    mm = lpad(m,2,"0")
    dd = lpad(d,2,"0")
    return "$yy-$mm-$dd"
end

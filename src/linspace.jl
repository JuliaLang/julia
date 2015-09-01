module LazyLinSpace

    export linspace
    import Base: map, float, big, show, isempty, step, length, first, last, start, next, done,
        getindex, unsafe_getindex, promote_rule, convert, reverse, eltype
    import Base.Operators: +, -, .+, .-, *, .*, ==, ./

    import ..AbstractFloat
    import ..@compat

    ## linspace and logspace

    immutable LinSpace{T<:AbstractFloat} <: Range{T}
        start::T
        stop::T
        len::T
        divisor::T
    end

    # float rationalization helper
    function rat(x)
        y = x
        a = d = 1
        b = c = 0
        m = maxintfloat(Float32)
        while abs(y) <= m
            f = trunc(Int,y)
            y -= f
            a, c = f*a + c, a
            b, d = f*b + d, b
            max(abs(a),abs(b)) <= convert(Int,m) || return c, d
            oftype(x,a)/oftype(x,b) == x && break
            y = inv(y)
        end
        return a, b
    end

    function linspace{T<:AbstractFloat}(start::T, stop::T, len::T)
        len == round(len) || throw(InexactError())
        0 <= len || error("linspace($start, $stop, $len): negative length")
        if len == 0
            n = convert(T, 2)
            if isinf(n*start) || isinf(n*stop)
                start /= n; stop /= n; n = one(T)
            end
            return LinSpace(-start, -stop, -one(T), n)
        end
        if len == 1
            start == stop || error("linspace($start, $stop, $len): endpoints differ")
            return LinSpace(-start, -start, zero(T), one(T))
        end
        n = convert(T, len - 1)
        len - n == 1 || error("linspace($start, $stop, $len): too long for $T")
        a0, b = rat(start)
        a = convert(T,a0)
        if a/convert(T,b) == start
            c0, d = rat(stop)
            c = convert(T,c0)
            if c/convert(T,d) == stop
                e = lcm(b,d)
                a *= div(e,b)
                c *= div(e,d)
                s = convert(T,n*e)
                if isinf(a*n) || isinf(c*n)
                    s, p = frexp(s)
                    p2 = oftype(s,2)^p
                    a /= p2; c /= p2
                end
                if a*n/s == start && c*n/s == stop
                    return LinSpace(a, c, len, s)
                end
            end
        end
        a, c, s = start, stop, n
        if isinf(a*n) || isinf(c*n)
            s, p = frexp(s)
            p2 = oftype(s,2)^p
            a /= p2; c /= p2
        end
        if a*n/s == start && c*n/s == stop
            return LinSpace(a, c, len, s)
        end
        return LinSpace(start, stop, len, n)
    end
    function linspace{T<:AbstractFloat}(start::T, stop::T, len::Real)
        T_len = convert(T, len)
        T_len == len || throw(InexactError())
        linspace(start, stop, T_len)
    end
    linspace(start::Real, stop::Real, len::Real=50) =
        linspace(promote(AbstractFloat(start), AbstractFloat(stop))..., len)

    function show(io::IO, r::LinSpace)
        print(io, "linspace(")
        show(io, first(r))
        print(io, ',')
        show(io, last(r))
        print(io, ',')
        show(io, length(r))
        print(io, ')')
    end

    logspace(start::Real, stop::Real, n::Integer=50) = 10.^linspace(start, stop, n)

    eltype{T}(r::LinSpace{T}) = T
    isempty(r::LinSpace) = length(r) == 0
    step{T}(r::LinSpace{T}) = ifelse(r.len <= 0, convert(T,NaN), (r.stop-r.start)/r.divisor)
    length(r::LinSpace) = convert(Integer,r.len + signbit(r.len - 1))
    first{T}(r::LinSpace{T}) = convert(T, (r.len-1)*r.start/r.divisor)
    last{T}(r::LinSpace{T}) = convert(T, (r.len-1)*r.stop/r.divisor)
    start(r::LinSpace) = 1
    done(r::LinSpace, i::Int) = length(r) < i
    next{T}(r::LinSpace{T}, i::Int) =
        (convert(T, ((r.len-i)*r.start + (i-1)*r.stop)/r.divisor), i+1)

    getindex{T}(r::LinSpace{T}, i::Int) = (checkbounds(r, i); unsafe_getindex(r, i))
    getindex{T}(r::LinSpace{T}, i::Integer) = (checkbounds(r, i); unsafe_getindex(r, convert(Int,i)))
    unsafe_getindex{T}(r::LinSpace{T}, i::Int) = convert(T, ((r.len-i)*r.start + (i-1)*r.stop)/r.divisor)

    getindex(r::LinSpace, s::OrdinalRange) = (checkbounds(r, s); unsafe_getindex(r, s))
    function unsafe_getindex{T}(r::LinSpace{T}, s::OrdinalRange)
        sl::T = length(s)
        ifirst = first(s)
        ilast = last(s)
        vfirst::T = ((r.len - ifirst) * r.start + (ifirst - 1) * r.stop) / r.divisor
        vlast::T = ((r.len - ilast) * r.start + (ilast - 1) * r.stop) / r.divisor
        return linspace(vfirst, vlast, sl)
    end

    function map{T<:AbstractFloat}(::Type{T}, r::LinSpace)
        new_len = T(r.len)
        new_len == r.len || error("$r: too long for $T")
        LinSpace(T(r.start), T(r.stop), new_len, T(r.divisor))
    end

    for fn in (:float,:big)
        @eval function $fn(r::LinSpace)
            new_len = $fn(r.len)
            new_len == r.len || error(string(r, ": too long for ", $fn))
            LinSpace($fn(r.start), $fn(r.stop), new_len, $fn(r.divisor))
        end
    end

    for fn in (:+, :-)
        @eval function $fn{T<:AbstractFloat}(r1::LinSpace{T}, r2::LinSpace{T})
                len = r1.len
                (len == r2.len ||
                 throw(DimensionMismatch("argument dimensions must match")))
                divisor1, divisor2 = r1.divisor, r2.divisor
                if divisor1 == divisor2
                    LinSpace{T}($fn(r1.start, r2.start), $fn(r1.stop, r2.stop),
                                len, divisor1)
                else
                    linspace(convert(T, $fn(first(r1), first(r2))),
                             convert(T, $fn(last(r1), last(r2))), len)
                end
            end

        @eval $fn(r1::(@compat Union{FloatRange, OrdinalRange, LinSpace}),
               r2::(@compat Union{FloatRange, OrdinalRange, LinSpace})) =
                   $fn(promote(r1, r2)...)
    end

    (==){T<:LinSpace}(r::T, s::T) = (first(r) == first(s)) & (length(r) == length(s)) & (last(r) == last(s))
    -(r::LinSpace)     = LinSpace(-r.start, -r.stop, r.len, r.divisor)

    function .+{T}(x::Real, r::LinSpace{T})
        x2 = x * r.divisor / (r.len - 1)
        LinSpace(x2 + r.start, x2 + r.stop, r.len, r.divisor)
    end

    function .-(x::Real, r::LinSpace)
        x2 = x * r.divisor / (r.len - 1)
        LinSpace(x2 - r.start, x2 - r.stop, r.len, r.divisor)
    end

    function .-(r::LinSpace, x::Real)
        x2 = x * r.divisor / (r.len - 1)
        LinSpace(r.start - x2, r.stop - x2, r.len, r.divisor)
    end

    .*(x::Real, r::LinSpace)     = LinSpace(x * r.start, x * r.stop, r.len, r.divisor)
    .*(r::LinSpace, x::Real)     = x .* r
    ./(r::LinSpace, x::Real)     = LinSpace(r.start / x, r.stop / x, r.len, r.divisor)

    promote_rule{T1,T2}(::Type{LinSpace{T1}},::Type{LinSpace{T2}}) =
        LinSpace{promote_type(T1,T2)}
    convert{T}(::Type{LinSpace{T}}, r::LinSpace{T}) = r
    convert{T}(::Type{LinSpace{T}}, r::LinSpace) =
        LinSpace{T}(r.start, r.stop, r.len, r.divisor)

    promote_rule{F,OR<:OrdinalRange}(::Type{LinSpace{F}}, ::Type{OR}) =
        LinSpace{promote_type(F,eltype(OR))}
    convert{T}(::Type{LinSpace{T}}, r::OrdinalRange) =
        linspace(convert(T, first(r)), convert(T, last(r)), convert(T, length(r)))
    convert{T}(::Type{LinSpace}, r::OrdinalRange{T}) =
        convert(LinSpace{typeof(float(first(r)))}, r)

    # Promote FloatRange to LinSpace
    promote_rule{F,OR<:FloatRange}(::Type{LinSpace{F}}, ::Type{OR}) =
        LinSpace{promote_type(F,eltype(OR))}
    convert{T}(::Type{LinSpace{T}}, r::FloatRange) =
        linspace(convert(T, first(r)), convert(T, last(r)), convert(T, length(r)))
    convert{T}(::Type{LinSpace}, r::FloatRange{T}) =
        convert(LinSpace{T}, r)

    reverse(r::LinSpace)     = LinSpace(r.stop, r.start, r.len, r.divisor)

end

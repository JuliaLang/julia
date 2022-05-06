# This file is a part of Julia. License is MIT: https://julialang.org/license

module Printf

using Base.Ryu

export @printf, @sprintf

# format specifier categories
const Ints = Union{Val{'d'}, Val{'i'}, Val{'u'}, Val{'x'}, Val{'X'}, Val{'o'}}
const Floats = Union{Val{'e'}, Val{'E'}, Val{'f'}, Val{'F'}, Val{'g'}, Val{'G'}, Val{'a'}, Val{'A'}}
const Chars = Union{Val{'c'}, Val{'C'}}
const Strings = Union{Val{'s'}, Val{'S'}}
const Pointer = Val{'p'}
const HexBases = Union{Val{'x'}, Val{'X'}, Val{'a'}, Val{'A'}}
const PositionCounter = Val{'n'}

"""
Typed representation of a format specifier.

`T` is a `Val{'_'}`, where `_` is a valid format specifier character.

Fields are the various modifiers allowed for various format specifiers.
"""
struct Spec{T} # T => %type => Val{'type'}
    leftalign::Bool
    plus::Bool
    space::Bool
    zero::Bool
    hash::Bool
    width::Int
    precision::Int
end

# recreate the format specifier string from a typed Spec
Base.string(f::Spec{T}; modifier::String="") where {T} =
    string("%", f.leftalign ? "-" : "", f.plus ? "+" : "", f.space ? " " : "",
        f.zero ? "0" : "", f.hash ? "#" : "", f.width > 0 ? f.width : "",
        f.precision == 0 ? ".0" : f.precision > 0 ? ".$(f.precision)" : "", modifier, char(T))
Base.show(io::IO, f::Spec) = print(io, string(f))

floatfmt(s::Spec{T}) where {T} =
    Spec{Val{'f'}}(s.leftalign, s.plus, s.space, s.zero, s.hash, s.width, 0)
ptrfmt(s::Spec{T}, x) where {T} =
    Spec{Val{'x'}}(s.leftalign, s.plus, s.space, s.zero, true, s.width, sizeof(x) == 8 ? 16 : 8)

"""
    Printf.Format(format_str)

Create a C printf-compatible format object that can be used for formatting values.

The input `format_str` can include any valid format specifier character and modifiers.

A `Format` object can be passed to `Printf.format(f::Format, args...)` to produce a
formatted string, or `Printf.format(io::IO, f::Format, args...)` to print the
formatted string directly to `io`.

For convenience, the `Printf.format"..."` string macro form can be used for building
a `Printf.Format` object at macro-expansion-time.

!!! compat "Julia 1.6"
    `Printf.Format` requires Julia 1.6 or later.
"""
struct Format{S, T}
    str::S # original full format string as CodeUnits
    # keep track of non-format specifier strings to print
    # length(substringranges) == length(formats) + 1
    # so when printing, we start with printing
      # str[substringranges[1]], then formats[1] + args[1]
      # then str[substringranges[2]], then formats[2] + args[2]
      # and so on, then at the end, str[substringranges[end]]
    substringranges::Vector{UnitRange{Int}}
    formats::T # Tuple of Specs
end

# what number base should be used for a given format specifier?
base(T) = T <: HexBases ? 16 : T <: Val{'o'} ? 8 : 10
char(::Type{Val{c}}) where {c} = c

# parse format string
function Format(f::AbstractString)
    isempty(f) && throw(ArgumentError("empty format string"))
    bytes = codeunits(f)
    len = length(bytes)
    pos = 1
    b = 0x00
    while pos <= len
        b = bytes[pos]
        pos += 1
        if b == UInt8('%')
            pos > len && throw(ArgumentError("invalid format string: '$f'"))
            if bytes[pos] == UInt8('%')
                # escaped '%'
                b = bytes[pos]
                pos += 1
            else
                break
            end
        end
    end
    strs = [1:pos - 1 - (b == UInt8('%'))]
    fmts = []
    while pos <= len
        b = bytes[pos]
        pos += 1
        # positioned at start of first format str %
        # parse flags
        leftalign = plus = space = zero = hash = false
        while true
            if b == UInt8('-')
                leftalign = true
            elseif b == UInt8('+')
                plus = true
            elseif b == UInt8(' ')
                space = true
            elseif b == UInt8('0')
                zero = true
            elseif b == UInt8('#')
                hash = true
            else
                break
            end
            pos > len && throw(ArgumentError("incomplete format string: '$f'"))
            b = bytes[pos]
            pos += 1
        end
        if leftalign
            zero = false
        end
        # parse width
        width = 0
        while b - UInt8('0') < 0x0a
            width = 10 * width + (b - UInt8('0'))
            b = bytes[pos]
            pos += 1
            pos > len && break
        end
        # parse precision
        precision = 0
        parsedprecdigits = false
        if b == UInt8('.')
            pos > len && throw(ArgumentError("incomplete format string: '$f'"))
            parsedprecdigits = true
            b = bytes[pos]
            pos += 1
            if pos <= len
                while b - UInt8('0') < 0x0a
                    precision = 10precision + (b - UInt8('0'))
                    b = bytes[pos]
                    pos += 1
                    pos > len && break
                end
            end
        end
        # parse length modifier (ignored)
        if b == UInt8('h') || b == UInt8('l')
            prev = b
            b = bytes[pos]
            pos += 1
            if b == prev
                pos > len && throw(ArgumentError("invalid format string: '$f'"))
                b = bytes[pos]
                pos += 1
            end
        elseif b in b"Ljqtz"
            b = bytes[pos]
            pos += 1
        end
        # parse type
        !(b in b"diouxXDOUeEfFgGaAcCsSpn") && throw(ArgumentError("invalid format string: '$f', invalid type specifier: '$(Char(b))'"))
        type = Val{Char(b)}
        if type <: Ints && precision > 0
            zero = false
        elseif (type <: Strings || type <: Chars) && !parsedprecdigits
            precision = -1
        elseif type <: Union{Val{'a'}, Val{'A'}} && !parsedprecdigits
            precision = -1
        elseif type <: Floats && !parsedprecdigits
            precision = 6
        end
        push!(fmts, Spec{type}(leftalign, plus, space, zero, hash, width, precision))
        start = pos
        while pos <= len
            b = bytes[pos]
            pos += 1
            if b == UInt8('%')
                pos > len && throw(ArgumentError("invalid format string: '$f'"))
                if bytes[pos] == UInt8('%')
                    # escaped '%'
                    b = bytes[pos]
                    pos += 1
                else
                    break
                end
            end
        end
        push!(strs, start:pos - 1 - (b == UInt8('%')))
    end
    return Format(bytes, strs, Tuple(fmts))
end

macro format_str(str)
    Format(str)
end

const hex = b"0123456789abcdef"
const HEX = b"0123456789ABCDEF"

# write out a single arg according to format options
# char
@inline function writechar(buf, pos, c)
    u = bswap(reinterpret(UInt32, c))
    while true
        buf[pos] = u % UInt8
        pos += 1
        (u >>= 8) == 0 && break
    end
    return pos
end

@inline function fmt(buf, pos, arg, spec::Spec{T}) where {T <: Chars}
    leftalign, width = spec.leftalign, spec.width
    c = Char(first(arg))
    w = textwidth(c)
    if !leftalign && width > w
        for _ = 1:(width - w)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    pos = writechar(buf, pos, c)
    if leftalign && width > w
        for _ = 1:(width - w)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    return pos
end

# strings
@inline function fmt(buf, pos, arg, spec::Spec{T}) where {T <: Strings}
    leftalign, hash, width, prec = spec.leftalign, spec.hash, spec.width, spec.precision
    str = string(arg)
    slen = textwidth(str) + (hash ? arg isa AbstractString ? 2 : 1 : 0)
    op = p = prec == -1 ? slen : min(slen, prec)
    if !leftalign && width > p
        for _ = 1:(width - p)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    if hash
        if arg isa Symbol
            buf[pos] = UInt8(':')
            pos += 1
            p -= 1
        elseif arg isa AbstractString
            buf[pos] = UInt8('"')
            pos += 1
            p -= 1
        end
    end
    for c in str
        p -= textwidth(c)
        p < 0 && break
        pos = writechar(buf, pos, c)
    end
    if hash && arg isa AbstractString && p > 0
        buf[pos] = UInt8('"')
        pos += 1
    end
    if leftalign && width > op
        for _ = 1:(width - op)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    return pos
end

# integers
toint(x) = x
toint(x::Rational) = Integer(x)

fmt(buf, pos, arg::AbstractFloat, spec::Spec{T}) where {T <: Ints} =
    fmt(buf, pos, arg, floatfmt(spec))

@inline function fmt(buf, pos, arg, spec::Spec{T}) where {T <: Ints}
    leftalign, plus, space, zero, hash, width, prec =
        spec.leftalign, spec.plus, spec.space, spec.zero, spec.hash, spec.width, spec.precision
    bs = base(T)
    arg2 = toint(arg)
    n = i = ndigits(arg2, base=bs, pad=1)
    neg = arg2 < 0
    x = arg2 isa Base.BitSigned ? unsigned(abs(arg2)) : abs(arg2)
    arglen = n + (neg || (plus | space)) +
        (T == Val{'o'} && hash ? 1 : 0) +
        (T == Val{'x'} && hash ? 2 : 0) + (T == Val{'X'} && hash ? 2 : 0)
    arglen2 = arglen < width && prec > 0 ? arglen + min(max(0, prec - n), width - arglen) : arglen
    if !leftalign && !zero && arglen2 < width
        # pad left w/ spaces
        for _ = 1:(width - arglen2)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    if neg
        buf[pos] = UInt8('-'); pos += 1
    elseif plus # plus overrides space
        buf[pos] = UInt8('+'); pos += 1
    elseif space
        buf[pos] = UInt8(' '); pos += 1
    end
    if T == Val{'o'} && hash
        buf[pos] = UInt8('0')
        pos += 1
    elseif T == Val{'x'} && hash
        buf[pos] = UInt8('0')
        buf[pos + 1] = UInt8('x')
        pos += 2
    elseif T == Val{'X'} && hash
        buf[pos] = UInt8('0')
        buf[pos + 1] = UInt8('X')
        pos += 2
    end
    if zero && arglen2 < width
        for _ = 1:(width - arglen2)
            buf[pos] = UInt8('0')
            pos += 1
        end
    elseif n < prec
        for _ = 1:(prec - n)
            buf[pos] = UInt8('0')
            pos += 1
        end
    elseif arglen < arglen2
        for _ = 1:(arglen2 - arglen)
            buf[pos] = UInt8('0')
            pos += 1
        end
    end
    while i > 0
        @inbounds buf[pos + i - 1] = bs == 16 ?
            (T == Val{'x'} ? hex[(x & 0x0f) + 1] : HEX[(x & 0x0f) + 1]) :
            (48 + (bs == 8 ? (x & 0x07) : rem(x, 10)))
        if bs == 8
            x >>= 3
        elseif bs == 16
            x >>= 4
        else
            x = oftype(x, div(x, 10))
        end
        i -= 1
    end
    pos += n
    if leftalign && arglen2 < width
        # pad right
        for _ = 1:(width - arglen2)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    return pos
end

# floats
"""
    Printf.tofloat(x)

Convert an argument to a Base float type for printf formatting.
By default, arguments are converted to `Float64` via `Float64(x)`.
Custom numeric types that have a conversion to a Base float type
that wish to hook into printf formatting can extend this method like:

```julia
Printf.tofloat(x::MyCustomType) = convert_my_custom_type_to_float(x)
```

For arbitrary precision numerics, you might extend the method like:

```julia
Printf.tofloat(x::MyArbitraryPrecisionType) = BigFloat(x)
```

!!! compat "Julia 1.6"
    This function requires Julia 1.6 or later.
"""
tofloat(x) = Float64(x)
tofloat(x::Base.IEEEFloat) = x
tofloat(x::BigFloat) = x

_snprintf(ptr, siz, str, arg) =
    @ccall "libmpfr".mpfr_snprintf(ptr::Ptr{UInt8}, siz::Csize_t, str::Ptr{UInt8};
                                   arg::Ref{BigFloat})::Cint

const __BIG_FLOAT_MAX__ = 8192

@inline function fmt(buf, pos, arg, spec::Spec{T}) where {T <: Floats}
    leftalign, plus, space, zero, hash, width, prec =
        spec.leftalign, spec.plus, spec.space, spec.zero, spec.hash, spec.width, spec.precision
    x = tofloat(arg)
    if x isa BigFloat
        if isfinite(x)
            GC.@preserve buf begin
                siz = length(buf) - pos + 1
                str = string(spec; modifier="R")
                len = _snprintf(pointer(buf, pos), siz, str, x)
                if len > siz
                    maxout = max(__BIG_FLOAT_MAX__,
                                 ceil(Int, precision(x) * log(2) / log(10)) + 25)
                    len > maxout &&
                        error("Over $maxout bytes $len needed to output BigFloat $x")
                    resize!(buf, len + 1)
                    len = _snprintf(pointer(buf, pos), len + 1, str, x)
                end
                len > 0 || throw(ArgumentError("invalid printf formatting $str for BigFloat"))
                return pos + len
            end
        end
        x = Float64(x)
    end
    if T == Val{'e'} || T == Val{'E'}
        newpos = Ryu.writeexp(buf, pos, x, prec, plus, space, hash, char(T), UInt8('.'))
    elseif T == Val{'f'} || T == Val{'F'}
        newpos = Ryu.writefixed(buf, pos, x, prec, plus, space, hash, UInt8('.'))
    elseif T == Val{'g'} || T == Val{'G'}
        if isinf(x) || isnan(x)
            newpos = Ryu.writeshortest(buf, pos, x, plus, space)
        else
            # C11-compliant general format
            prec = prec == 0 ? 1 : prec
            # format the value in scientific notation and parse the exponent part
            exp = let p = Ryu.writeexp(buf, pos, x, prec)
                b1, b2, b3, b4 = buf[p-4], buf[p-3], buf[p-2], buf[p-1]
                Z = UInt8('0')
                if b1 == UInt8('e')
                    # two-digit exponent
                    sign = b2 == UInt8('+') ? 1 : -1
                    exp = 10 * (b3 - Z) + (b4 - Z)
                else
                    # three-digit exponent
                    sign = b1 == UInt8('+') ? 1 : -1
                    exp = 100 * (b2 - Z) + 10 * (b3 - Z) + (b4 - Z)
                end
                flipsign(exp, sign)
            end
            if -4 ≤ exp < prec
                newpos = Ryu.writefixed(buf, pos, x, prec - (exp + 1), plus, space, hash, UInt8('.'), !hash)
            else
                newpos = Ryu.writeexp(buf, pos, x, prec - 1, plus, space, hash, T == Val{'g'} ? UInt8('e') : UInt8('E'), UInt8('.'), !hash)
            end
        end
    elseif T == Val{'a'} || T == Val{'A'}
        x, neg = x < 0 || x === -Base.zero(x) ? (-x, true) : (x, false)
        newpos = pos
        if neg
            buf[newpos] = UInt8('-')
            newpos += 1
        elseif plus
            buf[newpos] = UInt8('+')
            newpos += 1
        elseif space
            buf[newpos] = UInt8(' ')
            newpos += 1
        end
        if isnan(x)
            buf[newpos] = UInt8('N')
            buf[newpos + 1] = UInt8('a')
            buf[newpos + 2] = UInt8('N')
            newpos += 3
        elseif !isfinite(x)
            buf[newpos] = UInt8('I')
            buf[newpos + 1] = UInt8('n')
            buf[newpos + 2] = UInt8('f')
            newpos += 3
        else
            buf[newpos] = UInt8('0')
            newpos += 1
            buf[newpos] = T <: Val{'a'} ? UInt8('x') : UInt8('X')
            newpos += 1
            if arg == 0
                buf[newpos] = UInt8('0')
                newpos += 1
                if prec > 0
                    buf[newpos] = UInt8('.')
                    newpos += 1
                    while prec > 0
                        buf[newpos] = UInt8('0')
                        newpos += 1
                        prec -= 1
                    end
                end
                buf[newpos] = T <: Val{'a'} ? UInt8('p') : UInt8('P')
                buf[newpos + 1] = UInt8('+')
                buf[newpos + 2] = UInt8('0')
                newpos += 3
            else
                if prec > -1
                    s, p = frexp(x)
                    sigbits = 4 * min(prec, 13)
                    s = 0.25 * round(ldexp(s, 1 + sigbits))
                    # ensure last 2 exponent bits either 01 or 10
                    u = (reinterpret(UInt64, s) & 0x003f_ffff_ffff_ffff) >> (52 - sigbits)
                    i = n = (sizeof(u) << 1) - (leading_zeros(u) >> 2)
                else
                    s, p = frexp(x)
                    s *= 2.0
                    u = (reinterpret(UInt64, s) & 0x001f_ffff_ffff_ffff)
                    t = (trailing_zeros(u) >> 2)
                    u >>= (t << 2)
                    i = n = 14 - t
                end
                frac = u > 9 || hash || prec > 0
                while i > 1
                    buf[newpos + i] = T == Val{'a'} ? hex[(u & 0x0f) + 1] : HEX[(u & 0x0f) + 1]
                    u >>= 4
                    i -= 1
                    prec -= 1
                end
                if frac
                    buf[newpos + 1] = UInt8('.')
                end
                buf[newpos] = T == Val{'a'} ? hex[(u & 0x0f) + 1] : HEX[(u & 0x0f) + 1]
                newpos += n + frac
                while prec > 0
                    buf[newpos] = UInt8('0')
                    newpos += 1
                    prec -= 1
                end
                buf[newpos] = T <: Val{'a'} ? UInt8('p') : UInt8('P')
                newpos += 1
                p -= 1
                buf[newpos] = p < 0 ? UInt8('-') : UInt8('+')
                p = p < 0 ? -p : p
                newpos += 1
                n = i = ndigits(p, base=10, pad=1)
                while i > 0
                    buf[newpos + i - 1] = 48 + rem(p, 10)
                    p = oftype(p, div(p, 10))
                    i -= 1
                end
                newpos += n
            end
        end
    end
    if newpos - pos < width
        # need to pad
        if leftalign
            # easy case, just pad spaces after number
            for _ = 1:(width - (newpos - pos))
                buf[newpos] = UInt8(' ')
                newpos += 1
            end
        else
            # right aligned
            n = width - (newpos - pos)
            if zero
                ex = (arg < 0 || (plus | space)) + (T <: Union{Val{'a'}, Val{'A'}} ? 2 : 0)
                so = pos + ex
                len = (newpos - pos) - ex
                copyto!(buf, so + n, buf, so, len)
                for i = so:(so + n - 1)
                    buf[i] = UInt8('0')
                end
                newpos += n
            else
                copyto!(buf, pos + n, buf, pos, newpos - pos)
                for i = pos:(pos + n - 1)
                    buf[i] = UInt8(' ')
                end
                newpos += n
            end
        end
    end
    return newpos
end

# pointers
fmt(buf, pos, arg, spec::Spec{Pointer}) = fmt(buf, pos, UInt64(arg), ptrfmt(spec, arg))

# position counters
function fmt(buf, pos, arg::Ref{<:Integer}, ::Spec{PositionCounter})
    arg[] = pos - 1
    pos
end

# old Printf compat
function fix_dec end
function ini_dec end

# generic fallback
function fmtfallback(buf, pos, arg, spec::Spec{T}) where {T}
    leftalign, plus, space, zero, hash, width, prec =
        spec.leftalign, spec.plus, spec.space, spec.zero, spec.hash, spec.width, spec.precision
    buf2 = Base.StringVector(309 + 17 + 5)
    ise = T <: Union{Val{'e'}, Val{'E'}}
    isg = T <: Union{Val{'g'}, Val{'G'}}
    isf = T <: Val{'f'}
    if isg
        prec = prec == 0 ? 1 : prec
        arg = round(arg, sigdigits=prec)
    end
    n, pt, neg = isf ? fix_dec(arg, prec, buf2) : ini_dec(arg, min(prec + ise, length(buf2) - 1), buf2)
    if isg && !hash
        while buf2[n] == UInt8('0')
            n -= 1
        end
    end
    expform = ise || (isg && !(-4 < pt <= prec))
    n2 = n + (expform ? 4 : 0) + (prec > 0 || hash) + (neg || (plus | space)) +
        (isf && pt >= n ? prec + 1 : 0)
    if !leftalign && !zero && n2 < width
        # pad left w/ spaces
        for _ = 1:(width - n2)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    if neg
        buf[pos] = UInt8('-'); pos += 1
    elseif plus # plus overrides space
        buf[pos] = UInt8('+'); pos += 1
    elseif space
        buf[pos] = UInt8(' '); pos += 1
    end
    if zero && n2 < width
        for _ = 1:(width - n2)
            buf[pos] = UInt8('0')
            pos += 1
        end
    end
    if expform
        buf[pos] = buf2[1]
        pos += 1
        if n > 1 || hash
            buf[pos] = UInt8('.')
            pos += 1
            for i = 2:n
                buf[pos] = buf2[i]
                pos += 1
            end
        end
        buf[pos] = T <: Val{'e'} || T <: Val{'g'} ? UInt8('e') : UInt8('E')
        pos += 1
        exp = pt - 1
        buf[pos] = exp < 0 ? UInt8('-') : UInt8('+')
        pos += 1
        exp = abs(exp)
        if exp < 10
            buf[pos] = UInt8('0')
            buf[pos + 1] = 48 + exp
            pos += 2
        else
            buf[pos] = 48 + div(exp, 10)
            buf[pos + 1] = 48 + rem(exp, 10)
            pos += 2
        end
    elseif pt <= 0
        buf[pos] = UInt8('0')
        buf[pos + 1] = UInt8('.')
        pos += 2
        while pt < 0
            buf[pos] = UInt8('0')
            pos += 1
            pt += 1
        end
        for i = 1:n
            buf[pos] = buf2[i]
            pos += 1
        end
    elseif pt >= n
        for i = 1:n
            buf[pos] = buf2[i]
            pos += 1
        end
        while pt > n
            buf[pos] = UInt8('0')
            pos += 1
            n += 1
        end
        if hash || (isf && prec > 0)
            buf[pos] = UInt8('.')
            pos += 1
            while prec > 0
                buf[pos] = UInt8('0')
                pos += 1
                prec -= 1
            end
        end
    else
        for i = 1:pt
            buf[pos] = buf2[i]
            pos += 1
        end
        buf[pos] = UInt8('.')
        pos += 1
        for i = pt+1:n
            buf[pos] = buf2[i]
            pos += 1
        end
    end

    if leftalign && n2 < width
        # pad right
        for _ = 1:(width - n2)
            buf[pos] = UInt8(' ')
            pos += 1
        end
    end
    return pos
end

const UNROLL_UPTO = 16
# if you have your own buffer + pos, write formatted args directly to it
@inline function format(buf::Vector{UInt8}, pos::Integer, f::Format, args...)
    # write out first substring
    escapechar = false
    for i in f.substringranges[1]
        b = f.str[i]
        if !escapechar
            buf[pos] = b
            pos += 1
            escapechar = b === UInt8('%')
        else
            escapechar = false
        end
    end
    # for each format, write out arg and next substring
    # unroll up to 16 formats
    N = length(f.formats)
    Base.@nexprs 16 i -> begin
        if N >= i
            pos = fmt(buf, pos, args[i], f.formats[i])
            for j in f.substringranges[i + 1]
                b = f.str[j]
                if !escapechar
                    buf[pos] = b
                    pos += 1
                    escapechar = b === UInt8('%')
                else
                    escapechar = false
                end
            end
        end
    end
    if N > 16
        for i = 17:length(f.formats)
            pos = fmt(buf, pos, args[i], f.formats[i])
            for j in f.substringranges[i + 1]
                b = f.str[j]
                if !escapechar
                    buf[pos] = b
                    pos += 1
                    escapechar = b === UInt8('%')
                else
                    escapechar = false
                end
            end
        end
    end
    return pos
end

function plength(f::Spec{T}, x) where {T <: Chars}
    c = Char(first(x))
    w = textwidth(c)
    return max(f.width, w) + (ncodeunits(c) - w)
end
plength(f::Spec{Pointer}, x) = max(f.width, 2 * sizeof(x) + 2)

function plength(f::Spec{T}, x) where {T <: Strings}
    str = string(x)
    sw = textwidth(str)
    p = f.precision == -1 ? (sw + (f.hash ? (x isa Symbol ? 1 : 2) : 0)) : f.precision
    return max(f.width, p) + (sizeof(str) - sw)
end

function plength(f::Spec{T}, x) where {T <: Ints}
    x2 = toint(x)
    return max(f.width, f.precision + ndigits(x2, base=base(T), pad=1) + 5)
end

plength(f::Spec{T}, x::AbstractFloat) where {T <: Ints} =
    max(f.width, 0 + 309 + 17 + f.hash + 5)
plength(f::Spec{T}, x) where {T <: Floats} =
    max(f.width, f.precision + 309 + 17 + f.hash + 5)
plength(::Spec{PositionCounter}, x) = 0

@inline function computelen(substringranges, formats, args)
    len = sum(length, substringranges)
    N = length(formats)
    # unroll up to 16 formats
    Base.@nexprs 16 i -> begin
        if N >= i
            len += plength(formats[i], args[i])
        end
    end
    if N > 16
        for i = 17:length(formats)
            len += plength(formats[i], args[i])
        end
    end
    return len
end

@noinline argmismatch(a, b) =
    throw(ArgumentError("mismatch between # of format specifiers and provided args: $a != $b"))

"""
    Printf.format(f::Printf.Format, args...) => String
    Printf.format(io::IO, f::Printf.Format, args...)

Apply a printf format object `f` to provided `args` and return the formatted string
(1st method), or print directly to an `io` object (2nd method). See [`@printf`](@ref)
for more details on C `printf` support.
"""
function format end

function format(io::IO, f::Format, args...) # => Nothing
    length(f.formats) == length(args) || argmismatch(length(f.formats), length(args))
    buf = Base.StringVector(computelen(f.substringranges, f.formats, args))
    pos = format(buf, 1, f, args...)
    write(io, resize!(buf, pos - 1))
    return
end

function format(f::Format, args...) # => String
    length(f.formats) == length(args) || argmismatch(length(f.formats), length(args))
    buf = Base.StringVector(computelen(f.substringranges, f.formats, args))
    pos = format(buf, 1, f, args...)
    return String(resize!(buf, pos - 1))
end

"""
    @printf([io::IO], "%Fmt", args...)

Print `args` using C `printf` style format specification string.
Optionally, an `IO` may be passed as the first argument to redirect output.

# Examples
```jldoctest
julia> @printf "Hello %s" "world"
Hello world

julia> @printf "Scientific notation %e" 1.234
Scientific notation 1.234000e+00

julia> @printf "Scientific notation three digits %.3e" 1.23456
Scientific notation three digits 1.235e+00

julia> @printf "Decimal two digits %.2f" 1.23456
Decimal two digits 1.23

julia> @printf "Padded to length 5 %5i" 123
Padded to length 5   123

julia> @printf "Padded with zeros to length 6 %06i" 123
Padded with zeros to length 6 000123

julia> @printf "Use shorter of decimal or scientific %g %g" 1.23 12300000.0
Use shorter of decimal or scientific 1.23 1.23e+07
```

For a systematic specification of the format, see [here](https://www.cplusplus.com/reference/cstdio/printf/).
See also [`@sprintf`](@ref).

# Caveats
`Inf` and `NaN` are printed consistently as `Inf` and `NaN` for flags `%a`, `%A`,
`%e`, `%E`, `%f`, `%F`, `%g`, and `%G`. Furthermore, if a floating point number is
equally close to the numeric values of two possible output strings, the output
string further away from zero is chosen.

# Examples
```jldoctest
julia> @printf("%f %F %f %F", Inf, Inf, NaN, NaN)
Inf Inf NaN NaN

julia> @printf "%.0f %.1f %f" 0.5 0.025 -0.0078125
0 0.0 -0.007812
```

!!! compat "Julia 1.8"
    Starting in Julia 1.8, `%s` (string) and `%c` (character) widths are computed
    using [`textwidth`](@ref), which e.g. ignores zero-width characters
    (such as combining characters for diacritical marks) and treats certain
    "wide" characters (e.g. emoji) as width `2`.
"""
macro printf(io_or_fmt, args...)
    if io_or_fmt isa String
        fmt = Format(io_or_fmt)
        return esc(:($Printf.format(stdout, $fmt, $(args...))))
    else
        io = io_or_fmt
        isempty(args) && throw(ArgumentError("must provide required format string"))
        fmt = Format(args[1])
        return esc(:($Printf.format($io, $fmt, $(Base.tail(args)...))))
    end
end

"""
    @sprintf("%Fmt", args...)

Return [`@printf`](@ref) formatted output as string.

# Examples
```jldoctest
julia> @sprintf "this is a %s %15.1f" "test" 34.567
"this is a test            34.6"
```
"""
macro sprintf(fmt, args...)
    f = Format(fmt)
    return esc(:($Printf.format($f, $(args...))))
end

end # module

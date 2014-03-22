module Printf
using Base.Grisu
export @printf, @sprintf

### printf formatter generation ###

function gen(s::String)
    args = {}
    blk = Expr(:block, :(local neg, pt, len, exp))
    for x in parse(s)
        if isa(x,String)
            push!(blk.args, :(write(out, $(length(x)==1 ? x[1] : x))))
        else
            c = lowercase(x[end])
            f = c=='f' ? gen_f :
                c=='e' ? gen_e :
                c=='g' ? gen_g :
                c=='c' ? gen_c :
                c=='s' ? gen_s :
                c=='p' ? gen_p :
                         gen_d
            arg, ex = f(x...)
            push!(args, arg)
            push!(blk.args, ex)
        end
    end
    push!(blk.args, :nothing)
    return args, blk
end

### printf format string parsing ###

function parse(s::String)
    # parse format string in to stings and format tuples
    list = {}
    i = j = start(s)
    while !done(s,j)
        c, k = next(s,j)
        if c == '%'
            isempty(s[i:j-1]) || push!(list, s[i:j-1])
            flags, width, precision, conversion, k = parse1(s,k)
            '\'' in flags && error("printf format flag ' not yet supported")
            conversion == 'a'    && error("printf feature %a not yet supported")
            conversion == 'n'    && error("printf feature %n not supported")
            push!(list, conversion == '%' ? "%" : (flags,width,precision,conversion))
            i = j = k
        else
            j = k
        end
    end
    isempty(s[i:end]) || push!(list, s[i:end])
    # coalesce adjacent strings
    i = 1
    while i < length(list)
        if isa(list[i],String)
            for j = i+1:length(list)
                if !isa(list[j],String)
                    j -= 1
                    break
                end
                list[i] *= list[j]
            end
            splice!(list,i+1:j)
        end
        i += 1
    end
    return list
end

## parse a single printf specifier ##

# printf specifiers:
#   %                       # start
#   (\d+\$)?                # arg (not supported)
#   [\-\+#0' ]*             # flags
#   (\d+)?                  # width
#   (\.\d*)?                # precision
#   (h|hh|l|ll|L|j|t|z|q)?  # modifier (ignored)
#   [diouxXeEfFgGaAcCsSp%]  # conversion

next_or_die(s::String, k) = !done(s,k) ? next(s,k) :
    error("invalid printf format string: ", repr(s))

function parse1(s::String, k::Integer)
    j = k
    width = 0
    precision = -1
    c, k = next_or_die(s,k)
    # handle %%
    if c == '%'
        return "", width, precision, c, k
    end
    # parse flags
    while c in "#0- + '"
        c, k = next_or_die(s,k)
    end
    flags = ascii(s[j:k-2])
    # parse width
    while '0' <= c <= '9'
        width = 10*width + c-'0'
        c, k = next_or_die(s,k)
    end
    # parse precision
    if c == '.'
        c, k = next_or_die(s,k)
        if '0' <= c <= '9'
            precision = 0
            while '0' <= c <= '9'
                precision = 10*precision + c-'0'
                c, k = next_or_die(s,k)
            end
        end
    end
    # parse length modifer (ignored)
    if c == 'h' || c == 'l'
        prev = c
        c, k = next_or_die(s,k)
        if c == prev
            c, k = next_or_die(s,k)
        end
    elseif c in "Ljqtz"
        c, k = next_or_die(s,k)
    end
    # validate conversion
    if !(c in "diouxXDOUeEfFgGaAcCsSpn")
        error("invalid printf format string: ", repr(s))
    end
    # TODO: warn about silly flag/conversion combinations
    flags, width, precision, c, k
end

### printf formatter generation ###

function special_handler(flags::ASCIIString, width::Int)
    @gensym x
    blk = Expr(:block)
    pad = '-' in flags ? rpad : lpad
    pos = '+' in flags ? "+" :
          ' ' in flags ? " " : ""
    abn = quote
        isnan($x) ? $(pad("NaN", width)) :
         $x < 0   ? $(pad("-Inf", width)) :
                    $(pad("$(pos)Inf", width))
    end
    ex = :(isfinite($x) ? $blk : write(out, $abn))
    x, ex, blk
end

function pad(m::Int, n, c::Char)
    if m <= 1
        :($n > 0 && write(out,$c))
    else
        @gensym i
        quote
            $i = $n
            while $i > 0
                write(out,$c)
                $i -= 1
            end
        end
    end
end

function print_fixed(out, precision)
    pdigits = pointer(DIGITS)
    ndigits = LEN[1]
    pt = POINT[1]
    if pt <= 0
        # 0.0dddd0
        write(out, '0')
        write(out, '.')
        precision += pt
        while pt < 0
            write(out, '0')
            pt += 1
        end
        write(out, pdigits, ndigits)
        precision -= ndigits
    elseif ndigits <= pt
        # dddd000.000000
        write(out, pdigits, ndigits)
        while ndigits < pt
            write(out, '0')
            ndigits += 1
        end
        write(out, '.')
    else # 0 < pt < ndigits
        # dd.dd0000
        ndigits -= pt
        write(out, pdigits, pt)
        write(out, '.')
        write(out, pdigits+pt, ndigits)
        precision -= ndigits
    end
    while precision > 0
        write(out, '0')
        precision -= 1
    end
end

function print_exp(out, exp)
    write(out, exp < 0 ? '-' : '+')
    exp = abs(exp)
    d = div(exp,100)
    d > 0 && write(out, char('0'+d))
    exp = rem(exp,100)
    write(out, char('0'+div(exp,10)))
    write(out, char('0'+rem(exp,10)))
end

function gen_d(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print integer:
    #  [dDiu]: print decimal digits
    #  [o]:    print octal digits
    #  [x]:    print hex digits, lowercase
    #  [X]:    print hex digits, uppercase
    #
    # flags:
    #  (#): prefix hex with 0x/0X; octal leads with 0
    #  (0): pad left with zeros
    #  (-): left justify
    #  ( ): precede non-negative values with " "
    #  (+): precede non-negative values with "+"
    #
    x, ex, blk = special_handler(flags,width)
    # interpret the number
    prefix = ""
    if lowercase(c)=='o'
        f = '#' in flags ? :int_0ct : :int_oct
        push!(blk.args, :(($f)($x)))
    elseif c=='x'
        '#' in flags && (prefix = "0x")
        push!(blk.args, :(int_hex($x)))
    elseif c=='X'
        '#' in flags && (prefix = "0X")
        push!(blk.args, :(int_HEX($x)))
    else
        push!(blk.args, :(int_dec($x)))
    end
    push!(blk.args, :(neg = NEG[1]))
    push!(blk.args, :(pt  = POINT[1]))
    # calculate padding
    width -= length(prefix)
    space_pad = width > max(1,precision) && '-' in flags ||
                precision < 0 && width > 1 && !('0' in flags) ||
                precision >= 0 && width > precision
    padding = nothing
    if precision < 1; precision = 1; end
    if space_pad
        if '+' in flags || ' ' in flags
            width -= 1
            if width > precision
                padding = :($width-(pt > $precision ? pt : $precision))
            end
        else
            if width > precision
                padding = :($width-neg-(pt > $precision ? pt : $precision))
            end
        end
    end
    # print space padding
    if padding != nothing && !('-' in flags)
        push!(blk.args, pad(width-precision, padding, ' '))
    end
    # print sign
    '+' in flags ? push!(blk.args, :(write(out, neg?'-':'+'))) :
    ' ' in flags ? push!(blk.args, :(write(out, neg?'-':' '))) :
                   push!(blk.args, :(neg && write(out, '-')))
    # print prefix
    for ch in prefix
        push!(blk.args, :(write(out, $ch)))
    end
    # print zero padding & leading zeros
    if space_pad && precision > 1
        push!(blk.args, pad(precision-1, :($precision-pt), '0'))
    elseif !space_pad && width > 1
        zeros = '+' in flags || ' ' in flags ? :($(width-1)-pt) : :($width-neg-pt)
        push!(blk.args, pad(width-1, zeros, '0'))
    end
    # print integer
    push!(blk.args, :(write(out, pointer(DIGITS), pt)))
    # print padding
    if padding != nothing && '-' in flags
        push!(blk.args, pad(width-precision, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function gen_f(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print to fixed trailing precision
    #  [fF]: the only choice
    #
    # flags
    #  (#): always print a decimal point
    #  (0): pad left with zeros
    #  (-): left justify
    #  ( ): precede non-negative values with " "
    #  (+): precede non-negative values with "+"
    #
    x, ex, blk = special_handler(flags,width)
    # interpret the number
    if precision < 0; precision = 6; end
    push!(blk.args, :(fix_dec($x,$precision)))
    push!(blk.args, :(neg = NEG[1]))
    push!(blk.args, :(pt  = POINT[1]))
    push!(blk.args, :(len = LEN[1]))
    # calculate padding
    padding = nothing
    if precision > 0 || '#' in flags
        width -= precision+1
    end
    if '+' in flags || ' ' in flags
        width -= 1
        if width > 1
            padding = :($width-(pt > 0 ? pt : 1))
        end
    else
        if width > 1
            padding = :($width-(pt > 0 ? pt : 1)-neg)
        end
    end
    # print space padding
    if padding != nothing && !('-' in flags) && !('0' in flags)
        push!(blk.args, pad(width-1, padding, ' '))
    end
    # print sign
    '+' in flags ? push!(blk.args, :(write(out, neg?'-':'+'))) :
    ' ' in flags ? push!(blk.args, :(write(out, neg?'-':' '))) :
                   push!(blk.args, :(neg && write(out, '-')))
    # print zero padding
    if padding != nothing && !('-' in flags) && '0' in flags
        push!(blk.args, pad(width-1, padding, '0'))
    end
    # print digits
    if precision > 0
        push!(blk.args, :(print_fixed(out,$precision)))
    else
        push!(blk.args, :(write(out, pointer(DIGITS), len)))
        push!(blk.args, :(while pt >= (len+=1) write(out,'0') end))
        '#' in flags && push!(blk.args, :(write(out, '.')))
    end
    # print space padding
    if padding != nothing && '-' in flags
        push!(blk.args, pad(width-1, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function gen_e(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print float in scientific form:
    #  [e]: use 'e' to introduce exponent
    #  [E]: use 'E' to introduce exponent
    #
    # flags:
    #  (#): always print a decimal point
    #  (0): pad left with zeros
    #  (-): left justify
    #  ( ): precede non-negative values with " "
    #  (+): precede non-negative values with "+"
    #
    x, ex, blk = special_handler(flags,width)
    # interpret the number
    if precision < 0; precision = 6; end
    ndigits = min(precision+1,BUFLEN-1)
    push!(blk.args, :(ini_dec($x,$ndigits)))
    push!(blk.args, :(neg = NEG[1]))
    push!(blk.args, :(exp = POINT[1]-1))
    expmark = c=='E' ? "E" : "e"
    if precision==0 && '#' in flags
        expmark = string(".",expmark)
    end
    # calculate padding
    padding = nothing
    width -= precision+length(expmark)+(precision>0)+4
    # 4 = leading + expsign + 2 exp digits
    if '+' in flags || ' ' in flags
        width -= 1 # for the sign indicator
        if width > 0
            padding = :($width-((exp<=-100)|(100<=exp)))
        end
    else
        if width > 0
            padding = :($width-((exp<=-100)|(100<=exp))-neg)
        end
    end
    # print space padding
    if padding != nothing && !('-' in flags) && !('0' in flags)
        push!(blk.args, pad(width, padding, ' '))
    end
    # print sign
    '+' in flags ? push!(blk.args, :(write(out, neg?'-':'+'))) :
    ' ' in flags ? push!(blk.args, :(write(out, neg?'-':' '))) :
                    push!(blk.args, :(neg && write(out, '-')))
    # print zero padding
    if padding != nothing && !('-' in flags) && '0' in flags
        push!(blk.args, pad(width, padding, '0'))
    end
    # print digits
    push!(blk.args, :(write(out, DIGITS[1])))
    if precision > 0
        push!(blk.args, :(write(out, '.')))
        push!(blk.args, :(write(out, pointer(DIGITS)+1, $(ndigits-1))))
        if ndigits < precision+1
            n = precision+1-ndigits
            push!(blk.args, pad(n, n, '0'))
        end
    end
    for ch in expmark
        push!(blk.args, :(write(out, $ch)))
    end
    push!(blk.args, :(print_exp(out, exp)))
    # print space padding
    if padding != nothing && '-' in flags
        push!(blk.args, pad(width, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function gen_c(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print a character:
    #  [cC]: both the same for us (Unicode)
    #
    # flags:
    #  (0): pad left with zeros
    #  (-): left justify
    #
    @gensym x
    blk = Expr(:block, :($x = char($x)))
    if width > 1 && !('-' in flags)
        p = '0' in flags ? '0' : ' '
        push!(blk.args, pad(width-1, :($width-charwidth($x)), p))
    end
    push!(blk.args, :(write(out, $x)))
    if width > 1 && '-' in flags
        push!(blk.args, pad(width-1, :($width-charwidth($x)), ' '))
    end
    :(($x)::Integer), blk
end

function gen_s(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print a string:
    #  [sS]: both the same for us (Unicode)
    #
    # flags:
    #  (0): pad left with zeros
    #  (-): left justify
    #
    @gensym x
    blk = Expr(:block)
    if width > 0
        if !('#' in flags)
            push!(blk.args, :($x = string($x)))
        else
            push!(blk.args, :($x = repr($x)))
        end
        if !('-' in flags)
            push!(blk.args, pad(width, :($width-strwidth($x)), ' '))
        end
        push!(blk.args, :(write(out, $x)))
        if '-' in flags
            push!(blk.args, pad(width, :($width-strwidth($x)), ' '))
        end
    else
        if !('#' in flags)
            push!(blk.args, :(print(out, $x)))
        else
            push!(blk.args, :(show(out, $x)))
        end
    end
    :(($x)::Any), blk
end

# TODO: faster pointer printing.

function gen_p(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print pointer:
    #  [p]: the only option
    #
    @gensym x
    blk = Expr(:block)
    ptrwidth = WORD_SIZE>>2
    width -= ptrwidth+2
    if width > 0 && !('-' in flags)
        push!(blk.args, pad(width, width, ' '))
    end
    push!(blk.args, :(write(out, '0')))
    push!(blk.args, :(write(out, 'x')))
    push!(blk.args, :(write(out, bytestring(hex(unsigned($x), $ptrwidth)))))
    if width > 0 && '-' in flags
        push!(blk.args, pad(width, width, ' '))
    end
    :(($x)::Ptr), blk
end

function gen_g(flags::ASCIIString, width::Int, precision::Int, c::Char)
    error("printf \"%g\" format specifier not implemented")
end

### core unsigned integer decoding functions ###

macro handle_zero(ex)
    quote
        if $(esc(ex)) == 0
            POINT[1] = 1
            DIGITS[1] = '0'
            return
        end
    end
end

function decode_oct(x::Unsigned)
    @handle_zero x
    POINT[1] = i = div((sizeof(x)<<3)-leading_zeros(x)+2,3)
    while i > 0
        DIGITS[i] = '0'+(x&0x7)
        x >>= 3
        i -= 1
    end
end

function decode_0ct(x::Unsigned)
    # doesn't need special handling for zero
    POINT[1] = i = div((sizeof(x)<<3)-leading_zeros(x)+5,3)
    while i > 0
        DIGITS[i] = '0'+(x&0x7)
        x >>= 3
        i -= 1
    end
end

function decode_dec(x::Unsigned)
    @handle_zero x
    POINT[1] = i = Base.ndigits0z(x)
    while i > 0
        DIGITS[i] = '0'+rem(x,10)
        x = div(x,10)
        i -= 1
    end
end

function decode_hex(x::Unsigned, symbols::Array{Uint8,1})
    @handle_zero x
    POINT[1] = i = (sizeof(x)<<1)-(leading_zeros(x)>>2)
    while i > 0
        DIGITS[i] = symbols[(x&0xf)+1]
        x >>= 4
        i -= 1
    end
end

const hex_symbols = "0123456789abcdef".data
const HEX_symbols = "0123456789ABCDEF".data

decode_hex(x::Unsigned) = decode_hex(x,hex_symbols)
decode_HEX(x::Unsigned) = decode_hex(x,HEX_symbols)

function decode(b::Int, x::BigInt)
    neg = NEG[1] = x.size < 0
    pt = POINT[1] = Base.ndigits(x, abs(b))
    length(DIGITS) < pt+1 && resize!(DIGITS, pt+1)
    neg && (x.size = -x.size)
    ccall((:__gmpz_get_str, :libgmp), Ptr{Uint8},
          (Ptr{Uint8}, Cint, Ptr{BigInt}), DIGITS, b, &x)
    neg && (x.size = -x.size)
end

function decode_0ct(x::BigInt)
    neg = NEG[1] = x.size < 0
    DIGITS[1] = '0'
    if x.size == 0
        POINT[1] = 1
        return
    end
    pt = POINT[1] = Base.ndigits0z(x, 8) + 1
    length(DIGITS) < pt+1 && resize!(DIGITS, pt+1)
    neg && (x.size = -x.size)
    p = convert(Ptr{Uint8}, DIGITS) + 1
    ccall((:__gmpz_get_str, :libgmp), Ptr{Uint8},
          (Ptr{Uint8}, Cint, Ptr{BigInt}), p, 8, &x)
    neg && (x.size = -x.size)
end

### decoding functions directly used by printf generated code ###

# int_*(x)   => fixed precision, to 0th place, filled out
# fix_*(x,n) => fixed precision, to nth place, not filled out
# ini_*(x,n) => n initial digits, filled out

# alternate versions:
#   *_0ct(x,n) => ensure that the first octal digits is zero
#   *_HEX(x,n) => use uppercase digits for hexadecimal

## "int" decoding functions ##
#
# - sets neg[1]
# - sets point[1]
# - implies len[1] = point[1]
#

int_oct(x::Unsigned) = (NEG[1]=false; decode_oct(x))
int_0ct(x::Unsigned) = (NEG[1]=false; decode_0ct(x))
int_dec(x::Unsigned) = (NEG[1]=false; decode_dec(x))
int_hex(x::Unsigned) = (NEG[1]=false; decode_hex(x))
int_HEX(x::Unsigned) = (NEG[1]=false; decode_HEX(x))

macro handle_negative()
    quote
        if $(esc(:x)) < 0
            NEG[1] = true
            $(esc(:x)) = oftype($(esc(:x)),-$(esc(:x)))
        else
            NEG[1] = false
        end
    end
end

int_oct(x::Integer) = (@handle_negative; decode_oct(unsigned(x)))
int_0ct(x::Integer) = (@handle_negative; decode_0ct(unsigned(x)))
int_dec(x::Integer) = (@handle_negative; decode_dec(unsigned(x)))
int_hex(x::Integer) = (@handle_negative; decode_hex(unsigned(x)))
int_HEX(x::Integer) = (@handle_negative; decode_HEX(unsigned(x)))

int_oct(x::BigInt) = decode(8, x)
int_0ct(x::BigInt) = decode_0ct(x)
int_dec(x::BigInt) = decode(10, x)
int_hex(x::BigInt) = decode(16, x)
int_HEX(x::BigInt) = decode(-16, x)

const SmallFloatingPoint = Union(Float64,Float32,Float16)

function int_dec(x::SmallFloatingPoint)
    if x == 0.0
        NEG[1] = false
        POINT[1] = 1
        DIGITS[1] = '0'
        return
    end
    @grisu_ccall x Grisu.FIXED 0
    if LEN[1] == 0
        NEG[1] = false
        POINT[1] = 1
        DIGITS[1] = '0'
    else
        for i = LEN[1]+1:POINT[1]
            DIGITS[i] = '0'
        end
    end
end

int_oct(x::Real) = int_oct(integer(x)) # TODO: real float decoding.
int_0ct(x::Real) = int_0ct(integer(x)) # TODO: real float decoding.
int_dec(x::Real) = int_dec(float(x))
int_hex(x::Real) = int_hex(integer(x)) # TODO: real float decoding.
int_HEX(x::Real) = int_HEX(integer(x)) # TODO: real float decoding.

## fix decoding functions ##
#
# - sets neg[1]
# - sets point[1]
# - sets len[1]; if less than point[1], trailing zeros implied
#

fix_dec(x::Integer, n::Int) = (int_dec(x); LEN[1]=POINT[1])
fix_dec(x::Real, n::Int) = fix_dec(float(x),n)

function fix_dec(x::SmallFloatingPoint, n::Int)
    if n > BUFLEN-1; n = BUFLEN-1; end
    @grisu_ccall x Grisu.FIXED n
    if LEN[1] == 0
        NEG[1] = false
        POINT[1] = 1
        DIGITS[1] = '0'
    end
end

## ini decoding functions ##
#
# - sets neg[1]
# - sets point[1]
# - implies len[1] = n (requested digits)
#

function ini_dec(x::Unsigned, n::Int)
    k = ndigits(x)
    if k <= n
        POINT[1] = k
        for i = k:-1:1
            DIGITS[i] = '0'+rem(x,10)
            x = div(x,10)
        end
        for i = k+1:n
            DIGITS[i] = '0'
        end
    else
        p = Base.powers_of_ten[k-n+1]
        r = rem(x,p)
        if r >= (p>>1)
            x += p
            if x >= Base.powers_of_ten[k+1]
                p *= 10
                k += 1
            end
        end
        POINT[1] = k
        x = div(x,p)
        for i = n:-1:1
            DIGITS[i] = '0'+rem(x,10)
            x = div(x,10)
        end
    end
end

ini_dec(x::Integer, n::Int) = (@handle_negative; ini_dec(unsigned(x),n))
ini_dec(x::Real, n::Int) = ini_dec(float(x),n)

function ini_dec(x::SmallFloatingPoint, n::Int)
    if x == 0.0
        POINT[1] = 1
        NEG[1] = signbit(x)
        ccall(:memset, Ptr{Void}, (Ptr{Void}, Cint, Csize_t), DIGITS, '0', n)
    else
        @grisu_ccall x Grisu.PRECISION n
    end
end

function ini_dec(x::BigInt, n::Int)
    if x.size == 0
        POINT[1] = 1
        NEG[1] = false
        ccall(:memset, Ptr{Void}, (Ptr{Void}, Cint, Csize_t), DIGITS, '0', n)
    else
        d = Base.ndigits0z(x)
        if d <= n
            int_dec(x)
            d == n && return
            p = convert(Ptr{Void}, DIGITS) + POINT[1]
            ccall(:memset, Ptr{Void}, (Ptr{Void}, Cint, Csize_t), p, '0', n - POINT[1])
        else
            int_dec(iround(x/big(10)^(d-n)))
            POINT[1] = d
        end
    end
end

### external printf interface ###

is_str_expr(ex) =
    isa(ex,Expr) && (ex.head == :string || (ex.head == :macrocall && isa(ex.args[1],Symbol) &&
    endswith(string(ex.args[1]),"str")))

function _printf(macroname, io, fmt, args)
    isa(fmt, String) || error("$macroname: format must be a plain static string (no interpolation or prefix)")
    sym_args, blk = gen(fmt)
    if length(sym_args) != length(args)
        error("$macroname: wrong number of arguments")
    end
    for i = length(args):-1:1
        var = sym_args[i].args[1]
        unshift!(blk.args, :($var = $(esc(args[i]))))
    end
    unshift!(blk.args, :(out = $io))
    blk
end

macro printf(args...)
    !isempty(args) || error("@printf: called with zero arguments")
    if isa(args[1], String) || is_str_expr(args[1])
        _printf("@printf", :STDOUT, args[1], args[2:end])
    else
        (length(args) >= 2 && (isa(args[2], String) || is_str_expr(args[2]))) ||
            error("@printf: first or second argument must be a format string")
        _printf("@printf", esc(args[1]), args[2], args[3:end])
    end
end

macro sprintf(args...)
    !isempty(args) || error("@sprintf: called with zero arguments")
    isa(args[1], String) || is_str_expr(args[1]) || 
        error("@sprintf: first argument must be a format string")
    :(sprint(io->$(_printf("@sprintf", :io, args[1], args[2:end]))))
end

end # module

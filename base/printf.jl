module Printf
using Base.Grisu
export @printf, @sprintf

### printf formatter generation ###

function _gen(s::String)
    args = {:(out::IO)}
    blk = Expr(:block, :(local neg, pt, len, exp))
    for x in _parse(s)
        if isa(x,String)
            push!(blk.args, :(write(out, $(length(x)==1 ? x[1] : x))))
        else
            c = lowercase(x[end])
            f = c=='f' ? _gen_f :
                c=='e' ? _gen_e :
                c=='g' ? _gen_g :
                c=='c' ? _gen_c :
                c=='s' ? _gen_s :
                c=='p' ? _gen_p :
                         _gen_d
            arg, ex = f(x...)
            push!(args, arg)
            push!(blk.args, ex)
        end
    end
    push!(blk.args, :nothing)
    return args, blk
end

### printf format string parsing ###

function _parse(s::String)
    # parse format string in to stings and format tuples
    list = {}
    i = j = start(s)
    while !done(s,j)
        c, k = next(s,j)
        if c == '%'
            isempty(s[i:j-1]) || push!(list, s[i:j-1])
            flags, width, precision, conversion, k = _parse1(s,k)
            contains(flags,'\'') && error("printf format flag ' not yet supported")
            conversion == 'a'    && error("printf feature %a not yet supported")
            conversion == 'n'    && error("printf feature %n not supported")
            push!(list, conversion == '%' ? "%" : (flags,width,precision,conversion))
            i = j = k
        else
            j = k
        end
    end
    isempty(s[i:]) || push!(list, s[i:])
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
            delete!(list,i+1:j)
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

_next_or_die(s::String, k) = !done(s,k) ? next(s,k) :
    error("invalid printf format string: ", repr(s))

function _parse1(s::String, k::Integer)
    j = k
    width = 0
    precision = -1
    c, k = _next_or_die(s,k)
    # handle %%
    if c == '%'
        return "", width, precision, c, k
    end
    # parse flags
    while contains("#0- + '", c)
        c, k = _next_or_die(s,k)
    end
    flags = ascii(s[j:k-2])
    # parse width
    while '0' <= c <= '9'
        width = 10*width + c-'0'
        c, k = _next_or_die(s,k)
    end
    # parse precision
    if c == '.'
        c, k = _next_or_die(s,k)
        if '0' <= c <= '9'
            precision = 0
            while '0' <= c <= '9'
                precision = 10*precision + c-'0'
                c, k = _next_or_die(s,k)
            end
        end
    end
    # parse length modifer (ignored)
    if c == 'h' || c == 'l'
        prev = c
        c, k = _next_or_die(s,k)
        if c == prev
            c, k = _next_or_die(s,k)
        end
    elseif contains("Ljqtz",c)
        c, k = _next_or_die(s,k)
    end
    # validate conversion
    if !contains("diouxXDOUeEfFgGaAcCsSpn", c)
        error("invalid printf format string: ", repr(s))
    end
    # TODO: warn about silly flag/conversion combinations
    flags, width, precision, c, k
end

### printf formatter generation ###

function _special_handler(flags::ASCIIString, width::Int)
    @gensym x
    blk = Expr(:block)
    pad = contains(flags,'-') ? rpad : lpad
    pos = contains(flags,'+') ? "+" :
          contains(flags,' ') ? " " : ""
    abn = quote
        isnan($x) ? $(pad("NaN", width)) :
         $x < 0   ? $(pad("-Inf", width)) :
                    $(pad("$(pos)Inf", width))
    end
    ex = :(isfinite($x) ? $blk : write(out, $abn))
    x, ex, blk
end

function _pad(m::Int, n, c::Char)
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

function _print_fixed(out, precision)
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

function _print_exp(out, exp)
    write(out, exp < 0 ? '-' : '+')
    exp = abs(exp)
    d = div(exp,100)
    d > 0 && write(out, char('0'+d))
    exp = rem(exp,100)
    write(out, char('0'+div(exp,10)))
    write(out, char('0'+rem(exp,10)))
end

function _gen_d(flags::ASCIIString, width::Int, precision::Int, c::Char)
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
    x, ex, blk = _special_handler(flags,width)
    # interpret the number
    prefix = ""
    if lowercase(c)=='o'
        f = contains(flags,'#') ? :_int_0ct : :_int_oct
        push!(blk.args, :(($f)($x)))
    elseif c=='x'
        if contains(flags,'#'); prefix = "0x"; end
        push!(blk.args, :(_int_hex($x)))
    elseif c=='X'
        if contains(flags,'#'); prefix = "0X"; end
        push!(blk.args, :(_int_HEX($x)))
    else
        push!(blk.args, :(_int_dec($x)))
    end
    push!(blk.args, :(neg = NEG[1]))
    push!(blk.args, :(pt  = POINT[1]))
    # calculate padding
    width -= length(prefix)
    space_pad = width > max(1,precision) && contains(flags,'-') ||
                precision < 0 && width > 1 && !contains(flags,'0') ||
                precision >= 0 && width > precision
    padding = nothing
    if precision < 1; precision = 1; end
    if space_pad
        if contains(flags,'+') || contains(flags,' ')
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
    if padding != nothing && !contains(flags,'-')
        push!(blk.args, _pad(width-precision, padding, ' '))
    end
    # print sign
    contains(flags,'+') ? push!(blk.args, :(write(out, neg?'-':'+'))) :
    contains(flags,' ') ? push!(blk.args, :(write(out, neg?'-':' '))) :
                          push!(blk.args, :(neg && write(out, '-')))
    # print prefix
    for ch in prefix
        push!(blk.args, :(write(out, $ch)))
    end
    # print zero padding & leading zeros
    if space_pad && precision > 1
        push!(blk.args, _pad(precision-1, :($precision-pt), '0'))
    elseif !space_pad && width > 1
        zeros = contains(flags,'+') || contains(flags,' ') ?
            :($(width-1)-pt) : :($width-neg-pt)
        push!(blk.args, _pad(width-1, zeros, '0'))
    end
    # print integer
    push!(blk.args, :(write(out, pointer(DIGITS), pt)))
    # print padding
    if padding != nothing && contains(flags,'-')
        push!(blk.args, _pad(width-precision, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function _gen_f(flags::ASCIIString, width::Int, precision::Int, c::Char)
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
    x, ex, blk = _special_handler(flags,width)
    # interpret the number
    if precision < 0; precision = 6; end
    push!(blk.args, :(_fix_dec($x,$precision)))
    push!(blk.args, :(neg = NEG[1]))
    push!(blk.args, :(pt  = POINT[1]))
    push!(blk.args, :(len = LEN[1]))
    # calculate padding
    padding = nothing
    if precision > 0 || contains(flags,'#')
        width -= precision+1
    end
    if contains(flags,'+') || contains(flags,' ')
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
    if padding != nothing && !contains(flags,'-') && !contains(flags,'0')
        push!(blk.args, _pad(width-1, padding, ' '))
    end
    # print sign
    contains(flags,'+') ? push!(blk.args, :(write(out, neg?'-':'+'))) :
    contains(flags,' ') ? push!(blk.args, :(write(out, neg?'-':' '))) :
                          push!(blk.args, :(neg && write(out, '-')))
    # print zero padding
    if padding != nothing && !contains(flags,'-') && contains(flags,'0')
        push!(blk.args, _pad(width-1, padding, '0'))
    end
    # print digits
    if precision > 0
        push!(blk.args, :(_print_fixed(out,$precision)))
    else
        push!(blk.args, :(write(out, pointer(DIGITS), len)))
        push!(blk.args, :(while pt >= (len+=1) write(out,'0') end))
        contains(flags,'#') && push!(blk.args, :(write(out, '.')))
    end
    # print space padding
    if padding != nothing && contains(flags,'-')
        push!(blk.args, _pad(width-1, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function _gen_e(flags::ASCIIString, width::Int, precision::Int, c::Char)
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
    x, ex, blk = _special_handler(flags,width)
    # interpret the number
    if precision < 0; precision = 6; end
    ndigits = min(precision+1,BUFLEN-1)
    push!(blk.args, :(_ini_dec($x,$ndigits)))
    push!(blk.args, :(neg = NEG[1]))
    push!(blk.args, :(exp = POINT[1]-1))
    expmark = c=='E' ? "E" : "e"
    if precision==0 && contains(flags,'#')
        expmark = string(".",expmark)
    end
    # calculate padding
    padding = nothing
    width -= precision+length(expmark)+(precision>0)+4
    # 4 = leading + expsign + 2 exp digits
    if contains(flags,'+') || contains(flags,' ')
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
    if padding != nothing && !contains(flags,'-') && !contains(flags,'0')
        push!(blk.args, _pad(width, padding, ' '))
    end
    # print sign
    contains(flags,'+') ? push!(blk.args, :(write(out, neg?'-':'+'))) :
    contains(flags,' ') ? push!(blk.args, :(write(out, neg?'-':' '))) :
                          push!(blk.args, :(neg && write(out, '-')))
    # print zero padding
    if padding != nothing && !contains(flags,'-') && contains(flags,'0')
        push!(blk.args, _pad(width, padding, '0'))
    end
    # print digits
    push!(blk.args, :(write(out, DIGITS[1])))
    if precision > 0
        push!(blk.args, :(write(out, '.')))
        push!(blk.args, :(write(out, pointer(DIGITS)+1, $(ndigits-1))))
        if ndigits < precision+1
            n = precision+1-ndigits
            push!(blk.args, _pad(n, n, '0'))
        end
    end
    for ch in expmark
        push!(blk.args, :(write(out, $ch)))
    end
    push!(blk.args, :(_print_exp(out, exp)))
    # print space padding
    if padding != nothing && contains(flags,'-')
        push!(blk.args, _pad(width, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function _gen_c(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print a character:
    #  [cC]: both the same for us (Unicode)
    #
    # flags:
    #  (0): pad left with zeros
    #  (-): left justify
    #
    @gensym x
    blk = Expr(:block, :($x = char($x)))
    if width > 1 && !contains(flags,'-')
        p = contains(flags,'0') ? '0' : ' '
        push!(blk.args, _pad(width-1, :($width-charwidth($x)), p))
    end
    push!(blk.args, :(write(out, $x)))
    if width > 1 && contains(flags,'-')
        push!(blk.args, _pad(width-1, :($width-charwidth($x)), ' '))
    end
    :(($x)::Integer), blk
end

function _gen_s(flags::ASCIIString, width::Int, precision::Int, c::Char)
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
        if !contains(flags,'#')
            push!(blk.args, :($x = string($x)))
        else
            push!(blk.args, :($x = repr($x)))
        end
        if !contains(flags,'-')
            push!(blk.args, _pad(width, :($width-strwidth($x)), ' '))
        end
        push!(blk.args, :(write(out, $x)))
        if contains(flags,'-')
            push!(blk.args, _pad(width, :($width-strwidth($x)), ' '))
        end
    else
        if !contains(flags,'#')
            push!(blk.args, :(print(out, $x)))
        else
            push!(blk.args, :(show(out, $x)))
        end
    end
    :(($x)::Any), blk
end

# TODO: faster pointer printing.

function _gen_p(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print pointer:
    #  [p]: the only option
    #
    @gensym x
    blk = Expr(:block)
    ptrwidth = WORD_SIZE>>2
    width -= ptrwidth+2
    if width > 0 && !contains(flags,'-')
        push!(blk.args, _pad(width, width, ' '))
    end
    push!(blk.args, :(write(out, '0')))
    push!(blk.args, :(write(out, 'x')))
    push!(blk.args, :(write(out, bytestring(hex(unsigned($x), $ptrwidth)))))
    if width > 0 && contains(flags,'-')
        push!(blk.args, _pad(width, width, ' '))
    end
    :(($x)::Ptr), blk
end

### core unsigned integer decoding functions ###

macro _handle_zero()
    quote
        if $(esc(:x)) == 0
            POINT[1] = 1
            DIGITS[1] = '0'
            return
        end
    end
end

function _decode_oct(x::Unsigned)
    @_handle_zero
    POINT[1] = i = div((sizeof(x)<<3)-leading_zeros(x)+2,3)
    while i > 0
        DIGITS[i] = '0'+(x&0x7)
        x >>= 3
        i -= 1
    end
end

function _decode_0ct(x::Unsigned)
    POINT[1] = i = div((sizeof(x)<<3)-leading_zeros(x)+5,3)
    while i > 0
        DIGITS[i] = '0'+(x&0x7)
        x >>= 3
        i -= 1
    end
end

function _decode_dec(x::Unsigned)
    @_handle_zero
    POINT[1] = i = ndigits0z(x)
    while i > 0
        DIGITS[i] = '0'+rem(x,10)
        x = div(x,10)
        i -= 1
    end
end

function _decode_hex(x::Unsigned, symbols::Array{Uint8,1})
    @_handle_zero
    POINT[1] = i = (sizeof(x)<<1)-(leading_zeros(x)>>2)
    while i > 0
        DIGITS[i] = symbols[(x&0xf)+1]
        x >>= 4
        i -= 1
    end
end

const _hex_symbols = "0123456789abcdef".data
const _HEX_symbols = "0123456789ABCDEF".data

_decode_hex(x::Unsigned) = _decode_hex(x,_hex_symbols)
_decode_HEX(x::Unsigned) = _decode_hex(x,_HEX_symbols)

### decoding functions directly used by printf generated code ###

# int_*(x)   => fixed precision, to 0th place, filled out
# fix_*(x,n) => fixed precision, to nth place, not filled out
# ini_*(x,n) => n initial digits, filled out
# sig_*(x,n) => n initial digits, zero-stripped

# alternate versions:
#   *_0ct(x,n) => ensure that the first octal digits is zero
#   *_HEX(x,n) => use uppercase digits for hexadecimal

## "int" decoding functions ##
#
# - sets neg[1]
# - sets point[1]
# - implies len[1] = point[1]
#

_int_oct(x::Unsigned) = (NEG[1]=false; _decode_oct(x))
_int_0ct(x::Unsigned) = (NEG[1]=false; _decode_0ct(x))
_int_dec(x::Unsigned) = (NEG[1]=false; _decode_dec(x))
_int_hex(x::Unsigned) = (NEG[1]=false; _decode_hex(x))
_int_HEX(x::Unsigned) = (NEG[1]=false; _decode_HEX(x))

macro _handle_negative()
    quote
        if $(esc(:x)) < 0
            NEG[1] = true
            $(esc(:x)) = -$(esc(:x))
        else
            NEG[1] = false
        end
    end
end

_int_oct(x::Integer) = (@_handle_negative; _decode_oct(unsigned(x)))
_int_0ct(x::Integer) = (@_handle_negative; _decode_0ct(unsigned(x)))
_int_dec(x::Integer) = (@_handle_negative; _decode_dec(unsigned(x)))
_int_hex(x::Integer) = (@_handle_negative; _decode_hex(unsigned(x)))
_int_HEX(x::Integer) = (@_handle_negative; _decode_HEX(unsigned(x)))

_int_oct(x::Real) = _int_oct(integer(x)) # TODO: real float decoding.
_int_0ct(x::Real) = _int_0ct(integer(x)) # TODO: real float decoding.
_int_dec(x::Real) = _int_dec(float(x))
_int_hex(x::Real) = _int_hex(integer(x)) # TODO: real float decoding.
_int_HEX(x::Real) = _int_HEX(integer(x)) # TODO: real float decoding.

function _int_dec(x::FloatingPoint)
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

## fix decoding functions ##
#
# - sets neg[1]
# - sets point[1]
# - sets len[1]; if less than point[1], trailing zeros implied
#

_fix_dec(x::Integer, n::Int) = (_int_dec(x); LEN[1]=POINT[1])
_fix_dec(x::Real, n::Int) = _fix_dec(float(x),n)

function _fix_dec(x::FloatingPoint, n::Int)
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

function _ini_dec(x::Unsigned, n::Int)
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
        p = powers_of_ten[k-n+1]
        r = rem(x,p)
        if r >= (p>>1)
            x += p
            if x >= powers_of_ten[k+1]
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

_ini_dec(x::Integer, n::Int) = (@_handle_negative; _ini_dec(unsigned(x),n))
_ini_dec(x::Real, n::Int) = _ini_dec(float(x),n)

function _ini_dec(x::FloatingPoint, n::Int)
    if x == 0.0
        POINT[1] = 1
        NEG[1] = signbit(x)
        ccall(:memset, Void, (Ptr{Uint8}, Int32, Int), DIGITS, '0', n)
    else
        @grisu_ccall x Grisu.PRECISION n
    end
end

## sig decoding functions ##
#
# - sets neg[1]
# - sets point[1]
# - sets len[1]
#

function _sig_dec(x::Unsigned, n::Int)
    if x == 0
        NEG[1] = false
        POINT[1] = 1
        LEN[1] = 1
        DIGITS[1] = '0'
        return
    end
    k = ndigits0z(x)
    if k <= n
        POINT[1] = k
        for i = k:-1:1
            DIGITS[i] = '0'+rem(x,10)
            x = div(x,10)
        end
        while DIGITS[k] == '0'
            k -= 1
        end
        LEN[1] = k
    else
        p = powers_of_ten[k-n+1]
        r = rem(x,p)
        if r >= (p>>1)
            x += p
            if x >= powers_of_ten[k+1]
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
        while DIGITS[n] == '0'
            n -= 1
        end
        LEN[1] = n
    end
end

_sig_dec(x::Integer, n::Int) = (@_handle_negative; _sig_dec(unsigned(x),n))
_sig_dec(x::Real, n::Int) = _sig_dec(float(x),n)

function _sig_dec(x::FloatingPoint, n::Int)
    @grisu_ccall x Grisu.PRECISION n
    if x == 0.0; return; end
    while DIGITS[n] == '0'
        n -= 1
    end
    LEN[1] = n
end

### external printf interface ###

_is_str_expr(ex) =
    isa(ex,Expr) && ex.head==:macrocall && isa(ex.args[1],Symbol) &&
    (ex.args[1] == :str || ends_with(string(ex.args[1]),"_str"))

macro printf(args...)
    if length(args) == 0
        error("@printf: called with zero arguments")
    end
    if !isa(args[1],String) && !(length(args) > 1 && isa(args[2],String))
        if _is_str_expr(args[1]) || length(args) > 1 && _is_str_expr(args[2])
           error("format must be a plain static string (no interpolation or prefix)")
        end
        error("first or second argument must be a format string")
    end
    local io, fmt
    if isa(args[1],String)
        io = :(Base.OUTPUT_STREAM)
        fmt = args[1]
        args = args[2:]
    else
        io = args[1]
        fmt = args[2]
        args = args[3:]
    end
    args = {io,args...}
    sym_args, blk = _gen(fmt)
    if length(sym_args) != length(args)
        error("@printf: wrong number of arguments")
    end
    for i = length(args):-1:1
        var = sym_args[i].args[1]
        unshift!(blk.args, :($var = $(esc(args[i]))))
    end
    blk
end

macro sprintf(args...)
    :(sprint(io->@printf(io,$(map(esc,args)...))))
end

end # module

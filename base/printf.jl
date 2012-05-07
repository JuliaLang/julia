### printf formatter generation ###

function _jl_printf_gen(s::String)
    args = {:(out::IOStream)}
    blk = expr(:block, :(local neg, pt, len, exp))
    for x in _jl_printf_parse(s)
        if isa(x,String)
            push(blk.args, :(write(out, $(strlen(x)==1 ? x[1] : x))))
        else
            c = lc(x[end])
            f = c=='f' ? _jl_printf_f :
                c=='e' ? _jl_printf_e :
                c=='g' ? _jl_printf_g :
                c=='c' ? _jl_printf_c :
                c=='s' ? _jl_printf_s :
                c=='p' ? _jl_printf_p :
                         _jl_printf_d
            arg, ex = f(x...)
            push(args, arg)
            push(blk.args, ex)
        end
    end
    push(blk.args, :nothing)
    return args, blk
end

### printf format string parsing ###

function _jl_printf_parse(s::String)
    # parse format string in to stings and format tuples
    list = {}
    i = j = start(s)
    while !done(s,j)
        c, k = next(s,j)
        if c == '%'
            isempty(s[i:j-1]) || push(list, utf8(unescape_string(s[i:j-1])))
            flags, width, precision, conversion, k = _jl_printf_parse1(s,k)
            contains(flags,'\'') && error("printf format flag ' not yet supported")
            conversion == 'a'    && error("printf feature %a not yet supported")
            conversion == 'n'    && error("printf feature %n not supported")
            push(list, conversion == '%' ? "%" : (flags,width,precision,conversion))
            i = j = k
        else
            j = k
        end
    end
    isempty(s[i:]) || push(list, utf8(unescape_string(s[i:])))
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
            del(list,i+1:j)
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

_jl_next_or_die(s::String, k) = !done(s,k) ? next(s,k) :
    error("invalid printf format string: ", sshow(s))

function _jl_printf_parse1(s::String, k::Integer)
    j = k
    width = 0
    precision = -1
    c, k = _jl_next_or_die(s,k)
    # handle %%
    if c == '%'
        return "", width, precision, c, k
    end
    # parse flags
    while contains("#0- + '", c)
        c, k = _jl_next_or_die(s,k)
    end
    flags = ascii(s[j:k-2])
    # parse width
    while '0' <= c <= '9'
        width = 10*width + c-'0'
        c, k = _jl_next_or_die(s,k)
    end
    # parse precision
    if c == '.'
        c, k = _jl_next_or_die(s,k)
        if '0' <= c <= '9'
            precision = 0
            while '0' <= c <= '9'
                precision = 10*precision + c-'0'
                c, k = _jl_next_or_die(s,k)
            end
        end
    end
    # parse length modifer (ignored)
    if c == 'h' || c == 'l'
        prev = c
        c, k = _jl_next_or_die(s,k)
        if c == prev
            c, k = _jl_next_or_die(s,k)
        end
    elseif contains("Ljqtz",c)
        c, k = _jl_next_or_die(s,k)
    end
    # validate conversion
    if !contains("diouxXDOUeEfFgGaAcCsSpn", c)
        error("invalid printf format string: ", sshow(s))
    end
    # TODO: warn about silly flag/conversion combinations
    flags, width, precision, c, k
end

### printf formatter generation ###

function _jl_special_handler(flags::ASCIIString, width::Int)
    @gensym x
    blk = expr(:block)
    pad = contains(flags,'-') ? rpad : lpad
    pos = contains(flags,'+') ? "+" :
          contains(flags,' ') ? " " : ""
    abn = quote
        isnan($x) ? $(cstring(pad("NaN", width))) :
         $x < 0   ? $(cstring(pad("-Inf", width))) :
                    $(cstring(pad("$(pos)Inf", width)))
    end
    ex = :(isfinite($x) ? $blk : write(out, $abn))
    x, ex, blk
end

function _jl_printf_pad(m::Int, n, c::Char)
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

function _jl_print_fixed(out, precision)
    pdigits = pointer(_jl_digits)
    ndigits = _jl_length[1]
    pt = _jl_point[1]
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

function _jl_print_exp(out, exp)
    write(out, exp < 0 ? '-' : '+')
    exp = abs(exp)
    d = div(exp,100)
    d > 0 && write(out, char('0'+d))
    exp = rem(exp,100)
    write(out, char('0'+div(exp,10)))
    write(out, char('0'+rem(exp,10)))
end

function _jl_printf_d(flags::ASCIIString, width::Int, precision::Int, c::Char)
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
    x, ex, blk = _jl_special_handler(flags,width)
    # interpret the number
    prefix = ""
    if lc(c)=='o'
        f = contains(flags,'#') ? :_jl_int_0ct : :_jl_int_oct
        push(blk.args, :(($f)($x)))
    elseif c=='x'
        if contains(flags,'#'); prefix = "0x"; end
        push(blk.args, :(_jl_int_hex($x)))
    elseif c=='X'
        if contains(flags,'#'); prefix = "0X"; end
        push(blk.args, :(_jl_int_HEX($x)))
    else
        push(blk.args, :(_jl_int_dec($x)))
    end
    push(blk.args, :(neg = _jl_neg[1]))
    push(blk.args, :(pt  = _jl_point[1]))
    # calculate padding
    width -= strlen(prefix)
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
        push(blk.args, _jl_printf_pad(width-precision, padding, ' '))
    end
    # print sign
    contains(flags,'+') ? push(blk.args, :(write(out, neg?'-':'+'))) :
    contains(flags,' ') ? push(blk.args, :(write(out, neg?'-':' '))) :
                          push(blk.args, :(neg && write(out, '-')))
    # print prefix
    for ch in prefix
        push(blk.args, :(write(out, $ch)))
    end
    # print zero padding & leading zeros
    if space_pad && precision > 1
        push(blk.args, _jl_printf_pad(precision-1, :($precision-pt), '0'))
    elseif !space_pad && width > 1
        zeros = contains(flags,'+') || contains(flags,' ') ?
            :($(width-1)-pt) : :($width-neg-pt)
        push(blk.args, _jl_printf_pad(width-1, zeros, '0'))
    end
    # print integer
    push(blk.args, :(write(out, pointer(_jl_digits), pt)))
    # print padding
    if padding != nothing && contains(flags,'-')
        push(blk.args, _jl_printf_pad(width-precision, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function _jl_printf_f(flags::ASCIIString, width::Int, precision::Int, c::Char)
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
    x, ex, blk = _jl_special_handler(flags,width)
    # interpret the number
    if precision < 0; precision = 6; end
    push(blk.args, :(_jl_fix_dec($x,$precision)))
    push(blk.args, :(neg = _jl_neg[1]))
    push(blk.args, :(pt  = _jl_point[1]))
    push(blk.args, :(len = _jl_length[1]))
    # calculate padding
    padding = nothing
    if precision > 0 || contains(flags,'#')
        width -= precision+1
    end
    if contains(flags,'+') || contains(flags,' ')
        width -= 1
        if width > 1
            padding = :($width-pt)
        end
    else
        if width > 1
            padding = :($width-pt-neg)
        end
    end
    # print space padding
    if padding != nothing && !contains(flags,'-') && !contains(flags,'0')
        push(blk.args, _jl_printf_pad(width-1, padding, ' '))
    end
    # print sign
    contains(flags,'+') ? push(blk.args, :(write(out, neg?'-':'+'))) :
    contains(flags,' ') ? push(blk.args, :(write(out, neg?'-':' '))) :
                          push(blk.args, :(neg && write(out, '-')))
    # print zero padding
    if padding != nothing && !contains(flags,'-') && contains(flags,'0')
        push(blk.args, _jl_printf_pad(width-1, padding, '0'))
    end
    # print digits
    if precision > 0
        push(blk.args, :(_jl_print_fixed(out,$precision)))
    else
        push(blk.args, :(write(out, pointer(_jl_digits), pt)))
        contains(flags,'#') && push(blk.args, :(write(out, '.')))
    end
    # print space padding
    if padding != nothing && contains(flags,'-')
        push(blk.args, _jl_printf_pad(width-1, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function _jl_printf_e(flags::ASCIIString, width::Int, precision::Int, c::Char)
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
    x, ex, blk = _jl_special_handler(flags,width)
    # interpret the number
    if precision < 0; precision = 6; end
    ndigits = min(precision+1,_jl_buflen-1)
    push(blk.args, :(_jl_ini_dec($x,$ndigits)))
    push(blk.args, :(neg = _jl_neg[1]))
    push(blk.args, :(exp = _jl_point[1]-1))
    expmark = c=='E' ? "E" : "e"
    if precision==0 && contains(flags,'#')
        expmark = strcat(".",expmark)
    end
    # calculate padding
    padding = nothing
    width -= precision+strlen(expmark)+(precision>0)+4
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
        push(blk.args, _jl_printf_pad(width, padding, ' '))
    end
    # print sign
    contains(flags,'+') ? push(blk.args, :(write(out, neg?'-':'+'))) :
    contains(flags,' ') ? push(blk.args, :(write(out, neg?'-':' '))) :
                          push(blk.args, :(neg && write(out, '-')))
    # print zero padding
    if padding != nothing && !contains(flags,'-') && contains(flags,'0')
        push(blk.args, _jl_printf_pad(width, padding, '0'))
    end
    # print digits
    push(blk.args, :(write(out, _jl_digits[1])))
    if precision > 0
        push(blk.args, :(write(out, '.')))
        push(blk.args, :(write(out, pointer(_jl_digits)+1, $(ndigits-1))))
        if ndigits < precision+1
            n = precision+1-ndigits
            push(blk.args, _jl_printf_pad(n, n, '0'))
        end
    end
    for ch in expmark
        push(blk.args, :(write(out, $ch)))
    end
    push(blk.args, :(_jl_print_exp(out, exp)))
    # print space padding
    if padding != nothing && contains(flags,'-')
        push(blk.args, _jl_printf_pad(width, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function _jl_printf_c(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print a character:
    #  [cC]: both the same for us (Unicode)
    #
    # flags:
    #  (0): pad left with zeros
    #  (-): left justify
    #
    @gensym x
    blk = expr(:block, :($x = char($x)))
    if width > 1 && !contains(flags,'-')
        p = contains(flags,'0') ? '0' : ' '
        push(blk.args, _jl_printf_pad(width-1, :($width-charwidth($x)), p))
    end
    push(blk.args, :(write(out, $x)))
    if width > 1 && contains(flags,'-')
        push(blk.args, _jl_printf_pad(width-1, :($width-charwidth($x)), ' '))
    end
    :(($x)::Integer), blk
end

function _jl_printf_s(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print a string:
    #  [sS]: both the same for us (Unicode)
    #
    # flags:
    #  (0): pad left with zeros
    #  (-): left justify
    #
    @gensym x
    blk = expr(:block)
    if width > 0
        if !contains(flags,'#')
            push(blk.args, :($x = string($x)))
        else
            push(blk.args, :($x = sshow($x)))
        end
        if !contains(flags,'-')
            push(blk.args, _jl_printf_pad(width, :($width-strwidth($x)), ' '))
        end
        push(blk.args, :(write(out, $x)))
        if contains(flags,'-')
            push(blk.args, _jl_printf_pad(width, :($width-strwidth($x)), ' '))
        end
    else
        if !contains(flags,'#')
            push(blk.args, :(print($x)))
        else
            push(blk.args, :(show($x)))
        end
    end
    :(($x)::Any), blk
end

# TODO: faster pointer printing.

function _jl_printf_p(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print pointer:
    #  [p]: the only option
    #
    @gensym x
    blk = expr(:block)
    ptrwidth = WORD_SIZE>>2
    width -= ptrwidth+2
    if width > 0 && !contains(flags,'-')
        push(blk.args, _jl_printf_pad(width, width, ' '))
    end
    push(blk.args, :(write(out, '0')))
    push(blk.args, :(write(out, 'x')))
    push(blk.args, :(write(out, cstring(hex(unsigned($x), $ptrwidth)))))
    if width > 0 && contains(flags,'-')
        push(blk.args, _jl_printf_pad(width, width, ' '))
    end
    :(($x)::Ptr), blk
end

### core unsigned integer decoding functions ###

macro handle_zero()
    quote
        if x == 0
            _jl_point[1] = 1
            _jl_digits[1] = '0'
            return
        end
    end
end

function _jl_decode_oct(x::Unsigned)
    @handle_zero
    _jl_point[1] = i = div((sizeof(x)<<3)-leading_zeros(x)+2,3)
    while i > 0
        _jl_digits[i] = '0'+(x&0x7)
        x >>= 3
        i -= 1
    end
end

function _jl_decode_0ct(x::Unsigned)
    _jl_point[1] = i = div((sizeof(x)<<3)-leading_zeros(x)+5,3)
    while i > 0
        _jl_digits[i] = '0'+(x&0x7)
        x >>= 3
        i -= 1
    end
end

function _jl_decode_dec(x::Unsigned)
    @handle_zero
    _jl_point[1] = i = ndigits0z(x)
    while i > 0
        _jl_digits[i] = '0'+rem(x,10)
        x = div(x,10)
        i -= 1
    end
end

function _jl_decode_hex(x::Unsigned, symbols::Array{Uint8,1})
    @handle_zero
    _jl_point[1] = i = (sizeof(x)<<1)-(leading_zeros(x)>>2)
    while i > 0
        _jl_digits[i] = symbols[(x&0xf)+1]
        x >>= 4
        i -= 1
    end
end

const _jl_hex_symbols = "0123456789abcdef".data
const _jl_HEX_symbols = "0123456789ABCDEF".data

_jl_decode_hex(x::Unsigned) = _jl_decode_hex(x,_jl_hex_symbols)
_jl_decode_HEX(x::Unsigned) = _jl_decode_hex(x,_jl_HEX_symbols)

### decoding functions directly used by printf generated code ###

# _jl_int_*(x)   => fixed precision, to 0th place, filled out
# _jl_fix_*(x,n) => fixed precision, to nth place, not filled out
# _jl_ini_*(x,n) => n initial digits, filled out
# _jl_sig_*(x,n) => n initial digits, zero-stripped

# alternate versions:
#   _jl_*_0ct(x,n) => ensure that the first octal digits is zero
#   _jl_*_HEX(x,n) => use uppercase digits for hexadecimal

## "int" decoding functions ##
#
# - sets _jl_neg[1]
# - sets _jl_point[1]
# - implies _jl_length[1] = _jl_point[1]
#

_jl_int_oct(x::Unsigned) = (_jl_neg[1]=false; _jl_decode_oct(x))
_jl_int_0ct(x::Unsigned) = (_jl_neg[1]=false; _jl_decode_0ct(x))
_jl_int_dec(x::Unsigned) = (_jl_neg[1]=false; _jl_decode_dec(x))
_jl_int_hex(x::Unsigned) = (_jl_neg[1]=false; _jl_decode_hex(x))
_jl_int_HEX(x::Unsigned) = (_jl_neg[1]=false; _jl_decode_HEX(x))

macro handle_negative()
    quote
        if x < 0
            _jl_neg[1] = true
            x = -x
        else
            _jl_neg[1] = false
        end
    end
end

_jl_int_oct(x::Integer) = (@handle_negative; _jl_decode_oct(unsigned(x)))
_jl_int_0ct(x::Integer) = (@handle_negative; _jl_decode_0ct(unsigned(x)))
_jl_int_dec(x::Integer) = (@handle_negative; _jl_decode_dec(unsigned(x)))
_jl_int_hex(x::Integer) = (@handle_negative; _jl_decode_hex(unsigned(x)))
_jl_int_HEX(x::Integer) = (@handle_negative; _jl_decode_HEX(unsigned(x)))

_jl_int_oct(x::Real) = _jl_int_oct(integer(x)) # TODO: real float decoding.
_jl_int_0ct(x::Real) = _jl_int_0ct(integer(x)) # TODO: real float decoding.
_jl_int_dec(x::Real) = _jl_int_dec(float(x))
_jl_int_hex(x::Real) = _jl_int_hex(integer(x)) # TODO: real float decoding.
_jl_int_HEX(x::Real) = _jl_int_HEX(integer(x)) # TODO: real float decoding.

function _jl_int_dec(x::Float)
    if x == 0.0
        _jl_neg[1] = false
        _jl_point[1] = 1
        _jl_digits[1] = '0'
        return
    end
    @grisu_ccall x GRISU_FIXED 0
    if _jl_length[1] == 0
        _jl_neg[1] = false
        _jl_point[1] = 1
        _jl_digits[1] = '0'
    else
        for i = _jl_length[1]+1:_jl_point[1]
            _jl_digits[i] = '0'
        end
    end
end

## fix decoding functions ##
#
# - sets _jl_neg[1]
# - sets _jl_point[1]
# - sets _jl_length[1]; if less than _jl_point[1], trailing zeros implied
#

_jl_fix_dec(x::Integer, n::Int) = (_jl_int_dec(x); _jl_length[1]=_jl_point[1])
_jl_fix_dec(x::Real, n::Int) = _jl_fix_dec(float(x),n)

function _jl_fix_dec(x::Float, n::Int)
    if n > 17; n = 17; end
    @grisu_ccall x GRISU_FIXED n
    if _jl_length[1] == 0
        _jl_neg[1] = false
        _jl_point[1] = 1
        _jl_digits[1] = '0'
    end
end

## ini decoding functions ##
#
# - sets _jl_neg[1]
# - sets _jl_point[1]
# - implies _jl_length[1] = n (requested digits)
#

function _jl_ini_dec(x::Unsigned, n::Int)
    k = ndigits(x)
    if k <= n
        _jl_point[1] = k
        for i = k:-1:1
            _jl_digits[i] = '0'+rem(x,10)
            x = div(x,10)
        end
        for i = k+1:n
            _jl_digits[i] = '0'
        end
    else
        p = _jl_powers_of_ten[k-n+1]
        r = rem(x,p)
        if r >= (p>>1)
            x += p
            if x >= _jl_powers_of_ten[k+1]
                p *= 10
                k += 1
            end
        end
        _jl_point[1] = k
        x = div(x,p)
        for i = n:-1:1
            _jl_digits[i] = '0'+rem(x,10)
            x = div(x,10)
        end
    end
end

_jl_ini_dec(x::Integer, n::Int) = (@handle_negative; _jl_ini_dec(unsigned(x),n))
_jl_ini_dec(x::Real, n::Int) = _jl_ini_dec(float(x),n)

function _jl_ini_dec(x::Float, n::Int)
    if x == 0.0
        _jl_point[1] = 1
        _jl_neg[1] = signbit(x)
        ccall(:memset, Void, (Ptr{Uint8}, Int32, Int), _jl_digits, '0', n)
    else
        @grisu_ccall x GRISU_PRECISION n
    end
end

## sig decoding functions ##
#
# - sets _jl_neg[1]
# - sets _jl_point[1]
# - sets _jl_length[1]
#

function _jl_sig_dec(x::Unsigned, n::Int)
    if x == 0
        _jl_neg[1] = false
        _jl_point[1] = 1
        _jl_length[1] = 1
        _jl_digits[1] = '0'
        return
    end
    k = ndigits0z(x)
    if k <= n
        _jl_point[1] = k
        for i = k:-1:1
            _jl_digits[i] = '0'+rem(x,10)
            x = div(x,10)
        end
        while _jl_digits[k] == '0'
            k -= 1
        end
        _jl_length[1] = k
    else
        p = _jl_powers_of_ten[k-n+1]
        r = rem(x,p)
        if r >= (p>>1)
            x += p
            if x >= _jl_powers_of_ten[k+1]
                p *= 10
                k += 1
            end
        end
        _jl_point[1] = k
        x = div(x,p)
        for i = n:-1:1
            _jl_digits[i] = '0'+rem(x,10)
            x = div(x,10)
        end
        while _jl_digits[n] == '0'
            n -= 1
        end
        _jl_length[1] = n
    end
end

_jl_sig_dec(x::Integer, n::Int) = (@handle_negative; _jl_sig_dec(unsigned(x),n))
_jl_sig_dec(x::Real, n::Int) = _jl_sig_dec(float(x),n)

function _jl_sig_dec(x::Float, n::Int)
    @grisu_ccall x GRISU_PRECISION n
    if x == 0.0; return; end
    while _jl_digits[n] == '0'
        n -= 1
    end
    _jl_length[1] = n
end

### external printf interface ###

function f_str_f(f)
    args, blk = _jl_printf_gen(f)
    :(($expr(:tuple, args))->($blk))
end

macro f_str(f); f_str_f(f); end

macro printf(f, exps...)
    args, blk = _jl_printf_gen(f)
    if length(args) != length(exps)
        error("printf: wrong number of arguments")
    end
    for i = length(args):-1:1
        arg = args[i].args[1]
        unshift(blk.args, :($arg = $(exps[i])))
    end
    blk
end

fprintf(s::IOStream, f::Function, args...) = f(s, args...)
fprintf(s::IOStream, fmt::String, args...) = fprintf(s, eval(f_str_f(fmt)), args...)
printf(f::Union(Function,String), args...) = fprintf(OUTPUT_STREAM, f, args...)
sprintf(f::Union(Function,String), args...) = sprint(fprintf, f, args...)

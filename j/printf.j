## printf format string => function expression ##

function _jl_printf_gen(s::String)
    args = {}
    blk = expr(:block, :(out = current_output_stream()))
    i = j = start(s)
    while !done(s,j)
        c, k = next(s,j)
        if c == '%'
            if !isempty(s[i:j-1])
                str = check_utf8(unescape_string(s[i:j-1]))
                push(blk.args, :(write(out, $(strlen(str)==1?str[1]:str))))
            end
            flags, width, precision, c, k = _jl_printf_parse1(s,k)
            # TODO: warn about silly flag/conversion combinations
            if contains(flags,'\'')
                error("printf format flag ' not yet supported")
            end
            if c=='a'; error("printf feature %a not yet supported"); end
            if c=='n'; error("printf feature %n not supported"); end
            if c=='%'
                push(blk.args, :(print('%')))
            else
                # construct conversion expression
                C = c; c = lc(c)
                arg, ex = c=='f' ? _jl_printf_f(flags, width, precision, C) :
                          c=='e' ? _jl_printf_e(flags, width, precision, C) :
                          c=='g' ? _jl_printf_g(flags, width, precision, C) :
                          c=='c' ? _jl_printf_c(flags, width, precision, C) :
                          c=='s' ? _jl_printf_s(flags, width, precision, C) :
                          c=='p' ? _jl_printf_p(flags, width, precision, C) :
                                   _jl_printf_d(flags, width, precision, C)
                push(args, arg)
                push(blk.args, ex)
            end
            i = j = k
        else
            j = k
        end
    end
    if !isempty(s[i:])
        str = check_utf8(unescape_string(s[i:]))
        push(blk.args, :(write(out, $(strlen(str)==1?str[1]:str))))
    end
    # return args, exprs
    args = expr(:tuple, args)
    push(blk.args, :nothing)
    return :(($args)->($blk))
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
    error("invalid printf format string: ", show_to_string(s))

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
        error("invalid printf format string: ", show_to_string(s))
    end
    flags, width, precision, c, k
end

## printf formatter generation ##

function _jl_special_handler(flags::ASCIIString, width::Int)
    x = gensym()
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
        i = gensym()
        quote
            $i = $n
            while $i > 0
                write(out,$c)
                $i -= 1
            end
        end
    end
end

function _jl_print_integer(out, pdigits, ndigits, pt)
    if ndigits == 0
        write(out, '0')
    else
        write(out, pdigits, ndigits)
        pt -= ndigits
        while pt > 0
            write(out, '0')
            pt -= 1
        end
    end
end

function _jl_print_fixed(out, pdigits, ndigits, pt, precision)
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
        f = contains(flags,'#') ? :_jl_int8alt : :_jl_int8
        push(blk.args, :((neg,pdigits,ndigits,pt) = ($f)($x)))
    elseif c=='x'
        if contains(flags,'#'); prefix = "0x"; end
        push(blk.args, :((neg,pdigits,ndigits,pt) = _jl_int16($x)))
    elseif c=='X'
        if contains(flags,'#'); prefix = "0X"; end
        push(blk.args, :((neg,pdigits,ndigits,pt) = _jl_int16uc($x)))
    else
        push(blk.args, :((neg,pdigits,ndigits,pt) = _jl_int10($x)))
    end
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
            :($width-pt) : :($width-neg-pt)
        push(blk.args, _jl_printf_pad(width-1, zeros, '0'))
    end
    # print integer
    push(blk.args, :(_jl_print_integer(out,pdigits,ndigits,pt)))
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
    push(blk.args, :((neg,pdigits,ndigits,pt) = _jl_fix10($x,$precision)))
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
        push(blk.args, :(_jl_print_fixed(out,pdigits,ndigits,pt,$precision)))
    else
        push(blk.args, :(_jl_print_integer(out,pdigits,ndigits,pt)))
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
    push(blk.args, :((neg,pdigits,ndigits,pt) = _jl_sig10($x,$(precision+1))))
    push(blk.args, :(exp = pt-1))
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
        if width > 1
            padding = :($width-((exp<=-100)|(100<=exp)))
        end
    else
        if width > 1
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
    push(blk.args, :(write(out, pdigits, 1)))
    if precision > 0
        push(blk.args, :(write(out, '.')))
        push(blk.args, :(write(out, pdigits+1, ndigits-1)))
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

# TODO: implement and use charwidth for wide Unicode character handling
#       replace instances of -1 below with -charwidth($x).

function _jl_printf_c(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print a character:
    #  [cC]: both the same for us (Unicode)
    #
    # flags:
    #  (0): pad left with zeros
    #  (-): left justify
    #
    x = gensym()
    blk = expr(:block)
    if width > 1 && !contains(flags,'-')
        p = contains(flags,'0') ? '0' : ' '
        push(blk.args, _jl_printf_pad(width-1, width-1, p))
    end
    push(blk.args, :(write(out, char($x))))
    if width > 1 && contains(flags,'-')
        push(blk.args, _jl_printf_pad(width-1, width-1, ' '))
    end
    :(($x)::Integer), blk
end

# TODO: implement and use strwidth for wide Unicode character handling
#       replace instances of strlen($x) below with strwidth($x).

function _jl_printf_s(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print a string:
    #  [sS]: both the same for us (Unicode)
    #
    # flags:
    #  (0): pad left with zeros
    #  (-): left justify
    #
    x = gensym()
    blk = expr(:block)
    if width > 0
        if !contains(flags,'#')
            push(blk.args, :($x = string($x)))
        else
            push(blk.args, :($x = show_to_string($x)))
        end
        if !contains(flags,'-')
            push(blk.args, _jl_printf_pad(width, :($width-strlen($x)), ' '))
        end
        push(blk.args, :(write(out, $x)))
        if contains(flags,'-')
            push(blk.args, _jl_printf_pad(width, :($width-strlen($x)), ' '))
        end
    else
        if !contains(flags,'#')
            push(blk.args, :(print($x)))
        else
            push(blk.args, :(show($x)))
        end
    end
    x, blk
end

# TODO: faster pointer printing.

function _jl_printf_p(flags::ASCIIString, width::Int, precision::Int, c::Char)
    # print pointer:
    #  [p]: the only option
    #
    x = gensym()
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

global const _jl_sign   = Array(Bool,1)
global const _jl_digits = Array(Uint8,309+17)
global const _jl_length = Array(Int32,1)
global const _jl_point  = Array(Int32,1)

global const _jl_hex_lc = "0123456789abcdef".data
global const _jl_hex_uc = "0123456789ABCDEF".data

function decode_hex(x::Unsigned, symbols::Array{Uint8,1})
    pt = (sizeof(x)<<1)-(leading_zeros(x)>>2)
    tz = trailing_zeros(x)>>2
    _jl_point[1] = pt
    _jl_length[1] = i = pt-tz
    x >>= tz<<2
    while x > 0
        _jl_digits[i] = symbols[(x&0xf)+1]
        i -= 1; x >>= 4
    end
end
decode_hex(x::Unsigned)    = decode_hex(x,_jl_hex_lc)
decode_hex_uc(x::Unsigned) = decode_hex(x,_jl_hex_uc)

function decode_oct(x::Unsigned)
    pt = div((sizeof(x)<<3)-leading_zeros(x)+2,3)
    tz = div(trailing_zeros(x),3)
    _jl_point[1] = pt
    _jl_length[1] = i = pt-tz
    x >>= 3*tz
    while x > 0
        _jl_digits[i] = '0'+(x&0x7)
        i -= 1; x >>= 3
    end
end

let _digits = Array(Uint8,23) # long enough for oct(typemax(Uint64))+1
_digits[1] = '0' # leading zero for hacky use by octal alternate format (%#o)

# TODO: to be replaced with more efficient integer decoders...

global _jl_fix, _jl_sig, _jl_fix8alt, _jl_int10

function _jl_fix(base::Int, x::Integer, n::Int, u::Bool)
    digits = int2str(abs(x), base)
    if u; digits = uc(digits); end
    ndigits = 1
    for i = 1:strlen(digits)
        if digits[i]!='0'; ndigits=i; end
        _digits[i+1] = digits[i]
    end
    (x < 0, pointer(_digits)+1, ndigits, strlen(digits))
end
_jl_fix(base::Int, x::Integer, n::Int) = _jl_fix(base,x,n,false)

function _jl_sig(base::Int, x::Integer, n::Int)
    digits = int2str(abs(x), base)
    if strlen(digits) > n && digits[n+1]-'0' >= base/2
        digits.data[n] += 1
    end
    ndigits = 1
    for i = 1:n
        if digits[i]!='0'; ndigits=i; end
        _digits[i+1] = digits[i]
    end
    (x < 0, pointer(_digits)+1, ndigits, strlen(digits))
end

function _jl_fix8alt(x::Integer, n::Int)
    neg, pdigits, ndigits, pt = _jl_fix(8,x,n)
    if ndigits > 0 && _digits[2] != '0'
        pdigits -= 1
        ndigits += 1
        pt += 1
    end
    neg, pdigits, ndigits, pt
end

function _jl_int10(x::Real)
    if x == 0.0
        return false, pointer(_digits), 1, 1
    end
    grisu_fix(x,0)
end

end # let

_jl_int8(x::Integer) = _jl_fix8(x,0)
_jl_int8alt(x::Integer) = _jl_fix8alt(x,0)
_jl_int10(x::Integer) = _jl_fix10(x,0)
_jl_int16(x::Integer) = _jl_fix16(x,0)
_jl_int16uc(x::Integer) = _jl_fix16uc(x,0)

_jl_fix8(x::Integer, n::Int) = _jl_fix(8,x,n)
_jl_fix10(x::Integer, n::Int) = _jl_fix(10,x,n)
_jl_fix16(x::Integer, n::Int) = _jl_fix(16,x,n)
_jl_fix16uc(x::Integer, n::Int) = _jl_fix(16,x,n,true)

_jl_sig8 (x::Integer, n::Int) = _jl_fix(8,x,n)
_jl_sig10(x::Integer, n::Int) = _jl_fix(10,x,n)
_jl_sig16(x::Integer, n::Int) = _jl_fix(16,x,n)
_jl_sig16uc(x::Integer, n::Int) = _jl_fix(16,x,n,true)

_jl_int8(x::Real)    = _jl_int8(integer(x))    # TODO: replace with float decoding
_jl_int8alt(x::Real) = _jl_int8alt(integer(x)) # TODO: replace with float decoding
_jl_int16(x::Real)   = _jl_int16(integer(x))   # TODO: replace with float decoding
_jl_int16uc(x::Real) = _jl_int16uc(integer(x)) # TODO: replace with float decoding

_jl_fix8(x::Real, n::Int)    = error("octal float formatting not supported")
_jl_fix16(x::Real, n::Int)   = error("hex float formatting not implemented")
_jl_fix16uc(x::Real, n::Int) = error("hex float formatting not implemented")

_jl_sig8(x::Real, n::Int)    = error("octal float formatting not supported")
_jl_sig16(x::Real, n::Int)   = error("hex float formatting not implemented")
_jl_sig16uc(x::Real, n::Int) = error("hex float formatting not implemented")

_jl_fix10(x::Real, n::Int) = grisu_fix(x,n)
_jl_sig10(x::Real, n::Int) = grisu_sig(x,n)

## external printf interface ##

macro f_str(f); _jl_printf_gen(f); end

printf(f::Function, args...) = f(args...)
printf(f::String,   args...) = eval(_jl_printf_gen(f))(args...)
printf(s::IOStream, args...) = with_output_stream(s, printf, args...)

sprintf(f::Function, args...) = print_to_string(printf, f, args...)
sprintf(f::String,   args...) = print_to_string(printf, f, args...)

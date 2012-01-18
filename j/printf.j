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
    if !contains("diouxXDOUeEfFgGaAcCsSn", c)
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

function _jl_printf_pad(m::Int, n::Union(Symbol,Expr), c::Char)
    if m <= 1
        :($n > 0 && print($c))
    else
        i = gensym()
        quote
            $i = $n
            while $i > 0
                print($c)
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

function _jl_printf_f(flags::ASCIIString, width::Int, precision::Int, C::Char)
    # print to fixed trailing precision
    #  (#): always print a decimal point
    #  (0): pad left with zeros
    #  (-): left justify
    #  ( ): precede non-negative values with " "
    #  (+): precede non-negative values with "+"
    x, ex, blk = _jl_special_handler(flags,width)
    # interpret the number
    if precision < 0; precision = 6; end
    push(blk.args, :((neg,pdigits,ndigits,pt) = _jl_fix10($x,$precision)))
    # calculate padding
    padding = nothing
    if width > 1
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
    end
    # print space padding
    if padding != nothing && !contains(flags,'-') && !contains(flags,'0')
        push(blk.args, _jl_printf_pad(width-1, padding, ' '))
    end
    # print sign
    contains(flags,'+') ? push(blk.args, :(print(neg?'-':'+'))) :
    contains(flags,' ') ? push(blk.args, :(print(neg?'-':' '))) :
                          push(blk.args, :(neg && print('-')))
    # print zero padding
    if padding != nothing && !contains(flags,'-') && contains(flags,'0')
        push(blk.args, _jl_printf_pad(width-1, padding, '0'))
    end
    # print digits
    if precision > 0
        push(blk.args, :(_jl_print_fixed(out,pdigits,ndigits,pt,$precision)))
    else
        push(blk.args, :(_jl_print_integer(out,pdigits,ndigits,pt)))
        contains(flags,'#') && push(blk.args, :(print('.')))
    end
    # print space padding
    if padding != nothing && contains(flags,'-')
        push(blk.args, _jl_printf_pad(width-1, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

let _digits = Array(Uint8,20) # long enough for typemax(Uint64)

# TODO: to be replaced with more efficient integer decoders...

global _jl_fix, _jl_sig

function _jl_fix(base::Int, x::Integer, n::Int)
    digits = int2str(abs(x), base)
    ndigits = 0
    for i = 1:length(digits)
        if digits[i]!='0'; ndigits=i; end
        _digits[i] = digits[i]
    end
    (x < 0, pointer(_digits), ndigits, length(digits))
end

function _jl_sig(base::Int, x::Integer, n::Int)
    digits = int2str(abs(x), base)
    if length(digits) > n && digits[n+1]-'0' >= base/2
        digits.data[n] += 1
    end
    ndigits = 0
    for i = 1:n
        if digits[i]!='0'; ndigits=i; end
        _digits[i] = digits[i]
    end
    (x < 0, pointer(_digits), ndigits, length(digits))
end

end # let

_jl_fix8 (x::Integer, n::Int) = _jl_fix(8 , x, n)
_jl_fix10(x::Integer, n::Int) = _jl_fix(10, x, n)
_jl_fix16(x::Integer, n::Int) = _jl_fix(16, x, n)

_jl_sig8 (x::Integer, n::Int) = _jl_fix(8 , x, n)
_jl_sig10(x::Integer, n::Int) = _jl_fix(10, x, n)
_jl_sig16(x::Integer, n::Int) = _jl_fix(16, x, n)

_jl_fix8 (x::Real, n::Int) = error("octal float formatting not supported")
_jl_fix10(x::Real, n::Int) = grisu_fix(x, n)
_jl_fix16(x::Real, n::Int) = error("hex float formatting not implemented")

_jl_sig8 (x::Real, n::Int) = error("octal float formatting not supported")
_jl_sig10(x::Real, n::Int) = grisu_sig(x, n)
_jl_sig16(x::Real, n::Int) = error("hex float formatting not implemented")

## external printf interface ##

macro f_str(f); _jl_printf_gen(f); end

printf(f::Function, args...) = f(args...)
printf(f::String,   args...) = eval(_jl_printf_gen(f))(args...)
printf(s::IOStream, args...) = with_output_stream(s, printf, args...)

sprintf(f::Function, args...) = print_to_string(printf, f, args...)
sprintf(f::String,   args...) = print_to_string(printf, f, args...)

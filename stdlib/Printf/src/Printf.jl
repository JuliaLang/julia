# This file is a part of Julia. License is MIT: https://julialang.org/license

module Printf

export @printf, @sprintf
using Base.Grisu
using Base.GMP
using Unicode: textwidth

### printf formatter generation ###
const SmallFloatingPoint = Union{Float64,Float32,Float16}
const SmallNumber = Union{SmallFloatingPoint,Base.BitInteger}

function gen(s::AbstractString)
    args = []
    blk = Expr(:block, :(local neg, pt, len, exp, do_out, args, buf))
    gotbuf = false
    for x in parse(s)
        if isa(x,AbstractString)
            push!(blk.args, :(print(out, $(length(x)==1 ? x[1] : x))))
        else
            c = lowercase(x[end])
            f = c=='f' ? gen_f :
                c=='e' ? gen_e :
                c=='a' ? gen_a :
                c=='g' ? gen_g :
                c=='c' ? gen_c :
                c=='s' ? gen_s :
                c=='p' ? gen_p :
                         gen_d
            if !gotbuf && c != 'c' && c != 's' && c != 'p'
                push!(blk.args, :(buf = $Grisu.getbuf()))
                gotbuf = true
            end
            arg, ex = f(x...)
            push!(args, arg)
            push!(blk.args, ex)
        end
    end
    push!(blk.args, :nothing)
    return args, blk
end

### printf format string parsing ###

function parse(s::AbstractString)
    # parse format string into strings and format tuples
    list = []
    a = Iterators.Stateful(pairs(s))
    lastparse = firstindex(s)
    lastidx = 0 # invariant: lastidx == prevind(s, idx)
    for (idx, c) in a
        if c == '%'
            lastparse > lastidx || push!(list, s[lastparse:lastidx])
            flags, width, precision, conversion = parse1!(s,  a)
            '\'' in flags && error("printf format flag ' not yet supported")
            conversion == 'n'    && error("printf feature %n not supported")
            push!(list, conversion == '%' ? "%" : (flags,width,precision,conversion))
            lastparse = isempty(a) ? lastindex(s)+1 : peek(a)[1]
        end
        lastidx = idx
    end
    lastparse > lastindex(s) || push!(list, s[lastparse:end])
    # coalesce adjacent strings
    i = j = 1
    while i < length(list)
        if isa(list[i],AbstractString)
            for outer j = i+1:length(list)
                if !isa(list[j],AbstractString)
                    j -= 1
                    break
                end
                list[i] *= list[j]
            end
            deleteat!(list,i+1:j)
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

pop_or_die!(s, a) = !isempty(a) ? popfirst!(a) :
    throw(ArgumentError("invalid printf format string: $(repr(s))"))

function parse1!(s, a)
    width = 0
    precision = -1
    k, c = pop_or_die!(s, a)
    j = k
    # handle %%
    if c == '%'
        return "", width, precision, c
    end
    # parse flags
    while c in "#0- + '"
        k, c = pop_or_die!(s, a)
    end
    flags = String(s[j:k-1]) # All flags are 1 byte
    # parse width
    while '0' <= c <= '9'
        width = 10*width + c-'0'
        _, c = pop_or_die!(s, a)
    end
    # parse precision
    if c == '.'
        _, c = pop_or_die!(s, a)
        if '0' <= c <= '9'
            precision = 0
            while '0' <= c <= '9'
                precision = 10*precision + c-'0'
                _, c = pop_or_die!(s, a)
            end
        end
    end
    # parse length modifer (ignored)
    if c == 'h' || c == 'l'
        prev = c
        _, c = pop_or_die!(s, a)
        if c == prev
            _, c = pop_or_die!(s, a)
        end
    elseif c in "Ljqtz"
        _, c = pop_or_die!(s, a)
    end
    # validate conversion
    if !(c in "diouxXDOUeEfFgGaAcCsSpn")
        throw(ArgumentError("invalid printf format string: $(repr(s))"))
    end
    # TODO: warn about silly flag/conversion combinations
    flags, width, precision, c
end

### printf formatter generation ###

function special_handler(flags::String, width::Int)
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
    ex = :(isfinite($x) ? $blk : print(out, $abn))
    x, ex, blk
end

function pad(m::Int, n, c::Char)
    if m <= 1
        :($n > 0 && print(out,$c))
    else
        @gensym i
        quote
            $i = $n
            while $i > 0
                print(out,$c)
                $i -= 1
            end
        end
    end
end

function dynamic_pad(m, val, c::Char)
    @gensym i
    quote
        if $m <= 1
            $val > 0 && print(out,$c)
        else
            $i = $val
            while $i > 0
                print(out,$c)
                $i -= 1
            end
        end
    end
end

# returns the number of (ASCII) chars output by print_fixed
function print_fixed_width(precision, pt, ndigits, trailingzeros=true)
    count = 0
    if pt <= 0
        # 0.0dddd0
        count += 2
        precision += pt
        if pt < 0
            count -= pt
        end
        count += ndigits
        precision -= ndigits
    elseif ndigits <= pt
        # dddd000.000000
        count += ndigits
        if ndigits < pt
            count += pt - ndigits
        end
        count += trailingzeros
    else # 0 < pt < ndigits
        # dd.dd0000
        ndigits -= pt
        count += pt + 1 + ndigits
        precision -= ndigits
    end
    if trailingzeros && precision > 0
        count += precision
    end
    return count
end

# note: if print_fixed is changed, print_fixed_width should be changed accordingly
function print_fixed(out, precision, pt, ndigits, trailingzeros=true, buf = Grisu.getbuf())
    pdigits = pointer(buf)
    if pt <= 0
        # 0.0dddd0
        print(out, '0')
        print(out, '.')
        precision += pt
        while pt < 0
            print(out, '0')
            pt += 1
        end
        unsafe_write(out, pdigits, ndigits)
        precision -= ndigits
    elseif ndigits <= pt
        # dddd000.000000
        unsafe_write(out, pdigits, ndigits)
        while ndigits < pt
            print(out, '0')
            ndigits += 1
        end
        if trailingzeros
            print(out, '.')
        end
    else # 0 < pt < ndigits
        # dd.dd0000
        ndigits -= pt
        unsafe_write(out, pdigits, pt)
        print(out, '.')
        unsafe_write(out, pdigits+pt, ndigits)
        precision -= ndigits
    end
    if trailingzeros
        while precision > 0
            print(out, '0')
            precision -= 1
        end
    end
end

function print_exp_e(out, exp::Integer)
    print(out, exp < 0 ? '-' : '+')
    exp = abs(exp)
    d = div(exp,100)
    if d > 0
        if d >= 10
            print(out, exp)
            return
        end
        print(out, Char('0'+d))
    end
    exp = rem(exp,100)
    print(out, Char('0'+div(exp,10)))
    print(out, Char('0'+rem(exp,10)))
end

function print_exp_a(out, exp::Integer)
    print(out, exp < 0 ? '-' : '+')
    exp = abs(exp)
    print(out, exp)
end


function gen_d(flags::String, width::Int, precision::Int, c::Char)
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
        fn = '#' in flags ? :decode_0ct : :decode_oct
    elseif c=='x'
        '#' in flags && (prefix = "0x")
        fn = :decode_hex
    elseif c=='X'
        '#' in flags && (prefix = "0X")
        fn = :decode_HEX
    else
        fn = :decode_dec
    end
    push!(blk.args, :((do_out, args) = $fn(out, $x, $flags, $width, $precision, $c, buf)))
    ifblk = Expr(:if, :do_out, Expr(:block))
    push!(blk.args, ifblk)
    blk = ifblk.args[2]
    push!(blk.args, :((len, pt, neg) = args))
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
    if padding !== nothing && !('-' in flags)
        push!(blk.args, pad(width-precision, padding, ' '))
    end
    # print sign
    '+' in flags ? push!(blk.args, :(print(out, neg ? '-' : '+'))) :
    ' ' in flags ? push!(blk.args, :(print(out, neg ? '-' : ' '))) :
                   push!(blk.args, :(neg && print(out, '-')))
    # print prefix
    for ch in prefix
        push!(blk.args, :(print(out, $ch)))
    end
    # print zero padding & leading zeros
    if space_pad && precision > 1
        push!(blk.args, pad(precision-1, :($precision-pt), '0'))
    elseif !space_pad && width > 1
        zeros = '+' in flags || ' ' in flags ? :($(width-1)-pt) : :($width-neg-pt)
        push!(blk.args, pad(width-1, zeros, '0'))
    end
    # print integer
    push!(blk.args, :(unsafe_write(out, pointer(buf), pt)))
    # print padding
    if padding !== nothing && '-' in flags
        push!(blk.args, pad(width-precision, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function gen_f(flags::String, width::Int, precision::Int, c::Char)
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
    push!(blk.args, :((do_out, args) = fix_dec(out, $x, $flags, $width, $precision, $c, buf)))
    ifblk = Expr(:if, :do_out, Expr(:block))
    push!(blk.args, ifblk)
    blk = ifblk.args[2]
    push!(blk.args, :((len, pt, neg) = args))
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
    if padding !== nothing && !('-' in flags) && !('0' in flags)
        push!(blk.args, pad(width-1, padding, ' '))
    end
    # print sign
    '+' in flags ? push!(blk.args, :(print(out, neg ? '-' : '+'))) :
    ' ' in flags ? push!(blk.args, :(print(out, neg ? '-' : ' '))) :
                   push!(blk.args, :(neg && print(out, '-')))
    # print zero padding
    if padding !== nothing && !('-' in flags) && '0' in flags
        push!(blk.args, pad(width-1, padding, '0'))
    end
    # print digits
    if precision > 0
        push!(blk.args, :(print_fixed(out,$precision,pt,len,true,buf)))
    else
        push!(blk.args, :(unsafe_write(out, pointer(buf), len)))
        push!(blk.args, :(while pt >= (len+=1) print(out,'0') end))
        '#' in flags && push!(blk.args, :(print(out, '.')))
    end
    # print space padding
    if padding !== nothing && '-' in flags
        push!(blk.args, pad(width-1, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function gen_e(flags::String, width::Int, precision::Int, c::Char, inside_g::Bool=false)
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
    x, ex, blk = if inside_g
        @gensym x
        blk = Expr(:block)
        x, blk, blk
    else
        special_handler(flags,width)
    end
    # interpret the number
    if precision < 0; precision = 6; end
    ndigits = min(precision+1,length(Grisu.getbuf())-1)
    push!(blk.args, :((do_out, args) = ini_dec(out,$x,$ndigits, $flags, $width, $precision, $c, buf)))
    push!(blk.args, :(digits = buf))
    ifblk = Expr(:if, :do_out, Expr(:block))
    push!(blk.args, ifblk)
    blk = ifblk.args[2]
    push!(blk.args, :((len, pt, neg) = args))
    push!(blk.args, :(exp = pt-1))
    expmark = isuppercase(c) ? "E" : "e"
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
            padding = quote
                padn=$width
                if (exp<=-100)|(100<=exp)
                    if isa($x,SmallNumber)
                        padn -= 1
                    else
                        padn -= Base.ndigits0z(exp) - 2
                    end
                end
                padn
            end
        end
    else
        if width > 0
            padding = quote
                padn=$width-neg
                if (exp<=-100)|(100<=exp)
                    if isa($x,SmallNumber)
                        padn -= 1
                    else
                        padn -= Base.ndigits0z(exp) - 2
                    end
                end
                padn
            end
        end
    end
    # print space padding
    if padding !== nothing && !('-' in flags) && !('0' in flags)
        push!(blk.args, pad(width, padding, ' '))
    end
    # print sign
    '+' in flags ? push!(blk.args, :(print(out, neg ? '-' : '+'))) :
    ' ' in flags ? push!(blk.args, :(print(out, neg ? '-' : ' '))) :
                   push!(blk.args, :(neg && print(out, '-')))
    # print zero padding
    if padding !== nothing && !('-' in flags) && '0' in flags
        push!(blk.args, pad(width, padding, '0'))
    end
    # print digits
    push!(blk.args, :(write(out, digits[1])))
    if precision > 0
        if inside_g && !('#' in flags)
            push!(blk.args, :(endidx = $ndigits;
                              while endidx > 1 && digits[endidx] == UInt8('0')
                                  endidx -= 1
                              end;
                              if endidx > 1
                                  print(out, '.')
                                  unsafe_write(out, pointer(digits)+1, endidx-1)
                              end
                              ))
        else
            push!(blk.args, :(print(out, '.')))
            push!(blk.args, :(unsafe_write(out, pointer(digits)+1, $(ndigits-1))))
            if ndigits < precision+1
                n = precision+1-ndigits
                push!(blk.args, pad(n, n, '0'))
            end
        end
    end
    for ch in expmark
        push!(blk.args, :(print(out, $ch)))
    end
    push!(blk.args, :(print_exp_e(out, exp)))
    # print space padding
    if padding !== nothing && '-' in flags
        push!(blk.args, pad(width, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function gen_a(flags::String, width::Int, precision::Int, c::Char)
    # print float in hexadecimal format
    #  [a]: lowercase hex float, e.g. -0x1.cfp-2
    #  [A]: uppercase hex float, e.g. -0X1.CFP-2
    #
    # flags:
    #  (#): always print a decimal point
    #  (0): pad left with zeros
    #  (-): left justify
    #  ( ): precede non-negative values with " "
    #  (+): precede non-negative values with "+"
    #
    x, ex, blk = special_handler(flags,width)
    if c == 'A'
        hexmark, expmark = "0X", "P"
        fn = :ini_HEX
    else
        hexmark, expmark = "0x", "p"
        fn = :ini_hex
    end
    # if no precision, print max non-zero
    if precision < 0
        push!(blk.args, :((do_out, args) = $fn(out,$x, $flags, $width, $precision, $c, buf)))
    else
        ndigits = min(precision+1,length(Grisu.getbuf())-1)
        push!(blk.args, :((do_out, args) = $fn(out,$x,$ndigits, $flags, $width, $precision, $c, buf)))
    end
    push!(blk.args, :(digits = buf))
    ifblk = Expr(:if, :do_out, Expr(:block))
    push!(blk.args, ifblk)
    blk = ifblk.args[2]
    push!(blk.args, :((len, exp, neg) = args))
    if precision==0 && '#' in flags
        expmark = string(".",expmark)
    end
    # calculate padding
    padding = nothing
    if precision > 0
        width -= precision+length(hexmark)+length(expmark)+4
        # 4 = leading + expsign + 1 exp digit + decimal
    else
        width -= length(hexmark)+length(expmark)+3+(precision<0 && '#' in flags)
        # 3 = leading + expsign + 1 exp digit
    end
    if '+' in flags || ' ' in flags
        width -= 1 # for the sign indicator
        if width > 0
            padding = :($(width+1) - Base.ndigits(exp))
        end
    else
        if width > 0
            padding = :($(width+1) - neg - Base.ndigits(exp))
        end
    end
    if precision < 0 && width > 0
        if '#' in flags
            padding = :($padding - (len-1))
        else
            padding = :($padding - (len>1 ? len : 0))
        end
    end
    # print space padding
    if padding !== nothing && !('-' in flags) && !('0' in flags)
        push!(blk.args, pad(width, padding, ' '))
    end
    # print sign
    '+' in flags ? push!(blk.args, :(print(out, neg ? '-' : '+'))) :
    ' ' in flags ? push!(blk.args, :(print(out, neg ? '-' : ' '))) :
                    push!(blk.args, :(neg && print(out, '-')))
    # hex prefix
    for ch in hexmark
        push!(blk.args, :(print(out, $ch)))
    end
    # print zero padding
    if padding !== nothing && !('-' in flags) && '0' in flags
        push!(blk.args, pad(width, padding, '0'))
    end
    # print digits: assumes ASCII/UTF8 encoding of digits is okay for `out`
    push!(blk.args, :(write(out, digits[1])))
    if precision > 0
        push!(blk.args, :(print(out, '.')))
        push!(blk.args, :(unsafe_write(out, pointer(digits)+1, $(ndigits-1))))
        if ndigits < precision+1
            n = precision+1-ndigits
            push!(blk.args, pad(n, n, '0'))
        end
    elseif precision < 0
        ifvpblk = Expr(:if, :(len > 1), Expr(:block))
        vpblk = ifvpblk.args[2]
        if '#' in flags
            push!(blk.args, :(print(out, '.')))
        else
            push!(vpblk.args, :(print(out, '.')))
        end
        push!(vpblk.args, :(unsafe_write(out, pointer(digits)+1, len-1)))
        push!(blk.args, ifvpblk)
    end
    for ch in expmark
        push!(blk.args, :(print(out, $ch)))
    end
    push!(blk.args, :(print_exp_a(out, exp)))
    # print space padding
    if padding !== nothing && '-' in flags
        push!(blk.args, pad(width, padding, ' '))
    end
    # return arg, expr
    :(($x)::Real), ex
end

function gen_c(flags::String, width::Int, precision::Int, c::Char)
    # print a character:
    #  [cC]: both the same for us (Unicode)
    #
    # flags:
    #  (0): pad left with zeros
    #  (-): left justify
    #
    @gensym x
    blk = Expr(:block, :($x = Char($x)))
    if width > 1 && !('-' in flags)
        p = '0' in flags ? '0' : ' '
        push!(blk.args, pad(width-1, :($width-textwidth($x)), p))
    end
    push!(blk.args, :(print(out, $x)))
    if width > 1 && '-' in flags
        push!(blk.args, pad(width-1, :($width-textwidth($x)), ' '))
    end
    :(($x)::Integer), blk
end

function _limit(s, prec)
    prec >= sizeof(s) && return s
    p = prevind(s, prec+1)
    n = nextind(s, p)-1
    s[1:(prec>=n ? n : prevind(s,p))]
end

function gen_s(flags::String, width::Int, precision::Int, c::Char)
    # print a string:
    #  [sS]: both the same for us (Unicode)
    #
    # flags:
    #  (-): left justify
    #  (#): use `show`/`repr` instead of `print`/`string`
    #
    @gensym x
    blk = Expr(:block)
    if width > 0
        if !('#' in flags)
            push!(blk.args, :($x = string($x)))
        else
            push!(blk.args, :($x = repr($x)))
        end
        if precision!=-1
            push!(blk.args, :($x = _limit($x, $precision)))
        end
        if !('-' in flags)
            push!(blk.args, pad(width, :($width-textwidth($x)), ' '))
        end
        push!(blk.args, :(print(out, $x)))
        if '-' in flags
            push!(blk.args, pad(width, :($width-textwidth($x)), ' '))
        end
    else
        if precision!=-1
            push!(blk.args, :(io = IOBuffer()))
        else
            push!(blk.args, :(io = out))
        end
        if !('#' in flags)
            push!(blk.args, :(print(io, $x)))
        else
            push!(blk.args, :(show(io, $x)))
        end
        if precision!=-1
            push!(blk.args, :(print(out, _limit(String(take!(io)), $precision))))
        end
    end
    :(($x)::Any), blk
end

# TODO: faster pointer printing.

function gen_p(flags::String, width::Int, precision::Int, c::Char)
    # print pointer:
    #  [p]: the only option
    #
    # flags:
    #  (-): left justify
    #
    @gensym x
    blk = Expr(:block)
    ptrwidth = Sys.WORD_SIZE>>2
    width -= ptrwidth+2
    if width > 0 && !('-' in flags)
        push!(blk.args, pad(width, width, ' '))
    end
    push!(blk.args, :(print(out, '0')))
    push!(blk.args, :(print(out, 'x')))
    push!(blk.args, :(print(out, String(string(unsigned($x), pad = $ptrwidth, base = 16)))))
    if width > 0 && '-' in flags
        push!(blk.args, pad(width, width, ' '))
    end
    :(($x)::Ptr), blk
end

function gen_g(flags::String, width::Int, precision::Int, c::Char)
    # print to fixed trailing precision
    #  [g]: lower case e on scientific
    #  [G]: Upper case e on scientific
    #
    # flags
    #  (#): always print a decimal point
    #  (0): pad left with zeros
    #  (-): left justify
    #  ( ): precede non-negative values with " "
    #  (+): precede non-negative values with "+"
    #
    x, ex, blk = special_handler(flags,width)
    if precision < 0; precision = 6; end
    ndigits = min(precision+1,length(Grisu.getbuf())-1)
    # See if anyone else wants to handle it
    push!(blk.args, :((do_out, args) = ini_dec(out,$x,$ndigits, $flags, $width, $precision, $c, buf)))
    ifblk = Expr(:if, :do_out, Expr(:block))
    push!(blk.args, ifblk)
    blk = ifblk.args[2]
    push!(blk.args, :((len, pt, neg) = args))
    push!(blk.args, :(exp = pt-1))
    push!(blk.args, :(do_f = $precision > exp >= -4)) # Should we interpret like %f or %e?
    feblk = Expr(:if, :do_f, Expr(:block), Expr(:block))
    push!(blk.args, feblk)
    fblk = feblk.args[2]
    eblk = feblk.args[3]

    ### %f branch
    # Follow the same logic as gen_f() but more work has to be deferred until runtime
    # because precision is unknown until then.
    push!(fblk.args, :(fprec = $precision - (exp+1)))
    push!(fblk.args, :((do_out, args) = fix_dec(out, $x, $flags, $width, fprec, $c - 1, buf)))
    fifblk = Expr(:if, :do_out, Expr(:block))
    push!(fblk.args, fifblk)
    blk = fifblk.args[2]
    push!(blk.args, :((len, pt, neg) = args))
    push!(blk.args, :(padding = 0))
    push!(blk.args, :(width = $width))
    # need to compute value before left-padding since trailing zeros are elided
    push!(blk.args, :(width -= print_fixed_width(fprec,pt,len,$('#' in flags))))
    if '+' in flags || ' ' in flags
        push!(blk.args, :(width -= 1))
    else
        push!(blk.args, :(if neg width -= 1; end))
    end
    push!(blk.args, :(if width >= 1 padding = width; end))
    # print space padding
    if !('-' in flags) && !('0' in flags)
        padexpr = dynamic_pad(:width, :padding, ' ')
        push!(blk.args, :(if padding > 0
                          $padexpr; end))
    end
    # print sign
    '+' in flags ? push!(blk.args, :(print(out, neg ? '-' : '+'))) :
    ' ' in flags ? push!(blk.args, :(print(out, neg ? '-' : ' '))) :
                   push!(blk.args, :(neg && print(out, '-')))
    # print zero padding
    if !('-' in flags) && '0' in flags
        padexpr = dynamic_pad(:width, :padding, '0')
        push!(blk.args, :(if padding > 0
                          $padexpr; end))
    end
    # finally print value
    push!(blk.args, :(print_fixed(out,fprec,pt,len,$('#' in flags),buf)))
    # print space padding
    if '-' in flags
        padexpr = dynamic_pad(:width, :padding, ' ')
        push!(blk.args, :(if padding > 0
                          $padexpr; end))
    end

    ### %e branch
    # Here we can do all the work at macro expansion time
    var, eex = gen_e(flags, width, precision-1, c, true)
    push!(eblk.args, :($(var.args[1]) = $x))
    push!(eblk.args, eex)

    :(($x)::Real), ex
end

### core unsigned integer decoding functions ###

macro handle_zero(ex, digits)
    quote
        if $(esc(ex)) == 0
            $(esc(digits))[1] = '0'
            return Int32(1), Int32(1), $(esc(:neg))
        end
    end
end

decode_oct(out, d, flags::String, width::Int, precision::Int, c::Char, digits) = (true, decode_oct(d, digits))
decode_0ct(out, d, flags::String, width::Int, precision::Int, c::Char, digits) = (true, decode_0ct(d, digits))
decode_dec(out, d, flags::String, width::Int, precision::Int, c::Char, digits) = (true, decode_dec(d, digits))
decode_hex(out, d, flags::String, width::Int, precision::Int, c::Char, digits) = (true, decode_hex(d, digits))
decode_HEX(out, d, flags::String, width::Int, precision::Int, c::Char, digits) = (true, decode_HEX(d, digits))
fix_dec(out, d, flags::String, width::Int, precision::Int, c::Char, digits) = (true, fix_dec(d, precision, digits))
ini_dec(out, d, ndigits::Int, flags::String, width::Int, precision::Int, c::Char, digits) = (true, ini_dec(d, ndigits, digits))
ini_hex(out, d, ndigits::Int, flags::String, width::Int, precision::Int, c::Char, digits) = (true, ini_hex(d, ndigits, digits))
ini_HEX(out, d, ndigits::Int, flags::String, width::Int, precision::Int, c::Char, digits) = (true, ini_HEX(d, ndigits, digits))
ini_hex(out, d, flags::String, width::Int, precision::Int, c::Char, digits) = (true, ini_hex(d, digits))
ini_HEX(out, d, flags::String, width::Int, precision::Int, c::Char, digits) = (true, ini_HEX(d, digits))


# fallbacks for Real types without explicit decode_* implementation
decode_oct(d::Real, digits) = decode_oct(Integer(d), digits)
decode_0ct(d::Real, digits) = decode_0ct(Integer(d), digits)
decode_dec(d::Real, digits) = decode_dec(Integer(d), digits)
decode_hex(d::Real, digits) = decode_hex(Integer(d), digits)
decode_HEX(d::Real, digits) = decode_HEX(Integer(d), digits)

handlenegative(d::Unsigned) = (false, d)
function handlenegative(d::Integer)
    if d < 0
        return true, unsigned(oftype(d,-d))
    else
        return false, unsigned(d)
    end
end

function decode_oct(d::Integer, digits)
    neg, x = handlenegative(d)
    @handle_zero x digits
    pt = i = div((sizeof(x)<<3)-leading_zeros(x)+2,3)
    while i > 0
        digits[i] = 48+(x&0x7)
        x >>= 3
        i -= 1
    end
    return Int32(pt), Int32(pt), neg
end

function decode_0ct(d::Integer, digits)
    neg, x = handlenegative(d)
    # doesn't need special handling for zero
    pt = i = div((sizeof(x)<<3)-leading_zeros(x)+5,3)
    while i > 0
        digits[i] = 48+(x&0x7)
        x >>= 3
        i -= 1
    end
    return Int32(pt), Int32(pt), neg
end

function decode_dec(d::Integer, digits)
    neg, x = handlenegative(d)
    @handle_zero x digits
    pt = i = Base.ndigits0z(x)
    while i > 0
        digits[i] = 48+rem(x,10)
        x = div(x,10)
        i -= 1
    end
    return Int32(pt), Int32(pt), neg
end

function decode_hex(d::Integer, symbols::AbstractArray{UInt8,1}, digits)
    neg, x = handlenegative(d)
    @handle_zero x digits
    pt = i = (sizeof(x)<<1)-(leading_zeros(x)>>2)
    while i > 0
        digits[i] = symbols[(x&0xf)+1]
        x >>= 4
        i -= 1
    end
    return Int32(pt), Int32(pt), neg
end

const hex_symbols = b"0123456789abcdef"
const HEX_symbols = b"0123456789ABCDEF"

decode_hex(x::Integer, digits) = decode_hex(x,hex_symbols,digits)
decode_HEX(x::Integer, digits) = decode_hex(x,HEX_symbols,digits)

function decode(b::Int, x::BigInt, digits)
    neg = x.size < 0
    pt = Base.ndigits(x, base=abs(b))
    length(digits) < pt+1 && resize!(digits, pt+1)
    neg && (x.size = -x.size)
    GMP.MPZ.get_str!(digits, b, x)
    neg && (x.size = -x.size)
    return Int32(pt), Int32(pt), neg
end
decode_oct(x::BigInt, digits) = decode(8, x, digits)
decode_dec(x::BigInt, digits) = decode(10, x, digits)
decode_hex(x::BigInt, digits) = decode(16, x, digits)
decode_HEX(x::BigInt, digits) = decode(-16, x, digits)

function decode_0ct(x::BigInt, digits)
    neg = x.size < 0
    digits[1] = '0'
    if x.size == 0
        return Int32(1), Int32(1), neg
    end
    pt = Base.ndigits0z(x, 8) + 1
    length(digits) < pt+1 && resize!(digits, pt+1)
    neg && (x.size = -x.size)
    p = convert(Ptr{UInt8}, digits) + 1
    GMP.MPZ.get_str!(p, 8, x)
    neg && (x.size = -x.size)
    return neg, Int32(pt), Int32(pt)
end

### decoding functions directly used by printf generated code ###

# decode_*(x)=> fixed precision, to 0th place, filled out
# fix_*(x,n) => fixed precision, to nth place, not filled out
# ini_*(x,n) => n initial digits, filled out

# alternate versions:
#   *_0ct(x,n) => ensure that the first octal digits is zero
#   *_HEX(x,n) => use uppercase digits for hexadecimal

# - returns (len, point, neg)
# - implies len = point
#

function decode_dec(x::SmallFloatingPoint, digits)
    if x == 0.0
        digits[1] = '0'
        return (Int32(1), Int32(1), false)
    end
    len,pt,neg = grisu(x,Grisu.FIXED,0,digits)
    if len == 0
        digits[1] = '0'
        return (Int32(1), Int32(1), false)
    else
        for i = len+1:pt
            digits[i] = '0'
        end
    end
    return Int32(len), Int32(pt), neg
end
# TODO: implement decode_oct, decode_0ct, decode_hex, decode_HEX for SmallFloatingPoint

## fix decoding functions ##
#
# - returns (neg, point, len)
# - if len less than point, trailing zeros implied
#

# fallback for Real types without explicit fix_dec implementation
fix_dec(x::Real, n::Int, digits) = fix_dec(float(x),n,digits)

fix_dec(x::Integer, n::Int, digits) = decode_dec(x, digits)

function fix_dec(x::SmallFloatingPoint, n::Int, digits)
    if n > length(digits)-1; n = length(digits)-1; end
    len,pt,neg = grisu(x,Grisu.FIXED,n,digits)
    if len == 0
        digits[1] = '0'
        return (Int32(1), Int32(1), neg)
    end
    return Int32(len), Int32(pt), neg
end

## ini decoding functions ##
#
# - returns (neg, point, len)
# - implies len = n (requested digits)
#

# fallback for Real types without explicit fix_dec implementation
ini_dec(x::Real, n::Int, digits) = ini_dec(float(x),n,digits)

function ini_dec(d::Integer, n::Int, digits)
    neg, x = handlenegative(d)
    k = ndigits(x)
    if k <= n
        pt = k
        for i = k:-1:1
            digits[i] = '0'+rem(x,10)
            x = div(x,10)
        end
        for i = k+1:n
            digits[i] = '0'
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
        pt = k
        x = div(x,p)
        for i = n:-1:1
            digits[i] = '0'+rem(x,10)
            x = div(x,10)
        end
    end
    return n, pt, neg
end

function ini_dec(x::SmallFloatingPoint, n::Int, digits)
    if x == 0.0
        ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), digits, '0', n)
        return Int32(1), Int32(1), signbit(x)
    else
        len,pt,neg = grisu(x,Grisu.PRECISION,n,digits)
    end
    return Int32(len), Int32(pt), neg
end

function ini_dec(x::BigInt, n::Int, digits)
    if x.size == 0
        ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), digits, '0', n)
        return Int32(1), Int32(1), false
    end
    d = Base.ndigits0z(x)
    if d <= n
        info = decode_dec(x)
        d == n && return info
        p = convert(Ptr{Cvoid}, digits) + info[2]
        ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), p, '0', n - info[2])
        return info
    end
    return (n, d, decode_dec(round(BigInt,x/big(10)^(d-n)))[3])
end


ini_hex(x::Real, n::Int, digits) = ini_hex(x,n,hex_symbols,digits)
ini_HEX(x::Real, n::Int, digits) = ini_hex(x,n,HEX_symbols,digits)

ini_hex(x::Real, digits) = ini_hex(x,hex_symbols,digits)
ini_HEX(x::Real, digits) = ini_hex(x,HEX_symbols,digits)

ini_hex(x::Real, n::Int, symbols::AbstractArray{UInt8,1}, digits) = ini_hex(float(x), n, symbols, digits)
ini_hex(x::Real, symbols::AbstractArray{UInt8,1}, digits) = ini_hex(float(x), symbols, digits)

function ini_hex(x::SmallFloatingPoint, n::Int, symbols::AbstractArray{UInt8,1}, digits)
    x = Float64(x)
    if x == 0.0
        ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), digits, '0', n)
        return Int32(1), Int32(0), signbit(x)
    else
        s, p = frexp(x)
        sigbits = 4*min(n-1,13)
        s = 0.25*round(ldexp(s,1+sigbits))
        # ensure last 2 exponent bits either 01 or 10
        u = (reinterpret(UInt64,s) & 0x003f_ffff_ffff_ffff) >> (52-sigbits)
        if n > 14
            ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), digits, '0', n)
        end
        i = (sizeof(u)<<1)-(leading_zeros(u)>>2)
        while i > 0
            digits[i] = symbols[(u&0xf)+1]
            u >>= 4
            i -= 1
        end
        # pt is the binary exponent
        return Int32(n), Int32(p-1), x < 0.0
    end
end

function ini_hex(x::SmallFloatingPoint, symbols::AbstractArray{UInt8,1}, digits)
    x = Float64(x)
    if x == 0.0
        ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), digits, '0', 1)
        return Int32(1), Int32(0), signbit(x)
    else
        s, p = frexp(x)
        s *= 2.0
        u = (reinterpret(UInt64,s) & 0x001f_ffff_ffff_ffff)
        t = (trailing_zeros(u) >> 2)
        u >>= (t<<2)
        n = 14-t
        for i = n:-1:1
            digits[i] = symbols[(u&0xf)+1]
            u >>= 4
        end
        # pt is the binary exponent
        return Int32(n), Int32(p-1), x < 0.0
    end
end

function ini_hex(x::Integer, digits)
    len,pt,neg = decode_hex(x, digits)
    pt = (len-1)<<2
    len,pt,neg
end
function ini_HEX(x::Integer, digits)
    len,pt,neg = decode_HEX(x, digits)
    pt = (len-1)<<2
    len,pt,neg
end

# not implemented
ini_hex(x::Integer,ndigits::Int,digits) = throw(MethodError(ini_hex,(x,ndigits,digits)))

#BigFloat
fix_dec(out, d::BigFloat, flags::String, width::Int, precision::Int, c::Char, digits) = bigfloat_printf(out, d, flags, width, precision, c, digits)
ini_dec(out, d::BigFloat, ndigits::Int, flags::String, width::Int, precision::Int, c::Char, digits) = bigfloat_printf(out, d, flags, width, precision, c, digits)
ini_hex(out, d::BigFloat, ndigits::Int, flags::String, width::Int, precision::Int, c::Char, digits) = bigfloat_printf(out, d, flags, width, precision, c, digits)
ini_HEX(out, d::BigFloat, ndigits::Int, flags::String, width::Int, precision::Int, c::Char, digits) = bigfloat_printf(out, d, flags, width, precision, c, digits)
ini_hex(out, d::BigFloat, flags::String, width::Int, precision::Int, c::Char, digits) = bigfloat_printf(out, d, flags, width, precision, c, digits)
ini_HEX(out, d::BigFloat, flags::String, width::Int, precision::Int, c::Char, digits) = bigfloat_printf(out, d, flags, width, precision, c, digits)
function bigfloat_printf(out, d::BigFloat, flags::String, width::Int, precision::Int, c::Char, digits)
    fmt_len = sizeof(flags)+4
    if width > 0
        fmt_len += ndigits(width)
    end
    if precision >= 0
        fmt_len += ndigits(precision)+1
    end
    fmt = IOBuffer(maxsize=fmt_len)
    print(fmt, '%')
    print(fmt, flags)
    if width > 0
        print(fmt, width)
    end
    if precision == 0
        print(fmt, '.')
        print(fmt, '0')
    elseif precision > 0
        print(fmt, '.')
        print(fmt, precision)
    end
    print(fmt, 'R')
    print(fmt, c)
    write(fmt, UInt8(0))
    printf_fmt = take!(fmt)
    @assert length(printf_fmt) == fmt_len
    bufsiz = length(digits)
    lng = ccall((:mpfr_snprintf,:libmpfr), Int32,
                (Ptr{UInt8}, Culong, Ptr{UInt8}, Ref{BigFloat}...),
                digits, bufsiz, printf_fmt, d)
    lng > 0 || error("invalid printf formatting for BigFloat")
    unsafe_write(out, pointer(digits), min(lng, bufsiz-1))
    return (false, ())
end

### external printf interface ###

is_str_expr(ex) =
    isa(ex,Expr) && (ex.head === :string || (ex.head === :macrocall && isa(ex.args[1],Symbol) &&
    endswith(string(ex.args[1]),"str")))

function _printf(macroname, io, fmt, args)
    isa(fmt, AbstractString) || throw(ArgumentError("$macroname: format must be a plain static string (no interpolation or prefix)"))
    sym_args, blk = gen(fmt)

    has_splatting = false
    for arg in args
       if isa(arg, Expr) && arg.head === :...
          has_splatting = true
          break
       end
    end

    #
    #  Immediately check for corresponding arguments if there is no splatting
    #
    if !has_splatting && length(sym_args) != length(args)
       throw(ArgumentError("$macroname: wrong number of arguments ($(length(args))) should be ($(length(sym_args)))"))
    end

    for i = length(sym_args):-1:1
        var = sym_args[i].args[1]
        if has_splatting
           pushfirst!(blk.args, :($var = G[$i]))
        else
           pushfirst!(blk.args, :($var = $(esc(args[i]))))
        end
    end

    #
    #  Delay generation of argument list and check until evaluation time instead of macro
    #  expansion time if there is splatting.
    #
    if has_splatting
       x = Expr(:call,:tuple,args...)
       pushfirst!(blk.args,
          quote
             G = $(esc(x))
             if length(G) != $(length(sym_args))
                 throw(ArgumentError(string($macroname,": wrong number of arguments (",length(G),") should be (",$(length(sym_args)),")")))
             end
          end
       )
    end

    pushfirst!(blk.args, :(out = $io))
    Expr(:let, Expr(:block), blk)
end

"""
    @printf([io::IOStream], "%Fmt", args...)

Print `args` using C `printf` style format specification string, with some caveats:
`Inf` and `NaN` are printed consistently as `Inf` and `NaN` for flags `%a`, `%A`,
`%e`, `%E`, `%f`, `%F`, `%g`, and `%G`. Furthermore, if a floating point number is
equally close to the numeric values of two possible output strings, the output
string further away from zero is chosen.

Optionally, an [`IOStream`](@ref)
may be passed as the first argument to redirect output.

See also: [`@sprintf`](@ref)

# Examples
```jldoctest
julia> @printf("%f %F %f %F\\n", Inf, Inf, NaN, NaN)
Inf Inf NaN NaN\n

julia> @printf "%.0f %.1f %f\\n" 0.5 0.025 -0.0078125
1 0.0 -0.007813
```
"""
macro printf(args...)
    isempty(args) && throw(ArgumentError("@printf: called with no arguments"))
    if isa(args[1], AbstractString) || is_str_expr(args[1])
        _printf("@printf", :stdout, args[1], args[2:end])
    else
        (length(args) >= 2 && (isa(args[2], AbstractString) || is_str_expr(args[2]))) ||
            throw(ArgumentError("@printf: first or second argument must be a format string"))
        _printf("@printf", esc(args[1]), args[2], args[3:end])
    end
end

"""
    @sprintf("%Fmt", args...)

Return `@printf` formatted output as string.

# Examples
```jldoctest
julia> s = @sprintf "this is a %s %15.1f" "test" 34.567;

julia> println(s)
this is a test            34.6
```
"""
macro sprintf(args...)
    isempty(args) && throw(ArgumentError("@sprintf: called with zero arguments"))
    isa(args[1], AbstractString) || is_str_expr(args[1]) ||
        throw(ArgumentError("@sprintf: first argument must be a format string"))
    letexpr = _printf("@sprintf", :(IOBuffer()), args[1], args[2:end])
    push!(letexpr.args[2].args, :(String(take!(out))))
    letexpr
end

end

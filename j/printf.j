function _jl_strip_trz(digits::ASCIIString)
    n = length(digits)
    while n > 1 && digits[n] == '0'
        n -= 1
    end
    digits[1:n]
end

function _jl_fix(base::Int, x::Integer, n::Int)
    digits = int2str(abs(x), base)
    (x < 0, _jl_strip_trz(digits), length(digits))
end

function _jl_sig(base::Int, x::Integer, n::Int)
    digits = int2str(abs(x), base)
    pt = length(digits)
    if length(digits) > n && digits.data[n+1] >= base/2
        digits.data[n] += 1
        digits = digits[1:n]
    end
    (x < 0, _jl_strip_trz(digits), pt)
end

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

# printf formats:
#   %                       # start
#   (\d+\$)?                # arg (not supported)
#   [\-\+#0' ]*             # flags
#   (\d+)?                  # width
#   (\.\d+)?                # precision
#   (h|hh|l|ll|L|j|t|z|q)?  # modifier
#   [diouxXeEfFgGaAcCsSp%]  # conversion

_jl_next_or_die(s::String, k) = !done(s,k) ? next(s,k) :
    error("premature end of printf format string: ", show_to_string(s))

function _printf_gen(s::String)
    args = {}
    exprs = {}
    i = j = start(s)
    while !done(s,j)
        c, k = next(s,j)
        if c == '%'
            if !isempty(s[i:j-1])
                push(exprs, check_utf8(unescape_string(s[i:j-1])))
            end
            alternate = false
            pad_zeros = false
            left_just = false
            pre_space = false
            show_sign = false
            separated = false
            width = 0
            precision = -1
            c, k = _jl_next_or_die(s,k)
            # parse flags
            while true
                if     c == '#'  ; alternate = true
                elseif c == '0'  ; pad_zeros = true
                elseif c == '-'  ; left_just = true
                elseif c == ' '  ; pre_space = true
                elseif c == '+'  ; show_sign = true
                elseif c == '\'' ; separated = true
                else             ; break
                end
                c, k = _jl_next_or_die(s,k)
            end
            separated && error("format flag \"'\" not yet supported")
            # parse width
            while '0' <= c <= '9'
                width = 10*width + c-'0'
                c, k = _jl_next_or_die(s,k)
            end
            # parse precision
            if c == '.'
                precision = 0
                c, k = _jl_next_or_die(s,k)
                while '0' <= c <= '9'
                    precision = 10*precision + c-'0'
                    c, k = _jl_next_or_die(s,k)
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
            i = j = k
            # construct conversion expression
            arg = symbol("arg$(length(args)+1)")
            blk = expr(:block)
            C = c; c = lc(c)
            if c == '%'
                if s[thisind(s,k-1)] != '%'
                    error("invalid printf format string: ", show_to_string(s))
                end
                if isempty(exprs)
                    push(exprs, "%")
                else
                    exprs[end] *= "%"
                end
                continue
            end
            if contains("diouxefga", c)
                arg = :(($arg)::Real)
                # number interpretation
                if c == 'u'
                    push(blk.args, :(x = unsigned(x)))
                end
                if contains("diu", c)
                    push(blk.args, :((neg, digits, pt) = _jl_fix10(x,0)))
                elseif c == 'o'
                    push(blk.args, :((neg, digits, pt) = _jl_fix8(x,0)))
                elseif c == 'x'
                    push(blk.args, :((neg, digits, pt) = _jl_fix16(x,0)))
                elseif c == 'f'
                    n = precision < 0 ? 6 : precision
                    push(blk.args, :((neg, digits, pt) = _jl_fix10(x,$n)))
                elseif c == 'e'
                    n = precision < 0 ? 7 : precision+1
                    push(blk.args, :((neg, digits, pt) = _jl_sig10(x,$n)))
                elseif c == 'g'
                    n = precision < 0 ? 6 : precision
                    push(blk.args, :((neg, digits, pt) = _jl_sig10(x,$n)))
                elseif c == 'a'
                    n = precision < 0 ? 7 : precision+1
                    push(blk.args, :((neg, digits, pt) = _jl_sig16(x,$n)))
                else
                    @unexpected
                end
                # number formatting parameters
                plus   = show_sign ? "+" : pre_space ? " " : ""
                prefix = C=='x' && alternate || C=='a' ? "0x" :
                         C=='X' && alternate || C=='A' ? "0X" : ""
                expstr = c=='a' ? "p" : c=='A' ? "P" : c==C ? "e" : "E"
                padded = false
                # generate formatting code
                if C == 'X' || C == 'A'
                    push(blk.args, :(digits = uc(digits)))
                end
                if contains("dioux", c)
                    push(blk.args, :(x = rpad(digits,pt,"0")))
                    if c=='o' && alternate
                        push(blk.args, :(if x[1]!='0'; x = "0"*x; end))
                    end
                    if precision > 0
                        push(blk.args, :(x = lpad(x, $precision, "0")))
                    elseif width > 0 && pad_zeros && !left_just
                        n = width-length(prefix)-1 # for sign
                        push(blk.args, :(x = lpad(x,$n,"0")))
                        if isempty(plus)
                            plus = "0"
                        end
                        padded = true
                    end
                elseif c == 'f'
                    if precision < 0; precision = 6; end
                    if precision > 0 || alternate
                        push(blk.args, quote
                            if length(digits) <= pt
                                x = rpad(digits,pt,"0")*"."*$("0"^precision)
                            elseif pt <= 0
                                x = "0."*"0"^-pt*digits*"0"^($precision-length(digits)+pt)
                            else # 0 < pt < length(digits)
                                x = digits[1:pt]*"."*rpad(digits[pt+1:],$precision,"0")
                            end
                        end)
                    else
                        push(blk.args, :(x = rpad(digits, max(1,pt), "0")))
                    end
                    if width > 0 && pad_zeros && !left_just
                        n = width-length(prefix)-1 # for sign
                        push(blk.args, :(x = lpad(x,$n,"0")))
                        if isempty(plus)
                            plus = "0"
                        end
                        padded = true
                    end
                elseif c == 'e'
                    if precision < 0; precision = 6; end
                    if precision > 0
                        push(blk.args, quote
                            x = digits[1:1]*"."*rpad(digits[2:],$precision,"0")
                        end)
                    else
                        if alternate
                            push(blk.args, :(x = digits[1:1]*"."))
                        else
                            push(blk.args, :(x = digits[1:1]))
                        end
                    end
                    push(blk.args, :(e = pt-1))
                    push(blk.args, :(x *= $expstr*(e<0?"-":"+")*int2str(abs(e),10,2)))
                elseif c == 'g'
                    if precision < 0; precision = 6; end
                    if !alternate
                        push(blk.args, quote
                            e = pt-1
                            digits = _jl_strip_trz(digits)
                            if e < -4 || e >= $precision
                                x = length(digits) > 1 ?
                                    (digits[1:1]*"."*digits[2:]) : digits[1:1]
                                x *= $expstr*(e<0?"-":"+")*int2str(abs(e),10,2)
                            elseif pt <= 0
                                x = "0."*"0"^-pt*digits
                            elseif pt >= length(digits)
                                x = rpad(digits,pt,"0")
                            else
                                x = digits[1:pt]*"."*digits[pt+1:]
                            end
                        end)
                    else
                        push(blk.args, quote
                            e = pt-1
                            digits = rpad(digits,$precision,"0")
                            if e < -4 || e >= $precision
                                x = digits[1:1]*"."*digits[2:]
                                x *= $expstr*(e<0?"-":"+")*int2str(abs(e),10,2)
                            elseif pt <= 0
                                x = "0."*"0"^-pt*digits
                            elseif pt >= length(digits) # only equality possible
                                x = rpad(digits,pt,"0")*"."
                            else
                                x = digits[1:pt]*"."*digits[pt+1:]
                            end
                        end)
                    end
                elseif c == 'a'
                    error("printf feature \"%$C\" is not yet implemented")
                else
                    @unexpected
                end
                if length(prefix) > 0
                    push(blk.args, :(x = $prefix*x))
                end
                push(blk.args, :(x = (neg?"-":$plus)*x))
                abn = quote
                    if isnan(x)
                        x = "NaN"
                    elseif x < 0
                        x = "-Inf"
                    else
                        x = "Inf"
                    end
                end
                if width > 0
                    pad = left_just ? :rpad : :lpad
                    push(abn.args, :(x = ($pad)(x,$width)))
                end
                blk = quote
                    x = $arg
                    if isfinite(x)
                        $blk
                    else
                        $abn
                    end
                end
            elseif c == 'c'
                arg = :(($arg)::Integer)
                push(blk.args, :(x = char($arg)))
                if alternate
                    push(blk.args, :(x = show_to_string(x)))
                end
            elseif c == 's'
                if alternate
                    push(blk.args, :(x = show_to_string($arg)))
                else
                    push(blk.args, :(x = string($arg)))
                end
            elseif c == 'p'
                arg = :(($arg)::Ptr)
                push(blk.args, :(x = "0x"*hex(unsigned($arg), $(WORD_SIZE>>2))))
            elseif c == 'n'
                error("printf feature \"%n\" is not supported")
            else
                error("invalid printf format string: ", show_to_string(s))
            end
            if width > 0 && !padded
                pad = left_just ? :rpad : :lpad
                push(blk.args, :(x = ($pad)(x,$width)))
            end
            push(blk.args, :x)
            push(exprs, blk)
            push(args, arg)
        else
            j = k
        end
    end
    if !isempty(s[i:])
        push(exprs, check_utf8(unescape_string(s[i:j-1])))
    end
    # return args, exprs
    args = expr(:tuple, args)
    body = expr(:call, :print, exprs...)
    return :(($args)->($body))
end

macro f_str(f); _printf_gen(f); end

printf(f::Function, args...) = f(args...)
printf(f::String,   args...) = eval(_printf_gen(f))(args...)
printf(s::IOStream, args...) = with_output_stream(s, printf, args...)

sprintf(f::Function, args...) = print_to_string(printf, f, args...)
sprintf(f::String,   args...) = print_to_string(printf, f, args...)

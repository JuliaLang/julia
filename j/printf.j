# printf formats:
#   %                       # start
#   (\d+\$)?                # arg
#   [\-\+#0' ]*             # flags
#   (\d+)?                  # width
#   (\.\d+)?                # precision
#   (h|hh|l|ll|L|j|t|z|q)?  # modifier
#   [diouxXeEfFgGaAcCsSp%]  # conversion

_jl_next_or_die(s::String, k) = !done(s,k) ? next(s,k) :
    error("premature end of format string: ", show_to_string(s))

function _jl_format_parse(s::String)
    sx = {}
    i = j = start(s)
    arg = 1
    while !done(s,j)
        c, k = next(s,j)
        if c == '%'
            if !isempty(s[i:j-1])
                push(sx, check_utf8(unescape_string(s[i:j-1])))
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
            ex = symbol("arg$arg")
            if c == '%'
                if isempty(sx)
                    push(sx, "%")
                else
                    sx[end] *= "%"
                end
                continue
            elseif c == 'd' || c == 'i'
                ex = :(string(convert(Int64,$ex)))
            elseif contains("ouxXcCp", c)
                ex = :(string(convert(Uint64,$ex)))
            elseif contains("eEfFgGaA", c)
                ex = :(string(convert(Float64,$ex)))
            elseif c == 's' || c == 'S'
                ex = :(string($ex))
            elseif c == 'n'
                error("format feature \"%n\" is not supported")
            else
                error("invalid format string: ", show_to_string(s))
            end
            alternate && error("format flag \"#\" not yet supported")
            pad_zeros && error("format flag \"0\" not yet supported")
            separated && error("format flag \"'\" not yet supported")
            if pre_space || show_sign
                pre = show_sign ? '+' : ' '
                ex = quote
                    s = $ex
                    if s[1] != '-'
                        s = strcat($pre,s)
                    end
                    s
                end
            end
            if width > 0
                pad = left_just ? (:rpad) : (:lpad)
                ex = :(($pad)($ex,$width))
            end
            push(sx, ex)
            arg += 1
        else
            j = k
        end
    end
    if !isempty(s[i:])
        push(sx, check_utf8(unescape_string(s[i:j-1])))
    end
    args = expr(:tuple, { symbol("arg$n") | n=1:arg-1 })
    body = expr(:call, :print, sx...)
    return :(($args)->($body))
end

macro f_str(f); _jl_format_parse(f); end

printf(f::Function, args...) = f(args...)
printf(f::String, args...) = eval(_jl_format_parse(f))(args...)
printf(s::IOStream, args...) = with_output_stream(s, printf, args...)

sprintf(f::Function, args...) = print_to_string(printf, f, args...)
sprintf(f::String, args...) = print_to_string(printf, f, args...)

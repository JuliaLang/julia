# printf formats:
#   %                       # start
#   (\d+\$)?                # arg
#   [\-\+#0' ]*             # flags
#   (\d+)?                  # width
#   (\.\d+)?                # precision
#   (h|hh|l|ll|L|j|t|z|q)?  # modifier
#   [diouxXeEfFgGaAcCsSp%]  # conversion

const PRINTF_FLAG_ALTERNATE = 0x1 << 0
const PRINTF_FLAG_ZERO_PAD  = 0x1 << 1
const PRINTF_FLAG_LEFT_JUST = 0x1 << 2
const PRINTF_FLAG_PRE_SPACE = 0x1 << 3
const PRINTF_FLAG_SIGNED    = 0x1 << 4
const PRINTF_FLAG_SEPARATED = 0x1 << 5

_jl_next_or_die(s::String, k) = !done(s,k) ? next(s,k) :
    error("premature end of format string: ", show_to_string(s))

function _jl_format_parse(s::String)
    sx = {}
    i = j = start(s)
    n = 1
    while !done(s,j)
        c, k = next(s,j)
        if c == '%'
            if !isempty(s[i:j-1])
                push(sx, check_utf8(unescape_string(s[i:j-1])))
            end
            arg = n
            flags = 0x0
            width = 0
            precision = -1
            modifier = ""
            c, k = _jl_next_or_die(s,k)
            # # parse arg number
            # if '1' <= c <= '9'
            #     arg = c-'0'
            #     while true
            #         c, k = _jl_next_or_die(s,k)
            #         if !('0' <= c <= '9'); break; end
            #         arg = 10*arg + c-'0'
            #     end
            #     if c != '$'
            #         width = arg
            #         arg = n
            #         goto parse_precision
            #     end
            # end
            # parse flags
            while true
                if c == '#'      ; flags |= PRINTF_FLAG_ALTERNATE
                elseif c == '0'  ; flags |= PRINTF_FLAG_ZERO_PAD
                elseif c == '-'  ; flags |= PRINTF_FLAG_LEFT_JUST
                elseif c == ' '  ; flags |= PRINTF_FLAG_PRE_SPACE
                elseif c == '+'  ; flags |= PRINTF_FLAG_SIGNED
                elseif c == '\'' ; flags |= PRINTF_FLAG_SEPARATED
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
        # parse_precision:
            if c == '.'
                precision = 0
                c, k = _jl_next_or_die(s,k)
                while '0' <= c <= '9'
                    precision = 10*precision + c-'0'
                    c, k = _jl_next_or_die(s,k)
                end
            end
            # parse length modifer
            if c == 'h' || c == 'l'
                modifier = string(c)
                c, k = _jl_next_or_die(s,k)
                if c == modifier[1]
                    modifier = strcat(c,c)
                    c, k = _jl_next_or_die(s,k)
                end
            elseif contains("Ljqtz",c)
                modifier = string(c)
                c, k = _jl_next_or_die(s,k)
            end
            # parse conversion
            conversion = c
            if !contains("%ACEFGSXacdefgiopsux", conversion)
                error("invalid format string: ", show_to_string(s))
            end
            push(sx, {
                :arg        => arg,
                :flags      => flags,
                :width      => width,
                :precision  => precision,
                :length     => length,
                :conversion => conversion,
            })
            i = j = k
        else
            j = k
        end
    end
    if !isempty(s[i:])
        push(sx, check_utf8(unescape_string(s[i:j-1])))
    end
    # length(sx) == 1 && isa(sx[1],ByteString) ? sx[1] :
    #     expr(:call, :print_to_string, printer, sx...)
    return sx
end

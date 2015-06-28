# This file is a part of Julia. License is MIT: http://julialang.org/license

## string escaping & unescaping ##

escape_nul(s::AbstractString, i::Int) =
    !done(s,i) && '0' <= next(s,i)[1] <= '7' ? "\\x00" : "\\0"

function print_escaped(io, s::AbstractString, esc::AbstractString)
    i = start(s)
    while !done(s,i)
        c, j = next(s,i)
        c == '\0'       ? print(io, escape_nul(s,j)) :
        c == '\e'       ? print(io, "\\e") :
        c == '\\'       ? print(io, "\\\\") :
        c in esc        ? print(io, '\\', c) :
        '\a' <= c <= '\r' ? print(io, '\\', "abtnvfr"[Int(c)-6]) :
        isprint(c)      ? print(io, c) :
        c <= '\x7f'     ? print(io, "\\x", hex(c, 2)) :
        c <= '\uffff'   ? print(io, "\\u", hex(c, need_full_hex(s,j) ? 4 : 2)) :
                          print(io, "\\U", hex(c, need_full_hex(s,j) ? 8 : 4))
        i = j
    end
end

escape_string(s::AbstractString) = sprint(endof(s), print_escaped, s, "\"")
function print_quoted(io, s::AbstractString)
    print(io, '"')
    print_escaped(io, s, "\"\$") #"# work around syntax highlighting problem
    print(io, '"')
end

# bare minimum unescaping function unescapes only given characters

function print_unescaped_chars(io, s::AbstractString, esc::AbstractString)
    if !('\\' in esc)
        esc = string("\\", esc)
    end
    i = start(s)
    while !done(s,i)
        c, i = next(s,i)
        if c == '\\' && !done(s,i) && s[i] in esc
            c, i = next(s,i)
        end
        print(io, c)
    end
end

unescape_chars(s::AbstractString, esc::AbstractString) =
    sprint(endof(s), print_unescaped_chars, s, esc)

# general unescaping of traditional C and Unicode escape sequences

function print_unescaped(io, s::AbstractString)
    i = start(s)
    while !done(s,i)
        c, i = next(s,i)
        if !done(s,i) && c == '\\'
            c, i = next(s,i)
            if c == 'x' || c == 'u' || c == 'U'
                n = k = 0
                m = c == 'x' ? 2 :
                    c == 'u' ? 4 : 8
                while (k+=1) <= m && !done(s,i)
                    c, j = next(s,i)
                    n = '0' <= c <= '9' ? n<<4 + c-'0' :
                        'a' <= c <= 'f' ? n<<4 + c-'a'+10 :
                        'A' <= c <= 'F' ? n<<4 + c-'A'+10 : break
                    i = j
                end
                if k == 1
                    throw(ArgumentError("\\x used with no following hex digits in $(repr(s))"))
                end
                if m == 2 # \x escape sequence
                    write(io, UInt8(n))
                else
                    print(io, Char(n))
                end
            elseif '0' <= c <= '7'
                k = 1
                n = c-'0'
                while (k+=1) <= 3 && !done(s,i)
                    c, j = next(s,i)
                    n = ('0' <= c <= '7') ? n<<3 + c-'0' : break
                    i = j
                end
                if n > 255
                    throw(ArgumentError("octal escape sequence out of range"))
                end
                write(io, UInt8(n))
            else
                print(io, c == 'a' ? '\a' :
                          c == 'b' ? '\b' :
                          c == 't' ? '\t' :
                          c == 'n' ? '\n' :
                          c == 'v' ? '\v' :
                          c == 'f' ? '\f' :
                          c == 'r' ? '\r' :
                          c == 'e' ? '\e' : c)
            end
        else
            print(io, c)
        end
    end
end

unescape_string(s::AbstractString) = sprint(endof(s), print_unescaped, s)

macro b_str(s); :($(unescape_string(s)).data); end

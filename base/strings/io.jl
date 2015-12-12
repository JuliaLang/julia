# This file is a part of Julia. License is MIT: http://julialang.org/license

## core text I/O ##

function print(io::IO, x)
    lock(io)
    try
        show(io, x)
    finally
        unlock(io)
    end
end

function print(io::IO, xs...)
    lock(io)
    try
        for x in xs
            print(io, x)
        end
    finally
        unlock(io)
    end
end

println(io::IO, xs...) = print(io, xs..., '\n')

print(xs...)   = print(STDOUT, xs...)
println(xs...) = println(STDOUT, xs...)

## conversion of general objects to strings ##

function sprint(size::Integer, f::Function, args...; env=nothing)
    s = IOBuffer(Array(UInt8,size), true, true)
    truncate(s,0)
    if env !== nothing
        f(IOContext(s, env), args...)
    else
        f(s, args...)
    end
    bytestring(resize!(s.data, s.size))
end
sprint(f::Function, args...) = sprint(0, f, args...)

tostr_sizehint(x) = 0
tostr_sizehint(x::AbstractString) = endof(x)
tostr_sizehint(x::Float64) = 20
tostr_sizehint(x::Float32) = 12

function print_to_string(xs...; env=nothing)
    # specialized for performance reasons
    s = IOBuffer(Array(UInt8,tostr_sizehint(xs[1])), true, true)
    # specialized version of truncate(s,0)
    s.size = 0
    s.ptr = 1
    if env !== nothing
        env_io = IOContext(s, env)
        for x in xs
            print(env_io, x)
        end
    else
        for x in xs
            print(s, x)
        end
    end
    String(resize!(s.data, s.size))
end

string_with_env(env, xs...) = print_to_string(xs...; env=env)
string(xs...) = print_to_string(xs...)
bytestring(s::AbstractString...) = print_to_string(s...)

print(io::IO, s::AbstractString) = (write(io, s); nothing)
write(io::IO, s::AbstractString) = (len = 0; for c in s; len += write(io, c); end; len)
show(io::IO, s::AbstractString) = print_quoted(io, s)

write(to::AbstractIOBuffer, s::SubString{String}) =
    s.endof==0 ? 0 : write_sub(to, s.string.data, s.offset + 1, nextind(s, s.endof) - 1)

## printing literal quoted string data ##

# this is the inverse of print_unescaped_chars(io, s, "\\\")

function print_quoted_literal(io, s::AbstractString)
    print(io, '"')
    for c = s; c == '"' ? print(io, "\\\"") : print(io, c); end
    print(io, '"')
end

function repr(x)
    s = IOBuffer()
    showall(s, x)
    takebuf_string(s)
end

# IOBuffer views of a (byte)string:
IOBuffer(str::String) = IOBuffer(str.data)
IOBuffer(s::SubString{String}) = IOBuffer(sub(s.string.data, s.offset + 1 : s.offset + sizeof(s)))

# join is implemented using IO
function print_joined(io, strings, delim, last)
    i = start(strings)
    if done(strings,i)
        return
    end
    str, i = next(strings,i)
    print(io, str)
    is_done = done(strings,i)
    while !is_done
        str, i = next(strings,i)
        is_done = done(strings,i)
        print(io, is_done ? last : delim)
        print(io, str)
    end
end

function print_joined(io, strings, delim)
    i = start(strings)
    is_done = done(strings,i)
    while !is_done
        str, i = next(strings,i)
        is_done = done(strings,i)
        print(io, str)
        if !is_done
            print(io, delim)
        end
    end
end
print_joined(io, strings) = print_joined(io, strings, "")

join(args...) = sprint(print_joined, args...)

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

## multiline strings ##

"""
Calculate the width of leading blank space, and also return if string is blank

Returns:

* width of leading whitespace, flag if string is totally blank
"""
function indentation(str::AbstractString; tabwidth=8)
    count = 0
    for ch in str
        if ch == ' '
            count += 1
        elseif ch == '\t'
            count = div(count + tabwidth, tabwidth) * tabwidth
        else
            return count, false
        end
    end
    count, true
end

"""
Removes leading indentation from string

Returns:

* `String` of multiline string, with leading indentation of `indent` removed
"""
function unindent(str::AbstractString, indent::Int; tabwidth=8)
    indent == 0 && return str
    pos = start(str)
    endpos = endof(str)
    # Note: this loses the type of the original string
    buf = IOBuffer(Array(UInt8,endpos), true, true)
    truncate(buf,0)
    cutting = true
    col = 0     # current column (0 based)
    while pos <= endpos
        ch, pos = next(str,pos)
        if cutting
            if ch == ' '
                col += 1
            elseif ch == '\t'
                col = div(col + tabwidth, tabwidth) * tabwidth
            elseif ch == '\n'
                # Now we need to output enough indentation
                for i = 1:col-indent
                    write(buf, ' ')
                end
                col = 0
                write(buf, '\n')
            else
                cutting = false
                # Now we need to output enough indentation to get to
                # correct place
                for i = 1:col-indent
                    write(buf, ' ')
                end
                col += 1
                write(buf, ch)
            end
        elseif ch == '\t'       # Handle internal tabs
            upd = div(col + tabwidth, tabwidth) * tabwidth
            # output the number of spaces that would have been seen
            # with original indentation
            for i = 1:(upd-col)
                write(buf, ' ')
            end
            col = upd
        elseif ch == '\n'
            cutting = true
            col = 0
            write(buf, '\n')
        else
            col += 1
            write(buf, ch)
        end
    end
    # If we were still "cutting" when we hit the end of the string,
    # we need to output the right number of spaces for the indentation
    if cutting
        for i = 1:col-indent
            write(buf, ' ')
        end
    end
    takebuf_string(buf)
end

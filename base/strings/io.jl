# This file is a part of Julia. License is MIT: https://julialang.org/license

## core text I/O ##

"""
    print([io::IO], xs...)

Write to `io` (or to the default output stream [`stdout`](@ref)
if `io` is not given) a canonical (un-decorated) text representation
of values `xs` if there is one, otherwise call [`show`](@ref).
The representation used by `print` includes minimal formatting and tries to
avoid Julia-specific details.

# Examples
```jldoctest
julia> print("Hello World!")
Hello World!
julia> io = IOBuffer();

julia> print(io, "Hello", ' ', :World!)

julia> String(take!(io))
"Hello World!"
```
"""
function print(io::IO, x)
    lock(io)
    try
        show(io, x)
    finally
        unlock(io)
    end
    return nothing
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
    return nothing
end

"""
    println([io::IO], xs...)

Print (using [`print`](@ref)) `xs` followed by a newline.
If `io` is not supplied, prints to [`stdout`](@ref).

# Examples
```jldoctest
julia> println("Hello, world")
Hello, world

julia> io = IOBuffer();

julia> println(io, "Hello, world")

julia> String(take!(io))
"Hello, world\\n"
```
"""
println(io::IO, xs...) = print(io, xs..., '\n')

## conversion of general objects to strings ##

"""
    sprint(f::Function, args...; context=nothing, sizehint=0)

Call the given function with an I/O stream and the supplied extra arguments.
Everything written to this I/O stream is returned as a string.

The optional keyword argument `context` can be set to `:key=>value` pair
or an `IO` or [`IOContext`](@ref) object whose attributes are used for the I/O
stream passed to `f`.  The optional `sizehint` is a suggersted (in bytes)
to allocate for the buffer used to write the string.

# Examples
```jldoctest
julia> sprint(showcompact, 66.66666)
"66.6667"
```
"""
function sprint(f::Function, args...; context=nothing, sizehint::Integer=0)
    s = IOBuffer(sizehint=sizehint)
    if context !== nothing
        f(IOContext(s, context), args...)
    else
        f(s, args...)
    end
    String(resize!(s.data, s.size))
end

tostr_sizehint(x) = 0
tostr_sizehint(x::AbstractString) = lastindex(x)
tostr_sizehint(x::Float64) = 20
tostr_sizehint(x::Float32) = 12

function print_to_string(xs...; env=nothing)
    if isempty(xs)
        return ""
    end
    # specialized for performance reasons
    s = IOBuffer(sizehint=tostr_sizehint(xs[1]))
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

"""
    string(xs...)

Create a string from any values using the [`print`](@ref) function.

# Examples
```jldoctest
julia> string("a", 1, true)
"a1true"
```
"""
string(xs...) = print_to_string(xs...)

print(io::IO, s::AbstractString) = (write(io, s); nothing)
write(io::IO, s::AbstractString) = (len = 0; for c in s; len += write(io, c); end; len)
show(io::IO, s::AbstractString) = print_quoted(io, s)

write(to::GenericIOBuffer, s::SubString{String}) =
    s.ncodeunits ≤ 0 ? 0 : unsafe_write(to, pointer(s.string, s.offset+1), UInt(s.ncodeunits))

## printing literal quoted string data ##

# this is the inverse of print_unescaped_chars(io, s, "\\\")

function print_quoted_literal(io, s::AbstractString)
    print(io, '"')
    for c = s; c == '"' ? print(io, "\\\"") : print(io, c); end
    print(io, '"')
end

"""
    repr(x; context=nothing)

Create a string from any value using the [`show`](@ref) function.

The optional keyword argument `context` can be set to an `IO` or [`IOContext`](@ref)
object whose attributes are used for the I/O stream passed to `show`.

Note that `repr(x)` is usually similar to how the value of `x` would
be entered in Julia.  See also [`repr("text/plain", x)`](@ref) to instead
return a "pretty-printed" version of `x` designed more for human consumption,
equivalent to the REPL display of `x`.

# Examples
```jldoctest
julia> repr(1)
"1"

julia> repr(zeros(3))
"[0.0, 0.0, 0.0]"

```
"""
repr(x; context=nothing) = sprint(show, x; context=context)

# IOBuffer views of a (byte)string:

"""
    IOBuffer(string::String)

Create a read-only `IOBuffer` on the data underlying the given string.

# Examples
```jldoctest
julia> io = IOBuffer("Haho");

julia> String(take!(io))
"Haho"

julia> String(take!(io))
"Haho"
```
"""
IOBuffer(str::String) = IOBuffer(unsafe_wrap(Vector{UInt8}, str))
IOBuffer(s::SubString{String}) = IOBuffer(view(unsafe_wrap(Vector{UInt8}, s.string), s.offset + 1 : s.offset + sizeof(s)))

# join is implemented using IO

"""
    join(io::IO, strings, delim, [last])

Join an array of `strings` into a single string, inserting the given delimiter between
adjacent strings. If `last` is given, it will be used instead of `delim` between the last
two strings. For example,

# Examples
```jldoctest
julia> join(["apples", "bananas", "pineapples"], ", ", " and ")
"apples, bananas and pineapples"
```

`strings` can be any iterable over elements `x` which are convertible to strings
via `print(io::IOBuffer, x)`. `strings` will be printed to `io`.
"""
function join(io::IO, strings, delim, last)
    a = Iterators.Stateful(strings)
    isempty(a) && return
    print(io, popfirst!(a))
    for str in a
        print(io, isempty(a) ? last : delim)
        print(io, str)
    end
end

function join(io::IO, strings, delim)
    a = Iterators.Stateful(strings)
    for str in a
        print(io, str)
        !isempty(a) && print(io, delim)
    end
end
join(io::IO, strings) = join(io, strings, "")

join(strings) = sprint(join, strings)
join(strings, delim) = sprint(join, strings, delim)
join(strings, delim, last) = sprint(join, strings, delim, last)

## string escaping & unescaping ##

need_full_hex(c::Union{Nothing, Char}) = c !== nothing && isxdigit(c)
escape_nul(c::Union{Nothing, Char}) =
    (c !== nothing && '0' <= c <= '7') ? "\\x00" : "\\0"

"""
    escape_string(str::AbstractString[, esc::AbstractString]) -> AbstractString

General escaping of traditional C and Unicode escape sequences.
Any characters in `esc` are also escaped (with a backslash).
The reverse is [`unescape_string`](@ref).
"""
escape_string(s::AbstractString, esc::AbstractString) = sprint(escape_string, s, esc, sizehint=lastindex(s))
escape_string(s::AbstractString) = sprint(escape_string, s, "\"", sizehint=lastindex(s))

"""
    escape_string(io, str::AbstractString[, esc::AbstractString]) -> Nothing

Escape sequences in `str` and print result to `io`. See also [`unescape_string`](@ref).
"""
function escape_string(io, s::AbstractString, esc::AbstractString="")
    a = Iterators.Stateful(s)
    for c in a
        if c in esc
            print(io, '\\', c)
        elseif isascii(c)
            c == '\0'          ? print(io, escape_nul(peek(a))) :
            c == '\e'          ? print(io, "\\e") :
            c == '\\'          ? print(io, "\\\\") :
            c in esc           ? print(io, '\\', c) :
            '\a' <= c <= '\r'  ? print(io, '\\', "abtnvfr"[Int(c)-6]) :
            isprint(c)         ? print(io, c) :
                                 print(io, "\\x", hex(c, 2))
        elseif !isoverlong(c) && !ismalformed(c)
            isprint(c)         ? print(io, c) :
            c <= '\x7f'        ? print(io, "\\x", hex(c, 2)) :
            c <= '\uffff'      ? print(io, "\\u", hex(c, need_full_hex(peek(a)) ? 4 : 2)) :
                                 print(io, "\\U", hex(c, need_full_hex(peek(a)) ? 8 : 4))
        else # malformed or overlong
            u = bswap(reinterpret(UInt32, c))
            while true
                print(io, "\\x", hex(u % UInt8, 2))
                (u >>= 8) == 0 && break
            end
        end
    end
end

function print_quoted(io, s::AbstractString)
    print(io, '"')
    escape_string(io, s, "\"\$") #"# work around syntax highlighting problem
    print(io, '"')
end

# general unescaping of traditional C and Unicode escape sequences

# TODO: handle unescaping invalid UTF-8 sequences

"""
    unescape_string(str::AbstractString) -> AbstractString

General unescaping of traditional C and Unicode escape sequences. Reverse of
[`escape_string`](@ref).
"""
unescape_string(s::AbstractString) = sprint(unescape_string, s, sizehint=lastindex(s))

"""
    unescape_string(io, str::AbstractString) -> Nothing

Unescapes sequences and prints result to `io`. See also [`escape_string`](@ref).
"""
function unescape_string(io, s::AbstractString)
    a = Iterators.Stateful(s)
    for c in a
        if !isempty(a) && c == '\\'
            c = popfirst!(a)
            if c == 'x' || c == 'u' || c == 'U'
                n = k = 0
                m = c == 'x' ? 2 :
                    c == 'u' ? 4 : 8
                while (k += 1) <= m && !isempty(a)
                    nc = peek(a)
                    n = '0' <= nc <= '9' ? n<<4 + (nc-'0') :
                        'a' <= nc <= 'f' ? n<<4 + (nc-'a'+10) :
                        'A' <= nc <= 'F' ? n<<4 + (nc-'A'+10) : break
                    popfirst!(a)
                end
                if k == 1 || n > 0x10ffff
                    u = m == 4 ? 'u' : 'U'
                    throw(ArgumentError("invalid $(m == 2 ? "hex (\\x)" :
                                        "unicode (\\$u)") escape sequence"))
                end
                if m == 2 # \x escape sequence
                    write(io, UInt8(n))
                else
                    print(io, Char(n))
                end
            elseif '0' <= c <= '7'
                k = 1
                n = c-'0'
                while (k += 1) <= 3 && !isempty(a)
                    c = peek(a)
                    n = ('0' <= c <= '7') ? n<<3 + c-'0' : break
                    popfirst!(a)
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

macro b_str(s)
    v = codeunits(unescape_string(s))
    QuoteNode(v)
end

"""
    @raw_str -> String

Create a raw string without interpolation and unescaping.
The exception is that quotation marks still must be escaped. Backslashes
escape both quotation marks and other backslashes, but only when a sequence
of backslashes precedes a quote character. Thus, 2n backslashes followed by
a quote encodes n backslashes and the end of the literal while 2n+1 backslashes
followed by a quote encodes n backslashes followed by a quote character.

# Examples
```jldoctest
julia> println(raw"\\ \$x")
\\ \$x

julia> println(raw"\\"")
"

julia> println(raw"\\\\\\"")
\\"

julia> println(raw"\\\\x \\\\\\"")
\\\\x \\"
```
"""
macro raw_str(s); s; end

## multiline strings ##

"""
    indentation(str::AbstractString; tabwidth=8)

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
    unindent(str::AbstractString, indent::Int; tabwidth=8)

Remove leading indentation from string

Returns:

* `String` of multiline string, with leading indentation of `indent` removed
"""
function unindent(str::AbstractString, indent::Int; tabwidth=8)
    indent == 0 && return str
    # Note: this loses the type of the original string
    buf = IOBuffer(sizehint=sizeof(str))
    cutting = true
    col = 0     # current column (0 based)
    for ch in str
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
    String(take!(buf))
end

function String(chars::AbstractVector{Char})
    sprint(sizehint=length(chars)) do io
        for c in chars
            write(io, c)
        end
    end
end

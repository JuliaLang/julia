#-------------------------------------------------------------------------------
# This file contains utility functions for converting undecorated source
# strings into Julia values.  For example, string->number, string unescaping, etc.

"""
Convert a Julia source code string into a number.
"""
function julia_string_to_number(str::AbstractString, kind)
    str = replace(replace(str, '_'=>""), '−'=>'-')
    if kind == K"Integer"
        x = Base.tryparse(Int, str)
        if Int === Int32 && isnothing(x)
            x = Base.tryparse(Int64, str)
        end
        if isnothing(x)
            x = Base.tryparse(Int128, str)
            if isnothing(x)
                x = Base.parse(BigInt, str)
            end
        end
        return x
    elseif kind == K"Float"
        if !startswith(str,"0x") && 'f' in str
            # This is kind of awful. Should we have a separate Float32 literal
            # type produced by the lexer?  The `f` suffix is nonstandard after all.
            return Base.parse(Float32, replace(str, 'f'=>'e'))
        else
            return Base.parse(Float64, str)
        end
    elseif kind == K"HexInt"
        ndigits = length(str)-2
        return ndigits <= 2  ? Base.parse(UInt8, str)   :
               ndigits <= 4  ? Base.parse(UInt16, str)  :
               ndigits <= 8  ? Base.parse(UInt32, str)  :
               ndigits <= 16 ? Base.parse(UInt64, str)  :
               ndigits <= 32 ? Base.parse(UInt128, str) :
               Base.parse(BigInt, str)
    elseif kind == K"BinInt"
        ndigits = length(str)-2
        return ndigits <= 8   ? Base.parse(UInt8, str)   :
               ndigits <= 16  ? Base.parse(UInt16, str)  :
               ndigits <= 32  ? Base.parse(UInt32, str)  :
               ndigits <= 64  ? Base.parse(UInt64, str)  :
               ndigits <= 128 ? Base.parse(UInt128, str) :
               Base.parse(BigInt, str)
    elseif kind == K"OctInt"
        ndigits = length(str)-2
        x = Base.tryparse(UInt64, str)
        if isnothing(x)
            x = Base.tryparse(UInt128, str)
            if isnothing(x)
                x = Base.parse(BigInt, str)
            elseif ndigits > 43
                x = BigInt(x)
            end
        else
            x = ndigits <= 3  && x <= typemax(UInt8)  ? UInt8(x)   :
                ndigits <= 6  && x <= typemax(UInt16) ? UInt16(x)  :
                ndigits <= 11 && x <= typemax(UInt32) ? UInt32(x)  :
                ndigits <= 22                         ? x          :
                ndigits <= 43                         ? UInt128(x) :
                BigInt(x)
        end
        return x
    end
end


#-------------------------------------------------------------------------------
is_indentation(c) = c == ' ' || c == '\t'

"""
Process Julia source code escape sequences for raw strings
"""
function unescape_raw_string(io::IO, str::AbstractString, is_cmd::Bool)
    delim = is_cmd ? '`' : '"'
    i = firstindex(str)
    lastidx = lastindex(str)
    while i <= lastidx
        c = str[i]
        if c != '\\'
            if c == '\r'
                # convert literal \r and \r\n in strings to \n (issue #11988)
                if i+1 <= lastidx && str[i+1] == '\n'
                    i += 1
                end
                c = '\n'
            end
            write(io, c)
            i = nextind(str, i)
            continue
        end
        # Process \ escape sequences
        j = i
        while j <= lastidx && str[j] == '\\'
            j += 1
        end
        nbackslash = j - i
        if (j <= lastidx && str[j] == delim) || j > lastidx
            # Backslashes before a delimiter must also be escaped
            nbackslash = div(nbackslash,2)
        end
        for k = 1:nbackslash
            write(io, '\\')
        end
        i = j
        if i <= lastidx
            write(io, str[i])
            i = nextind(str, i)
        end
    end
end

"""
Process Julia source code escape sequences for non-raw strings.
`str` should be passed without delimiting quotes.
"""
function unescape_julia_string(io::IO, str::AbstractString)
    i = firstindex(str)
    lastidx = lastindex(str)
    while i <= lastidx
        c = str[i]
        if c != '\\'
            if c == '\r'
                # convert literal \r and \r\n in strings to \n (issue #11988)
                if i+1 <= lastidx && str[i+1] == '\n'
                    i += 1
                end
                c = '\n'
            end
            write(io, c)
            i = nextind(str, i)
            continue
        end
        # Process \ escape sequences.  See also Base.unescape_string which some
        # of this code derives from (but which disallows \` \' \$)
        i += 1
        if i > lastidx
            break
        end
        c = str[i]
        if c == 'x' || c == 'u' || c == 'U'
            n = k = 0
            m = c == 'x' ? 2 :
                c == 'u' ? 4 : 8
            while (k += 1) <= m && i+1 <= lastidx
                nc = str[i+1]
                n = '0' <= nc <= '9' ? n<<4 + (nc-'0') :
                    'a' <= nc <= 'f' ? n<<4 + (nc-'a'+10) :
                    'A' <= nc <= 'F' ? n<<4 + (nc-'A'+10) : break
                i += 1
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
            while (k += 1) <= 3 && i+1 <= lastidx
                c = str[i+1]
                n = ('0' <= c <= '7') ? n<<3 + c-'0' : break
                i += 1
            end
            if n > 255
                throw(ArgumentError("octal escape sequence out of range"))
            end
            write(io, UInt8(n))
        else
            u = # C escapes
                c == 'n' ? '\n' :
                c == 't' ? '\t' :
                c == 'r' ? '\r' :
                c == 'e' ? '\e' :
                c == 'b' ? '\b' :
                c == 'f' ? '\f' :
                c == 'v' ? '\v' :
                c == 'a' ? '\a' :
                # Literal escapes allowed in Julia source
                c == '\\' ? '\\' :
                c == '\'' ? '\'' :
                c == '"' ? '"' :
                c == '$' ? '$' :
                c == '`' ? '`' :
                throw(ArgumentError("Invalid escape sequence \\$c"))
            write(io, u)
        end
        i = nextind(str, i)
    end
end

function unescape_julia_string(str::AbstractString, is_cmd::Bool, is_raw::Bool)
    io = IOBuffer()
    if is_raw
        unescape_raw_string(io, str, is_cmd)
    else
        unescape_julia_string(io, str)
    end
    String(take!(io))
end

#-------------------------------------------------------------------------------
# Unicode normalization. As of Julia 1.8, this is part of Base and the Unicode
# stdlib under the name `Unicode.julia_chartransform`. See
# https://github.com/JuliaLang/julia/pull/42561
#
# To allow use on older Julia versions and to workaround the bug
# https://github.com/JuliaLang/julia/issues/45716
# we reproduce a specialized version of that logic here.

# static wrapper around user callback function
function utf8proc_custom_func(codepoint::UInt32, ::Ptr{Cvoid})::UInt32
    (codepoint == 0x025B ? 0x03B5 :
    codepoint == 0x00B5 ? 0x03BC :
    codepoint == 0x00B7 ? 0x22C5 :
    codepoint == 0x0387 ? 0x22C5 :
    codepoint == 0x2212 ? 0x002D :
    codepoint)
end

function utf8proc_decompose(str, options, buffer, nwords)
    ret = ccall(:utf8proc_decompose_custom, Int, (Ptr{UInt8}, Int, Ptr{UInt8}, Int, Cint, Ptr{Cvoid}, Ptr{Cvoid}),
                str, sizeof(str), buffer, nwords, options,
                @cfunction(utf8proc_custom_func, UInt32, (UInt32, Ptr{Cvoid})), C_NULL)
    ret < 0 && utf8proc_error(ret)
    return ret
end

function utf8proc_map(str::Union{String,SubString{String}}, options::Integer)
    nwords = utf8proc_decompose(str, options, C_NULL, 0)
    buffer = Base.StringVector(nwords*4)
    nwords = utf8proc_decompose(str, options, buffer, nwords)
    nbytes = ccall(:utf8proc_reencode, Int, (Ptr{UInt8}, Int, Cint), buffer, nwords, options)
    nbytes < 0 && utf8proc_error(nbytes)
    return String(resize!(buffer, nbytes))
end

function normalize_identifier(str)
    flags = Base.Unicode.UTF8PROC_STABLE | Base.Unicode.UTF8PROC_COMPOSE
    utf8proc_map(str, flags)
end


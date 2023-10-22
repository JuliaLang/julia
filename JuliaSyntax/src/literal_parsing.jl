"""
Nontrivia tokens (leaf nodes / literals) which are malformed are parsed into
ErrorVal when `ignore_errors=true` during parsing.
"""
struct ErrorVal
end

Base.show(io::IO, ::ErrorVal) = printstyled(io, "✘", color=:light_red)

#-------------------------------------------------------------------------------
# This file contains utility functions for converting undecorated source
# strings into Julia values.  For example, string->number, string unescaping, etc.

function parse_int_literal(str::AbstractString)
    # TODO: A specialized code path here can be a lot faster and also
    # allocation free
    str = replace(replace(str, '_'=>""), '−'=>'-')
    x = Base.tryparse(Int, str)
    if Int === Int32 && isnothing(x)
        x = Base.tryparse(Int64, str)
    end
    if isnothing(x)
        x = Base.tryparse(Int128, str)
        if x === nothing
            x = Base.parse(BigInt, str)
        end
    end
    return x
end

function parse_uint_literal(str::AbstractString, k)
    str = replace(str, '_'=>"")
    if startswith(str, '+')
        str = str[2:end]
    end
    ndigits = length(str)-2
    if k == K"HexInt"
        return ndigits <= 2  ? Base.parse(UInt8, str)   :
               ndigits <= 4  ? Base.parse(UInt16, str)  :
               ndigits <= 8  ? Base.parse(UInt32, str)  :
               ndigits <= 16 ? Base.parse(UInt64, str)  :
               ndigits <= 32 ? Base.parse(UInt128, str) :
               Base.parse(BigInt, str)
    elseif k == K"BinInt"
        ndigits = length(str)-2
        return ndigits <= 8   ? Base.parse(UInt8, str)   :
               ndigits <= 16  ? Base.parse(UInt16, str)  :
               ndigits <= 32  ? Base.parse(UInt32, str)  :
               ndigits <= 64  ? Base.parse(UInt64, str)  :
               ndigits <= 128 ? Base.parse(UInt128, str) :
               Base.parse(BigInt, str)
    elseif k == K"OctInt"
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
"""
Like `Base.parse(Union{Float64,Float32}, str)`, but permits float underflow

Parse a Float64. str[firstind:lastind] must be a valid floating point literal
string. If the value is outside Float64 range.
"""
function parse_float_literal(::Type{T}, str::Union{String,SubString,Vector{UInt8}},
        firstind::Integer, endind::Integer) where {T} # force specialize with where {T}
    strsize = endind - firstind
    bufsz = 50
    if strsize < bufsz
        buf = Ref{NTuple{bufsz, UInt8}}()
        ptr = Base.unsafe_convert(Ptr{UInt8}, pointer_from_objref(buf))
        GC.@preserve str buf begin
            n = _copy_normalize_number!(ptr, pointer(str, firstind), strsize)
            _unsafe_parse_float(T, ptr, n)
        end
    else
        # Slower path with allocation.
        buf = Vector{UInt8}(undef, strsize+1)
        ptr = pointer(buf)
        GC.@preserve str buf begin
            n = _copy_normalize_number!(ptr, pointer(str, firstind), strsize)
            _unsafe_parse_float(T, ptr, n)
        end
    end
end

# Like replace(replace(str, '_'=>""), '−'=>'-')
# dest must be of size at least srcsize+1
function _copy_normalize_number!(dest, src, srcsize)
    i = 0
    j = 0
    while i < srcsize
        b = unsafe_load(src + i)
        if b == UInt8('_')
            i += 1
            continue
        elseif b == 0xe2 && i+2 < srcsize &&
                unsafe_load(src + i + 1) == 0x88 &&
                unsafe_load(src + i + 2) == 0x92
            # src at i,i+1,i+2 is UTF-8 code for unicode minus sign '−'
            b = UInt8('-')
            i += 2
        end
        unsafe_store!(dest+j, b)
        i += 1
        j += 1
    end
    unsafe_store!(dest+j, 0x00)
    return j
end

# Internals of parse_float_literal, split into a separate function to avoid some
# apparent codegen issues https://github.com/JuliaLang/julia/issues/46509
# (perhaps we don't want the `buf` in `GC.@preserve buf` to be stack allocated
# on one branch and heap allocated in another?)
@inline function _unsafe_parse_float(::Type{Float64}, ptr, strsize)
    Libc.errno(0)
    endptr = Ref{Ptr{UInt8}}(C_NULL)
    x = ccall(:jl_strtod_c, Cdouble, (Ptr{UInt8}, Ptr{Ptr{UInt8}}), ptr, endptr)
    @check endptr[] == ptr + strsize
    status = :ok
    if Libc.errno() == Libc.ERANGE
        # strtod man page:
        # * If  the  correct  value  would cause overflow, plus or
        #   minus HUGE_VAL, HUGE_VALF, or HUGE_VALL is returned and
        #   ERANGE is stored in errno.
        # * If the correct value would cause underflow, a value with
        #   magnitude no larger than DBL_MIN, FLT_MIN, or LDBL_MIN is
        #   returned and ERANGE is stored in errno.
        status = abs(x) < 1 ? :underflow : :overflow
    end
    return (x, status)
end

@inline function _unsafe_parse_float(::Type{Float32}, ptr, strsize)
    # Convert float exponent 'f' to 'e' for strtof, eg, 1.0f0 => 1.0e0
    # Presumes we can modify the data in ptr!
    for p in ptr+strsize-1:-1:ptr
        if unsafe_load(p) == UInt8('f')
            unsafe_store!(p, UInt8('e'))
            break
        end
    end
    Libc.errno(0)
    endptr = Ref{Ptr{UInt8}}(C_NULL)
    status = :ok
    @static if Sys.iswindows()
        # Call strtod here and convert to Float32 on the Julia side because
        # strtof seems buggy on windows and doesn't set ERANGE correctly on
        # overflow. See also
        # https://github.com/JuliaLang/julia/issues/46544
        x = Float32(ccall(:jl_strtod_c, Cdouble, (Ptr{UInt8}, Ptr{Ptr{UInt8}}), ptr, endptr))
        if isinf(x)
            status = :overflow
            # Underflow not detected, but that will only be a warning elsewhere.
        end
    else
        x = ccall(:jl_strtof_c, Cfloat, (Ptr{UInt8}, Ptr{Ptr{UInt8}}), ptr, endptr)
    end
    @check endptr[] == ptr + strsize
    if Libc.errno() == Libc.ERANGE
        status = abs(x) < 1 ? :underflow : :overflow
    end
    return (x, status)
end


#-------------------------------------------------------------------------------
"""
Process Julia source code escape sequences for raw strings
"""
function unescape_raw_string(io::IO, txtbuf::Vector{UInt8},
                             firstind, endind, is_cmd::Bool)
    delim = is_cmd ? u8"`" : u8"\""
    i = firstind
    while i < endind
        c = txtbuf[i]
        if c != u8"\\"
            if c == u8"\r"
                # convert literal \r and \r\n in strings to \n (issue #11988)
                if i+1 < endind && txtbuf[i+1] == u8"\n"
                    i += 1
                end
                c = u8"\n"
            end
            write(io, c)
            i += 1
            continue
        end
        # Process \ escape sequences
        j = i
        while j < endind && txtbuf[j] == u8"\\"
            j += 1
        end
        nbackslash = j - i
        if (j < endind && txtbuf[j] == delim) || j >= endind
            # Backslashes before a delimiter must also be escaped
            nbackslash = div(nbackslash,2)
        end
        for k = 1:nbackslash
            write(io, u8"\\")
        end
        i = j
        if i < endind
            write(io, txtbuf[i])
            i += 1
        end
    end
end

"""
Process Julia source code escape sequences for non-raw strings.
`txtbuf` should be passed without delimiting quotes.
"""
function unescape_julia_string(io::IO, txtbuf::Vector{UInt8},
                               firstind, endind, diagnostics)
    had_error = false
    i = firstind
    while i < endind
        c = txtbuf[i]
        if c != u8"\\"
            if c == u8"\r"
                # convert literal \r and \r\n in strings to \n (issue #11988)
                if i+1 < endind && txtbuf[i+1] == u8"\n"
                    i += 1
                end
                c = u8"\n"
            end
            write(io, c)
            i = nextind(txtbuf, i)
            continue
        end
        # Process \ escape sequences.  See also Base.unescape_string which some
        # of this code derives from (but which disallows \` \' \$)
        escstart = i
        i += 1
        if i >= endind
            emit_diagnostic(diagnostics, escstart:endind-1,
                            error="invalid escape sequence")
            had_error = true
            break
        end
        c = txtbuf[i]
        if c == u8"x" || c == u8"u" || c == u8"U"
            n = k = 0
            m = c == u8"x" ? 2 :
                c == u8"u" ? 4 : 8
            while (k += 1) <= m && i+1 < endind
                nc = txtbuf[i+1]
                n = u8"0" <= nc <= u8"9" ? n<<4 + (nc-u8"0") :
                    u8"a" <= nc <= u8"f" ? n<<4 + (nc-u8"a"+10) :
                    u8"A" <= nc <= u8"F" ? n<<4 + (nc-u8"A"+10) : break
                i += 1
            end
            if k == 1 || n > 0x10ffff
                u = m == 4 ? u8"u" : u8"U"
                msg = (m == 2) ? "invalid hex escape sequence" :
                                 "invalid unicode escape sequence"
                emit_diagnostic(diagnostics, escstart:i, error=msg)
                had_error = true
            else
                if m == 2 # \x escape sequence
                    write(io, UInt8(n))
                else
                    print(io, Char(n))
                end
            end
        elseif u8"0" <= c <= u8"7"
            k = 1
            n = Int(c - u8"0")
            while (k += 1) <= 3 && i+1 < endind
                c = txtbuf[i+1]
                n = (u8"0" <= c <= u8"7") ? n<<3 + c-u8"0" : break
                i += 1
            end
            if n > 255
                emit_diagnostic(diagnostics, escstart:i,
                                error="invalid octal escape sequence")
                had_error = true
            else
                write(io, UInt8(n))
            end
        else
            u = # C escapes
                c == u8"n" ? u8"\n" :
                c == u8"t" ? u8"\t" :
                c == u8"r" ? u8"\r" :
                c == u8"e" ? u8"\e" :
                c == u8"b" ? u8"\b" :
                c == u8"f" ? u8"\f" :
                c == u8"v" ? u8"\v" :
                c == u8"a" ? u8"\a" :
                # Literal escapes allowed in Julia source
                c == u8"\\" ? u8"\\" :
                c == u8"'" ? u8"'" :
                c == u8"\"" ? u8"\"" :
                c == u8"$" ? u8"$" :
                c == u8"`" ? u8"`" :
                nothing
            if isnothing(u)
                emit_diagnostic(diagnostics, escstart:i,
                                error="invalid escape sequence")
                had_error = true
            else
                write(io, u)
            end
        end
        # For non-ascii characters we may not be in the middle of the UTF-8
        # encoding for that char, but this doesn't matter because unescaping
        # only relies on the ascii subset.
        i += 1
    end
    return had_error
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
    (codepoint == 0x025B ? 0x03B5 :  # 'ɛ' => 'ε'
    codepoint == 0x00B5 ? 0x03BC :   # 'µ' => 'μ'
    codepoint == 0x00B7 ? 0x22C5 :   # '·' => '⋅'
    codepoint == 0x0387 ? 0x22C5 :   # '·' => '⋅'
    codepoint == 0x2212 ? 0x002D :   # '−' (\minus) => '-'
    codepoint == 0x210F ? 0x0127 :   # 'ℏ' (\hslash) => 'ħ' \hbar
    codepoint)
end

function utf8proc_decompose(str, options, buffer, nwords)
    ret = ccall(:utf8proc_decompose_custom, Int, (Ptr{UInt8}, Int, Ptr{UInt8}, Int, Cint, Ptr{Cvoid}, Ptr{Cvoid}),
                str, sizeof(str), buffer, nwords, options,
                @cfunction(utf8proc_custom_func, UInt32, (UInt32, Ptr{Cvoid})), C_NULL)
    ret < 0 && Base.Unicode.utf8proc_error(ret)
    return ret
end

function utf8proc_map(str::Union{String,SubString{String}}, options::Integer)
    nwords = utf8proc_decompose(str, options, C_NULL, 0)
    buffer = Base.StringVector(nwords*4)
    nwords = utf8proc_decompose(str, options, buffer, nwords)
    nbytes = ccall(:utf8proc_reencode, Int, (Ptr{UInt8}, Int, Cint), buffer, nwords, options)
    nbytes < 0 && Base.Unicode.utf8proc_error(nbytes)
    return String(resize!(buffer, nbytes))
end

function normalize_identifier(str)
    flags = Base.Unicode.UTF8PROC_STABLE | Base.Unicode.UTF8PROC_COMPOSE
    return isascii(str) ? str : utf8proc_map(str, flags)
end


#-------------------------------------------------------------------------------
function parse_julia_literal(txtbuf::Vector{UInt8}, head::SyntaxHead, srcrange)
    k = kind(head)
    # Any errors parsing literals are represented as ErrorVal() - this can
    # happen when the user sets `ignore_errors=true` during parsing.
    if k == K"Float"
        v, code = parse_float_literal(Float64, txtbuf, first(srcrange),
                                      last(srcrange)+1)
        return (code === :ok || code === :underflow) ? v : ErrorVal()
    elseif k == K"Float32"
        v, code = parse_float_literal(Float32, txtbuf, first(srcrange),
                                      last(srcrange)+1)
        return (code === :ok || code === :underflow) ? v : ErrorVal()
    elseif k == K"Char"
        io = IOBuffer()
        had_error = unescape_julia_string(io, txtbuf, first(srcrange),
                                          last(srcrange)+1, Diagnostic[])
        if had_error
            return ErrorVal()
        else
            seek(io, 0)
            c = read(io, Char)
            return eof(io) ? c : ErrorVal()
        end
    elseif k in KSet"String CmdString"
        io = IOBuffer()
        had_error = false
        if has_flags(head, RAW_STRING_FLAG)
            unescape_raw_string(io, txtbuf, first(srcrange), last(srcrange)+1,
                                k == K"CmdString")
        else
            had_error = unescape_julia_string(io, txtbuf, first(srcrange),
                                              last(srcrange)+1, Diagnostic[])
        end
        return had_error ? ErrorVal() : String(take!(io))
    elseif k == K"true"
        return true
    elseif k == K"false"
        return false
    end

    # TODO: Avoid allocating temporary String here
    val_str = String(txtbuf[srcrange])
    if k == K"Integer"
        parse_int_literal(val_str)
    elseif k in KSet"BinInt OctInt HexInt"
        parse_uint_literal(val_str, k)
    elseif k == K"Identifier"
        if has_flags(head, RAW_STRING_FLAG)
            io = IOBuffer()
            unescape_raw_string(io, txtbuf, first(srcrange), last(srcrange)+1, false)
            Symbol(normalize_identifier(String(take!(io))))
        else
            Symbol(normalize_identifier(val_str))
        end
    elseif is_operator(k)
        isempty(srcrange)  ?
            Symbol(untokenize(k)) : # synthetic invisible tokens
            Symbol(normalize_identifier(val_str))
    elseif k == K"error"
        ErrorVal()
    elseif k == K"MacroName"
        Symbol("@$(normalize_identifier(val_str))")
    elseif k == K"StringMacroName"
        Symbol("@$(normalize_identifier(val_str))_str")
    elseif k == K"CmdMacroName"
        Symbol("@$(normalize_identifier(val_str))_cmd")
    elseif k == K"core_@cmd"
        Symbol("core_@cmd")
    elseif is_syntax_kind(head)
        nothing
    elseif is_keyword(k)
        # This should only happen for tokens nested inside errors
        Symbol(val_str)
    else
        # Other kinds should only happen for tokens nested inside errors
        # TODO: Consolidate this with the is_keyword() above? Something else?
        ErrorVal()
    end
end


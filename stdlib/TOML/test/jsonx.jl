# Vendored from https://raw.githubusercontent.com/JuliaIO/JSON.jl/refs/heads/master/vendor/jsonx.jl

module JSONX

"""
    JSONX.parse(json_str::String)
    JSONX.parse(bytes::AbstractVector{UInt8})

Parse a JSON string or byte array and return a Julia value.
Returns one of: Dict{String, Any}, Vector{Any}, String, Int64, Float64, Bool, or Nothing.
Numbers without decimal points or exponents are parsed as Int64, falling back to Float64 on overflow.
"""
function parse(json_str::String)
    pos = 1
    len = ncodeunits(json_str)
    pos = skip_whitespace(json_str, pos, len)
    pos > len && throw(ArgumentError("Empty or whitespace-only JSON"))
    result, new_pos = parse_value(json_str, pos, len)
    new_pos = skip_whitespace(json_str, new_pos, len)
    new_pos <= len && throw(ArgumentError("Trailing content after JSON"))
    return result
end

parse(bytes::AbstractVector{UInt8}) = parse(String(bytes))

"""
    JSONX.parsefile(filename::String)

Parse a JSON file and return a Julia value.
"""
parsefile(filename::String) = parse(read(filename, String))

"""
    JSONX.json(value)

Convert a Julia value to a JSON string.
Supports: AbstractDict, NamedTuple, AbstractVector, AbstractSet, Tuple, AbstractString, Number, Bool, Nothing, Missing.
"""
function json(value)
    io = IOBuffer()
    write_json(io, value)
    return String(take!(io))
end

# Helper function to skip whitespace
function skip_whitespace(str::String, pos::Int, len::Int)
    while pos <= len && isspace(Char(codeunit(str, pos)))
        pos += 1
    end
    return pos
end

# Unicode handling functions (adapted from JSON.jl)
utf16_is_surrogate(c::UInt16) = (c & 0xf800) == 0xd800
utf16_get_supplementary(lead::UInt16, trail::UInt16) = Char(UInt32(lead-0xd7f7)<<10 + trail)

function reverseescapechar(b)
    b == UInt8('"')  && return UInt8('"')
    b == UInt8('\\') && return UInt8('\\')
    b == UInt8('/')  && return UInt8('/')
    b == UInt8('b')  && return UInt8('\b')
    b == UInt8('f')  && return UInt8('\f')
    b == UInt8('n')  && return UInt8('\n')
    b == UInt8('r')  && return UInt8('\r')
    b == UInt8('t')  && return UInt8('\t')
    return 0x00
end

function unescape_string(str::String, start_pos::Int, end_pos::Int)
    # Unescape the string (we know it needs unescaping when this is called)
    io = IOBuffer()
    pos = start_pos
    while pos < end_pos
        c = codeunit(str, pos)
        if c == UInt8('\\')
            pos += 1
            pos >= end_pos && throw(ArgumentError("Unexpected end of input in string"))
            esc_c = codeunit(str, pos)
            if esc_c == UInt8('u')
                # Unicode escape sequence
                pos + 4 >= end_pos && throw(ArgumentError("Incomplete Unicode escape"))
                # Parse 4 hex digits
                c = UInt16(0)
                for offset in 1:4
                    bb = codeunit(str, pos + offset)
                    nv = if UInt8('0') <= bb <= UInt8('9')
                             bb - UInt8('0')
                         elseif UInt8('A') <= bb <= UInt8('F')
                             bb - (UInt8('A') - 10)
                         elseif UInt8('a') <= bb <= UInt8('f')
                             bb - (UInt8('a') - 10)
                         else
                             throw(ArgumentError("Invalid Unicode escape"))
                         end
                    c = (c << 4) + UInt16(nv)
                end
                pos += 4

                if utf16_is_surrogate(c)
                    # Check for surrogate pair
                    if pos + 6 < end_pos && codeunit(str, pos + 1) == UInt8('\\') && codeunit(str, pos + 2) == UInt8('u')
                        # Parse next 4 hex digits
                        c2 = UInt16(0)
                        for offset in 3:6
                            bb = codeunit(str, pos + offset)
                            nv = if UInt8('0') <= bb <= UInt8('9')
                                     bb - UInt8('0')
                                 elseif UInt8('A') <= bb <= UInt8('F')
                                     bb - (UInt8('A') - 10)
                                 elseif UInt8('a') <= bb <= UInt8('f')
                                     bb - (UInt8('a') - 10)
                                 else
                                     throw(ArgumentError("Invalid Unicode escape"))
                                 end
                            c2 = (c2 << 4) + UInt16(nv)
                        end
                        if utf16_is_surrogate(c2)
                            # Valid surrogate pair
                            ch = utf16_get_supplementary(c, c2)
                            print(io, ch)
                            pos += 6
                        else
                            # Invalid trailing surrogate, treat lead as lone
                            ch = Char(c)
                            print(io, ch)
                        end
                    else
                        # Lone surrogate - this is valid, just emit the character
                        ch = Char(c)
                        print(io, ch)
                    end
                else
                    # Non-surrogate Unicode
                    ch = Char(c)
                    print(io, ch)
                end
            else
                # Simple escape sequence
                b = reverseescapechar(esc_c)
                b == 0x00 && throw(ArgumentError("Invalid escape sequence \\$(Char(esc_c))"))
                print(io, Char(b))
            end

            pos += 1
        else
            # Regular character
            print(io, str[pos])
            pos = nextind(str, pos)
        end

    end
    return String(take!(io))
end

# Parse-related functionality
function parse_value(str::String, pos::Int, len::Int)
    pos > len && throw(ArgumentError("Unexpected end of input"))
    c = codeunit(str, pos)
    if c == UInt8('{')
        return parse_object(str, pos, len)
    elseif c == UInt8('[')
        return parse_array(str, pos, len)
    elseif c == UInt8('"')
        return parse_string(str, pos, len)
    elseif c == UInt8('n') && pos + 3 <= len && codeunit(str, pos + 1) == UInt8('u') && codeunit(str, pos + 2) == UInt8('l') && codeunit(str, pos + 3) == UInt8('l')
        return nothing, pos + 4
    elseif c == UInt8('t') && pos + 3 <= len && codeunit(str, pos + 1) == UInt8('r') && codeunit(str, pos + 2) == UInt8('u') && codeunit(str, pos + 3) == UInt8('e')
        return true, pos + 4
    elseif c == UInt8('f') && pos + 4 <= len && codeunit(str, pos + 1) == UInt8('a') && codeunit(str, pos + 2) == UInt8('l') && codeunit(str, pos + 3) == UInt8('s') && codeunit(str, pos + 4) == UInt8('e')
        return false, pos + 5
    elseif c == UInt8('-') || (UInt8('0') <= c <= UInt8('9'))
        return parse_number(str, pos, len)
    else
        throw(ArgumentError("Invalid JSON value starting at position $pos"))
    end
end

function parse_object(str::String, pos::Int, len::Int)
    codeunit(str, pos) != UInt8('{') && throw(ArgumentError("Expected '{' at position $pos"))
    pos += 1
    result = Dict{String, Any}()
    pos = skip_whitespace(str, pos, len)
    pos > len && throw(ArgumentError("Unexpected end of input in object"))
    codeunit(str, pos) == UInt8('}') && return result, pos + 1
    while true
        codeunit(str, pos) != UInt8('"') && throw(ArgumentError("Expected '\"' at position $pos"))
        key, pos = parse_string(str, pos, len)
        pos = skip_whitespace(str, pos, len)
        (pos > len || codeunit(str, pos) != UInt8(':')) && throw(ArgumentError("Expected ':' at position $pos"))
        pos += 1
        pos = skip_whitespace(str, pos, len)
        pos > len && throw(ArgumentError("Unexpected end of input in object"))
        value, pos = parse_value(str, pos, len)
        result[key] = value
        pos = skip_whitespace(str, pos, len)
        pos > len && throw(ArgumentError("Unexpected end of input in object"))
        if codeunit(str, pos) == UInt8('}')
            return result, pos + 1
        elseif codeunit(str, pos) == UInt8(',')
            pos += 1
            pos = skip_whitespace(str, pos, len)
        else
            throw(ArgumentError("Expected ',' or '}' at position $pos"))
        end
    end
end

function parse_array(str::String, pos::Int, len::Int)
    codeunit(str, pos) != UInt8('[') && throw(ArgumentError("Expected '[' at position $pos"))
    pos += 1
    result = []
    pos = skip_whitespace(str, pos, len)
    pos > len && throw(ArgumentError("Unexpected end of input in array"))
    codeunit(str, pos) == UInt8(']') && return result, pos + 1
    while true
        value, pos = parse_value(str, pos, len)
        push!(result, value)
        pos = skip_whitespace(str, pos, len)
        pos > len && throw(ArgumentError("Unexpected end of input in array"))
        if codeunit(str, pos) == UInt8(']')
            return result, pos + 1
        elseif codeunit(str, pos) == UInt8(',')
            pos += 1
            pos = skip_whitespace(str, pos, len)
            pos <= len && codeunit(str, pos) == UInt8(']') && throw(ArgumentError("Trailing comma not allowed in JSON"))
        else
            throw(ArgumentError("Expected ',' or ']' at position $pos"))
        end
    end
end

function parse_string(str::String, pos::Int, len::Int)
    codeunit(str, pos) != UInt8('"') && throw(ArgumentError("Expected '\"' at position $pos"))
    pos += 1
    start_pos = pos
    needs_unescape = false
    while pos <= len
        c = codeunit(str, pos)
        if c == UInt8('"')
            return needs_unescape ? unescape_string(str, start_pos, pos) : GC.@preserve(str, unsafe_string(pointer(str, start_pos), pos - start_pos)), pos + 1
        elseif c == UInt8('\\')
            needs_unescape = true
            pos += 1
            pos > len && throw(ArgumentError("Unexpected end of input in string"))
            esc_c = codeunit(str, pos)
            if esc_c == UInt8('"') || esc_c == UInt8('\\') || esc_c == UInt8('/') || esc_c == UInt8('b') || esc_c == UInt8('f') || esc_c == UInt8('n') || esc_c == UInt8('r') || esc_c == UInt8('t') || esc_c == UInt8('u')
                # Valid escape sequence, continue
            else
                throw(ArgumentError("Invalid escape sequence \\$(Char(esc_c))"))
            end
        elseif Int(c) < 0x20
            throw(ArgumentError("Control character in string"))
        end
        pos += 1
    end
    throw(ArgumentError("Unterminated string"))
end

function parse_number(str::String, pos::Int, len::Int)
    start_pos = pos
    has_decimal_or_exp = false
    while pos <= len
        c = codeunit(str, pos)
        if c == UInt8('-') || (UInt8('0') <= c <= UInt8('9')) || c == UInt8('+')
            pos += 1
        elseif c == UInt8('.') || c == UInt8('e') || c == UInt8('E')
            has_decimal_or_exp = true
            pos += 1
        else
            break
        end
    end
    num_str = @view str[start_pos:pos-1]

    # Try parsing as Int64 if no decimal point or exponent
    if !has_decimal_or_exp
        try
            return Base.parse(Int64, num_str), pos
        catch
            # Fall back to Float64 if Int64 parsing fails (e.g., overflow)
        end
    end

    # Parse as Float64
    try
        return Base.parse(Float64, num_str), pos
    catch
        throw(ArgumentError("Invalid number format"))
    end
end

# JSON writing functionality

struct JSONText
    text::String
end

function write_json(io::IO, value)
    if value === nothing || value === missing
        print(io, "null")
    elseif value isa Bool
        print(io, value ? "true" : "false")
    elseif value isa Number
        value isa Complex && throw(ArgumentError("Cannot serialize Complex numbers to JSON"))
        print(io, value)
    elseif value isa JSONText
        print(io, value.text)
    elseif value isa AbstractString
        write_string(io, value)
    elseif value isa AbstractVector || value isa AbstractSet || value isa Tuple
        write_array(io, value)
    elseif value isa AbstractDict || value isa NamedTuple
        write_object(io, value)
    elseif value isa Symbol || value isa Enum
        write_string(io, string(value))
    else
        throw(ArgumentError("Cannot serialize $(typeof(value)) to JSON"))
    end
end

function write_string(io::IO, str::AbstractString)
    print(io, '"')
    for c in str
        if c == '"'
            print(io, "\\\"")
        elseif c == '\\'
            print(io, "\\\\")
        elseif c == '\b'
            print(io, "\\b")
        elseif c == '\f'
            print(io, "\\f")
        elseif c == '\n'
            print(io, "\\n")
        elseif c == '\r'
            print(io, "\\r")
        elseif c == '\t'
            print(io, "\\t")
        elseif Int(c) < 0x20
            print(io, "\\u", string(Int(c), base=16, pad=4))
        else
            print(io, c)
        end
    end
    print(io, '"')
end

function write_array(io::IO, arr::Union{AbstractVector, AbstractSet, Tuple})
    print(io, '[')
    for (i, item) in enumerate(arr)
        i > 1 && print(io, ',')
        write_json(io, item)
    end
    print(io, ']')
end

function write_object(io::IO, dict::Union{AbstractDict, NamedTuple})
    print(io, '{')
    first = true
    for (key, value) in pairs(dict)
        !first && print(io, ',')
        first = false
        write_string(io, string(key))
        print(io, ':')
        write_json(io, value)
    end
    print(io, '}')
end

end # module
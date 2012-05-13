load("iostring.jl")
load("lru.jl")

bswap(c::Char) = identity(c) # white lie which won't work for multibyte characters

# Represents a packed composite type
type Struct
    canonical::String
    endianness::String
    pack::Function
    unpack::Function
    struct::Type
    size::Int
end
function Struct{T}(::Type{T}, endianness)
    if !isa(T, CompositeKind)
        error("Type $T is not a composite type.")
    end
    if !isbitsequivalent(T)
        error("Type $T is not bits-equivalent.")
    end
    s = string(T)
    if has(STRUCTS, s)
        return STRUCTS[s]
    end
    types = composite_fieldinfo(T)
    size = calcsize(types)
    packer, unpacker = endianness_converters(endianness)
    pack = struct_pack(packer, types, T)
    unpack = struct_unpack(unpacker, types, T)
    struct_utils(T)
    STRUCTS[s] = Struct(s, endianness, pack, unpack, T, size)
end
Struct{T}(::Type{T}) = Struct(T, "native")

canonicalize(s::String) = replace(s, r"\s|#.*$"m, "")

# TODO Handle strings and arrays
function isbitsequivalent{T}(::Type{T})
    if isa(T, BitsKind)
        return true
    elseif !isa(T, CompositeKind)
        return false
    end
    for S in T.types
        if !isbitsequivalent(S)
            return false
        end
    end
    true
end
isbitsequivalent(struct::Struct) = isbitsequivalent(struct.struct)

function composite_fieldinfo{T}(::Type{T})
    types = cell(length(T.names))
    for i in 1:length(T.names)
        types[i] = T.types[i], 1, T.names[i]
    end
    types
end

function calcsize(types)
    size = 0
    for (elemtype, dims) in types
        typ = elemtype <: Array ? eltype(elemtype) : elemtype
        size += if isa(typ, BitsKind)
            prod(dims)*sizeof(typ)
        elseif typ == Nothing # isa(Nothing, CompositeKind) == true
            prod(dims)
        elseif isa(typ, CompositeKind)
            prod(dims)*sizeof(Struct(typ))
        else
            error("Improper type $typ in struct.")
        end
    end
    size
end

# Struct string syntax
# Note that not all of these are checked by the regex itself but are verified after
# Presented in regex-style EBNF
#
# struct_string := endianness? element_specifier*
# endianness := "<" | ">" | "!" | "=" | "@"
# element_specifier := name? size? type
# name := "[" identifier "]"
# size := unsigned_integer | "(" unsigned_integer ("," unsigned_integer)* ")"
# type := predefined_type | "{" identifier "}"
# predefined_type := "x" | "c" | "b" | "B" | "?" | "h" | "i" | "I" | "l" | "L" | "q" | "Q" | "f" | "d"
function struct_parse(s::String)
    t = {}
    i = 2
    endianness = if s[1] == '<'
        "little"
    elseif s[1] == '>' || s[1] == '!'
        "big"
    elseif s[1] == '='
        "native"
    elseif s[1] == '@'
        println("Warning: struct does not support fully native structures.")
        "native"
    else
        i = 1 # no byte order command
        "native"
    end
    
    tmap = {'x' => Nothing,
            'c' => Char,
            'b' => Int8,
            'B' => Uint8,
            '?' => Bool,
            'h' => Int16,
            'H' => Uint16,
            'i' => Int32,
            'I' => Uint32,
            'l' => Int32,
            'L' => Uint32,
            'q' => Int64,
            'Q' => Uint64,
            'f' => Float32,
            'd' => Float64,
            #'s' => ASCIIString, #TODO
            }
    t = {}
    while i <= length(s)
        m = match(r"^                      # at the beginning of the string, find
                  (?:\[([a-zA-Z]\w*)\])?   # an optional name in []
                  (?:                      # then
                      (\d+)                # a vector length
                      |                    # or
                      \((\d+(?:,\d+)*)\)   # a comma-separated array size in ()
                  )?                       # or neither
                  (?:                      # followed by either
                      ([a-zA-Z?])          # a predefined type specifier
                      |                    # or
                      {([a-zA-Z]\w*)}      # another type in {}
                  )
                  "x, s[i:end])
        if isa(m, Nothing)
            error("Failed to compile struct; syntax error at ...$(s[i])...")
        end
        name, oneD, nD, typ, custtyp = m.captures
        dims = if isa(oneD, Nothing) && isa(nD, Nothing)
            1
        elseif isa(nD, Nothing)
            int(oneD)
        else
            tuple(map(int, split(nD, ','))...)
        end
        elemtype = if isa(custtyp, Nothing)
            tmap[typ[1]]
        else
            testtype = eval(symbol(custtyp)) #is there an easier way to do this?
            if isbitsequivalent(testtype)
                testtype
            else
                error("Failed to compile struct; $testtype is not a bits-equivalent type.")
            end
        end
        i += length(m.match)
        t = {t...; (elemtype, dims, name)}
    end
    (endianness, t)
end

# Generate an anonymous composite type from a list of its element types
function gen_typelist(types::Array)
    xprs = {}
    for (typ, dims, name) in types
        if typ == Nothing
            continue
        end
        fn = !isa(name, Nothing) ? symbol(name) : gensym("field$(length(xprs)+1)")
        xpr = if dims == 1
            :(($fn)::($typ))
        else
            if typ != ASCIIString
                sz = length(dims)
                :(($fn)::Array{($typ),($sz)})
            else
                :(($fn)::($typ))
            end
        end
        xprs = {xprs...; xpr}
    end
    xprs         
end
function gen_type(types)
    @gensym struct
    fields = gen_typelist(types)
    typedef = quote
        type $struct
            $(fields...)
        end
        $struct
    end
    eval(typedef)
end

# Generate an unpack function for a composite type
function gen_readers(convert::Function, types::Array, stream::Symbol)
    xprs = similar(types)
    for i in 1:length(types)
        typ, dims = types[i]
        xprs[i] = if typ == Nothing
            nskip = prod(dims)
            :(skip($stream, $nskip); nothing)
        elseif isa(typ, CompositeKind)
            :(unpack($stream, $typ))
        elseif dims == 1
            :(($convert)(read($stream, $typ)))
        else
            :(map($convert, read($stream, $typ, $dims...)))
        end
    end
    xprs
end
function struct_unpack(convert, types, struct_type)
    @gensym in
    readers = gen_readers(convert, types, in)
    unpackdef = quote
        (($in)::IO) -> begin
            # Can we be assured the order of evaluation of arguments?
            ($struct_type)($(readers...))
        end
    end
    eval(unpackdef)
end

# Generate a pack function for a composite type
function gen_writers(convert::Function, types::Array, struct_type, stream::Symbol, struct::Symbol)
    @gensym fieldnames
    xprs = {:(local $fieldnames = $struct_type.names)}
    elnum = 0
    for (typ, dims) in types
        elnum += 1
        xpr = if typ == Nothing
            nskip = prod(dims)
            elnum -= 1
            @gensym ii
            :(for $ii = 1:nskip; write($stream, uint8(0)); end)
        elseif isa(typ, CompositeKind)
            :(pack($stream, getfield($struct, ($fieldnames)[$elnum])))
        elseif dims == 1
            :(write($stream, ($convert)(getfield($struct, ($fieldnames)[$elnum]))))
        else
            ranges = tuple([1:d for d in dims]...)
            :(write($stream, map($convert, ref(getfield($struct, ($fieldnames)[$elnum]), ($ranges)...))))
        end
        xprs = {xprs...; xpr}
    end
    xprs
end
function struct_pack(convert, types, struct_type)
    @gensym out struct
    writers = gen_writers(convert, types, struct_type, out, struct)
    packdef = quote
        (($out)::IO, ($struct)::($struct_type)) -> begin
            $(writers...)
        end
    end
    eval(packdef)
end

function endianness_converters(endianness::String)
    if endianness == "big"
        hton, ntoh
    elseif endianness == "little"
        htol, ltoh
    elseif endianness == "native"
        identity, identity
    else
        error("Endianness must be \"big\", \"little\", or \"native\"; got \"$endianness\".")
    end
end

function struct_utils(struct_type)
    @eval ref(struct::($struct_type), i::Integer) = struct.(($struct_type).names[i])
    @eval ref(struct::($struct_type), x) = [struct.(($struct_type).names[i]) for i in x]
    @eval length(struct::($struct_type)) = length(($struct_type).names)
    # this could be better
    @eval isequal(a::($struct_type), b::($struct_type)) = isequal(a[1:end], b[1:end])
end

# Structure cache:
# * a given canonical struct string returns the same struct (until you run out of cache space)
# * we don't spend time regenerating types and functions when we don't have to
const STRUCTS = BoundedLRU()

function interp_struct_parse(str::String)
    s = canonicalize(str)
    if has(STRUCTS, s)
        return STRUCTS[s]
    end
    endianness, types = struct_parse(s)
    size = calcsize(types)
    packer, unpacker = endianness_converters(endianness)
    struct_type = gen_type(types)
    pack = struct_pack(packer, types, struct_type)
    unpack = struct_unpack(unpacker, types, struct_type)
    struct_utils(struct_type)
    STRUCTS[s] = Struct(s, endianness, pack, unpack, struct_type, size)
end

macro s_str(str)
    interp_struct_parse(eval(_jl_interp_parse(str)))
end

# Julian aliases for the "object-style" calls to pack/unpack/struct
function pack(out::IO, s::Struct, struct_or_only_item)
    if isa(struct_or_only_item, s.struct)
        s.pack(out, struct_or_only_item)
    else
        s.pack(out, s.struct(struct_or_only_item))
    end
end
pack{T}(out::IO, composite::T) = pack(out, Struct(T), composite)
pack(out::IO, s::Struct, args...) = s.pack(out, s.struct(args...))

unpack(in::IO, s::Struct) = s.unpack(in)
unpack{T}(in::IO, ::Type{T}) = Struct(T).unpack(in)

struct(s::Struct, items...) = s.struct(items...)
sizeof(s::Struct) = s.size

# Convenience methods when you just want to use strings
macro withIOString(iostr, ex)
    quote
        $iostr = IOString()
        $ex
        $iostr
    end
end
pack(s::Struct, arg) = @withIOString iostr pack(iostr, s, arg)
pack(s::Struct, args...) = @withIOString iostr pack(iostr, s, args...)
pack(composite) = @withIOString iostr pack(iostr, composite)

unpack(str::String, s::Struct) = unpack(IOString(str), s)
unpack(str::String, ctyp) = unpack(IOString(str), ctyp)

## object-oriented Regex interface ##

include("pcre.jl")

const DEFAULT_OPTS = PCRE.JAVASCRIPT_COMPAT | PCRE.UTF8 | PCRE.NO_UTF8_CHECK

type Regex
    pattern::ByteString
    options::UInt32
    regex::Ptr{Void}
    extra::Ptr{Void}
    ovec::Vector{Int32}


    function Regex(pattern::AbstractString, options::Integer)
        pattern = bytestring(pattern)
        options = int32(options)
        if (options & ~PCRE.OPTIONS_MASK) != 0
            error("invalid regex options: $options")
        end
        re = compile(new(pattern, options, C_NULL, C_NULL, Array(Int32, 0)))
        finalizer(re,
            function(re::Regex)
                re.extra != C_NULL && PCRE.free_study(re.extra)
                re.regex != C_NULL && PCRE.free(re.regex)
            end)
        re
    end
end

function Regex(pattern::AbstractString, flags::AbstractString)
    options = DEFAULT_OPTS
    for f in flags
        options |= f=='i' ? PCRE.CASELESS  :
                   f=='m' ? PCRE.MULTILINE :
                   f=='s' ? PCRE.DOTALL    :
                   f=='x' ? PCRE.EXTENDED  :
                   error("unknown regex flag: $f")
    end
    Regex(pattern, options)
end
Regex(pattern::AbstractString) = Regex(pattern, DEFAULT_OPTS)

function compile(regex::Regex)
    if regex.regex == C_NULL
        regex.regex = PCRE.compile(regex.pattern, regex.options & PCRE.COMPILE_MASK)
        regex.extra = PCRE.study(regex.regex, PCRE.STUDY_JIT_COMPILE)
        ncap  = PCRE.info(regex.regex, regex.extra,
                          PCRE.INFO_CAPTURECOUNT, Int32)
        resize!(regex.ovec, 3(ncap+1))
    end
    regex
end

macro r_str(pattern, flags...) Regex(pattern, flags...) end
macro r_mstr(pattern, flags...) Regex(pattern, flags...) end

copy(r::Regex) = r

function show(io::IO, re::Regex)
    imsx = PCRE.CASELESS|PCRE.MULTILINE|PCRE.DOTALL|PCRE.EXTENDED
    if (re.options & ~imsx) == DEFAULT_OPTS
        print(io, 'r')
        print_quoted_literal(io, re.pattern)
        if (re.options & PCRE.CASELESS ) != 0; print(io, 'i'); end
        if (re.options & PCRE.MULTILINE) != 0; print(io, 'm'); end
        if (re.options & PCRE.DOTALL   ) != 0; print(io, 's'); end
        if (re.options & PCRE.EXTENDED ) != 0; print(io, 'x'); end
    else
        print(io, "Regex(")
        show(io, re.pattern)
        print(io, ',')
        show(io, re.options)
        print(io, ')')
    end
end

# TODO: map offsets into non-ByteStrings back to original indices.
# or maybe it's better to just fail since that would be quite slow

immutable RegexMatch
    match::SubString{UTF8String}
    captures::Vector{Union(Void,SubString{UTF8String})}
    offset::Int
    offsets::Vector{Int}
end

function show(io::IO, m::RegexMatch)
    print(io, "RegexMatch(")
    show(io, m.match)
    if !isempty(m.captures)
        print(io, ", ")
        for i = 1:length(m.captures)
            print(io, i, "=")
            show(io, m.captures[i])
            if i < length(m.captures)
                print(io, ", ")
            end
        end
    end
    print(io, ")")
end

function ismatch(r::Regex, s::AbstractString, offset::Integer=0)
    compile(r)
    return PCRE.exec(r.regex, r.extra, bytestring(s), offset, r.options & PCRE.EXECUTE_MASK,
                     r.ovec)
end

function ismatch(r::Regex, s::SubString, offset::Integer=0)
    compile(r)
    return PCRE.exec(r.regex, r.extra, s, offset, r.options & PCRE.EXECUTE_MASK,
                  r.ovec)
end

function match(re::Regex, str::UTF8String, idx::Integer, add_opts::UInt32=uint32(0))
    opts = re.options & PCRE.EXECUTE_MASK | add_opts
    compile(re)
    if !PCRE.exec(re.regex, re.extra, str, idx-1, opts, re.ovec)
        return nothing
    end
    n = length(re.ovec)/3 - 1
    mat = SubString(str, re.ovec[1]+1, re.ovec[2])
    cap = Union(Void,SubString{UTF8String})[
            re.ovec[2i+1] < 0 ? nothing : SubString(str, re.ovec[2i+1]+1, re.ovec[2i+2]) for i=1:n ]
    off = Int[ re.ovec[2i+1]::Int32+1 for i=1:n ]
    RegexMatch(mat, cap, re.ovec[1]+1, off)
end

match(re::Regex, str::Union(ByteString,SubString), idx::Integer, add_opts::UInt32=uint32(0)) =
    match(re, utf8(str), idx, add_opts)

match(r::Regex, s::AbstractString) = match(r, s, start(s))
match(r::Regex, s::AbstractString, i::Integer) =
    error("regex matching is only available for bytestrings; use bytestring(s) to convert")

function matchall(re::Regex, str::UTF8String, overlap::Bool=false)
    regex = compile(re).regex
    extra = re.extra
    n = length(str.data)
    matches = SubString{UTF8String}[]
    offset = int32(0)
    opts = re.options & PCRE.EXECUTE_MASK
    opts_nonempty = opts | PCRE.ANCHORED | PCRE.NOTEMPTY_ATSTART
    prevempty = false
    ovec = Array(Int32, 3)
    while true
        result = ccall((:pcre_exec, :libpcre), Int32,
                       (Ptr{Void}, Ptr{Void}, Ptr{UInt8}, Int32,
                       Int32, Int32, Ptr{Int32}, Int32),
                       regex, extra, str, n,
                       offset, prevempty ? opts_nonempty : opts, ovec, 3)

        if result < 0
            if prevempty && offset < n
                offset = int32(nextind(str, offset + 1) - 1)
                prevempty = false
                continue
            else
                break
            end
        end

        push!(matches, SubString(str, ovec[1]+1, ovec[2]))
        prevempty = offset == ovec[2]
        if overlap
            if !prevempty
                offset = int32(ovec[1]+1)
            end
        else
            offset = ovec[2]
        end
    end
    matches
end

matchall(re::Regex, str::Union(ByteString,SubString), overlap::Bool=false) =
    matchall(re, utf8(str), overlap)

function search(str::Union(ByteString,SubString), re::Regex, idx::Integer)
    if idx > nextind(str,endof(str))
        throw(BoundsError())
    end
    opts = re.options & PCRE.EXECUTE_MASK
    compile(re)
    PCRE.exec(re.regex, re.extra, str, idx-1, opts, re.ovec) ?
        ((re.ovec[1]+1):prevind(str,re.ovec[2]+1)) : (0:-1)
end
search(s::AbstractString, r::Regex, idx::Integer) =
    error("regex search is only available for bytestrings; use bytestring(s) to convert")
search(s::AbstractString, r::Regex) = search(s,r,start(s))

immutable RegexMatchIterator
    regex::Regex
    string::UTF8String
    overlap::Bool

    function RegexMatchIterator(regex::Regex, string::AbstractString, ovr::Bool=false)
        new(regex, string, ovr)
    end
end
compile(itr::RegexMatchIterator) = (compile(itr.regex); itr)
eltype(itr::RegexMatchIterator) = RegexMatch
start(itr::RegexMatchIterator) = match(itr.regex, itr.string, 1, uint32(0))
done(itr::RegexMatchIterator, prev_match) = (prev_match == nothing)

# Assumes prev_match is not nothing
function next(itr::RegexMatchIterator, prev_match)
    prevempty = isempty(prev_match.match)

    if itr.overlap
        if !prevempty
            offset = nextind(itr.string, prev_match.offset)
        else
            offset = prev_match.offset
        end
    else
        offset = prev_match.offset + endof(prev_match.match)
    end

    opts_nonempty = uint32(PCRE.ANCHORED | PCRE.NOTEMPTY_ATSTART)
    while true
        mat = match(itr.regex, itr.string, offset,
                    prevempty ? opts_nonempty : uint32(0))

        if mat === nothing
            if prevempty && offset <= length(itr.string.data)
                offset = nextind(itr.string, offset)
                prevempty = false
                continue
            else
                break
            end
        else
            return (prev_match, mat)
        end
    end
    (prev_match, nothing)
end

function eachmatch(re::Regex, str::AbstractString, ovr::Bool=false)
    RegexMatchIterator(re,str,ovr)
end

eachmatch(re::Regex, str::AbstractString) = RegexMatchIterator(re,str)

# miscellaneous methods that depend on Regex being defined

filter!(r::Regex, v) = filter!(x->ismatch(r,x), v)
filter(r::Regex, v)  = filter(x->ismatch(r,x), v)

filter!(r::Regex, d::Dict) = filter!((k,v)->ismatch(r,k),d)
filter(r::Regex,  d::Dict) = filter!(r,copy(d))


# Don't serialize the pointers
function serialize(s, r::Regex)
    serialize_type(s, typeof(r))
    serialize(s, r.pattern)
    serialize(s, r.options)
end

function deserialize(s, t::Type{Regex})
    pattern = deserialize(s)
    options = deserialize(s)
    Regex(pattern, options)
end

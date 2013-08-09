## object-oriented Regex interface ##

include("pcre.jl")

const DEFAULT_OPTS = PCRE.JAVASCRIPT_COMPAT | PCRE.UTF8 | PCRE.NO_UTF8_CHECK

immutable Regex
    pattern::ByteString
    options::Uint32
    regex::Array{Uint8}

    function Regex(pattern::String, options::Integer)
        pattern = bytestring(pattern)
        options = uint32(options)
        if (options & ~PCRE.OPTIONS_MASK) != 0
            error("invalid regex options: $options")
        end
        regex = PCRE.compile(pattern, options & PCRE.COMPILE_MASK)
        new(pattern, options, regex)
    end
end

function Regex(pattern::String, flags::String)
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
Regex(pattern::String) = Regex(pattern, DEFAULT_OPTS)

macro r_str(pattern, flags...) Regex(pattern, flags...) end

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
    captures::Vector{Union(Nothing,SubString{UTF8String})}
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

# TODO: add ismatch with an offset.
ismatch(r::Regex, s::String) =
    PCRE.exec(r.regex, C_NULL, bytestring(s), 0, r.options & PCRE.EXECUTE_MASK, false)

function match(re::Regex, str::UTF8String, idx::Integer, add_opts::Uint32=uint32(0),
               extra::Ptr{Void}=C_NULL)
    opts = re.options & PCRE.EXECUTE_MASK | add_opts
    m, n = PCRE.exec(re.regex, extra, str, idx-1, opts, true)
    if isempty(m); return nothing; end
    mat = SubString(str, m[1]+1, m[2])
    cap = Union(Nothing,SubString{UTF8String})[
            m[2i+1] < 0 ? nothing : SubString(str, m[2i+1]+1, m[2i+2]) for i=1:n ]
    off = Int[ m[2i+1]::Int32+1 for i=1:n ]
    RegexMatch(mat, cap, m[1]+1, off)
end

match(re::Regex, str::ByteString, idx::Integer, add_opts::Uint32=uint32(0)) =
    match(re, utf8(str), idx, add_opts)

match(r::Regex, s::String) = match(r, s, start(s))
match(r::Regex, s::String, i::Integer) =
    error("regex matching is only available for bytestrings; use bytestring(s) to convert")

function matchall(re::Regex, str::UTF8String, overlap::Bool=false)
    extra = PCRE.study(re.regex, PCRE.STUDY_JIT_COMPILE)
    n = length(str.data)
    matches = SubString{UTF8String}[]
    offset = int32(0)
    opts = re.options & PCRE.EXECUTE_MASK
    opts_nonempty = opts | PCRE.ANCHORED | PCRE.NOTEMPTY_ATSTART
    prevempty = false
    ovec = Array(Int32, 3)
    while true
        result = ccall((:pcre_exec, :libpcre), Int32,
                       (Ptr{Void}, Ptr{Void}, Ptr{Uint8}, Int32,
                       Int32, Int32, Ptr{Int32}, Int32),
                       re.regex, extra, str, n,
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
                offset = int32(nextind(str, offset + 1) - 1)
            end
        else
            offset = ovec[2]
        end
    end
    PCRE.free_study(extra)
    matches
end

matchall(re::Regex, str::ByteString, overlap::Bool=false) =
    matchall(re, utf8(str), overlap)

function search(str::ByteString, re::Regex, idx::Integer)
    if idx > nextind(str,endof(str))
        throw(BoundsError())
    end
    opts = re.options & PCRE.EXECUTE_MASK
    m, n = PCRE.exec(re.regex, C_NULL, str, idx-1, opts, true)
    isempty(m) ? (0:-1) : ((m[1]+1):prevind(str,m[2]+1))
end
search(s::String, r::Regex, idx::Integer) =
    error("regex search is only available for bytestrings; use bytestring(s) to convert")
search(s::String, r::Regex) = search(s,r,start(s))

immutable RegexMatchIterator
    regex::Regex
    string::UTF8String
    overlap::Bool
    extra::Ptr{Void}

    function RegexMatchIterator(regex::Regex, string::String, ovr::Bool=false)
        extra = PCRE.study(regex.regex, PCRE.STUDY_JIT_COMPILE)
        new(regex, string, ovr, extra)
    end
end

eltype(itr::RegexMatchIterator) = RegexMatch
start(itr::RegexMatchIterator) = match(itr.regex, itr.string, 1, uint32(0), itr.extra)
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
                    prevempty ? opts_nonempty : uint32(0), itr.extra)

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

    PCRE.free_study(itr.extra)
    (prev_match, nothing)
end

function eachmatch(re::Regex, str::String, ovr::Bool=false)
    RegexMatchIterator(re,str,ovr)
end

eachmatch(re::Regex, str::String) = RegexMatchIterator(re,str)

# miscellaneous methods that depend on Regex being defined

filter!(r::Regex, v) = filter!(x->ismatch(r,x), v)
filter(r::Regex, v)  = filter(x->ismatch(r,x), v)

filter!(r::Regex, d::Dict) = filter!((k,v)->ismatch(r,k),d)
filter(r::Regex,  d::Dict) = filter!(r,copy(d))

## object-oriented Regex interface ##

include("pcre.jl")

const DEFAULT_OPTS = PCRE.JAVASCRIPT_COMPAT | PCRE.UTF8

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
    match::ByteString
    captures::Vector{Union(Nothing,ByteString)}
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

function match(re::Regex, str::ByteString, idx::Integer)
    opts = re.options & PCRE.EXECUTE_MASK
    m, n = PCRE.exec(re.regex, C_NULL, str, idx-1, opts, true)
    if isempty(m); return nothing; end
    mat = str[m[1]+1:m[2]]
    cap = Union(Nothing,ByteString)[
            m[2i+1] < 0 ? nothing : str[m[2i+1]+1:m[2i+2]] for i=1:n ]
    off = Int[ m[2i+1]::Int32+1 for i=1:n ]
    RegexMatch(mat, cap, m[1]+1, off)
end
match(r::Regex, s::String) = match(r, s, start(s))
match(r::Regex, s::String, i::Integer) =
    error("regex matching is only available for bytestrings; use bytestring(s) to convert")

function search(str::ByteString, re::Regex, idx::Integer)
    len = length(str.data)
    if idx >= len+2
        return idx == len+2 ? (0:-1) : error(BoundsError)
    end
    opts = re.options & PCRE.EXECUTE_MASK
    m, n = PCRE.exec(re.regex, C_NULL, str, idx-1, opts, true)
    isempty(m) ? (0:-1) : ((m[1]+1):m[2])
end
search(s::String, r::Regex, idx::Integer) =
    error("regex search is only available for bytestrings; use bytestring(s) to convert")
search(s::String, r::Regex) = search(s,r,start(s))

immutable RegexMatchIterator
    regex::Regex
    string::ByteString
    overlap::Bool
end

start(itr::RegexMatchIterator) = match(itr.regex, itr.string)
done(itr::RegexMatchIterator, m) = m == nothing
next(itr::RegexMatchIterator, m) =
    (m, match(itr.regex, itr.string, m.offset + (itr.overlap ? 1 : length(m.match))))

each_match(re::Regex, str::String, ovr::Bool) = RegexMatchIterator(re,str,ovr)
each_match(re::Regex, str::String)            = RegexMatchIterator(re,str,false)

# miscellaneous methods that depend on Regex being defined

filter!(r::Regex, d::Dict) = filter!((k,v)->ismatch(r,k),d)
filter(r::Regex,  d::Dict) = filter!(r,copy(d))

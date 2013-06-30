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

function match(re::Regex, str::ByteString, idx::Integer, add_opts::Uint32)
    opts = re.options & PCRE.EXECUTE_MASK | add_opts
    m, n = PCRE.exec(re.regex, C_NULL, str, idx-1, opts, true)
    if isempty(m); return nothing; end
    mat = str[m[1]+1:m[2]]
    cap = Union(Nothing,ByteString)[
            m[2i+1] < 0 ? nothing : str[m[2i+1]+1:m[2i+2]] for i=1:n ]
    off = Int[ m[2i+1]::Int32+1 for i=1:n ]
    RegexMatch(mat, cap, m[1]+1, off)
end
match(re::Regex, str::ByteString, idx::Integer) = match(re, str, idx, uint32(0))
match(r::Regex, s::String) = match(r, s, start(s))
match(r::Regex, s::String, i::Integer) =
    error("regex matching is only available for bytestrings; use bytestring(s) to convert")

function matchall(re::Regex, str::ByteString, overlap::Bool)
    [eachmatch(re, str, overlap)...]
end

matchall(re::Regex, str::ByteString) = matchall(re, str, false)

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
    is_utf::Bool

    function RegexMatchIterator(regex::Regex, string::String, ovr::Bool)
        re_opts = PCRE.info(regex.regex, C_NULL, PCRE.INFO_OPTIONS, Uint32)
        is_utf = (re_opts & PCRE.UTF8) != 0

        new(regex, string, ovr, is_utf)
    end
    RegexMatchIterator(regex::Regex, string::String) = RegexMatchIterator(regex, string, false)
end

start(itr::RegexMatchIterator) = match(itr.regex, itr.string, 1)
done(itr::RegexMatchIterator, prev_match) = (prev_match == nothing)

# Assumes prev_match is not nothing
function next(itr::RegexMatchIterator, prev_match)
    m = prev_match
    str = itr.string

    while true
      opts = uint32(0)
      if m != nothing
          idx = itr.overlap ? next(str, m.offset)[2] : m.offset + length(m.match.data)

          if length(m.match) == 0
              if m.offset == length(str.data) + 1
                  break
              end
              opts = opts | PCRE.ANCHORED | PCRE.NOTEMPTY_ATSTART
          end
      end

      m = match(itr.regex, str, idx, opts)
      if m == nothing
          if opts == 0
              break
          end
          idx = next(str, idx)[2]
          continue
      end

      return (prev_match, m)
    end

    (prev_match, nothing)
end

eachmatch(re::Regex, str::String, ovr::Bool) = RegexMatchIterator(re,str,ovr)
eachmatch(re::Regex, str::String)            = RegexMatchIterator(re,str)

# miscellaneous methods that depend on Regex being defined

filter!(r::Regex, v) = filter!(x->ismatch(r,x), v)
filter(r::Regex, v)  = filter(x->ismatch(r,x), v)

filter!(r::Regex, d::Dict) = filter!((k,v)->ismatch(r,k),d)
filter(r::Regex,  d::Dict) = filter!(r,copy(d))

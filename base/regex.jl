include("pcre.jl")

## object-oriented Regex interface ##

type Regex
    pattern::ByteString
    options::Int32
    regex::Array{Uint8}
    extra::Ptr{Void}

    function Regex(pat::String, opts::Integer, study::Bool)
        pat = cstring(pat); opts = int32(opts)
        if (opts & ~PCRE_OPTIONS_MASK) != 0
            error("invalid regex option(s)")
        end
        re = pcre_compile(pat, opts & PCRE_COMPILE_MASK)
        ex = study ? pcre_study(re) : C_NULL
        new(pat, opts, re, ex)
    end
end
Regex(p::String, s::Bool)    = Regex(p, 0, s)
Regex(p::String, o::Integer) = Regex(p, o, false)
Regex(p::String)             = Regex(p, 0, false)

# TODO: make sure thing are escaped in a way PCRE
# likes so that Julia all the Julia string quoting
# constructs are correctly handled.

macro r_str(pattern, flags...)
    options = PCRE_UTF8
    for fx in flags, f in fx
        options |= f=='i' ? PCRE_CASELESS  :
                   f=='m' ? PCRE_MULTILINE :
                   f=='s' ? PCRE_DOTALL    :
                   f=='x' ? PCRE_EXTENDED  :
                   error("unknown regex flag: $f")
    end
    Regex(pattern, options)
end

function show(io, re::Regex)
    imsx = PCRE_CASELESS|PCRE_MULTILINE|PCRE_DOTALL|PCRE_EXTENDED
    if (re.options & ~imsx) == PCRE_UTF8
        print(io, 'r')
        print_quoted_literal(io, re.pattern)
        if (re.options & PCRE_CASELESS ) != 0; print(io, 'i'); end
        if (re.options & PCRE_MULTILINE) != 0; print(io, 'm'); end
        if (re.options & PCRE_DOTALL   ) != 0; print(io, 's'); end
        if (re.options & PCRE_EXTENDED ) != 0; print(io, 'x'); end
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

type RegexMatch
    match::ByteString
    captures::Tuple
    offset::Int
    offsets::Vector{Int}
end

function show(io, m::RegexMatch)
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

matches(r::Regex, s::String, o::Integer) =
    pcre_exec(r.regex, r.extra, cstring(s), 0, o, false)
matches(r::Regex, s::String) = matches(r, s, r.options & PCRE_EXECUTE_MASK)

contains(s::String, r::Regex, opts::Integer) = matches(r,s,opts)
contains(s::String, r::Regex)                = matches(r,s)

function match(re::Regex, str::ByteString, idx::Integer, opts::Integer)
    m, n = pcre_exec(re.regex, re.extra, str, idx-1, opts, true)
    if isempty(m); return nothing; end
    mat = str[m[1]+1:m[2]]
    cap = ntuple(n, i->(m[2i+1] < 0 ? nothing : str[m[2i+1]+1:m[2i+2]]))
    off = map(i->m[2i+1]+1, [1:n])
    RegexMatch(mat, cap, m[1]+1, off)
end
match(r::Regex, s::String, i::Integer, o::Integer) = match(r, cstring(s), i, o)
match(r::Regex, s::String, i::Integer) = match(r, s, i, r.options & PCRE_EXECUTE_MASK)
match(r::Regex, s::String) = match(r, s, start(s))

function search(str::ByteString, re::Regex, idx::Integer)
    len = length(str)
    if idx >= len+2
        return idx == len+2 ? (0,0) : error("index out of range")
    end
    opts = re.options & PCRE_EXECUTE_MASK
    m, n = pcre_exec(re.regex, re.extra, str, idx-1, opts, true)
    isempty(m) ? (0,0) : (m[1]+1,m[2]+1)
end
search(s::ByteString, r::Regex) = search(s,r,start(s))

type RegexMatchIterator
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

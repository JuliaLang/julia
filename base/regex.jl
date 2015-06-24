# This file is a part of Julia. License is MIT: http://julialang.org/license

## object-oriented Regex interface ##

include("pcre.jl")

const DEFAULT_COMPILER_OPTS = PCRE.UTF | PCRE.NO_UTF_CHECK | PCRE.ALT_BSUX
const DEFAULT_MATCH_OPTS = PCRE.NO_UTF_CHECK

type Regex
    pattern::ByteString
    compile_options::UInt32
    match_options::UInt32
    regex::Ptr{Void}
    extra::Ptr{Void}
    ovec::Vector{Csize_t}
    match_data::Ptr{Void}


    function Regex(pattern::AbstractString, compile_options::Integer,
                   match_options::Integer)
        pattern = bytestring(pattern)
        compile_options = UInt32(compile_options)
        match_options = UInt32(match_options)
        if (compile_options & ~PCRE.COMPILE_MASK) != 0
            throw(ArgumentError("invalid regex compile options: $compile_options"))
        end
        if (match_options & ~PCRE.EXECUTE_MASK) !=0
            throw(ArgumentError("invalid regex match options: $match_options"))
        end
        re = compile(new(pattern, compile_options, match_options, C_NULL,
                         C_NULL, Csize_t[], C_NULL))
        finalizer(re, re->begin
                              re.regex == C_NULL || PCRE.free_re(re.regex)
                              re.match_data == C_NULL || PCRE.free_match_data(re.match_data)
                          end)
        re
    end
end

function Regex(pattern::AbstractString, flags::AbstractString)
    options = DEFAULT_COMPILER_OPTS
    for f in flags
        options |= f=='i' ? PCRE.CASELESS  :
                   f=='m' ? PCRE.MULTILINE :
                   f=='s' ? PCRE.DOTALL    :
                   f=='x' ? PCRE.EXTENDED  :
                   throw(ArgumentError("unknown regex flag: $f"))
    end
    Regex(pattern, options, DEFAULT_MATCH_OPTS)
end
Regex(pattern::AbstractString) = Regex(pattern, DEFAULT_COMPILER_OPTS, DEFAULT_MATCH_OPTS)

function compile(regex::Regex)
    if regex.regex == C_NULL
        regex.regex = PCRE.compile(regex.pattern, regex.compile_options)
        PCRE.jit_compile(regex.regex)
        regex.match_data = PCRE.create_match_data(regex.regex)
        regex.ovec = PCRE.get_ovec(regex.match_data)
    end
    regex
end

macro r_str(pattern, flags...) Regex(pattern, flags...) end

copy(r::Regex) = r

function show(io::IO, re::Regex)
    imsx = PCRE.CASELESS|PCRE.MULTILINE|PCRE.DOTALL|PCRE.EXTENDED
    opts = re.compile_options
    if (opts & ~imsx) == DEFAULT_COMPILER_OPTS
        print(io, 'r')
        print_quoted_literal(io, re.pattern)
        if (opts & PCRE.CASELESS ) != 0; print(io, 'i'); end
        if (opts & PCRE.MULTILINE) != 0; print(io, 'm'); end
        if (opts & PCRE.DOTALL   ) != 0; print(io, 's'); end
        if (opts & PCRE.EXTENDED ) != 0; print(io, 'x'); end
    else
        print(io, "Regex(")
        show(io, re.pattern)
        print(io, ',')
        show(io, opts)
        print(io, ')')
    end
end

# TODO: map offsets into non-ByteStrings back to original indices.
# or maybe it's better to just fail since that would be quite slow

immutable RegexMatch
    match::SubString{UTF8String}
    captures::Vector{Union{Void,SubString{UTF8String}}}
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
    return PCRE.exec(r.regex, bytestring(s), offset, r.match_options,
                     r.match_data)
end

function ismatch(r::Regex, s::SubString, offset::Integer=0)
    compile(r)
    return PCRE.exec(r.regex, s, offset, r.match_options,
                     r.match_data)
end

call(r::Regex, s) = ismatch(r, s)

function match(re::Regex, str::UTF8String, idx::Integer, add_opts::UInt32=UInt32(0))
    compile(re)
    opts = re.match_options | add_opts
    if !PCRE.exec(re.regex, str, idx-1, opts, re.match_data)
        return nothing
    end
    ovec = re.ovec
    n = div(length(ovec),2) - 1
    mat = SubString(str, ovec[1]+1, ovec[2])
    cap = Union{Void,SubString{UTF8String}}[
            ovec[2i+1] == PCRE.UNSET ? nothing : SubString(str, ovec[2i+1]+1, ovec[2i+2]) for i=1:n ]
    off = Int[ ovec[2i+1]+1 for i=1:n ]
    RegexMatch(mat, cap, ovec[1]+1, off)
end

match(re::Regex, str::Union{ByteString,SubString}, idx::Integer, add_opts::UInt32=UInt32(0)) =
    match(re, utf8(str), idx, add_opts)

match(r::Regex, s::AbstractString) = match(r, s, start(s))
match(r::Regex, s::AbstractString, i::Integer) =
    throw(ArgumentError("regex matching is only available for bytestrings; use bytestring(s) to convert"))

function matchall(re::Regex, str::UTF8String, overlap::Bool=false)
    regex = compile(re).regex
    n = length(str.data)
    matches = SubString{UTF8String}[]
    offset = UInt32(0)
    opts = re.match_options
    opts_nonempty = opts | PCRE.ANCHORED | PCRE.NOTEMPTY_ATSTART
    prevempty = false
    ovec = re.ovec
    while true
        result = PCRE.exec(regex, str, offset, prevempty ? opts_nonempty : opts, re.match_data)
        if !result
            if prevempty && offset < n
                offset = UInt32(nextind(str, offset + 1) - 1)
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
                offset = UInt32(ovec[1]+1)
            end
        else
            offset = ovec[2]
        end
    end
    matches
end

matchall(re::Regex, str::Union{ByteString,SubString}, overlap::Bool=false) =
    matchall(re, utf8(str), overlap)

function search(str::Union{ByteString,SubString}, re::Regex, idx::Integer)
    if idx > nextind(str,endof(str))
        throw(BoundsError())
    end
    opts = re.match_options
    compile(re)
    PCRE.exec(re.regex, str, idx-1, opts, re.match_data) ?
        ((Int(re.ovec[1])+1):prevind(str,Int(re.ovec[2])+1)) : (0:-1)
end
search(s::AbstractString, r::Regex, idx::Integer) =
    throw(ArgumentError("regex search is only available for bytestrings; use bytestring(s) to convert"))
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
eltype(::Type{RegexMatchIterator}) = RegexMatch
start(itr::RegexMatchIterator) = match(itr.regex, itr.string, 1, UInt32(0))
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

    opts_nonempty = UInt32(PCRE.ANCHORED | PCRE.NOTEMPTY_ATSTART)
    while true
        mat = match(itr.regex, itr.string, offset,
                    prevempty ? opts_nonempty : UInt32(0))

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

## comparison ##

function ==(a::Regex, b::Regex)
    a.pattern == b.pattern && a.compile_options == b.compile_options && a.match_options == b.match_options
end

## hash ##
const hashre_seed = UInt === UInt64 ? 0x67e195eb8555e72d : 0xe32373e4
function hash(r::Regex, h::UInt)
    h += hashre_seed
    h = hash(r.pattern, h)
    h = hash(r.compile_options, h)
    h = hash(r.match_options, h)
end

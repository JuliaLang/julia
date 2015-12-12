# This file is a part of Julia. License is MIT: http://julialang.org/license

## object-oriented Regex interface ##

include("pcre.jl")

const DEFAULT_COMPILER_OPTS = PCRE.UTF | PCRE.NO_UTF_CHECK | PCRE.ALT_BSUX
const DEFAULT_MATCH_OPTS = PCRE.NO_UTF_CHECK

type Regex
    pattern::String
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

"""
    @r_str -> Regex

Construct a regex, such as `r"^[a-z]*\$"`. The regex also accepts one or more flags, listed
after the ending quote, to change its behaviour:

- `i` enables case-insensitive matching
- `m` treats the `^` and `\$` tokens as matching the start and end of individual lines, as
  opposed to the whole string.
- `s` allows the `.` modifier to match newlines.
- `x` enables "comment mode": whitespace is enabled except when escaped with `\\`, and `#`
  is treated as starting a comment.

For example, this regex has all three flags enabled:

```julia
julia> match(r"a+.*b+.*?d\$"ism, "Goodbye,\\nOh, angry,\\nBad world\\n")
RegexMatch("angry,\\nBad world")
```
"""
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

# TODO: map offsets into strings in other encodings back to original indices.
# or maybe it's better to just fail since that would be quite slow

immutable RegexMatch
    match::SubString{String}
    captures::Vector{Union{Void,SubString{String}}}
    offset::Int
    offsets::Vector{Int}
    regex::Regex
end

function show(io::IO, m::RegexMatch)
    print(io, "RegexMatch(")
    show(io, m.match)
    idx_to_capture_name = PCRE.capture_names(m.regex.regex)
    if !isempty(m.captures)
        print(io, ", ")
        for i = 1:length(m.captures)
            # If the capture group is named, show the name.
            # Otherwise show its index.
            capture_name = get(idx_to_capture_name, i, i)
            print(io, capture_name, "=")
            show(io, m.captures[i])
            if i < length(m.captures)
                print(io, ", ")
            end
        end
    end
    print(io, ")")
end

# Capture group extraction
getindex(m::RegexMatch, idx::Integer) = m.captures[idx]
function getindex(m::RegexMatch, name::Symbol)
    idx = PCRE.substring_number_from_name(m.regex.regex, name)
    idx <= 0 && error("no capture group named $name found in regex")
    m[idx]
end
getindex(m::RegexMatch, name::AbstractString) = m[Symbol(name)]

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

(r::Regex)(s) = ismatch(r, s)

function match(re::Regex, str::Union{SubString{String}, String}, idx::Integer, add_opts::UInt32=UInt32(0))
    compile(re)
    opts = re.match_options | add_opts
    if !PCRE.exec(re.regex, str, idx-1, opts, re.match_data)
        return nothing
    end
    ovec = re.ovec
    n = div(length(ovec),2) - 1
    mat = SubString(str, ovec[1]+1, ovec[2])
    cap = Union{Void,SubString{String}}[
            ovec[2i+1] == PCRE.UNSET ? nothing : SubString(str, ovec[2i+1]+1, ovec[2i+2]) for i=1:n ]
    off = Int[ ovec[2i+1]+1 for i=1:n ]
    RegexMatch(mat, cap, ovec[1]+1, off, re)
end

_utf8(str) = utf8(str)
match{T<:String}(re::Regex, str::Union{T,SubString{T}}, idx::Integer, add_opts::UInt32=UInt32(0)) =
    match(re, _utf8(str), idx, add_opts)

match(r::Regex, s::AbstractString) = match(r, s, start(s))
match(r::Regex, s::AbstractString, i::Integer) =
    throw(ArgumentError("regex matching is only available for bytestrings; use bytestring(s) to convert"))

function matchall(re::Regex, str::String, overlap::Bool=false)
    regex = compile(re).regex
    n = length(str.data)
    matches = SubString{String}[]
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

matchall(re::Regex, str::Union{String,SubString}, overlap::Bool=false) =
    matchall(re, utf8(str), overlap)

function search(str::Union{String,SubString}, re::Regex, idx::Integer)
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

immutable SubstitutionString{T<:AbstractString} <: AbstractString
    string::T
end

endof(s::SubstitutionString) = endof(s.string)
next(s::SubstitutionString, idx::Int) = next(s.string, idx)
function show(io::IO, s::SubstitutionString)
    print(io, "s")
    show(io, s.string)
end

macro s_str(string) SubstitutionString(string) end

replace_err(repl) = error("Bad replacement string: $repl")

function _write_capture(io, re, group)
    len = PCRE.substring_length_bynumber(re.match_data, group)
    ensureroom(io, len+1)
    PCRE.substring_copy_bynumber(re.match_data, group,
        pointer(io.data, io.ptr), len+1)
    io.ptr += len
    io.size = max(io.size, io.ptr - 1)
end

function _replace(io, repl_s::SubstitutionString, str, r, re)
    const SUB_CHAR = '\\'
    const GROUP_CHAR = 'g'
    const LBRACKET = '<'
    const RBRACKET = '>'
    repl = repl_s.string
    i = start(repl)
    e = endof(repl)
    while i <= e
        if repl[i] == SUB_CHAR
            next_i = nextind(repl, i)
            next_i > e && replace_err(repl)
            if repl[next_i] == SUB_CHAR
                write(io, SUB_CHAR)
                i = nextind(repl, next_i)
            elseif isnumber(repl[next_i])
                group = parse(Int, repl[next_i])
                i = nextind(repl, next_i)
                while i <= e
                    if isnumber(repl[i])
                        group = 10group + parse(Int, repl[i])
                        i = nextind(repl, i)
                    else
                        break
                    end
                end
                _write_capture(io, re, group)
            elseif repl[next_i] == GROUP_CHAR
                i = nextind(repl, next_i)
                if i > e || repl[i] != LBRACKET
                    replace_err(repl)
                end
                i = nextind(repl, i)
                i > e && replace_err(repl)
                groupstart = i
                while repl[i] != RBRACKET
                    i = nextind(repl, i)
                    i > e && replace_err(repl)
                end
                #  TODO: avoid this allocation
                groupname = SubString(repl, groupstart, prevind(repl, i))
                if isnumber(groupname)
                    _write_capture(io, re, parse(Int, groupname))
                else
                    group = PCRE.substring_number_from_name(re.regex, groupname)
                    group < 0 && replace_err("Group $groupname not found in regex $re")
                    _write_capture(io, re, group)
                end
                i = nextind(repl, i)
            else
                replace_err(repl)
            end
        else
            write(io, repl[i])
            i = nextind(repl, i)
        end
    end
end

immutable RegexMatchIterator
    regex::Regex
    string::String
    overlap::Bool

    function RegexMatchIterator(regex::Regex, string::AbstractString, ovr::Bool=false)
        new(regex, string, ovr)
    end
end
compile(itr::RegexMatchIterator) = (compile(itr.regex); itr)
eltype(::Type{RegexMatchIterator}) = RegexMatch
start(itr::RegexMatchIterator) = match(itr.regex, itr.string, 1, UInt32(0))
done(itr::RegexMatchIterator, prev_match) = (prev_match === nothing)

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

function eachmatch(re::Regex, str::AbstractString, ovr::Bool)
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

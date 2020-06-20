# This file is a part of Julia. License is MIT: https://julialang.org/license

## object-oriented Regex interface ##

include("pcre.jl")

const DEFAULT_COMPILER_OPTS = PCRE.UTF | PCRE.NO_UTF_CHECK | PCRE.ALT_BSUX | PCRE.UCP
const DEFAULT_MATCH_OPTS = PCRE.NO_UTF_CHECK

"""
    Regex(pattern[, flags])

A type representing a regular expression. `Regex` objects can be used to match strings
with [`match`](@ref).

`Regex` objects can be created using the [`@r_str`](@ref) string macro. The
`Regex(pattern[, flags])` constructor is usually used if the `pattern` string needs
to be interpolated. See the documentation of the string macro for details on flags.
"""
mutable struct Regex
    pattern::String
    compile_options::UInt32
    match_options::UInt32
    regex::Ptr{Cvoid}

    function Regex(pattern::AbstractString, compile_options::Integer,
                   match_options::Integer)
        pattern = String(pattern)
        compile_options = UInt32(compile_options)
        match_options = UInt32(match_options)
        if (compile_options & ~PCRE.COMPILE_MASK) != 0
            throw(ArgumentError("invalid regex compile options: $compile_options"))
        end
        if (match_options & ~PCRE.EXECUTE_MASK) !=0
            throw(ArgumentError("invalid regex match options: $match_options"))
        end
        re = compile(new(pattern, compile_options, match_options, C_NULL))
        finalizer(re) do re
            re.regex == C_NULL || PCRE.free_re(re.regex)
        end
        re
    end
end

function Regex(pattern::AbstractString, flags::AbstractString)
    options = DEFAULT_COMPILER_OPTS
    for f in flags
        if f == 'a'
            options &= ~PCRE.UCP
        else
            options |= f=='i' ? PCRE.CASELESS  :
                       f=='m' ? PCRE.MULTILINE :
                       f=='s' ? PCRE.DOTALL    :
                       f=='x' ? PCRE.EXTENDED  :
                       throw(ArgumentError("unknown regex flag: $f"))
        end
    end
    Regex(pattern, options, DEFAULT_MATCH_OPTS)
end
Regex(pattern::AbstractString) = Regex(pattern, DEFAULT_COMPILER_OPTS, DEFAULT_MATCH_OPTS)

function compile(regex::Regex)
    if regex.regex == C_NULL
        if PCRE.PCRE_COMPILE_LOCK === nothing
            regex.regex = PCRE.compile(regex.pattern, regex.compile_options)
            PCRE.jit_compile(regex.regex)
        else
            l = PCRE.PCRE_COMPILE_LOCK::Threads.SpinLock
            lock(l)
            try
                if regex.regex == C_NULL
                    regex.regex = PCRE.compile(regex.pattern, regex.compile_options)
                    PCRE.jit_compile(regex.regex)
                end
            finally
                unlock(l)
            end
        end
    end
    regex
end

"""
    @r_str -> Regex

Construct a regex, such as `r"^[a-z]*\$"`, without interpolation and unescaping (except for
quotation mark `"` which still has to be escaped). The regex also accepts one or more flags,
listed after the ending quote, to change its behaviour:

- `i` enables case-insensitive matching
- `m` treats the `^` and `\$` tokens as matching the start and end of individual lines, as
  opposed to the whole string.
- `s` allows the `.` modifier to match newlines.
- `x` enables "comment mode": whitespace is enabled except when escaped with `\\`, and `#`
  is treated as starting a comment.
- `a` disables `UCP` mode (enables ASCII mode). By default `\\B`, `\\b`, `\\D`, `\\d`, `\\S`,
  `\\s`, `\\W`, `\\w`, etc. match based on Unicode character properties. With this option,
  these sequences only match ASCII characters.

See `Regex` if interpolation is needed.

# Examples
```jldoctest
julia> match(r"a+.*b+.*?d\$"ism, "Goodbye,\\nOh, angry,\\nBad world\\n")
RegexMatch("angry,\\nBad world")
```
This regex has the first three flags enabled.
"""
macro r_str(pattern, flags...) Regex(pattern, flags...) end

function show(io::IO, re::Regex)
    imsxa = PCRE.CASELESS|PCRE.MULTILINE|PCRE.DOTALL|PCRE.EXTENDED|PCRE.UCP
    opts = re.compile_options
    if (opts & ~imsxa) == (DEFAULT_COMPILER_OPTS & ~imsxa)
        print(io, 'r')
        print_quoted_literal(io, re.pattern)
        if (opts & PCRE.CASELESS ) != 0; print(io, 'i'); end
        if (opts & PCRE.MULTILINE) != 0; print(io, 'm'); end
        if (opts & PCRE.DOTALL   ) != 0; print(io, 's'); end
        if (opts & PCRE.EXTENDED ) != 0; print(io, 'x'); end
        if (opts & PCRE.UCP      ) == 0; print(io, 'a'); end
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

struct RegexMatch
    match::SubString{String}
    captures::Vector{Union{Nothing,SubString{String}}}
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

function occursin(r::Regex, s::AbstractString; offset::Integer=0)
    compile(r)
    return PCRE.exec_r(r.regex, String(s), offset, r.match_options)
end

function occursin(r::Regex, s::SubString; offset::Integer=0)
    compile(r)
    return PCRE.exec_r(r.regex, s, offset, r.match_options)
end

"""
    startswith(s::AbstractString, prefix::Regex)

Return `true` if `s` starts with the regex pattern, `prefix`.

!!! note
    `startswith` does not compile the anchoring into the regular
    expression, but instead passes the anchoring as
    `match_option` to PCRE. If compile time is amortized,
    `occursin(r"^...", s)` is faster than `startswith(s, r"...")`.

See also [`occursin`](@ref) and [`endswith`](@ref).

!!! compat "Julia 1.2"
     This method requires at least Julia 1.2.

# Examples
```jldoctest
julia> startswith("JuliaLang", r"Julia|Romeo")
true
```
"""
function startswith(s::AbstractString, r::Regex)
    compile(r)
    return PCRE.exec_r(r.regex, String(s), 0, r.match_options | PCRE.ANCHORED)
end

function startswith(s::SubString, r::Regex)
    compile(r)
    return PCRE.exec_r(r.regex, s, 0, r.match_options | PCRE.ANCHORED)
end

"""
    endswith(s::AbstractString, suffix::Regex)

Return `true` if `s` ends with the regex pattern, `suffix`.

!!! note
    `endswith` does not compile the anchoring into the regular
    expression, but instead passes the anchoring as
    `match_option` to PCRE. If compile time is amortized,
    `occursin(r"...\$", s)` is faster than `endswith(s, r"...")`.

See also [`occursin`](@ref) and [`startswith`](@ref).

!!! compat "Julia 1.2"
     This method requires at least Julia 1.2.

# Examples
```jldoctest
julia> endswith("JuliaLang", r"Lang|Roberts")
true
```
"""
function endswith(s::AbstractString, r::Regex)
    compile(r)
    return PCRE.exec_r(r.regex, String(s), 0, r.match_options | PCRE.ENDANCHORED)
end

function endswith(s::SubString, r::Regex)
    compile(r)
    return PCRE.exec_r(r.regex, s, 0, r.match_options | PCRE.ENDANCHORED)
end

"""
    match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])

Search for the first match of the regular expression `r` in `s` and return a `RegexMatch`
object containing the match, or nothing if the match failed. The matching substring can be
retrieved by accessing `m.match` and the captured sequences can be retrieved by accessing
`m.captures` The optional `idx` argument specifies an index at which to start the search.

# Examples
```jldoctest
julia> rx = r"a(.)a"
r"a(.)a"

julia> m = match(rx, "cabac")
RegexMatch("aba", 1="b")

julia> m.captures
1-element Array{Union{Nothing, SubString{String}},1}:
 "b"

julia> m.match
"aba"

julia> match(rx, "cabac", 3) === nothing
true
```
"""
function match end

function match(re::Regex, str::Union{SubString{String}, String}, idx::Integer, add_opts::UInt32=UInt32(0))
    compile(re)
    opts = re.match_options | add_opts
    matched, data = PCRE.exec_r_data(re.regex, str, idx-1, opts)
    if !matched
        PCRE.free_match_data(data)
        return nothing
    end
    n = div(PCRE.ovec_length(data), 2) - 1
    p = PCRE.ovec_ptr(data)
    mat = SubString(str, unsafe_load(p, 1)+1, prevind(str, unsafe_load(p, 2)+1))
    cap = Union{Nothing,SubString{String}}[unsafe_load(p,2i+1) == PCRE.UNSET ? nothing :
                                        SubString(str, unsafe_load(p,2i+1)+1,
                                                  prevind(str, unsafe_load(p,2i+2)+1)) for i=1:n]
    off = Int[ unsafe_load(p,2i+1)+1 for i=1:n ]
    result = RegexMatch(mat, cap, unsafe_load(p,1)+1, off, re)
    PCRE.free_match_data(data)
    return result
end

match(r::Regex, s::AbstractString) = match(r, s, firstindex(s))
match(r::Regex, s::AbstractString, i::Integer) = throw(ArgumentError(
    "regex matching is only available for the String type; use String(s) to convert"
))

findnext(re::Regex, str::Union{String,SubString}, idx::Integer) = _findnext_re(re, str, idx, C_NULL)

# TODO: return only start index and update deprecation
function _findnext_re(re::Regex, str::Union{String,SubString}, idx::Integer, match_data::Ptr{Cvoid})
    if idx > nextind(str,lastindex(str))
        throw(BoundsError())
    end
    opts = re.match_options
    compile(re)
    alloc = match_data == C_NULL
    if alloc
        matched, data = PCRE.exec_r_data(re.regex, str, idx-1, opts)
    else
        matched = PCRE.exec(re.regex, str, idx-1, opts, match_data)
        data = match_data
    end
    if matched
        p = PCRE.ovec_ptr(data)
        ans = (Int(unsafe_load(p,1))+1):prevind(str,Int(unsafe_load(p,2))+1)
    else
        ans = nothing
    end
    alloc && PCRE.free_match_data(data)
    return ans
end
findnext(r::Regex, s::AbstractString, idx::Integer) = throw(ArgumentError(
    "regex search is only available for the String type; use String(s) to convert"
))
findfirst(r::Regex, s::AbstractString) = findnext(r,s,firstindex(s))


"""
    findall(
        pattern::Union{AbstractString,Regex},
        string::AbstractString;
        overlap::Bool = false,
    )

Return a `Vector{UnitRange{Int}}` of all the matches for `pattern` in `string`.
Each element of the returned vector is a range of indices where the
matching sequence is found, like the return value of [`findnext`](@ref).

If `overlap=true`, the matching sequences are allowed to overlap indices in the
original string, otherwise they must be from disjoint character ranges.

# Examples
```jldoctest
julia> findall("a", "apple")
1-element Array{UnitRange{Int64},1}:
 1:1

julia> findall("nana", "banana")
1-element Array{UnitRange{Int64},1}:
 3:6

julia> findall("a", "banana")
3-element Array{UnitRange{Int64},1}:
 2:2
 4:4
 6:6
```
"""
function findall(t::Union{AbstractString,Regex}, s::AbstractString; overlap::Bool=false)
    found = UnitRange{Int}[]
    i, e = firstindex(s), lastindex(s)
    while true
        r = findnext(t, s, i)
        isnothing(r) && break
        push!(found, r)
        j = overlap || isempty(r) ? first(r) : last(r)
        j > e && break
        @inbounds i = nextind(s, j)
    end
    return found
end

"""
    count(
        pattern::Union{AbstractString,Regex},
        string::AbstractString;
        overlap::Bool = false,
    )

Return the number of matches for `pattern` in `string`. This is equivalent to
calling `length(findall(pattern, string))` but more efficient.

If `overlap=true`, the matching sequences are allowed to overlap indices in the
original string, otherwise they must be from disjoint character ranges.
"""
function count(t::Union{AbstractString,Regex}, s::AbstractString; overlap::Bool=false)
    n = 0
    i, e = firstindex(s), lastindex(s)
    while true
        r = findnext(t, s, i)
        isnothing(r) && break
        n += 1
        j = overlap || isempty(r) ? first(r) : last(r)
        j > e && break
        @inbounds i = nextind(s, j)
    end
    return n
end

"""
    SubstitutionString(substr)

Stores the given string `substr` as a `SubstitutionString`, for use in regular expression
substitutions. Most commonly constructed using the [`@s_str`](@ref) macro.

```jldoctest
julia> SubstitutionString("Hello \\\\g<name>, it's \\\\1")
s"Hello \\\\g<name>, it's \\\\1"

julia> subst = s"Hello \\g<name>, it's \\1"
s"Hello \\\\g<name>, it's \\\\1"

julia> typeof(subst)
SubstitutionString{String}

```

"""
struct SubstitutionString{T<:AbstractString} <: AbstractString
    string::T
end

ncodeunits(s::SubstitutionString) = ncodeunits(s.string)
codeunit(s::SubstitutionString) = codeunit(s.string)
codeunit(s::SubstitutionString, i::Integer) = codeunit(s.string, i)
isvalid(s::SubstitutionString, i::Integer) = isvalid(s.string, i)
iterate(s::SubstitutionString, i::Integer...) = iterate(s.string, i...)

function show(io::IO, s::SubstitutionString)
    print(io, "s")
    show(io, s.string)
end

"""
    @s_str -> SubstitutionString

Construct a substitution string, used for regular expression substitutions.  Within the
string, sequences of the form `\\N` refer to the Nth capture group in the regex, and
`\\g<groupname>` refers to a named capture group with name `groupname`.

```jldoctest
julia> msg = "#Hello# from Julia";

julia> replace(msg, r"#(.+)# from (?<from>\\w+)" => s"FROM: \\g<from>; MESSAGE: \\1")
"FROM: Julia; MESSAGE: Hello"
```
"""
macro s_str(string) SubstitutionString(string) end

# replacement

struct RegexAndMatchData
    re::Regex
    match_data::Ptr{Cvoid}
    RegexAndMatchData(re::Regex) = (compile(re); new(re, PCRE.create_match_data(re.regex)))
end

findnext(pat::RegexAndMatchData, str, i) = _findnext_re(pat.re, str, i, pat.match_data)

_pat_replacer(r::Regex) = RegexAndMatchData(r)

_free_pat_replacer(r::RegexAndMatchData) = PCRE.free_match_data(r.match_data)

replace_err(repl) = error("Bad replacement string: $repl")

function _write_capture(io, re::RegexAndMatchData, group)
    len = PCRE.substring_length_bynumber(re.match_data, group)
    ensureroom(io, len+1)
    PCRE.substring_copy_bynumber(re.match_data, group,
        pointer(io.data, io.ptr), len+1)
    io.ptr += len
    io.size = max(io.size, io.ptr - 1)
end


const SUB_CHAR = '\\'
const GROUP_CHAR = 'g'
const KEEP_ESC = [SUB_CHAR, GROUP_CHAR, '0':'9'...]

function _replace(io, repl_s::SubstitutionString, str, r, re::RegexAndMatchData)
    LBRACKET = '<'
    RBRACKET = '>'
    repl = unescape_string(repl_s.string, KEEP_ESC)
    i = firstindex(repl)
    e = lastindex(repl)
    while i <= e
        if repl[i] == SUB_CHAR
            next_i = nextind(repl, i)
            next_i > e && replace_err(repl)
            if repl[next_i] == SUB_CHAR
                write(io, SUB_CHAR)
                i = nextind(repl, next_i)
            elseif isdigit(repl[next_i])
                group = parse(Int, repl[next_i])
                i = nextind(repl, next_i)
                while i <= e
                    if isdigit(repl[i])
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
                if all(isdigit, groupname)
                    _write_capture(io, re, parse(Int, groupname))
                else
                    group = PCRE.substring_number_from_name(re.re.regex, groupname)
                    group < 0 && replace_err("Group $groupname not found in regex $(re.re)")
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

struct RegexMatchIterator
    regex::Regex
    string::String
    overlap::Bool

    function RegexMatchIterator(regex::Regex, string::AbstractString, ovr::Bool=false)
        new(regex, string, ovr)
    end
end
compile(itr::RegexMatchIterator) = (compile(itr.regex); itr)
eltype(::Type{RegexMatchIterator}) = RegexMatch
IteratorSize(::Type{RegexMatchIterator}) = SizeUnknown()

function iterate(itr::RegexMatchIterator, (offset,prevempty)=(1,false))
    opts_nonempty = UInt32(PCRE.ANCHORED | PCRE.NOTEMPTY_ATSTART)
    while true
        mat = match(itr.regex, itr.string, offset,
                    prevempty ? opts_nonempty : UInt32(0))

        if mat === nothing
            if prevempty && offset <= sizeof(itr.string)
                offset = nextind(itr.string, offset)
                prevempty = false
                continue
            else
                break
            end
        else
            if itr.overlap
                if !isempty(mat.match)
                    offset = nextind(itr.string, mat.offset)
                else
                    offset = mat.offset
                end
            else
                offset = mat.offset + ncodeunits(mat.match)
            end
            return (mat, (offset, isempty(mat.match)))
        end
    end
    nothing
end

"""
    eachmatch(r::Regex, s::AbstractString; overlap::Bool=false)

Search for all matches of the regular expression `r` in `s` and return an iterator over the
matches. If `overlap` is `true`, the matching sequences are allowed to overlap indices in the
original string, otherwise they must be from distinct character ranges.

# Examples
```jldoctest
julia> rx = r"a.a"
r"a.a"

julia> m = eachmatch(rx, "a1a2a3a")
Base.RegexMatchIterator(r"a.a", "a1a2a3a", false)

julia> collect(m)
2-element Array{RegexMatch,1}:
 RegexMatch("a1a")
 RegexMatch("a3a")

julia> collect(eachmatch(rx, "a1a2a3a", overlap = true))
3-element Array{RegexMatch,1}:
 RegexMatch("a1a")
 RegexMatch("a2a")
 RegexMatch("a3a")
```
"""
eachmatch(re::Regex, str::AbstractString; overlap = false) =
    RegexMatchIterator(re, str, overlap)

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

## String operations ##

"""
    *(s::Regex, t::Union{Regex,AbstractString,AbstractChar}) -> Regex
    *(s::Union{Regex,AbstractString,AbstractChar}, t::Regex) -> Regex

Concatenate regexes, strings and/or characters, producing a [`Regex`](@ref).
String and character arguments must be matched exactly in the resulting regex,
meaning that the contained characters are devoid of any special meaning
(they are quoted with "\\Q" and "\\E").

!!! compat "Julia 1.3"
     This method requires at least Julia 1.3.

# Examples
```jldoctest
julia> match(r"Hello|Good bye" * ' ' * "world", "Hello world")
RegexMatch("Hello world")

julia> r = r"a|b" * "c|d"
r"(?:a|b)\\Qc|d\\E"

julia> match(r, "ac") == nothing
true

julia> match(r, "ac|d")
RegexMatch("ac|d")
```
"""
function *(r1::Union{Regex,AbstractString,AbstractChar}, rs::Union{Regex,AbstractString,AbstractChar}...)
    mask = PCRE.CASELESS | PCRE.MULTILINE | PCRE.DOTALL | PCRE.EXTENDED # imsx
    match_opts   = nothing # all args must agree on this
    compile_opts = nothing # all args must agree on this
    shared = mask
    for r in (r1, rs...)
        r isa Regex || continue
        if match_opts === nothing
            match_opts = r.match_options
            compile_opts = r.compile_options & ~mask
        else
            r.match_options == match_opts &&
                r.compile_options & ~mask == compile_opts ||
                throw(ArgumentError("cannot multiply regexes: incompatible options"))
        end
        shared &= r.compile_options
    end
    unshared = mask & ~shared
    Regex(string(wrap_string(r1, unshared), wrap_string.(rs, Ref(unshared))...), compile_opts | shared, match_opts)
end

*(r::Regex) = r # avoids wrapping r in a useless subpattern

wrap_string(r::Regex, unshared::UInt32) = string("(?", regex_opts_str(r.compile_options & unshared), ':', r.pattern, ')')
# if s contains raw"\E", split '\' and 'E' within two distinct \Q...\E groups:
wrap_string(s::AbstractString, ::UInt32) =  string("\\Q", replace(s, raw"\E" => raw"\\E\QE"), "\\E")
wrap_string(s::AbstractChar, ::UInt32) = string("\\Q", s, "\\E")

regex_opts_str(opts) = (isassigned(_regex_opts_str) ? _regex_opts_str[] : init_regex())[opts]

# UInt32 to String mapping for some compile options
const _regex_opts_str = Ref{ImmutableDict{UInt32,String}}()

@noinline init_regex() = _regex_opts_str[] = foldl(0:15, init=ImmutableDict{UInt32,String}()) do d, o
    opt = UInt32(0)
    str = ""
    if o & 1 != 0
        opt |= PCRE.CASELESS
        str *= 'i'
    end
    if o & 2 != 0
        opt |= PCRE.MULTILINE
        str *= 'm'
    end
    if o & 4 != 0
        opt |= PCRE.DOTALL
        str *= 's'
    end
    if o & 8 != 0
        opt |= PCRE.EXTENDED
        str *= 'x'
    end
    ImmutableDict(d, opt => str)
end


"""
    ^(s::Regex, n::Integer)

Repeat a regex `n` times.

!!! compat "Julia 1.3"
     This method requires at least Julia 1.3.

# Examples
```jldoctest
julia> r"Test "^2
r"(?:Test ){2}"

julia> match(r"Test "^2, "Test Test ")
RegexMatch("Test Test ")
```
"""
^(r::Regex, i::Integer) = Regex(string("(?:", r.pattern, "){$i}"), r.compile_options, r.match_options)

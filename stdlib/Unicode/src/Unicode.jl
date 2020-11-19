# This file is a part of Julia. License is MIT: https://julialang.org/license

module Unicode

export graphemes, isemoji

"""
    Unicode.normalize(s::AbstractString; keywords...)
    Unicode.normalize(s::AbstractString, normalform::Symbol)

Normalize the string `s`. By default, canonical composition (`compose=true`) is performed without ensuring
Unicode versioning stability (`compat=false`), which produces the shortest possible equivalent string
but may introduce composition characters not present in earlier Unicode versions.

Alternatively, one of the four "normal forms" of the Unicode standard can be specified:
`normalform` can be `:NFC`, `:NFD`, `:NFKC`, or `:NFKD`.  Normal forms C
(canonical composition) and D (canonical decomposition) convert different visually identical
representations of the same abstract string into a single canonical form, with form C being
more compact.  Normal forms KC and KD additionally canonicalize "compatibility equivalents":
they convert characters that are abstractly similar but visually distinct into a single
canonical choice (e.g. they expand ligatures into the individual characters), with form KC
being more compact.

Alternatively, finer control and additional transformations may be obtained by calling
`Unicode.normalize(s; keywords...)`, where any number of the following boolean keywords
options (which all default to `false` except for `compose`) are specified:

* `compose=false`: do not perform canonical composition
* `decompose=true`: do canonical decomposition instead of canonical composition
  (`compose=true` is ignored if present)
* `compat=true`: compatibility equivalents are canonicalized
* `casefold=true`: perform Unicode case folding, e.g. for case-insensitive string comparison
* `newline2lf=true`, `newline2ls=true`, or `newline2ps=true`: convert various newline
  sequences (LF, CRLF, CR, NEL) into a linefeed (LF), line-separation (LS), or
  paragraph-separation (PS) character, respectively
* `stripmark=true`: strip diacritical marks (e.g. accents)
* `stripignore=true`: strip Unicode's "default ignorable" characters (e.g. the soft hyphen
  or the left-to-right marker)
* `stripcc=true`: strip control characters; horizontal tabs and form feeds are converted to
  spaces; newlines are also converted to spaces unless a newline-conversion flag was
  specified
* `rejectna=true`: throw an error if unassigned code points are found
* `stable=true`: enforce Unicode versioning stability (never introduce characters missing from earlier Unicode versions)

For example, NFKC corresponds to the options `compose=true, compat=true, stable=true`.

# Examples
```jldoctest
julia> "é" == Unicode.normalize("é") #LHS: Unicode U+00e9, RHS: U+0065 & U+0301
true

julia> "μ" == Unicode.normalize("µ", compat=true) #LHS: Unicode U+03bc, RHS: Unicode U+00b5
true

julia> Unicode.normalize("JuLiA", casefold=true)
"julia"

julia> Unicode.normalize("JúLiA", stripmark=true)
"JuLiA"
```
"""
function normalize end
normalize(s::AbstractString, nf::Symbol) = Base.Unicode.normalize(s, nf)
normalize(s::AbstractString; kwargs...) = Base.Unicode.normalize(s; kwargs...)

"""
    Unicode.isassigned(c) -> Bool

Returns `true` if the given char or integer is an assigned Unicode code point.

# Examples
```jldoctest
julia> Unicode.isassigned(101)
true

julia> Unicode.isassigned('\\x01')
true
```
"""
isassigned(c) = Base.Unicode.isassigned(c)

"""
    graphemes(s::AbstractString) -> GraphemeIterator

Returns an iterator over substrings of `s` that correspond to the extended graphemes in the
string, as defined by Unicode UAX #29. (Roughly, these are what users would perceive as
single characters, even though they may contain more than one codepoint; for example a
letter combined with an accent mark is a single grapheme.)
"""
graphemes(s::AbstractString) = Base.Unicode.GraphemeIterator{typeof(s)}(s)



include("emoji_ranges.jl")
const SKIN_COLORS = 0x1F3FB:0x1F3FF
const REGIONAL_INDICATORS = 0x1F1E6:0x1F1FF
const ZWJ = '\u200d'    # Zero-width joiner

"""
    isemoji(Union{AbstractChar, AbstractString}) -> Bool
Test whether a character or string is a single emoji
Combined emoji sequences separated by the zero-width joiner character `'\u200d'`
such as 👨‍❤️‍👨 `['👨',  '\u200d', '❤', '\uFE0F', '\u200d', '👨']` are supported,
though this function cannot determine whether a given sequence of emoji and
zero-width joiners would result in a valid composite emoji.
### Examples
```jldoctest
julia> Unicode.isemoji("👨‍❤️‍👨")
true

julia> Unicode.isemoji('👨')
true

julia> Unicode.isemoji('A')
false
```
"""
function isemoji(c::AbstractChar)
    u = UInt32(c)
    isemoji_code(u)
end

isemoji(s::AbstractString) = _isemoji(s)

function isemoji_code(u::UInt32)
    @inbounds for emojiset in EMOJI_RANGES
        u in emojiset && return true
    end
    return false
end

# a single emoji comprises one of several patterns
# 1. a single character which is a basic emoji
# 2. a pair of two regional indicator flags
# 3. a single emoji character with a modifier character (FE0F or skin color after it)
# 4. a pound sign, asterisk, or digit followed by FE0F 20E3 or just 20E3
# 5. a single regional indicator and then 6 small letter characters
#     (subnational flags, currently only england, scotland and wales)
# 6. Several emojis separated by zero-width joiners
function _isemoji(s::AbstractString; ZWJ_allowed = true)
    isempty(s) && return false
    codepoints = codepoint.(c for c in s)
    L = length(codepoints)
    if L == 1 # single character emoji (pattern 1)
        isemoji_code(codepoints[1]) && return true
    elseif L == 2
        # Check for country flag pattern (pattern 2)
        if (codepoints[2] in REGIONAL_INDICATORS &&
            codepoints[1] in REGIONAL_INDICATORS)
                return true
        elseif isemoji_code(codepoints[1])
            # Check for skin color or FE0F modifier pattern (pattern 2)
            if codepoints[2] in SKIN_COLORS || codepoints[2] == 0x0FE0F
                return true
            end
        elseif isdigit(s[1]) && codepoints[end] == 0x020E3
	    # Check for keycap pattern (pattern 4)
            return true
	else
            return false
        end
    else
        # Check for keycap pattern (pattern 4)
        if L == 3 && (isdigit(s[1]) || s[1] == '#' || s[1] == '*')
            if codepoints[2] == 0x0FE0F && codepoints[end] == 0x020E3
                return true
            else
                return false
            end
        # Check for England, Scotland and Wales (pattern 5)
        elseif L == 7 && codepoints[1] == 0x1F3F4 && all(c in 0xE0062:0xE007F for c in codepoints[2:end])
            return true
        elseif ZWJ_allowed
            # check for zero-width join patterns (pattern 6)
            s[1] == ZWJ || s[end] == ZWJ && return false
            splitstr = split(s, ZWJ)
            all(x -> (_isemoji(x, ZWJ_allowed = false)), splitstr) && return true
        end
    end
    return false
end
end

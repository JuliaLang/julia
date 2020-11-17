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

const emoji_data = download("https://www.unicode.org/Public/13.0.0/ucd/emoji/emoji-data.txt")

"""
    extract_emoji_column(emoji_data, column = 1; type_field = "")
Read the selected column from a provided unicode emoji data file
(i.e. https://www.unicode.org/Public/13.0.0/ucd/emoji/emoji-data.txt).
Optionally select only columns beginning with `type_field`
"""
function extract_emoji_column(emoji_data, column = 1; type_field = "")
    lines = readlines(emoji_data)
    filter!(line -> !isempty(line) && !startswith(line, "#"),  lines)
    splitlines = [strip.(split(line, ";")) for line in lines]
    first_col = [splitline[column] for splitline in splitlines if startswith(splitline[2], type_field)]
end

# parse a string of the form "AAAA...FFFF" into 0xAAAA:0xFFFF
parse_unicode_range_str(range_str) = let s = split(range_str, "..")
    if length(s) > 2 || length(s) < 1
        return nothing
    else
        s1 = tryparse(UInt32, "0x" * s[1])
        s1 === nothing && return nothing
        if length(s) == 1
            return s1:s1
        else
            s2 = tryparse(UInt32, "0x" * s[2])
            s2 === nothing && return nothing
            return s1:s2
        end
    end
end

# Get all ranges containing valid single emoji from file
const EMOJI_RANGES = parse_unicode_range_str.(extract_emoji_column(emoji_data, 1, type_field = "Emoji"))
const ZWJ = '\u200d'    # Zero-width joiner
const VAR_SELECTOR = '\uFE0F'   # Variation selector
# Handle England, Scotland, Wales flags and keycaps
const SPECIAL_CASES = ["🏴󠁧󠁢󠁥󠁮󠁧󠁿", "🏴󠁧󠁢󠁳󠁣󠁴󠁿", "🏴󠁧󠁢󠁷󠁬󠁳󠁿", "#️⃣", "*️⃣", "0️⃣", "1️⃣", "2️⃣", "3️⃣", "4️⃣", "5️⃣", "6️⃣", "7️⃣", "8️⃣", "9️⃣"]

"""
    isemoji(Union{AbstractChar, AbstractString}) -> Bool

Test whether a character is an emoji, or whether all elements in a given string are emoji. Includes identifying composite emoji.
Empty strings return `true` as they contain no characters which aren't emoji.
Combined emoji sequences separated by the zero-width joiner character `'\u200d'`
such as 👨‍❤️‍👨 `['👨',  '\u200d', '❤', '\uFE0F', '\u200d', '👨']` are supported, though this function cannot determine whether a
given sequence of emoji and zero-width joiners would result in a valid composite emoji.
"""
function isemoji(c::AbstractChar)
    u = UInt32(c)
    @inbounds for emojiset in EMOJI_RANGES
        u in emojiset && return true
    end
    return false
end

function isemoji(s::AbstractString)
    s in SPECIAL_CASES && return true
    isempty(s) && return true
    s[end] == ZWJ && return false
    ZWJ_allowed = false
    VAR_SELECTOR_allowed = false
    emoji_allowed = true
    # make sure string follows sequence of basic emoji chars
    # separated by ZWJ and VAR_SELECTOR characters
    @inbounds for c in s
        if c == ZWJ
            !ZWJ_allowed && return false
            ZWJ_allowed = false
            VAR_SELECTOR_allowed = false
        elseif c == VAR_SELECTOR
            !VAR_SELECTOR_allowed && return false
            VAR_SELECTOR_allowed = false
        else
            !isemoji(c) && return false
            ZWJ_allowed = true
            VAR_SELECTOR_allowed = true
        end
    end
    return true
end

end

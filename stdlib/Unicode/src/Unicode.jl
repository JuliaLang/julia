# This file is a part of Julia. License is MIT: https://julialang.org/license

module Unicode

export graphemes, isequivalent

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

using Base.Unicode: utf8proc_error, UTF8PROC_DECOMPOSE, UTF8PROC_CASEFOLD, UTF8PROC_STRIPMARK

function decompose_char!(codepoint::Union{Integer,Char}, dest::Vector{UInt32}, options::Integer)
  ret = @ccall utf8proc_decompose_char(codepoint::UInt32, dest::Ptr{UInt32}, length(dest)::Int, options::Cint, C_NULL::Ptr{Cint})::Int
  ret < 0 && utf8proc_error(ret)
  return ret
end

"""
    isequivalent(s1::AbstractString, s2::AbstractString; casefold=false, stripmark=false)

Return whether `s1` and `s2` are canonically equivalent Unicode strings.   If `casefold=true`,
ignores case (performs Unicode case-folding); if `stripmark=true`, strips diacritical marks
and other combining characters.

# Examples

For example, the string `"noël"` can be constructed in two canonically equivalent ways
in Unicode, depending on whether `"ë"` is formed from a single codepoint U+00EB or
from the ASCII character `'o'` followed by the U+0308 combining-diaeresis character.

```jldoctest
julia> s1 = "no\u00EBl"
"nöel"

julia> s2 = "noe\u0308l"
"nöel"

julia> s1 == s2
false

julia> isequivalent(s1, s2)
true

julia> isequivalent(s1, "noel", stripmark=true)
true

julia> isequivalent(s1, "NOËL", casefold=true)
true
```
"""
function isequivalent(s1::AbstractString, s2::AbstractString; casefold::Bool=false, stripmark::Bool=false)
  function decompose_next_char!(c, state, d, options, s)
      n = decompose_char!(c, d, options)
      if n > length(d) # may be possible in future Unicode versions?
          n = decompose_char!(c, resize!(d, n), options)
      end
      return 1, n, iterate(s, state)
  end
  options = UTF8PROC_DECOMPOSE
  casefold && (options |= UTF8PROC_CASEFOLD)
  stripmark && (options |= UTF8PROC_STRIPMARK)
  i1,i2 = iterate(s1),iterate(s2)
  d1,d2 = Vector{UInt32}(undef, 4), Vector{UInt32}(undef, 4) # codepoint buffers
  n1 = n2 = 0 # lengths of codepoint buffers
  j1 = j2 = 1 # indices in d1, d2
  while true
      if j1 > n1
          i1 === nothing && return i2 === nothing && j2 > n2
          j1, n1, i1 = decompose_next_char!(UInt32(i1[1]), i1[2], d1, options, s1)
      end
      if j2 > n2
          i2 === nothing && return false
          j2, n2, i2 = decompose_next_char!(UInt32(i2[1]), i2[2], d2, options, s2)
      end
      d1[j1] == d2[j2] || return false
      j1 += 1; j2 += 1
  end
end

end

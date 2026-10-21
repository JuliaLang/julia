# [Strings](@id lib-strings)

This section gives a reference list of string related functions in Julia's `Base`
module. For a general introduction to strings in Julia language, please refer to the
[Strings](@ref man-strings) section of the Julia language manual.

## Characters

```@docs
Core.AbstractChar
Core.Char
Base.codepoint
```

```@docs
Base.iscntrl
Base.isdigit
Base.isletter
Base.islowercase
Base.isnumeric
Base.isprint
Base.ispunct
Base.isspace
Base.isuppercase
Base.isxdigit
Base.raw_substring
```

## String Basics

```@docs
Core.AbstractString
Core.String
```

```@docs
Base.string
Base.repr(::Any)
Core.String(::AbstractString)
```

```@docs
Base.length(::AbstractString)
Base.sizeof(::AbstractString)
Base.textwidth
```

```@docs
Base.SubString
Base.LazyString
```

## Concatenation

```@docs
Base.:*(::Union{AbstractChar, AbstractString}, ::Union{AbstractChar, AbstractString}...)
Base.:^(::Union{AbstractString, AbstractChar}, ::Integer)
Base.repeat(::AbstractString, ::Integer)
Base.repeat(::AbstractChar, ::Integer)
```

## Comparison

```@docs
Base.isless(::AbstractString, ::AbstractString)
Base.:(==)(::AbstractString, ::AbstractString)
Base.cmp(::AbstractString, ::AbstractString)
```

## Encoding

```@docs
Base.transcode
Base.ncodeunits(::AbstractString)
Base.codeunit
Base.codeunits
Base.ascii
Base.isascii
```

```@docs
Base.unsafe_string
```

```@docs
Base.thisind
Base.nextind(::AbstractString, ::Integer, ::Integer)
Base.prevind(::AbstractString, ::Integer, ::Integer)
```

```@docs
Base.isvalid(::Any)
Base.isvalid(::Any, ::Any)
Base.isvalid(::AbstractString, ::Integer)
Base.ismalformed
Base.isoverlong
Base.show_invalid
```

## Non-Standard String Literals

This section describes several additional string types, and also
[non-standard string literals](@ref non-standard-string-literals).
The list here is not exhaustive, for example there are also
- [version number literals](@ref man-version-number-literals) of the form [`v"..."`](@ref @v_str),
- [markdown string literals](@ref stdlib-markdown-literals) of the form `md"..."`,
- [regular expressions and substitution string literals](@ref base-regex-literals), and
- large integer literals for 128 bit constants of the form
  [`int128"..."`](@ref Base.@int128_str) and [`uint128"..."`](@ref Base.@uint128_str).

```@docs
Base.@lazy_str
Base.@raw_str
Base.@b_str
Base.Docs.@html_str
Base.Docs.@text_str
```

## [Regular Expressions](@id base-regex-literals)

See also the [Julia manual section about this topic](@ref man-regex-literals).

```@docs
Base.Regex
Base.@r_str
Base.SubstitutionString
Base.@s_str
Base.match
Base.eachmatch
Base.RegexMatch
Base.keys(::RegexMatch)
Base.:*(r1::Union{Regex,AbstractString,AbstractChar}, rs::Union{Regex,AbstractString,AbstractChar}...)
Base.:^(::Regex, ::Integer)
```

## Padding, Truncating, Chopping and Stripping

```@docs
Base.lpad
Base.rpad
```

```@docs
Base.ltruncate
Base.rtruncate
Base.ctruncate
```

```@docs
Base.first(::AbstractString, ::Integer)
Base.last(::AbstractString, ::Integer)
```

```@docs
Base.chop
Base.chopprefix
Base.chopsuffix
Base.chomp
```

```@docs
Base.strip
Base.lstrip
Base.rstrip
```

## Splitting and Joining

```@docs
Base.eachsplit
Base.eachrsplit
Base.split
Base.rsplit
```

```@docs
Base.join
```


## Find and Replace

See also the [section about regular expressions](@ref base-regex-literals).

```@docs
Base.findfirst(::AbstractString, ::AbstractString)
Base.findnext(::AbstractString, ::AbstractString, ::Integer)
Base.findnext(::AbstractChar, ::AbstractString, ::Integer)
Base.findlast(::AbstractString, ::AbstractString)
Base.findlast(::AbstractChar, ::AbstractString)
Base.findprev(::AbstractString, ::AbstractString, ::Integer)
```

```@docs
Base.occursin
Base.contains
```

```@docs
Base.startswith
Base.endswith
```

```@docs
Base.replace(::IO, s::AbstractString, ::Pair...)
```


## Transforming Strings

```@docs
Base.reverse(::Union{String,SubString{String}})
```

```@docs
Base.uppercase
Base.lowercase
Base.titlecase
Base.uppercasefirst
Base.lowercasefirst
```

```@docs
Base.escape_string
Base.escape_raw_string
Base.unescape_string
```

## [Annotated Strings](@id base-annotated-strings)

!!! note
    The API for `AnnotatedStrings` is considered experimental and is subject to change between
    Julia versions.

```@docs
Base.AnnotatedString
Base.AnnotatedChar
Base.annotatedstring
Base.annotations
Base.annotate!
Base.unannotate
```

### [Displaying annotations](@id base-annotated-display)

How an annotated string looks when it is printed to a terminal or shown as HTML is up to the
types of its annotation values. A package says how the values of its own types display with
three functions of [`Base.AnnotatedDisplay`](@ref):

- [`context`](@ref Base.AnnotatedDisplay.context) reads what displaying them needs, once per display;
- [`reduction`](@ref Base.AnnotatedDisplay.reduction) gives the state of a region from its values of the type;
- [`transition`](@ref Base.AnnotatedDisplay.transition) writes the change from one state to another.

These rules hold:

- A value's concrete type says how it displays, so a string displays the same whatever value
  type holds it, `Any` included.
- `reduction` is called for each concrete type `T` of the values, with all of a region's
  values, and reads those with `value isa T`. With none, it gives the baseline state, where
  display starts and ends.
- Each value type has one type of state, compared with `===`. For HTML, a state's fields are
  its visible properties.
- In plain text, `transition` writes the change between two states. In HTML, each transition
  opens one element within the current one, and a transition to `nothing` closes it.
- The types display in the order of the value type's union, and in HTML a later type's
  elements enclose an earlier type's.

For example, a `Tag` that displays in brackets in a terminal, and as a `<mark>` in HTML:

```jldoctest annotated-display
julia> struct Tag
           name::String
       end

julia> struct Tagged # The state: the innermost tag of a region, if it has one
           name::String
       end

julia> Base.AnnotatedDisplay.reduction(::MIME, ::Type{Tag}, values) =
           Tagged(foldl((name, value) -> if value isa Tag value.name else name end, values; init = ""))

julia> function Base.AnnotatedDisplay.transition(io::IO, ::MIME"text/plain", from::Tagged, to::Tagged)
           from === to && return
           isempty(from.name) || print(io, ']')
           isempty(to.name) || print(io, '[', to.name, ' ')
       end

julia> Base.AnnotatedDisplay.transition(io::IO, ::MIME"text/html", ::Tagged, to::Tagged) =
           print(io, "<mark title=\"", to.name, "\">")

julia> Base.AnnotatedDisplay.transition(io::IO, ::MIME"text/html", ::Tagged, ::Nothing) =
           print(io, "</mark>")

julia> s = Base.AnnotatedString("an example", [(4:10, Tag("noun"))]);

julia> print(IOContext(stdout, :color => true), s)
an [noun example]

julia> show(stdout, MIME("text/html"), s)
an <mark title="noun">example</mark>
```

```@docs
Base.AnnotatedDisplay
Base.AnnotatedDisplay.context
Base.AnnotatedDisplay.reduction
Base.AnnotatedDisplay.transition
```

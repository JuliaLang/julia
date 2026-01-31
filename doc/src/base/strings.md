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
  [`int128"..."`](@ref Base.@int128_str) resp. [`uint128"..."`](@ref Base.@uint128_str).

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
```

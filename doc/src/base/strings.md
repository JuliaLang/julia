# [Strings](@id lib-strings)

TODO: link to `manual/strings` and explain how these two pages related to each other

## General

```@docs
Core.AbstractString
```

```@docs
Base.:*(::Union{AbstractChar, AbstractString}, ::Union{AbstractChar, AbstractString}...)
Base.:^(::Union{AbstractString, AbstractChar}, ::Integer)
Base.repeat(::AbstractString, ::Integer)
Base.repeat(::AbstractChar, ::Integer)
```

```@docs
Base.string
Base.repr(::Any)
Core.String(::AbstractString)
```

```@docs
Base.isless(::AbstractString, ::AbstractString)
Base.:(==)(::AbstractString, ::AbstractString)
Base.cmp(::AbstractString, ::AbstractString)
```

## Characters and Encoding

```@docs
Core.AbstractChar
Core.Char
Base.transcode
Base.ncodeunits(::AbstractString)
Base.codeunit
Base.codeunits
Base.ascii
```

```@docs
Base.unsafe_string
```

```@docs
Base.codepoint
Base.length(::AbstractString)
Base.sizeof(::AbstractString)
Base.textwidth
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

## String and Character Predicates

```@docs
Base.isascii
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

## Additional String Types and Non-Standard String Literals

TODO: also mention / link to Regex, SubstitutionString

TODO: mention / link to `@v_str`

TODO: the `@b_str` docstring should link to "Byte Array Literals" section,
or at the very least mention *that term*

TODO

```@docs
Base.SubString
Base.LazyString
Base.@lazy_str
Base.@raw_str
Base.@b_str
Base.Docs.@html_str
Base.Docs.@text_str
```

## Regular Expressions

TODO: link to Regex section in `manual/strings/#man-regex-literals`
(and vice-versa)

```@docs
Base.Regex
Base.@r_str
Base.SubstitutionString
Base.@s_str
Base.match
Base.eachmatch
Base.RegexMatch
Base.keys(::RegexMatch)
```

TODO:  `*` method for Regex?

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

TODO: mention and link to Regular expressions section 

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


## Transforming strings

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

## Miscellaneous

TODO: where to put this? perhaps better in IO chapter?

```@docs
Base.takestring!
```

## `AnnotatedString`s

!!! note
    The API for AnnotatedStrings is considered experimental and is subject to change between
    Julia versions.

```@docs
Base.AnnotatedString
Base.AnnotatedChar
Base.annotatedstring
Base.annotations
Base.annotate!
```

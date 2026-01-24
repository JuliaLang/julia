# [Strings](@id lib-strings)

```@docs
Core.AbstractString
Core.AbstractChar
Core.Char
Base.codepoint
Base.length(::AbstractString)
Base.sizeof(::AbstractString)
Base.:*(::Union{AbstractChar, AbstractString}, ::Union{AbstractChar, AbstractString}...)
Base.:^(::Union{AbstractString, AbstractChar}, ::Integer)
Base.string
Base.repeat(::AbstractString, ::Integer)
Base.repeat(::AbstractChar, ::Integer)
Base.repr(::Any)
Core.String(::AbstractString)
Core.String(::AbstractVector{UInt8})
Core.String
Base.SubString
Base.LazyString
Base.@lazy_str
Base.transcode
Base.unsafe_string
Base.ncodeunits(::AbstractString)
Base.codeunit
Base.codeunits
Base.CodeUnits
Base.ascii
Base.Regex
Base.@r_str
Base.SubstitutionString
Base.@s_str
Base.@raw_str
Base.@b_str
Base.takestring!
Base.Docs.@html_str
Base.Docs.@text_str
Base.isvalid(::Any)
Base.isvalid(::Any, ::Any)
Base.isvalid(::AbstractString, ::Integer)
Base.ismalformed
Base.isoverlong
Base.show_invalid
Base.match
Base.eachmatch
Base.RegexMatch
Base.keys(::RegexMatch)
Base.isless(::AbstractString, ::AbstractString)
Base.:(==)(::AbstractString, ::AbstractString)
Base.cmp(::AbstractString, ::AbstractString)
Base.lpad
Base.rpad
Base.ltruncate
Base.rtruncate
Base.ctruncate
Base.findfirst(::AbstractString, ::AbstractString)
Base.findfirst(::AbstractChar, ::AbstractString)
Base.findnext(::AbstractString, ::AbstractString, ::Integer)
Base.findnext(::AbstractChar, ::AbstractString, ::Integer)
Base.findnext(::Any, ::Any)
Base.findnext(::Function, ::Any, ::Any)
Base.findnext(::AbstractVector{<:Union{Int8, UInt8}}, ::AbstractVector{<:Union{Int8, UInt8}}, ::Integer)
Base.findlast(::AbstractString, ::AbstractString)
Base.findlast(::AbstractChar, ::AbstractString)
Base.findlast(::AbstractVector{<:Union{Int8, UInt8}}, ::AbstractVector{<:Union{Int8, UInt8}})
Base.findprev(::AbstractString, ::AbstractString, ::Integer)
Base.findprev(::AbstractChar, ::AbstractString, ::Integer)
Base.findprev(::Any, ::Any)
Base.findprev(::Function, ::Any, ::Any)
Base.findprev(::AbstractVector{<:Union{Int8, UInt8}}, ::AbstractVector{<:Union{Int8, UInt8}}, ::Integer)
Base.findall(::AbstractChar, ::AbstractString)
Base.findall(::Union{AbstractPattern, AbstractString, AbstractVector{UInt8}}, ::Union{AbstractPattern, AbstractString, AbstractVector{UInt8}})
Base.occursin
Base.reverse(::Union{String,SubString{String}})
Base.replace(::IO, s::AbstractString, ::Pair...)
Base.eachsplit
Base.eachrsplit
Base.split
Base.rsplit
Base.strip
Base.lstrip
Base.rstrip
Base.startswith
Base.endswith
Base.contains
Base.first(::AbstractString, ::Integer)
Base.last(::AbstractString, ::Integer)
Base.uppercase
Base.lowercase
Base.titlecase
Base.uppercasefirst
Base.lowercasefirst
Base.join
Base.chop
Base.chopprefix
Base.chopsuffix
Base.chomp
Base.thisind
Base.nextind(::AbstractString, ::Integer, ::Integer)
Base.prevind(::AbstractString, ::Integer, ::Integer)
Base.textwidth
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
Base.escape_string
Base.escape_raw_string
Base.unescape_string
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

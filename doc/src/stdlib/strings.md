# [Strings](@id lib-strings)

```@docs
Base.length(::AbstractString)
Base.sizeof(::AbstractString)
Base.:*(::Union{Char, AbstractString}, ::Union{Char, AbstractString}...)
Base.:^(::AbstractString, ::Integer)
Base.string
Base.repeat(::AbstractString, ::Integer)
Base.repeat(::Char, ::Integer)
Base.repr
Core.String(::AbstractString)
Base.SubString
Base.transcode
Base.unsafe_string
Base.codeunit(::AbstractString, ::Integer)
Base.ascii
Base.@r_str
Base.@raw_str
Base.Docs.@html_str
Base.Docs.@text_str
Base.isvalid(::Any)
Base.isvalid(::Any, ::Any)
Base.isvalid(::AbstractString, ::Integer)
Base.ismatch
Base.match
Base.eachmatch
Base.matchall
Base.isless(::AbstractString, ::AbstractString)
Base.:(==)(::AbstractString, ::AbstractString)
Base.cmp(::AbstractString, ::AbstractString)
Base.lpad
Base.rpad
Base.search
Base.rsearch
Base.searchindex
Base.rsearchindex
Base.contains(::AbstractString, ::AbstractString)
Base.reverse(::Union{String,SubString{String}})
Base.replace
Base.split
Base.rsplit
Base.strip
Base.lstrip
Base.rstrip
Base.startswith
Base.endswith
Base.first(::AbstractString, ::Integer)
Base.last(::AbstractString, ::Integer)
Base.join
Base.chop
Base.chomp
Base.ind2chr
Base.chr2ind
Base.thisind
Base.nextind
Base.prevind
Base.Random.randstring
Core.Symbol
Base.escape_string
Base.unescape_string
```

# [Strings](@id lib-strings)

```@docs
Base.length(::AbstractString)
Base.sizeof(::AbstractString)
Base.:*(::AbstractString, ::Any...)
Base.:^(::AbstractString, ::Integer)
Base.string
Base.repr
Core.String(::AbstractString)
Base.transcode
Base.unsafe_string
Base.codeunit(::AbstractString, ::Integer)
Base.ascii
Base.@r_str
Base.Docs.@html_str
Base.Docs.@text_str
Base.UTF8proc.normalize_string
Base.UTF8proc.graphemes
Base.isvalid(::Any)
Base.isvalid(::Any, ::Any)
Base.isvalid(::AbstractString, ::Integer)
Base.UTF8proc.is_assigned_char
Base.ismatch
Base.match
Base.eachmatch
Base.matchall
Base.lpad
Base.rpad
Base.search
Base.rsearch
Base.searchindex
Base.rsearchindex
Base.contains(::AbstractString, ::AbstractString)
Base.reverse(::AbstractString)
Base.replace
Base.split
Base.rsplit
Base.strip
Base.lstrip
Base.rstrip
Base.startswith
Base.endswith
Base.uppercase
Base.lowercase
Base.titlecase
Base.ucfirst
Base.lcfirst
Base.join
Base.chop
Base.chomp
Base.ind2chr
Base.chr2ind
Base.nextind
Base.prevind
Base.Random.randstring
Base.UTF8proc.charwidth
Base.strwidth
Base.UTF8proc.isalnum
Base.UTF8proc.isalpha
Base.isascii
Base.UTF8proc.iscntrl
Base.UTF8proc.isdigit
Base.UTF8proc.isgraph
Base.UTF8proc.islower
Base.UTF8proc.isnumber
Base.UTF8proc.isprint
Base.UTF8proc.ispunct
Base.UTF8proc.isspace
Base.UTF8proc.isupper
Base.isxdigit
Core.Symbol
Base.escape_string
Base.unescape_string
```

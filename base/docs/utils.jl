# This file is a part of Julia. License is MIT: https://julialang.org/license

# Text / HTML objects

import .Base: print, show, ==, hash

export HTML, @html_str

export HTML, Text

"""
`HTML(s)`: Create an object that renders `s` as html.

    HTML("<div>foo</div>")

You can also use a stream for large amounts of data:

    HTML() do io
      println(io, "<div>foo</div>")
    end

!!! warning
    `HTML` is currently exported to maintain
    backwards compatibility, but this export
    is deprecated. It is recommended to use
    this type as [`Docs.HTML`](@ref) or to explicitly
    import it from `Docs`.
"""
mutable struct HTML{T}
    content::T
end

function HTML(xs...)
    HTML() do io
        for x in xs
            print(io, x)
        end
    end
end

show(io::IO, ::MIME"text/html", h::HTML) = print(io, h.content)
show(io::IO, ::MIME"text/html", h::HTML{<:Function}) = h.content(io)

"""
    @html_str -> Docs.HTML

Create an `HTML` object from a literal string.

# Examples
```jldoctest
julia> html"Julia"
HTML{String}("Julia")
```
"""
macro html_str(s)
    :(HTML($s))
end

function catdoc(xs::HTML...)
    HTML() do io
        for x in xs
            show(io, MIME"text/html"(), x)
        end
    end
end

export Text, @text_str

"""
`Text(s)`: Create an object that renders `s` as plain text.

    Text("foo")

You can also use a stream for large amounts of data:

    Text() do io
      println(io, "foo")
    end

!!! warning
    `Text` is currently exported to maintain
    backwards compatibility, but this export
    is deprecated. It is recommended to use
    this type as [`Docs.Text`](@ref) or to explicitly
    import it from `Docs`.
"""
mutable struct Text{T}
    content::T
end

print(io::IO, t::Text) = print(io, t.content)
print(io::IO, t::Text{<:Function}) = t.content(io)
show(io::IO, t::Text) = print(io, t)

==(t1::T, t2::T) where {T<:Union{HTML,Text}} = t1.content == t2.content
hash(t::T, h::UInt) where {T<:Union{HTML,Text}} = hash(T, hash(t.content, h))

"""
    @text_str -> Docs.Text

Create a `Text` object from a literal string.

# Examples
```jldoctest
julia> text"Julia"
Julia
```
"""
macro text_str(s)
    :(Text($s))
end

function catdoc(xs::Text...)
    Text() do io
        for x in xs
            show(io, MIME"text/plain"(), x)
        end
    end
end

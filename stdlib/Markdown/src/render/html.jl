# This file is a part of Julia. License is MIT: https://julialang.org/license

include("rich.jl")

# Utils


# Handle URI encoding, poorly. The code here was initially copied from
# Base.Filesystem, then modified. The original code implements RFC3986 Section
# 2.1 and 2.2.
#
# We changed encode_uri_component to not encode characters declared as
# "reserved" by RFC3986 Section 2.2 (i.e., those from the gen-delims and
# sub-delims sets). Instead the user is expected to percent encode these (or
# not) as needed -- the alternative would be implement full URI parsing, which
# is non-trivial. That said, it would be better to do that, by e.g. using the
# URIs.jl package, but that is not an option for us, as it is not a stdlib...
#
# As a special affordance, we deviate from the "reserved" list in one way: we
# do *not* exclude '[' and ']' from percent encoding, even though they are in
# the gen-delims set. They are only used to encode IPv6 literal addresses in
# the URI, which is (still) rare. But they do occur in query strings and
# indeed in the CommonMark spec tests.

percent_escape(s) = '%' * join(map(b -> uppercase(string(b, base=16)), codeunits(s)), '%')
encode_uri_component(s::AbstractString) = replace(s, r"[^A-Za-z0-9\-_.~/:?#@!$&'()*+,;=]+" => percent_escape)
encode_uri_component(s::Symbol) = encode_uri_component(string(s))

function withtag(f, io::IO, tag, attrs...)
    print(io, "<$tag")
    for (attr, value) in attrs
        print(io, " ", attr, "=\"")
        htmlesc(io, value)
        print(io, "\"")
    end
    f === nothing && return print(io, " />")

    print(io, ">")
    f()
    print(io, "</$tag>")
end

tag(io::IO, tag, attrs...) = withtag(nothing, io, tag, attrs...)

function htmlesc(io::IO, s::AbstractString)
    replace(io, s, '<'=>"&lt;", '>'=>"&gt;", '"'=>"&quot;", '&'=>"&amp;")
end
function htmlesc(io::IO, s::Symbol)
    htmlesc(io, string(s))
end
function htmlesc(io::IO, xs::Union{AbstractString,Symbol}...)
    for s in xs
        htmlesc(io, s)
    end
end
function htmlesc(s::Union{AbstractString,Symbol})
    sprint(htmlesc, s)
end

# Block elements

function html(io::IO, content::Vector)
    for md in content
        html(io, md)
        println(io)
    end
end

html(io::IO, md::MD) = html(io, md.content)

function html(io::IO, header::Header{l}) where l
    withtag(io, "h$l") do
        htmlinline(io, header.text)
    end
end

function html(io::IO, code′::Code)
    if code′.language == "styled"
        code′ = Code("", String(styled(code′.code)))
    end
    code = code′
    withtag(io, :pre) do
        maybe_lang = !isempty(code.language) ? Any[:class=>"language-$(code.language)"] : []
        withtag(io, :code, maybe_lang...) do
            htmlesc(io, code.code)
            !isempty(code.code) && println(io)
        end
    end
end

function html(io::IO, md::Paragraph)
    withtag(io, :p) do
        htmlinline(io, md.content)
    end
end

function html(io::IO, md::HTML)
    for line in md.content[1:end-1]
        println(io, line)
    end
    print(io, md.content[end])
end

function html(io::IO, md::BlockQuote)
    withtag(io, :blockquote) do
        println(io)
        html(io, md.content)
    end
end

function html(io::IO, f::Footnote)
    withtag(io, :div, :class => "footnote", :id => "footnote-$(f.id)") do
        withtag(io, :p, :class => "footnote-title") do
            print(io, f.id)
        end
        html(io, f.text)
    end
end

function html(io::IO, md::Admonition)
    withtag(io, :div, :class => "admonition $(md.category)") do
        withtag(io, :p, :class => "admonition-title") do
            print(io, md.title)
        end
        html(io, md.content)
    end
end

function html(io::IO, md::List)
    maybe_attr = md.ordered > 1 ? Any[:start => string(md.ordered)] : []
    withtag(io, isordered(md) ? :ol : :ul, maybe_attr...) do
        for item in md.items
            println(io)
            withtag(io, :li) do
                if md.loose
                    println(io)
                    html(io, item)
                else
                    htmltight(io, item)
                end
            end
        end
        println(io)
    end
end

htmltight(io::IO, md) = html(io, md)
htmltight(io::IO, md::Paragraph) = htmlinline(io, md.content)
function htmltight(io::IO, content::Vector)
    for md in content
        htmltight(io, md)
    end
end

function html(io::IO, md::HorizontalRule)
    tag(io, :hr)
end

html(io::IO, x) = tohtml(io, x)

# Inline elements

function htmlinline(io::IO, content::Vector)
    for x in content
        htmlinline(io, x)
    end
end

function htmlinline(io::IO, code′::Code)
    if code′.language == "styled"
        code′ = Code("", String(styled(code′.code)))
    end
    code = code′
    withtag(io, :code) do
        htmlesc(io, code.code)
    end
end

function htmlinline(io::IO, md::Union{Symbol,AbstractString})
    htmlinline(io, String(md))
end

function htmlinline(io::IO, s::String)
    # Spaces at the end of the line and beginning of the next line are removed
    s = replace(s, r"[ \t]+\n" => "\n")
    s = replace(s, r"\n[ \t]+" => "\n")
    htmlesc(io, s)
end

function htmlinline(io::IO, md::Bold)
    withtag(io, :strong) do
        htmlinline(io, md.text)
    end
end

function htmlinline(io::IO, md::Italic)
    withtag(io, :em) do
        htmlinline(io, md.text)
    end
end

function htmlinline(io::IO, md::Strikethrough)
    withtag(io, :s) do
        htmlinline(io, md.text)
    end
end

function htmlinline(io::IO, md::Image)
    tag(io, :img, :src=>encode_uri_component(md.url), :alt=>md.alt)
end


function htmlinline(io::IO, f::Footnote)
    withtag(io, :a, :href => "#footnote-$(f.id)", :class => "footnote") do
        print(io, "[", f.id, "]")
    end
end

function htmlinline(io::IO, link::Link)
    withtag(io, :a, :href=>encode_uri_component(link.url)) do
        htmlinline(io, link.text)
    end
end

function htmlinline(io::IO, br::LineBreak)
    tag(io, :br)
    println(io)
end

htmlinline(io::IO, x) = tohtml(io, x)

# API

export html

"""
    html([io::IO], md)

Output the contents of the Markdown object `md` in HTML format, either
writing to an (optional) `io` stream or returning a string.

One can alternatively use `show(io, "text/html", md)` or `repr("text/html", md)`, which
differ in that they wrap the output in a `<div class="markdown"> ... </div>` element.

# Examples
```jldoctest
julia> html(md"hello _world_")
"<p>hello <em>world</em></p>\\n"
```
"""
html(md) = sprint(html, md)

function show(io::IO, ::MIME"text/html", md::MD)
    withtag(io, :div, :class=>"markdown") do
        html(io, md)
    end
end

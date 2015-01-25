using Base.Markdown
import Base.Markdown: MD, Paragraph, Italic, plain, term, html
import Base: writemime

# Basics
# Equality is checked by making sure the HTML representations are
# the same

@test md"foo" == MD(Paragraph("foo"))
@test md"foo *bar* baz" == MD(Paragraph(["foo ", Italic("bar"), " baz"]))

# Basic plain (markdown) output

@test md"foo" |> plain == "foo\n"
@test md"foo *bar* baz" |> plain == "foo *bar* baz\n"

# HTML output

@test md"foo *bar* baz" |> html == "<p>foo <em>bar</em> baz</p>\n"

# Interpolation / Custom types

type Reference
    ref
end

ref(x) = Reference(x)

ref(fft)

writemime(io::IO, m::MIME"text/plain", r::Reference) =
    print(io, "$(r.ref) (see Julia docs)")

@test md"Behaves like $(ref(fft))" == md"Behaves like fft (see Julia docs)"

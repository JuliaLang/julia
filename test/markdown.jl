using Base.Markdown
import Base.Markdown: MD, Paragraph, Italic, plain, term

# Basics

@test md"foo" == MD(Paragraph("foo"))
@test plain(md"foo") == "foo\n"
@test md"foo *bar* baz" == MD(Paragraph(["foo ", Italic("bar"), " baz"]))
@test md"foo *bar* baz" |> plain == "foo *bar* baz\n"

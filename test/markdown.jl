# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Markdown
import Base.Markdown: MD, Paragraph, Header, Italic, Bold, LineBreak, plain, term, html, rst, Table, Code, LaTeX, Footnote
import Base: writemime

# Basics
# Equality is checked by making sure the HTML output is
# the same – the structure itself may be different.

@test md"foo" == MD(Paragraph("foo"))
@test md"foo *bar* baz" == MD(Paragraph(["foo ", Italic("bar"), " baz"]))
@test md"""foo
bar""" == MD(Paragraph(["foo bar"]))
@test md"""foo\
bar""" == MD(Paragraph(["foo", LineBreak(), "bar"]))

@test md"#no title" == MD(Paragraph(["#no title"]))
@test md"# title" == MD(Header{1}("title"))
@test md"""
  #
  empty
  """ == MD(Header{1}(""), Paragraph("empty"))
@test md"## section" == MD(Header{2}("section"))
@test md"# title *foo* `bar` **baz**" ==
    MD(Header{1}(["title ", Italic("foo")," ",Code("bar")," ",Bold("baz")]))
@test md"""
h1
===""" == md"# h1"
@test md"""
h2
   ---""" == md"## h2"

@test md"**foo *bar* baz**" == MD(Paragraph(Bold(["foo ", Italic("bar"), " baz"])))
@test md"*foo **bar** baz*" == MD(Paragraph(Italic(["foo ", Bold("bar"), " baz"])))

@test md"""```julia
foo
```
""" == MD(Code("julia", "foo"))

@test md"foo ``bar`` baz" == MD(Paragraph(["foo ", LaTeX("bar"), " baz"]))

@test md"""
```math
...
```
""" == MD(LaTeX("..."))

code_in_code = md"""
````
```
````
"""
@test code_in_code == MD(Code("```"))
@test plain(code_in_code) == "````\n```\n````\n"

@test md"A footnote [^foo]." == MD(Paragraph(["A footnote ", Footnote("foo", nothing), "."]))

@test md"[^foo]: footnote" == MD(Paragraph([Footnote("foo", Any[" footnote"])]))

@test md"""
* one
* two

1. pirate
2. ninja
3. zombie""" == Markdown.MD([Markdown.List(["one", "two"]),
                             Markdown.List(["pirate", "ninja", "zombie"], true)])

@test md"Foo [bar]" == MD(Paragraph("Foo [bar]"))
@test md"Foo [bar](baz)" != MD(Paragraph("Foo [bar](baz)"))
@test md"Foo \[bar](baz)" == MD(Paragraph("Foo [bar](baz)"))

# Basic plain (markdown) output

@test md"foo" |> plain == "foo\n"
@test md"foo *bar* baz" |> plain == "foo *bar* baz\n"
@test md"# title" |> plain == "# title\n"
@test md"## section" |> plain == "## section\n"
@test md"## section `foo`" |> plain == "## section `foo`\n"
@test md"""Hello

---
World""" |> plain == "Hello\n\n---\n\nWorld\n"
@test md"[*a*](b)" |> plain == "[*a*](b)\n"
@test md"""
> foo
>
>   * bar
>
> ```
> baz
> ```""" |> plain == """> foo\n>\n>   * bar\n>\n> ```\n> baz\n> ```\n\n"""

# Terminal (markdown) output

# multiple whitespace is ignored
@test sprint(term, md"a  b") == "  a b\n"
@test sprint(term, md"- a
        not code") == "    •  a not code\n"
@test sprint(term, md"[x](https://julialang.org)") == "  x\n"
@test sprint(term, md"![x](https://julialang.org)") == "  (Image: x)\n"

# enumeration is normalized
@test sprint(term, md"""
    1. a
    3. b""") == "    1. a\n    2. b\n"

# HTML output

@test md"foo *bar* baz" |> html == "<p>foo <em>bar</em> baz</p>\n"
@test md"something ***" |> html == "<p>something ***</p>\n"
@test md"# h1## " |> html == "<h1>h1##</h1>\n"
@test md"## h2 ### " |> html == "<h2>h2</h2>\n"
@test md"###### h6" |> html == "<h6>h6</h6>\n"
@test md"####### h7" |> html == "<p>####### h7</p>\n"
@test md"   >" |> html == "<blockquote>\n</blockquote>\n"
@test md"1. Hello" |> html == "<ol>\n<li>Hello</li>\n</ol>\n"
@test md"* World" |> html == "<ul>\n<li>World</li>\n</ul>\n"
@test md"# title *blah*" |> html == "<h1>title <em>blah</em></h1>\n"
@test md"## title *blah*" |> html == "<h2>title <em>blah</em></h2>\n"
@test md"""Hello

---
World""" |> html == "<p>Hello</p>\n<hr />\n<p>World</p>\n"
@test md"`escape</code>`" |> html == "<p><code>escape&lt;/code&gt;</code></p>\n"

@test md"""
    code1

    code2
""" |> html == "<pre><code>code1\n\ncode2</code></pre>\n" # single code block

# @test md"""
# - Foo
#  ---
# - Bar""" |> html == "<ul>\n<li>Foo</li>\n</ul>\n<hr />\n<ul>\n<li>Bar</li>\n</ul>\n"
@test md"""
h1
===
h2
---
not
== =""" |> html == "<h1>h1</h1>\n<h2>h2</h2>\n<p>not &#61;&#61; &#61;</p>\n"

# Latex output
book = md"""
# Title

Some discussion

> A quote

## Section *important*

Some **bolded**

- list1
- list2
"""
@test latex(book) == "\\section{Title}\nSome discussion\n\\begin{quote}\nA quote\n\\end{quote}\n\\subsection{Section \\emph{important}}\nSome \\textbf{bolded}\n\\begin{itemize}\n\\item list1\n\\item list2\n\\end{itemize}\n"

# writemime output

let out =
    """
    # Title

    Some discussion

    > A quote


    ## Section *important*

    Some **bolded**

      * list1
      * list2
    """
    @test sprint(io -> writemime(io, "text/plain", book)) == out
    @test sprint(io -> writemime(io, "text/markdown", book)) == out
end
let out =
    """
    <div class="markdown"><h1>Title</h1>
    <p>Some discussion</p>
    <blockquote>
    <p>A quote</p>
    </blockquote>
    <h2>Section <em>important</em></h2>
    <p>Some <strong>bolded</strong></p>
    <ul>
    <li>list1</li>
    <li>list2</li>
    </ul>
    </div>"""
    @test sprint(io -> writemime(io, "text/html", book)) == out
end
let out =
    """
    \\section{Title}
    Some discussion
    \\begin{quote}
    A quote
    \\end{quote}
    \\subsection{Section \\emph{important}}
    Some \\textbf{bolded}
    \\begin{itemize}
    \\item list1
    \\item list2
    \\end{itemize}
    """
    @test sprint(io -> writemime(io, "text/latex", book)) == out
end
let out =
    """
    Title
    *****


    Some discussion

        A quote


    Section *important*
    ===================


    Some **bolded**

    * list1
    * list2
    """
    @test sprint(io -> writemime(io, "text/rst", book)) == out
end

# rst rendering

for (input, output) in (
        md"foo *bar* baz"     => "foo *bar* baz\n",
        md"something ***"     => "something ***\n",
        md"# h1## "           => "h1##\n****\n\n",
        md"## h2 ### "        => "h2\n==\n\n",
        md"###### h6"         => "h6\n..\n\n",
        md"####### h7"        => "####### h7\n",
        md"   >"              => "    \n\n",
        md"1. Hello"          => "1. Hello\n",
        md"* World"           => "* World\n",
        md"``x + y``"         => ":math:`x + y`\n",
        md"# title *blah*"    => "title *blah*\n************\n\n",
        md"## title *blah*"   => "title *blah*\n============\n\n",
        md"[`x`](:func:`x`)"  => ":func:`x`\n",
        md"[`x`](:obj:`x`)"   => ":obj:`x`\n",
        md"[`x`](:ref:`x`)"   => ":ref:`x`\n",
        md"[`x`](:exc:`x`)"   => ":exc:`x`\n",
        md"[`x`](:class:`x`)" => ":class:`x`\n",
        md"[`x`](:const:`x`)" => ":const:`x`\n",
        md"[`x`](:data:`x`)"  => ":data:`x`\n",
        md"[`x`](:???:`x`)"   => "```x`` <:???:`x`>`_\n",
        md"[x](y)"            => "`x <y>`_\n",
    )
    @test rst(input) == output
end

# Interpolation / Custom types

type Reference
    ref
end

ref(x) = Reference(x)

ref(mean)

writemime(io::IO, m::MIME"text/plain", r::Reference) =
    print(io, "$(r.ref) (see Julia docs)")

mean_ref = md"Behaves like $(ref(mean))"
@test plain(mean_ref) == "Behaves like mean (see Julia docs)\n"
@test html(mean_ref) == "<p>Behaves like mean &#40;see Julia docs&#41;</p>\n"

writemime(io::IO, m::MIME"text/html", r::Reference) =
    Markdown.withtag(io, :a, :href=>"test") do
        Markdown.htmlesc(io, Markdown.plaininline(r))
    end
@test html(mean_ref) == "<p>Behaves like <a href=\"test\">mean &#40;see Julia docs&#41;</a></p>\n"

@test md"""
````julia
foo()
````""" == md"""
```julia
foo()
```"""

# GH tables
@test md"""
    a  | b
    ---|---
    1  | 2""" == MD(Table(Any[["a","b"],
                              ["1","2"]], [:r, :r]))

@test md"""
    | a  |  b | c |
    | :-- | --: | --- |
    | d`gh`hg | hgh**jhj**ge | f |""" == MD(Table(Any[["a","b","c"],
                                                      Any[["d",Code("gh"),"hg"],
                                                          ["hgh",Bold("jhj"),"ge"],
                                                          "f"]],
                                                  [:l, :r, :r]))
@test md"""
no|table
no error
""" == MD([Paragraph(Any["no|table no error"])])

let t = """a   |   b
    :-- | --:
    1   |   2
    """
    @test Markdown.parse(t) == MD(Table(Any[Any["a", "b"], Any["1", "2"]], [:l, :r]))
end

let text =
    """
    | a   |   b |
    |:--- | ---:|
    | 1   |   2 |
    """,
    table = Markdown.parse(text)
    @test text == Markdown.plain(table)
end
let text =
    """
    | Markdown | Table |  Test |
    |:-------- |:-----:| -----:|
    | foo      | `bar` | *baz* |
    | `bar`    |  baz  | *foo* |
    """,
    table = Markdown.parse(text)
    @test text == Markdown.plain(table)
end

# LaTeX extension

let in_dollars =
    """
    We have \$x^2 < x\$ whenever:

    \$|x| < 1\$

    etc.
    """,
    in_backticks =
    """
    We have ``x^2 < x`` whenever:

    ```math
    |x| < 1
    ```

    etc.
    """,
    out_plain =
    """
    We have \$x^2 < x\$ whenever:

    \$\$
    |x| < 1
    \$\$

    etc.
    """,
    out_rst =
    """
    We have :math:`x^2 < x` whenever:

    .. math::

        |x| < 1

    etc.
    """,
    out_latex =
    """
    We have \$x^2 < x\$ whenever:
    \$\$|x| < 1\$\$
    etc.
    """,
    dollars   = Markdown.parse(in_dollars),
    backticks = Markdown.parse(in_backticks),
    latex_doc = MD(
        Any[Paragraph(Any["We have ", LaTeX("x^2 < x"), " whenever:"]),
            LaTeX("|x| < 1"),
            Paragraph(Any["etc."])
    ])

    @test out_plain == Markdown.plain(dollars)
    @test out_plain == Markdown.plain(backticks)

    @test out_rst   == Markdown.rst(dollars)
    @test out_rst   == Markdown.rst(backticks)

    @test out_latex == Markdown.latex(dollars)
    @test out_latex == Markdown.latex(backticks)

    @test latex_doc == dollars
    @test latex_doc == backticks
end

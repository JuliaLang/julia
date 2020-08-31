# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Markdown
import Markdown: MD, Paragraph, Header, Italic, Bold, LineBreak, plain, term, html, rst, Table, Code, LaTeX, Footnote
import Base: show

# Basics
# Equality is checked by making sure the HTML output is
# the same – the structure itself may be different.

@test md"foo" == MD(Paragraph("foo"))
@test md"foo *bar* baz" == MD(Paragraph(["foo ", Italic("bar"), " baz"]))
@test md"foo _bar_ baz" == MD(Paragraph(["foo ", Italic("bar"), " baz"]))
@test md"foo **bar** baz" == MD(Paragraph(["foo ", Bold("bar"), " baz"]))
@test md"foo __bar__ baz" == MD(Paragraph(["foo ", Bold("bar"), " baz"]))
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

let text = "Foo ```bar` ``baz`` ```\n",
    md = Markdown.parse(text)
    @test text == Markdown.plain(md)
end

@test isempty(Markdown.parse("\r"))
@test Markdown.parse("hello\r") == MD(Paragraph(["hello"]))
@test Markdown.parse("hello\r*julia*") == MD(Paragraph(Any["hello ", Italic(Any["julia"])]))

@test md"A footnote [^foo]." == MD(Paragraph(["A footnote ", Footnote("foo", nothing), "."]))

@test md"[^foo]: footnote" == MD([Footnote("foo", Any[Paragraph(Any["footnote"])])])

let text =
    """
    A paragraph with some footnotes,[^1] and another.[^note]

    [^1]: Footnote text for the first.

    [^note]: A longer footnote:

        Indented paragraphs are part of the footnote.

            some.code

        And *another* paragraph.

    This isn't part of the footnote.
    """,
    md = Markdown.parse(text)
    @test length(md.content) == 4
    @test isa(md.content[1], Markdown.Paragraph)
    @test isa(md.content[2], Markdown.Footnote)
    @test isa(md.content[3], Markdown.Footnote)
    @test isa(md.content[4], Markdown.Paragraph)

    @test md.content[2].id == "1"
    @test md.content[3].id == "note"

    @test length(md.content[3].text) == 4

    let expected =
            """
            A paragraph with some footnotes,[^1] and another.[^note]

            [^1]: Footnote text for the first.

            [^note]:
                A longer footnote:

                Indented paragraphs are part of the footnote.

                ```
                some.code
                ```

                And *another* paragraph.


            This isn't part of the footnote.
            """
        @test Markdown.plain(md) == expected
    end
    let expected =
            """
            A paragraph with some footnotes,[1]_ and another.[note]_

            .. [1] Footnote text for the first.

            .. [note]
               A longer footnote:

               Indented paragraphs are part of the footnote.

               .. code-block:: julia

                   some.code

               And *another* paragraph.


            This isn't part of the footnote.
            """
        @test Markdown.rst(md) == expected
    end
    let html = Markdown.html(md)
        @test occursin(",<a href=\"#footnote-1\" class=\"footnote\">[1]</a>", html)
        @test occursin(".<a href=\"#footnote-note\" class=\"footnote\">[note]</a>", html)
        @test occursin("<div class=\"footnote\" id=\"footnote-1\"><p class=\"footnote-title\">1</p>", html)
        @test occursin("<div class=\"footnote\" id=\"footnote-note\"><p class=\"footnote-title\">note</p>", html)
    end
    let latex = Markdown.latex(md)
        @test occursin(",\\footnotemark[1]", latex)
        @test occursin(".\\footnotemark[note]", latex)
        @test occursin("\n\\footnotetext[1]{Footnote text for", latex)
        @test occursin("\n\\footnotetext[note]{A longer footnote:\n", latex)
    end
end

let doc = md"""
* one
* two

1. pirate
2. ninja
3. zombie"""
    @test isa(doc.content[1], Markdown.List)
    @test isa(doc.content[2], Markdown.List)
    @test doc.content[1].items[1][1].content[1] == "one"
    @test doc.content[1].items[2][1].content[1] == "two"
    @test doc.content[2].items[1][1].content[1] == "pirate"
    @test doc.content[2].items[2][1].content[1] == "ninja"
    @test doc.content[2].items[3][1].content[1] == "zombie"
end

let doc = Markdown.parse(
        """
        A paragraph...
        - one
        - two
           * three
           * four
        ... another paragraph.
        """
    )
    @test length(doc.content) === 3
    @test isa(doc.content[1], Markdown.Paragraph)
    @test isa(doc.content[2], Markdown.List)
    @test isa(doc.content[3], Markdown.Paragraph)

    @test length(doc.content[2].items) === 2
    @test doc.content[2].items[1][1].content[1] == "one"
    @test length(doc.content[2].items[2]) == 2
    @test doc.content[2].items[2][1].content[1] == "two"

    @test isa(doc.content[2].items[2][2], Markdown.List)
    @test length(doc.content[2].items[2][2].items) === 2
    @test doc.content[2].items[2][2].items[1][1].content[1] == "three"
    @test doc.content[2].items[2][2].items[2][1].content[1] == "four"
end

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
@test sprint(term, md"a  b") == "  a b"
@test sprint(term, md"[x](https://julialang.org)") == "  x (https://julialang.org)"
@test sprint(term, md"[x](@ref)") == "  x"
@test sprint(term, md"[x](@ref something)") == "  x"
@test sprint(term, md"![x](https://julialang.org)") == "  (Image: x)"

# math (LaTeX)
@test sprint(term, md"""
```math
A = Q R
```
""") == "  A = Q R"

# enumeration is normalized
let doc = Markdown.parse(
        """
        1. a
        3. b
        """
    )
    @test occursin("1. ", sprint(term, doc))
    @test occursin("2. ", sprint(term, doc))
    @test !occursin("3. ", sprint(term, doc))
end

# Testing margin when printing Tables to the terminal.
@test sprint(term, md"""
| R |
|---|
| L |
""") == "  R\n  –\n  L"

@test sprint(term, md"""
!!! note "Tables in admonitions"

    | R |
    |---|
    | L |
""") == "  │ Tables in admonitions\n  │\n  │  R\n  │  –\n  │  L"

# HTML output
@test md"foo *bar* baz" |> html == "<p>foo <em>bar</em> baz</p>\n"
@test md"something ***" |> html == "<p>something ***</p>\n"
@test md"# h1## " |> html == "<h1>h1##</h1>\n"
@test md"## h2 ### " |> html == "<h2>h2</h2>\n"
@test md"###### h6" |> html == "<h6>h6</h6>\n"
@test md"####### h7" |> html == "<p>####### h7</p>\n"
@test md"   >" |> html == "<blockquote>\n</blockquote>\n"
@test md"1. Hello" |> html == "<ol>\n<li><p>Hello</p>\n</li>\n</ol>\n"
@test md"* World" |> html == "<ul>\n<li><p>World</p>\n</li>\n</ul>\n"
@test md"# title *blah*" |> html == "<h1>title <em>blah</em></h1>\n"
@test md"## title *blah*" |> html == "<h2>title <em>blah</em></h2>\n"
@test md"<https://julialang.org>" |> html == """<p><a href="https://julialang.org">https://julialang.org</a></p>\n"""
@test md"<mailto://a@example.com>" |> html == """<p><a href="mailto://a@example.com">mailto://a@example.com</a></p>\n"""
@test md"<https://julialang.org/not a link>" |> html == "<p>&lt;https://julialang.org/not a link&gt;</p>\n"
@test md"""<https://julialang.org/nota
link>""" |> html == "<p>&lt;https://julialang.org/nota link&gt;</p>\n"
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
@test latex(book) == "\\section{Title}\nSome discussion\n\n\\begin{quote}\nA quote\n\n\\end{quote}\n\\subsection{Section \\emph{important}}\nSome \\textbf{bolded}\n\n\\begin{itemize}\n\\item list1\n\n\n\\item list2\n\n\\end{itemize}\n"
table = md"""
 a | b
---|---
 1 | 2
"""
@test latex(table) ==
    "\\begin{tabular}\n{r | r}\na & b \\\\\n\\hline\n1 & 2 \\\\\n\\end{tabular}\n"

# mime output
let out =
    @test sprint(show, "text/plain", book) ==
        "  Title\n  ≡≡≡≡≡≡≡\n\n  Some discussion\n\n  │  A quote\n\n  Section important\n  ===================\n\n  Some bolded\n\n    •    list1\n\n    •    list2"
    @test sprint(show, "text/markdown", book) ==
        """
        # Title

        Some discussion

        > A quote


        ## Section *important*

        Some **bolded**

          * list1
          * list2
        """
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
    <li><p>list1</p>
    </li>
    <li><p>list2</p>
    </li>
    </ul>
    </div>"""
    @test sprint(show, "text/html", book) == out
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
    @test sprint(show, "text/latex", book) == out
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
    @test sprint(show, "text/rst", book) == out
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
mutable struct Reference
    ref
end

ref(x) = Reference(x)

ref(sum)

show(io::IO, m::MIME"text/plain", r::Reference) =
    print(io, "$(r.ref) (see Julia docs)")

sum_ref = md"Behaves like $(ref(sum))"
@test plain(sum_ref) == "Behaves like sum (see Julia docs)\n"
@test html(sum_ref) == "<p>Behaves like sum &#40;see Julia docs&#41;</p>\n"

show(io::IO, m::MIME"text/html", r::Reference) =
    Markdown.withtag(io, :a, :href=>"test") do
        Markdown.htmlesc(io, Markdown.plaininline(r))
    end
@test html(sum_ref) == "<p>Behaves like <a href=\"test\">sum &#40;see Julia docs&#41;</a></p>\n"

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
    @test Markdown.html(table) == """<table><tr><th align="left">Markdown</th><th align="center">Table</th><th align="right">Test</th></tr><tr><td align="left">foo</td><td align="center"><code>bar</code></td><td align="right"><em>baz</em></td></tr><tr><td align="left"><code>bar</code></td><td align="center">baz</td><td align="right"><em>foo</em></td></tr></table>\n"""
end
let text =
    """
    | a        |   b |
    |:-------- | ---:|
    | `x \\| y` |   2 |
    """,
    table = Markdown.parse(text)
    @test text == Markdown.plain(table)
    @test Markdown.html(table) == """<table><tr><th align="left">a</th><th align="right">b</th></tr><tr><td align="left"><code>x | y</code></td><td align="right">2</td></tr></table>\n"""
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

# Nested backticks for inline code and math.
let t_1 = "`code` ``math`` ```code``` ````math```` `````code`````",
    t_2 = "`` `math` `` ``` `code` ``code`` ``` ```` `math` ``math`` ```math``` ````",
    t_3 = "`` ` `` ``` `` ` `` ` ` ```",
    t_4 = """`code
    over several
    lines` ``math
    over several
    lines`` ``math with
    ` some extra ` ` backticks`
    ``""",
    t_5 = "``code at end of string`",
    t_6 = "```math at end of string``"
    @test Markdown.parse(t_1) == MD(Paragraph([
        Code("code"),
        " ",
        LaTeX("math"),
        " ",
        Code("code"),
        " ",
        LaTeX("math"),
        " ",
        Code("code"),
    ]))
    @test Markdown.parse(t_2) == MD(Paragraph([
        LaTeX("`math`"),
        " ",
        Code("`code` ``code``"),
        " ",
        LaTeX("`math` ``math`` ```math```"),
    ]))
    @test Markdown.parse(t_3) == MD(Paragraph([
        LaTeX("`"),
        " ",
        Code("`` ` `` ` `"),
    ]))
    @test Markdown.parse(t_4) == MD(Paragraph([
        Code("code over several lines"),
        " ",
        LaTeX("math over several lines"),
        " ",
        LaTeX("math with ` some extra ` ` backticks`")
    ]))
    @test Markdown.parse(t_5) == MD(Paragraph([
        "`",
        Code("code at end of string"),
    ]))
    @test Markdown.parse(t_6) == MD(Paragraph([
        "`",
        LaTeX("math at end of string"),
    ]))
end

# Admonitions.
let t_1 =
        """
        # Foo

        !!! note

        !!! warning "custom title"

        ## Bar

        !!! danger ""

        !!!
        """,
    t_2 =
        """
        !!! note
            foo bar baz

        !!! warning "custom title"
            - foo
            - bar
            - baz

            foo bar baz

        !!! danger ""

            ```
            foo
            ```

                bar

            # baz
        """,
    m_1 = Markdown.parse(t_1),
    m_2 = Markdown.parse(t_2)

    # Content Tests.

    @test isa(m_1.content[2], Markdown.Admonition)
    @test m_1.content[2].category == "note"
    @test m_1.content[2].title == "Note"
    @test m_1.content[2].content == []

    @test isa(m_1.content[3], Markdown.Admonition)
    @test m_1.content[3].category == "warning"
    @test m_1.content[3].title == "custom title"
    @test m_1.content[3].content == []

    @test isa(m_1.content[5], Markdown.Admonition)
    @test m_1.content[5].category == "danger"
    @test m_1.content[5].title == ""
    @test m_1.content[5].content == []

    @test isa(m_1.content[6], Markdown.Paragraph)

    @test isa(m_2.content[1], Markdown.Admonition)
    @test m_2.content[1].category == "note"
    @test m_2.content[1].title == "Note"
    @test isa(m_2.content[1].content[1], Markdown.Paragraph)

    @test isa(m_2.content[2], Markdown.Admonition)
    @test m_2.content[2].category == "warning"
    @test m_2.content[2].title == "custom title"
    @test isa(m_2.content[2].content[1], Markdown.List)
    @test isa(m_2.content[2].content[2], Markdown.Paragraph)

    @test isa(m_2.content[3], Markdown.Admonition)
    @test m_2.content[3].category == "danger"
    @test m_2.content[3].title == ""
    @test isa(m_2.content[3].content[1], Markdown.Code)
    @test isa(m_2.content[3].content[2], Markdown.Code)
    @test isa(m_2.content[3].content[3], Markdown.Header{1})

    # Rendering Tests.
    let out = Markdown.plain(m_1),
        expected =
            """
            # Foo

            !!! note
            \n\n
            !!! warning "custom title"
            \n\n
            ## Bar

            !!! danger ""
            \n\n
            !!!
            """
        @test out == expected
    end
    let out = Markdown.rst(m_1),
        expected =
            """
            Foo
            ***
            \n
            .. note::
            \n\n
            .. warning:: custom title
            \n\n
            Bar
            ===
            \n
            .. danger::
            \n\n
            !!!
            """
        @test out == expected
    end
    let out = Markdown.latex(m_1),
        expected =
            """
            \\section{Foo}
            \\begin{quote}
            \\textbf{note}

            Note

            \\end{quote}
            \\begin{quote}
            \\textbf{warning}

            custom title

            \\end{quote}
            \\subsection{Bar}
            \\begin{quote}
            \\textbf{danger}
            \n\n
            \\end{quote}
            !!!

            """
        @test out == expected
    end
    let out = Markdown.html(m_1),
        expected =
            """
            <h1>Foo</h1>
            <div class="admonition note"><p class="admonition-title">Note</p></div>
            <div class="admonition warning"><p class="admonition-title">custom title</p></div>
            <h2>Bar</h2>
            <div class="admonition danger"><p class="admonition-title"></p></div>
            <p>&#33;&#33;&#33;</p>
            """
        @test out == expected
    end

    let out = Markdown.plain(m_2),
        expected =
            """
            !!! note
                foo bar baz


            !!! warning "custom title"
                  * foo
                  * bar
                  * baz

                foo bar baz


            !!! danger ""
                ```
                foo
                ```

                ```
                bar
                ```

                # baz

            """
        @test out == expected
    end
    let out = Markdown.rst(m_2),
        expected =
            """
            .. note::
               foo bar baz


            .. warning:: custom title
               * foo
               * bar
               * baz

               foo bar baz


            .. danger::
               .. code-block:: julia

                   foo

               .. code-block:: julia

                   bar

               baz
               ***

            """
        @test out == expected
    end
end

# Nested Lists.
let text =
        """
        1. A paragraph
           with two lines.

               indented code

           > A block quote.

        - one

         two

        - one

          two


        - baz

        + ```
          foo
          ```

        1. foo
        2. bar
        3. baz
        """,
    md = Markdown.parse(text)

    # Content and structure tests.

    @test length(md.content) == 6
    @test length(md.content[1].items) == 1
    @test length(md.content[1].items[1]) == 3
    @test isa(md.content[1].items[1][1], Markdown.Paragraph)
    @test isa(md.content[1].items[1][2], Markdown.Code)
    @test isa(md.content[1].items[1][3], Markdown.BlockQuote)
    @test length(md.content[2].items) == 1
    @test isa(md.content[2].items[1][1], Markdown.Paragraph)
    @test isa(md.content[3], Markdown.Paragraph)
    @test length(md.content[4].items) == 1
    @test isa(md.content[4].items[1][1], Paragraph)
    @test isa(md.content[4].items[1][2], Paragraph)
    @test length(md.content[5].items) == 2
    @test isa(md.content[5].items[1][1], Markdown.Paragraph)
    @test isa(md.content[5].items[2][1], Markdown.Code)
    @test length(md.content[6].items) == 3
    @test md.content[6].items[1][1].content[1] == "foo"
    @test md.content[6].items[2][1].content[1] == "bar"
    @test md.content[6].items[3][1].content[1] == "baz"

    # Rendering tests.
    let expected =
            """
            1. A paragraph with two lines.

                ```
                indented code
                ```

                > A block quote.

              * one

            two

              * one

                two

              * baz
              * ```
                foo
                ```

            1. foo
            2. bar
            3. baz
            """
        @test expected == Markdown.plain(md)
    end

    let expected =
            """
            <ol>
            <li><p>A paragraph with two lines.</p>
            <pre><code>indented code</code></pre>
            <blockquote>
            <p>A block quote.</p>
            </blockquote>
            </li>
            </ol>
            <ul>
            <li><p>one</p>
            </li>
            </ul>
            <p>two</p>
            <ul>
            <li><p>one</p>
            <p>two</p>
            </li>
            </ul>
            <ul>
            <li><p>baz</p>
            </li>
            <li><pre><code>foo</code></pre>
            </li>
            </ul>
            <ol>
            <li><p>foo</p>
            </li>
            <li><p>bar</p>
            </li>
            <li><p>baz</p>
            </li>
            </ol>
            """
        @test expected == Markdown.html(md)
    end

    let expected =
            """
            1. A paragraph with two lines.

               .. code-block:: julia

                   indented code

                   A block quote.

            * one

            two

            * one

              two

            * baz
            * .. code-block:: julia

                  foo

            1. foo
            2. bar
            3. baz
            """
        @test expected == Markdown.rst(md)
    end
end

# Ordered list starting number.
let text =
        """
        42. foo
        43. bar


        1. foo
        2. bar


        - foo
        - bar
        """,
    md = Markdown.parse(text)

    @test md.content[1].ordered == 42
    @test md.content[2].ordered == 1
    @test md.content[3].ordered == -1

    let expected =
            """
            <ol start="42">
            <li><p>foo</p>
            </li>
            <li><p>bar</p>
            </li>
            </ol>
            <ol>
            <li><p>foo</p>
            </li>
            <li><p>bar</p>
            </li>
            </ol>
            <ul>
            <li><p>foo</p>
            </li>
            <li><p>bar</p>
            </li>
            </ul>
            """
        @test expected == Markdown.html(md)
    end

    let expected =
            """
            \\begin{itemize}
            \\item[42. ] foo


            \\item[43. ] bar

            \\end{itemize}
            \\begin{itemize}
            \\item[1. ] foo


            \\item[2. ] bar

            \\end{itemize}
            \\begin{itemize}
            \\item foo


            \\item bar

            \\end{itemize}
            """
        @test expected == Markdown.latex(md)
    end
end

# issue 20225, check this can print
@test typeof(sprint(Markdown.term, Markdown.parse(" "))) == String

# different output depending on whether color is requested:	+# issue 20225, check this can print
let buf = IOBuffer()
    @test typeof(sprint(Markdown.term, Markdown.parse(" "))) == String
    show(buf, "text/plain", md"*emph*")
    @test String(take!(buf)) == "  emph"
    show(buf, "text/markdown", md"*emph*")
    @test String(take!(buf)) == "*emph*\n"
    show(IOContext(buf, :color=>true), "text/plain", md"*emph*")
    @test String(take!(buf)) == "  \e[4memph\e[24m"
end

# table rendering with term #25213
t = """
    a   |   b
    :-- | --:
    1   |   2"""
@test sprint(Markdown.term, Markdown.parse(t), 0) == "  a b\n  – –\n  1 2"

# test Base.copy
let
    md = doc"test"
    md′ = copy(md)
    @test length(md) == length(md′) == 1
    push!(md, "new")
    @test length(md) == 2
    @test length(md′) == 1

    @test !haskey(md.meta, :foo)
    md.meta[:foo] = 42
    @test !haskey(md′.meta, :foo)
end

let
    v = Markdown.parse("foo\n\n- 1\n- 2\n\n- 3\n\n\n- 1\n- 2\n\nbar\n\n- 1\n\n  2\n- 4\n\nbuz\n\n- 1\n- 2\n  3\n- 4\n")
    @test v.content[2].loose
    @test !v.content[3].loose
    @test v.content[5].loose
    @test !v.content[7].loose
end

# issue #29995
let m = Markdown.parse("---"), io = IOBuffer()
    show(io, "text/latex", m)
    @test String(take!(io)) == "\\rule{\\textwidth}{1pt}\n"
end

# issue #16194: interpolation in md"..." strings
@testset "issue #16194: interpolation in md\"...\" strings" begin
    x = "X"
    contains_X(md) = occursin(x, sprint(show, MIME("text/plain"), md))
    @test contains_X(md"# $x") # H1
    @test contains_X(md"## $x") # H2
    @test contains_X(md"### $x") # H3
    @test contains_X(md"x = $x") # Paragraph
    @test contains_X(md"- $x") # List
    @test contains_X(md"[$x](..)") # Link
    @test contains_X(md"**$x**") # Bold
    @test contains_X(md"*$x*") # Italic
    @test contains_X( # Table
        md"""
        | name |
        |------|
        |  $x  |
        """)
end

@testset "issue #37232: linebreaks" begin
    s = @md_str """
       Misc:\\
       - line\\
       """
    @test sprint(show, MIME("text/plain"), s) == "  Misc:\n  - line\n  "
end

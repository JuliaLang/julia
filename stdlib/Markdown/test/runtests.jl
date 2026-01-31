# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Markdown, StyledStrings
import Markdown: MD, Paragraph, Header, Italic, Bold, Strikethrough, LineBreak, Table, Code, LaTeX, Footnote
import Base: show

# Basics
# Equality is checked by making sure the HTML output is
# the same – the structure itself may be different.
@testset "basics" begin
    @test md"foo" == MD(Paragraph("foo"))
    @test md"foo *bar* baz" == MD(Paragraph(["foo ", Italic("bar"), " baz"]))
    @test md"foo _bar_ baz" == MD(Paragraph(["foo ", Italic("bar"), " baz"]))
    @test md"foo **bar** baz" == MD(Paragraph(["foo ", Bold("bar"), " baz"]))
    @test md"foo __bar__ baz" == MD(Paragraph(["foo ", Bold("bar"), " baz"]))
    @test md"foo ~bar~ baz" == MD(Paragraph(["foo ", Strikethrough("bar"), " baz"]))
    @test md"foo ~~bar~~ baz" == MD(Paragraph(["foo ", Strikethrough("bar"), " baz"]))
    @test md"""foo
    bar""" == MD(Paragraph(["foo\nbar"]))
    @test md"""foo\
    bar""" == MD(Paragraph(["foo", LineBreak(), "bar"]))

    @test md"#no title" == MD(Paragraph(["#no title"]))
    @test md"# title" == MD(Header{1}("title"))
    @test md"""
      #
      empty
      """ == MD(Header{1}(""), Paragraph("empty"))
    @test md"## section" == MD(Header{2}("section"))
    @test md"# title *foo* `bar` **baz** ~~qux~~" ==
        MD(Header{1}(["title ", Italic("foo")," ",Code("bar")," ",Bold("baz")," ",Strikethrough("qux")]))
    @test md"""
    h1
    ===""" == md"# h1"
    @test md"""
    h2
       ---""" == md"## h2"

    @test md"**foo *bar* baz**" == MD(Paragraph(Bold(["foo ", Italic("bar"), " baz"])))
    @test md"*foo **bar** baz*" == MD(Paragraph(Italic(["foo ", Bold("bar"), " baz"])))
end

@testset "fenced code blocks" begin

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
    @test Markdown.plain(code_in_code) == "````\n```\n````\n"

    text = "Foo ```bar` ``baz`` ```\n"
    md = Markdown.parse(text)
    @test text == Markdown.plain(md)

    @test md"""
    ````julia
    foo()
    ````""" == md"""
    ```julia
    foo()
    ```"""
end

@testset "linefeeds" begin
    @test isempty(Markdown.parse("\r"))
    @test Markdown.parse("hello\r") == MD(Paragraph(["hello"]))
    @test Markdown.parse("hello\r*julia*") == MD(Paragraph(Any["hello\n", Italic(Any["julia"])]))
end

@testset "footnotes" begin
    @test md"A footnote [^foo]." == MD(Paragraph(["A footnote ", Footnote("foo", nothing), "."]))

    @test md"[^foo]: footnote" == MD([Footnote("foo", Any[Paragraph(Any["footnote"])])])

    text =
    """
    A paragraph with some footnotes,[^1] and another.[^note]

    [^1]: Footnote text for the first.

    [^note]: A longer footnote:

        Indented paragraphs are part of the footnote.

            some.code

        And *another* paragraph.

    \tAnd a third paragraph indented with a *tab*.

    This isn't part of the footnote.
    """
    md = Markdown.parse(text)
    @test length(md) == 4
    @test isa(md[1], Markdown.Paragraph)
    @test isa(md[2], Markdown.Footnote)
    @test isa(md[3], Markdown.Footnote)
    @test isa(md[4], Markdown.Paragraph)

    @test md[2].id == "1"
    @test md[3].id == "note"

    @test length(md[3].text) == 5

    expected =
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

                And a third paragraph indented with a *tab*.


            This isn't part of the footnote.
            """
    @test Markdown.plain(md) == expected

    expected =
            """
            A paragraph with some footnotes,[1]_ and another.[note]_

            .. [1] Footnote text for the first.

            .. [note]
               A longer footnote:

               Indented paragraphs are part of the footnote.

               .. code-block:: julia

                   some.code

               And *another* paragraph.

               And a third paragraph indented with a *tab*.


            This isn't part of the footnote.
            """
    @test Markdown.rst(md) == expected

    html = Markdown.html(md)
    @test occursin(",<a href=\"#footnote-1\" class=\"footnote\">[1]</a>", html)
    @test occursin(".<a href=\"#footnote-note\" class=\"footnote\">[note]</a>", html)
    @test occursin("<div class=\"footnote\" id=\"footnote-1\"><p class=\"footnote-title\">1</p>", html)
    @test occursin("<div class=\"footnote\" id=\"footnote-note\"><p class=\"footnote-title\">note</p>", html)

    latex = Markdown.latex(md)
    @test occursin(",\\footnotemark[1]", latex)
    @test occursin(".\\footnotemark[note]", latex)
    @test occursin("\n\\footnotetext[1]{Footnote text for", latex)
    @test occursin("\n\\footnotetext[note]{A longer footnote:\n", latex)
end

@testset "lists" begin
    doc = md"""
    * one
    * two

    1. pirate
    2. ninja
    3. zombie"""
    @test typeof.(doc) == [Markdown.List, Markdown.List]
    @test doc[1].items[1][1].content[1] == "one"
    @test doc[1].items[2][1].content[1] == "two"
    @test doc[2].items[1][1].content[1] == "pirate"
    @test doc[2].items[2][1].content[1] == "ninja"
    @test doc[2].items[3][1].content[1] == "zombie"

    doc = Markdown.parse(
        """
        A paragraph...
        - one
        - two
           * three
           * four
        ... another paragraph.
        """
    )
    @test typeof.(doc) == [Markdown.Paragraph, Markdown.List, Markdown.Paragraph]

    @test length(doc[2].items) === 2
    @test doc[2].items[1][1].content[1] == "one"
    @test length(doc[2].items[2]) == 2
    @test doc[2].items[2][1].content[1] == "two"

    @test isa(doc[2].items[2][2], Markdown.List)
    @test length(doc[2].items[2][2].items) === 2
    @test doc[2].items[2][2].items[1][1].content[1] == "three"
    @test doc[2].items[2][2].items[2][1].content[1] == "four"
end

@testset "Links" begin
    @test md"Foo [bar]" == MD(Paragraph("Foo [bar]"))
    @test md"Foo [bar](baz)" != MD(Paragraph("Foo [bar](baz)"))
    @test md"Foo \[bar](baz)" == MD(Paragraph("Foo [bar](baz)"))
end

@testset "Basic plain (markdown) output" begin
    @test md"foo" |> Markdown.plain == "foo\n"
    @test md"foo *bar* baz" |> Markdown.plain == "foo *bar* baz\n"
    @test md"# title" |> Markdown.plain == "# title\n"
    @test md"## section" |> Markdown.plain == "## section\n"
    @test md"## section `foo`" |> Markdown.plain == "## section `foo`\n"
    @test md"""Hello

    ---
    World""" |> Markdown.plain == "Hello\n\n---\n\nWorld\n"
    @test md"[*a*](b)" |> Markdown.plain == "[*a*](b)\n"
    @test md"""
    > foo
    >
    >   * bar
    >
    > ```
    > baz
    > ```""" |> Markdown.plain == """
    > foo
    >
    >   * bar
    >
    > ```
    > baz
    > ```

    """
end

@testset "Terminal (markdown) output" begin
    # multiple whitespace is ignored
    @test sprint(Markdown.term, md"a  b") == "  a b"
    @test sprint(Markdown.term, md"[x](https://julialang.org)") == "  x"
    @test sprint(Markdown.term, md"[x](@ref)") == "  x"
    @test sprint(Markdown.term, md"[x](@ref something)") == "  x"
    @test sprint(Markdown.term, md"![x](https://julialang.org)") == "  (Image: x)"

    # math (LaTeX)
    @test sprint(Markdown.term, md"""
    ```math
    A = Q R
    ```
    """) == "  A = Q R"

    # enumeration is normalized
    doc = Markdown.parse(
        """
        1. a
        3. b
        """
    )
    @test occursin("1. ", sprint(Markdown.term, doc))
    @test occursin("2. ", sprint(Markdown.term, doc))
    @test !occursin("3. ", sprint(Markdown.term, doc))

    # Testing margin when printing Tables to the terminal.
    @test sprint(Markdown.term, md"""
    | R |
    |---|
    | L |
    """) == "  R\n  –\n  L"

    @test sprint(Markdown.term, md"""
    !!! note "Tables in admonitions"

        | R |
        |---|
        | L |
    """) == "  │ Tables in admonitions\n  │\n  │  R\n  │  –\n  │  L"
end

@testset "Issue #38275" begin
    function test_list_wrap(str, lenmin, lenmax)
        strs = rstrip.(split(str, '\n'))
        l = length.(strs)
        for i = 1:length(l)-1
            if l[i] != 0 && l[i+1] != 0    # the next line isn't blank, so this line should be "full"
                lenmin <= l[i] <= lenmax || return false
            else
                l[i] <= lenmax || return false   # this line isn't too long (but there is no min)
            end
        end

        # Check consistent indentation
        # First, locate the list labels ends (position of bullet, or the "." at
        # the end of a numeric label
        labelends = findfirst.((r"[.•–▪] ",), strs)
        # sanity checks: label end locations must be either equal or separated by at least one char
        sorted_labels = unique(sort(filter(!isnothing, labelends)))
        for i in 1:length(sorted_labels)-1
            first(sorted_labels[i]) + 1 < first(sorted_labels[i+1]) || return false
        end

        # next check that after each label / bullet the following lines have the right indent
        k = first(labelends[1])+1
        rex = Regex('^' * " "^k * "\\w")
        for (i, le) in enumerate(labelends)
            if le === nothing
                # every unlabeled line is indented to text in labeled lines
                (isempty(strs[i]) || match(rex, strs[i]) !== nothing) || return false
            else
                # determine indent for following lines
                k = first(le)+1
                rex = Regex('^' * " "^k * "\\w")
            end
        end
        return true
    end

    doc = md"""
    1. a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij

       - a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij

         - a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij

           999. a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij

           1000. a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij

       - a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij

    2. a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij a bc def ghij
    """
    str = sprint(Markdown.term, doc, 50)
    @test test_list_wrap(str, 40, 50)
    str = sprint(Markdown.term, doc, 60)
    @test test_list_wrap(str, 50, 60)
    str = sprint(Markdown.term, doc, 80)
    @test test_list_wrap(str, 70, 80)
end

@testset "HTML output" begin
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
    @test md"<https://julialang.org>" |> html == """<p><a href="https://julialang.org">https://julialang.org</a></p>\n"""
    @test md"<mailto://a@example.com>" |> html == """<p><a href="mailto://a@example.com">mailto://a@example.com</a></p>\n"""
    @test md"<https://julialang.org/not a link>" |> html == "<p>&lt;https://julialang.org/not a link&gt;</p>\n"
    @test md"""<https://julialang.org/nota
            link>""" |> html == "<p>&lt;https://julialang.org/nota\nlink&gt;</p>\n"
    @test md"""Hello

    ---
    World""" |> html == "<p>Hello</p>\n<hr />\n<p>World</p>\n"
    @test md"`escape</code>`" |> html == "<p><code>escape&lt;/code&gt;</code></p>\n"

    @test md"""
        code1

        code2
    """ |> html == "<pre><code>code1\n\ncode2\n</code></pre>\n" # single code block

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
    == =""" |> html ==
    """
    <h1>h1</h1>
    <h2>h2</h2>
    <p>not
    == =</p>
    """
end

@testset "the 'book' example input" begin
    book = md"""
    # Title

    Some discussion

    > A quote

    ## Section *important*

    Some **bolded**

    - list1
    - list2
    """

    # Latex output
    @test latex(book) == "\\section{Title}\nSome discussion\n\n\\begin{quote}\nA quote\n\n\\end{quote}\n\\subsection{Section \\emph{important}}\nSome \\textbf{bolded}\n\n\\begin{itemize}\n\\item list1\n\n\n\\item list2\n\n\\end{itemize}\n"
    table = md"""
     a | b
    ---|---
     1 | 2
    """
    @test latex(table) ==
        "\\begin{tabular}\n{r | r}\na & b \\\\\n\\hline\n1 & 2 \\\\\n\\end{tabular}\n"

    # mime output
    @test sprint(show, "text/plain", book) ==
        """
          Title
          ≡≡≡≡≡

          Some discussion

          │  A quote

          Section important
          =================

          Some bolded

          • list1
          • list2
        """ |> chomp
    @test sprint(show, "text/plain", md"#") == "" # edge case of empty header
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

    out =
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
    @test sprint(show, "text/html", book) == out

    out =
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

    out =
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

@testset "rst rendering" begin
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
        @test Markdown.rst(input) == output
    end
end

@testset "Interpolation / Custom types" begin
    mutable struct Reference1
        ref
    end

    Base.show(io::IO, m::MIME"text/plain", r::Reference1) =
        print(io, "$(r.ref) (see Julia docs)")

    sum_ref = md"Behaves like $(Reference1(sum))"
    @test Markdown.plain(sum_ref) == "Behaves like sum (see Julia docs)\n"
    @test Markdown.html(sum_ref) == "<p>Behaves like sum (see Julia docs)</p>\n"

    @testset "JuliaLang/julia#59783 and #53362" begin
        x = 1
        result = md"""
        $x

        [^1]: $x

        !!! note
        $x

        > $x
        """
        expected = """
        1

        [^1]: 1

        !!! note



        1

        > 1

        """
        @test Markdown.plain(result) == expected
    end

    mutable struct Reference2
        ref
    end

    sum_ref = md"Behaves like $(Reference2(sum))"
    Base.show(io::IO, m::MIME"text/plain", r::Reference2) =
        print(io, "$(r.ref) (see Julia docs)")
    Base.show(io::IO, m::MIME"text/html", r::Reference2) =
        Markdown.withtag(io, :a, :href=>"test") do
            Markdown.htmlesc(io, sprint(Markdown.plaininline, r))
        end
    @test Markdown.html(sum_ref) == "<p>Behaves like <a href=\"test\">sum (see Julia docs)</a></p>\n"
end

@testset "GH tables" begin
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
    |   | b |
    |:--|--:|
    | 1 |   |""" == MD(Table(Any[[Any[],"b"],
                                 ["1",Any[]]], [:l, :r]))

    @test md"""
    no|table
    no error
    """ == MD([Paragraph(Any["no|table\nno error"])])

    t = """a   |   b
    :-- | --:
    1   |   2
    """
    @test Markdown.parse(t) == MD(Table(Any[Any["a", "b"], Any["1", "2"]], [:l, :r]))

    text =
    """
    | a   |   b |
    |:--- | ---:|
    | 1   |   2 |
    """
    table = Markdown.parse(text)
    @test text == Markdown.plain(table)

    text =
    """
    | Markdown | Table |  Test |
    |:-------- |:-----:| -----:|
    | foo      | `bar` | *baz* |
    | `bar`    |  baz  | *foo* |
    """
    table = Markdown.parse(text)
    @test text == Markdown.plain(table)
    @test Markdown.html(table) == """<table><tr><th align="left">Markdown</th><th align="center">Table</th><th align="right">Test</th></tr><tr><td align="left">foo</td><td align="center"><code>bar</code></td><td align="right"><em>baz</em></td></tr><tr><td align="left"><code>bar</code></td><td align="center">baz</td><td align="right"><em>foo</em></td></tr></table>\n"""

    text =
    """
    | a        |   b |
    |:-------- | ---:|
    | `x \\| y` |   2 |
    """
    table = Markdown.parse(text)
    @test text == Markdown.plain(table)
    @test Markdown.html(table) == """<table><tr><th align="left">a</th><th align="right">b</th></tr><tr><td align="left"><code>x | y</code></td><td align="right">2</td></tr></table>\n"""
end

@testset "LaTeX extension" begin
    in_dollars =
    """
    We have \$x^2 < x\$ whenever:

    \$|x| < 1\$

    etc.
    """
    in_backticks =
    """
    We have ``x^2 < x`` whenever:

    ```math
    |x| < 1
    ```

    etc.
    """
    out_plain =
    """
    We have \$x^2 < x\$ whenever:

    \$\$
    |x| < 1
    \$\$

    etc.
    """
    out_rst =
    """
    We have :math:`x^2 < x` whenever:

    .. math::

        |x| < 1

    etc.
    """
    out_latex =
    """
    We have \$x^2 < x\$ whenever:

    \$\$|x| < 1\$\$
    etc.

    """
    dollars   = Markdown.parse(in_dollars)
    backticks = Markdown.parse(in_backticks)
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

@testset "Nested backticks for inline code and math" begin
    t_1 = "`code` ``math`` ```code``` ````math```` `````code`````"
    t_2 = "`` `math` `` ``` `code` ``code`` ``` ```` `math` ``math`` ```math``` ````"
    t_3 = "`` ` `` ``` `` ` `` ` ` ```"
    t_4 = """`code
    over several
    lines` ``math
    over several
    lines`` ``math with
    ` some extra ` ` backticks`
    ``"""
    t_5 = "``code at end of string`"
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

@testset "Admonitions" begin
    t_1 =
        """
        # Foo

        !!! note

        !!! warning "custom title"

        ## Bar

        !!! danger ""

        !!!
        """
    t_2 =
        """
        !!! note
            foo bar baz

        \tsecond tab-indented paragraph

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
        """
    m_1 = Markdown.parse(t_1)
    m_2 = Markdown.parse(t_2)

    # Content Tests.

    @test isa(m_1[2], Markdown.Admonition)
    @test m_1[2].category == "note"
    @test m_1[2].title == "Note"
    @test m_1[2].content == []

    @test isa(m_1[3], Markdown.Admonition)
    @test m_1[3].category == "warning"
    @test m_1[3].title == "custom title"
    @test m_1[3].content == []

    @test isa(m_1[5], Markdown.Admonition)
    @test m_1[5].category == "danger"
    @test m_1[5].title == ""
    @test m_1[5].content == []

    @test isa(m_1[6], Markdown.Paragraph)

    @test isa(m_2[1], Markdown.Admonition)
    @test m_2[1].category == "note"
    @test m_2[1].title == "Note"
    @test isa(m_2[1].content[1], Markdown.Paragraph)
    @test isa(m_2[1].content[2], Markdown.Paragraph)

    @test isa(m_2[2], Markdown.Admonition)
    @test m_2[2].category == "warning"
    @test m_2[2].title == "custom title"
    @test isa(m_2[2].content[1], Markdown.List)
    @test isa(m_2[2].content[2], Markdown.Paragraph)

    @test isa(m_2[3], Markdown.Admonition)
    @test m_2[3].category == "danger"
    @test m_2[3].title == ""
    @test isa(m_2[3].content[1], Markdown.Code)
    @test isa(m_2[3].content[2], Markdown.Code)
    @test isa(m_2[3].content[3], Markdown.Header{1})

    # Rendering Tests.
    actual = Markdown.plain(m_1)
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
    @test actual == expected

    actual = Markdown.rst(m_1)
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
    @test actual == expected

    actual = Markdown.latex(m_1)
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
    @test actual == expected

    actual = Markdown.html(m_1)
    expected =
            """
            <h1>Foo</h1>
            <div class="admonition note"><p class="admonition-title">Note</p></div>
            <div class="admonition warning"><p class="admonition-title">custom title</p></div>
            <h2>Bar</h2>
            <div class="admonition danger"><p class="admonition-title"></p></div>
            <p>!!!</p>
            """
    @test actual == expected

    actual = Markdown.plain(m_2)
    expected =
            """
            !!! note
                foo bar baz

                second tab-indented paragraph


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
    @test actual == expected

    actual = Markdown.rst(m_2)
    expected =
            """
            .. note::
               foo bar baz

               second tab-indented paragraph


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
    @test actual == expected
end

@testset "Nested Lists" begin
    text =
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
        """
    md = Markdown.parse(text)

    # Content and structure tests.

    @test typeof.(md) == [Markdown.List, Markdown.List, Paragraph, Markdown.List, Markdown.List, Markdown.List]
    @test length(md[1].items) == 1
    @test typeof.(md[1].items[1]) == [Markdown.Paragraph, Markdown.Code, Markdown.BlockQuote]
    @test length(md[2].items) == 1
    @test isa(md[2].items[1][1], Markdown.Paragraph)
    @test isa(md[3], Markdown.Paragraph)
    @test length(md[4].items) == 2
    @test typeof.(md[4].items[1]) == [Paragraph, Paragraph]
    @test typeof.(md[4].items[2]) == [Paragraph]
    @test length(md[5].items) == 1
    @test typeof.(md[5].items[1]) == [Code]
    @test length(md[6].items) == 3
    @test md[6].items[1][1].content[1] == "foo"
    @test md[6].items[2][1].content[1] == "bar"
    @test md[6].items[3][1].content[1] == "baz"

    # Rendering tests.
    expected =
            """
            1. A paragraph
               with two lines.

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

    expected =
            """
            <ol>
            <li>
            <p>A paragraph
            with two lines.</p>
            <pre><code>indented code
            </code></pre>
            <blockquote>
            <p>A block quote.</p>
            </blockquote>
            </li>
            </ol>
            <ul>
            <li>
            <p>one</p>
            </li>
            </ul>
            <p>two</p>
            <ul>
            <li>
            <p>one</p>
            <p>two</p>
            </li>
            <li>
            <p>baz</p>
            </li>
            </ul>
            <ul>
            <li>
            <pre><code>foo
            </code></pre>
            </li>
            </ul>
            <ol>
            <li>foo</li>
            <li>bar</li>
            <li>baz</li>
            </ol>
            """
    @test expected == Markdown.html(md)

    expected =
            """
            1. A paragraph
               with two lines.

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

@testset "Ordered list starting number." begin
    text =
        """
        42. foo
        43. bar


        1. foo
        2. bar


        - foo
        - bar
        """
    md = Markdown.parse(text)

    @test typeof.(md) == [Markdown.List, Markdown.List]
    @test md[1].ordered == 42
    @test md[2].ordered == -1

    expected =
            """
            <ol start="42">
            <li>
            <p>foo</p>
            </li>
            <li>
            <p>bar</p>
            </li>
            <li>
            <p>foo</p>
            </li>
            <li>
            <p>bar</p>
            </li>
            </ol>
            <ul>
            <li>foo</li>
            <li>bar</li>
            </ul>
            """
    @test expected == Markdown.html(md)

    expected =
            raw"""
            \begin{itemize}
            \item[42. ] foo


            \item[43. ] bar


            \item[44. ] foo


            \item[45. ] bar

            \end{itemize}
            \begin{itemize}
            \item foo


            \item bar

            \end{itemize}
            """
    @test expected == Markdown.latex(md)
end

@testset "issue 20225, check this can print" begin
    @test typeof(sprint(Markdown.term, Markdown.parse(" "))) == String

    # different output depending on whether color is requested: +# issue 20225, check this can print
    buf = IOBuffer()
    @test typeof(sprint(Markdown.term, Markdown.parse(" "))) == String
    show(buf, "text/plain", md"*emph*")
    @test String(take!(buf)) == "  emph"
    show(buf, "text/markdown", md"*emph*")
    @test String(take!(buf)) == "*emph*\n"
    show(IOContext(buf, :color=>true), "text/plain", md"*emph*")
    @test String(take!(buf)) in ("  \e[3memph\e[23m", "  \e[4memph\e[24m")

    word = "Markdown" # disable underline when wrapping lines
    buf = IOBuffer()
    ctx = IOContext(buf, :color => true, :displaysize => (displaysize(buf)[1], length(word)))
    long_italic_text = Markdown.parse('_' * join(fill(word, 10), ' ') * '_')
    show(ctx, MIME("text/plain"), long_italic_text)
    lines = split(String(take!(buf)), '\n')
    @test endswith(lines[begin], r"\e\[2[34]m")
    @test startswith(lines[begin+1], Regex(' '^Markdown.margin * "\e\\[[34]m"))

    word = "Markdown" # pre is of size Markdown.margin when wrapping title
    buf = IOBuffer()
    ctx = IOContext(buf, :color => true, :displaysize => (displaysize(buf)[1], length(word)))
    long_title = Markdown.parse("# " * join(fill(word, 3)))
    show(ctx, MIME("text/plain"), long_title)
    lines = split(String(take!(buf)), '\n')
    @test all(l -> startswith(l, ' '^Markdown.margin * StyledStrings.ANSI_STYLE_CODES.bold_weight) ||
                   startswith(l, StyledStrings.ANSI_STYLE_CODES.bold_weight * ' '^Markdown.margin),
              lines)
end

@testset "issue 49454, check this can print" begin
    struct Struct49454 end
    Base.show(io::IO, ::Struct49454) =
        print(io, Base.text_colors[:underline], "Struct 49454()", Base.text_colors[:normal])

    buf = IOBuffer()
    ctx = IOContext(buf, :color => true, :displaysize => (displaysize(buf)[1], 10))
    show(ctx, MIME("text/plain"), md"""
    text without $(Struct49454()) underline.
    """)
    lines = split(String(take!(buf)), '\n')
    @test !occursin(Base.text_colors[:underline], lines[end])
end

@testset "table rendering with term #25213" begin
    t = """
        a   |   b
        :-- | --:
        1   |   2"""
    @test sprint(Markdown.term, Markdown.parse(t), 0) == "  a b\n  – –\n  1 2"
end

@testset "test Base.copy" begin
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

@testset "issue #26598: loose lists" begin
    md = Markdown.parse(
            """
            foo

            - 1
            - 2

            - 3


            - 1
            - 2

            bar

            - 1

              2
            - 4

            buz

            - 1
            - 2
              3
            - 4
            """)
    @test typeof.(md) == [Markdown.Paragraph, Markdown.List,
                          Markdown.Paragraph, Markdown.List,
                          Markdown.Paragraph, Markdown.List]
    @test md[2].loose
    @test md[4].loose
    @test !md[6].loose
end

@testset "issue #29995" begin
    m = Markdown.parse("---")
    io = IOBuffer()
    show(io, "text/latex", m)
    @test String(take!(io)) == "\\rule{\\textwidth}{1pt}\n"
end

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
    @test contains_X(md"~$x~") # Strikethrough
    @test contains_X( # Table
        md"""
        | name |
        |------|
        |  $x  |
        """)
end

@testset "issue #40080: empty list item breaks display()" begin
    d = TextDisplay(devnull)
    display(d, md"""
               1. hello
               2.
               """)
end

@testset "issue #37232: linebreaks" begin
    s = @md_str """
       Misc:\\
       - line\\
         break
       """
    @test sprint(show, MIME("text/plain"), s) == "  Misc:\n  - line\n   break"
end

@testset "linebreaks in lists" begin
    # similar to the preceding test, but unlike that, actually uses a Markdown list
    # (the prior example might look like it uses a list, but it doesn't)
    s = @md_str """
       Misc:\\
       stuff
       - line\\
         break
       """
    @test sprint(show, MIME("text/plain"), s) ==
            raw"""
              Misc:
              stuff

              • line
                break
            """ |> chomp
    @test Markdown.plain(s) ==
            raw"""
            Misc:\
            stuff

              * line\
                break
            """
    @test Markdown.html(s) ==
            raw"""
            <p>Misc:<br />
            stuff</p>
            <ul>
            <li>line<br />
            break</li>
            </ul>
            """
    @test Markdown.latex(s) ==
            raw"""
            Misc:\\
            stuff

            \begin{itemize}
            \item line\\
            break

            \end{itemize}
            """
end

@testset "pullrequest #57664: en_or_em_dash" begin
    # Test that two hyphens (--) is parsed as en dash (–)
    # and three hyphens (---) is parsed as em dash (—)
    hyphen_text = md"foo - bar"
    en_dash_text = md"foo -- bar"
    em_dash_text = md"foo --- bar"

    @test sprint(show, "text/markdown", hyphen_text) == "foo - bar\n"
    @test sprint(show, "text/markdown", en_dash_text) == "foo – bar\n"
    @test sprint(show, "text/markdown", em_dash_text) == "foo — bar\n"

    # Test that parsing works for hyphen-minus (-), en dash (–) and em dash (—)
    hyphen_text = md"foo - bar"
    en_dash_text = md"foo – bar"
    em_dash_text = md"foo — bar"

    @test hyphen_text |> Markdown.plain == "foo - bar\n"
    @test en_dash_text |> Markdown.plain == "foo – bar\n"
    @test em_dash_text |> Markdown.plain == "foo — bar\n"
end

@testset "pullrequest #41552: a code block has \\end{verbatim}" begin
    s1 = md"""
         ```tex
         \begin{document}
         \end{document}
         ```
         """
    s2 = md"""
         ```tex
         \begin{verbatim}
         \end{verbatim}
         ```
         """
    @test Markdown.latex(s1) == """
                                \\begin{verbatim}
                                \\begin{document}
                                \\end{document}
                                \\end{verbatim}
                                """
    @test_throws ErrorException Markdown.latex(s2)
end

@testset "issue #42139: autolink" begin
    # ok
    @test md"<mailto:foo@bar.com>" |> html == """<p><a href="mailto:foo@bar.com">mailto:foo@bar.com</a></p>\n"""
    # not ok
    @test md"<mailto foo@bar.com>" |> html == """<p>&lt;mailto foo@bar.com&gt;</p>\n"""
    # see issue #42139
    @test md"<一轮红日初升>" |> html == """<p>&lt;一轮红日初升&gt;</p>\n"""
end

@testset "Docstrings" begin
    @test isempty(Docs.undocumented_names(Markdown))
end

@testset "Non-Markdown" begin
    # https://github.com/JuliaLang/julia/issues/37765
    @test isa(Markdown.insert_hlines(Text("foo")), Text)
    # https://github.com/JuliaLang/julia/issues/37757
    @test Markdown.insert_hlines(nothing) === nothing
end

@testset "issue #59967: indented code blocks with more than one blank line" begin
    # Test the broken case in issue: indented code block with multiple blank lines
    md = Markdown.parse("""
    - code block inside a list with more than one blank line with indentation works
      ```julia
      domaths(x::Number) = x + 5


      domath(x::Int) = x + 10
      ```
    - another entry, now testing code blocks without fences

          # this is a code block
          x = 1 + 1


          # Two empty lines don't interrupt the code
          y = x * 3

    - a final list entry

    And now to something completely different!
    """)
    expected =
    """
    <ul>
    <li>
    <p>code block inside a list with more than one blank line with indentation works</p>
    <pre><code class="language-julia">domaths(x::Number) = x + 5


    domath(x::Int) = x + 10
    </code></pre>
    </li>
    <li>
    <p>another entry, now testing code blocks without fences</p>
    <pre><code># this is a code block
    x = 1 + 1


    # Two empty lines don't interrupt the code
    y = x * 3
    </code></pre>
    </li>
    <li>
    <p>a final list entry</p>
    </li>
    </ul>
    <p>And now to something completely different!</p>
    """

    @test expected == Markdown.html(md)
end

@testset "Lazy Strings" begin
    @test Markdown.parse(lazy"foo") == Markdown.parse("foo")
end

@testset "rendering of nested lists, with hard breaks" begin
    #
    # Test an unordered list.
    #
    m = md"""
    An unordered list:
    - top level\
      with an extra line
      - second level\
        again with an extra line
        - third level\
          yet again with an extra line
          - fourth level\
            and another extra line
            - fifth level\
              final extra line
    - back to top level
    """

    # test HTML rendering
    expected = """
      An unordered list:

      • top level
        with an extra line
        – second level
          again with an extra line
          ▪ third level
            yet again with an extra line
            – fourth level
              and another extra line
              ▪ fifth level
                final extra line
      • back to top level
    """ |> chomp

    actual = sprint(show, MIME("text/plain"), m)
    @test expected == actual

    # test Markdown rendering
    # FIXME: actually the hard breaks are *not* correctly round tripped,
    # but at least the indentation is correct now
    expected = raw"""
    An unordered list:

      * top level\
        with an extra line
          * second level\
            again with an extra line
              * third level\
                yet again with an extra line
                  * fourth level\
                    and another extra line
                      * fifth level\
                        final extra line
      * back to top level
    """

    actual = Markdown.plain(m)
    @test expected == actual

    #
    # Test an ordered list. These behave differently if the number of list
    # entries increases to another power of ten. For example, when going from
    # 9 to 10 list entries. We test this here.
    #
    m = md"""
    An ordered list:
    1. top level\
       with an extra line
       1. second level\
          again with an extra line
           999. third level\
                yet again with an extra line
                1. fourth level\
                   and another extra line
                   1. fifth level\
                      final extra line
          1000. more third level\
                with an extra line
    1. back to top level
    """

    # test HTML rendering
    expected = """
      An ordered list:

      1. top level
         with an extra line
         1. second level
            again with an extra line
             999. third level
                  yet again with an extra line
                  1. fourth level
                     and another extra line
                     1. fifth level
                        final extra line
            1000. more third level
                  with an extra line
      2. back to top level
    """ |> chomp

    actual = sprint(show, MIME("text/plain"), m)
    @test expected == actual

    # test Markdown rendering
    # FIXME: actually the hard breaks are *not* correctly round tripped,
    # but at least the indentation is correct now
    expected = raw"""
    An ordered list:

    1. top level\
       with an extra line
       1. second level\
          again with an extra line
          999. third level\
               yet again with an extra line
               1. fourth level\
                  and another extra line
                  1. fifth level\
                     final extra line
          1000. more third level\
                with an extra line
    2. back to top level
    """

    actual = Markdown.plain(m)
    @test expected == actual
end

@testset "indexing and iterator interface" begin
    md = md"""
    # Headline

    Some text

    - item 1
    - item 2

    ***

    The end.
    """

    hr = Markdown.HorizontalRule()

    @test !isempty(md)
    @test length(md) == 5
    @test firstindex(md) == 1
    @test lastindex(md) == 5
    @test md[4] isa Markdown.HorizontalRule  # getindex!
    md[4] = hr  # setindex!
    @test md[4] === hr
    # broadcast via iteration
    @test typeof.(md) == [Markdown.Header{1}, Markdown.Paragraph, Markdown.List, Markdown.HorizontalRule, Markdown.Paragraph]
    @test Base.IteratorSize(md) == Base.HasLength()
    @test eltype(md) == Any

    push!(md, hr)
    @test !isempty(md)
    @test length(md) == 6
    @test firstindex(md) == 1
    @test lastindex(md) == 6
    @test md[6] === hr
    @test typeof.(md) == [Markdown.Header{1}, Markdown.Paragraph, Markdown.List, Markdown.HorizontalRule, Markdown.Paragraph, Markdown.HorizontalRule]
end

@testset "issue #46991: Preserve non-breaking space" begin
    # reference: normal spaces
    input = "abc\\\n    | def"
    # and now with non-breaking space
    # Julia's CI won't let us use non-breaking spaces in here directly,
    # so we insert them manually
    nbsp = "\u00a0"^4
    input_nbsp = "abc\\\n$nbsp| def"

    md = Markdown.parse(input)
    md_nbsp = Markdown.parse(input_nbsp)

    str = sprint(Markdown.term, md)
    str_nbsp = sprint(Markdown.term, md_nbsp)

    # regular version: four leading spaces got into a single one
    @test str == "  abc\n   | def"
    # non-breaking version: four leading spaces got preserved
    @test str_nbsp == "  abc\n  $nbsp| def"
end

include("test_spec_roundtrip_common.jl")
include("test_spec_roundtrip_github.jl")
include("test_spec_roundtrip_julia.jl")

include("test_spec_html_common.jl")
include("test_spec_html_github.jl")
include("test_spec_html_julia.jl")

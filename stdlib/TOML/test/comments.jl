# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using TOML

function parse_with_comments(str)
    comments = TOML.Comments()
    data = TOML.parse(str; comments)
    return data, comments
end

function sprint_with_comments(data, comments; kws...)
    io = IOBuffer()
    TOML.print(io, data; comments, kws...)
    return String(take!(io))
end

@testset "attached, inline and floating comments" begin
    str = """
    # floating root comment

    name = "MyPkg"
    # attached to deps
    [deps]
    Dates = "ade2ca70"
    # `Dep` is used for fooing bars
    Dep = "0796e94c"

    [compat]
    julia = "1"
    Dep = "~1.1" # see issue 123 before bumping
    """
    data, comments = parse_with_comments(str)
    @test data["name"] == "MyPkg"
    @test comments.floating[()] == [" floating root comment"]
    @test comments.items[("deps",)].above == [" attached to deps"]
    @test comments.items[("deps", "Dep")].above == [" `Dep` is used for fooing bars"]
    @test comments.items[("deps", "Dep")].inline === nothing
    @test comments.items[("compat", "Dep")].inline == " see issue 123 before bumping"
    @test !haskey(comments.items, ("compat", "julia"))
end

@testset "print and reparse is a fixed point" begin
    str = """
    # preamble comment

    name = "MyPkg"
    # attached to deps
    [deps] # inline on header
    # dep comment
    Dep = "0796e94c"
    [compat]
    Dep = "~1.1" # inline compat
    """
    data, comments = parse_with_comments(str)
    for sorted in (false, true)
        out = sprint_with_comments(data, comments; sorted)
        data2, comments2 = parse_with_comments(out)
        @test data2 == data
        @test comments2 == comments
        @test sprint_with_comments(data2, comments2; sorted) == out
    end
end

@testset "blank line separates attached from floating" begin
    # From the discussion in JuliaLang/julia#42697: `1` and `2` are separated
    # from `bar` by blank lines and float to the top of the (root) table,
    # `3` is directly above `bar` and attaches to it.
    str = """
    foo = 123
    # 1

    # 2

    # 3
    bar = 456
    """
    data, comments = parse_with_comments(str)
    @test comments.floating[()] == [" 1", " 2"]
    @test comments.items[("bar",)].above == [" 3"]
    @test !haskey(comments.items, ("foo",))
end

@testset "comments in values attach to the owning entry" begin
    str = """
    # above a
    a = [1, # one
         # two
         2]
    b = { x = 1 } # inline b
    """
    data, comments = parse_with_comments(str)
    @test comments.items[("a",)].above == [" above a", " one", " two"]
    @test comments.items[("b",)].inline == " inline b"
    @test !haskey(comments.items, ("b", "x"))
end

@testset "dotted and quoted keys" begin
    str = """
    # above dotted
    a.b.c = 1
    ["weird key"]
    # above weird entry
    "x.y" = 2 # inline weird
    """
    data, comments = parse_with_comments(str)
    @test comments.items[("a", "b", "c")].above == [" above dotted"]
    @test comments.items[("weird key", "x.y")].above == [" above weird entry"]
    @test comments.items[("weird key", "x.y")].inline == " inline weird"
    out = sprint_with_comments(data, comments)
    data2, comments2 = parse_with_comments(out)
    @test data2 == data
    @test comments2 == comments
end

@testset "array of tables" begin
    # Comments cannot be attached to items inside array-of-tables elements
    # (key paths do not distinguish between elements): only the comment block
    # above the first `[[...]]` header is kept, attached to the array itself.
    str = """
    # above first
    [[x]]
    a = 1 # dropped
    # dropped too
    [[x]]
    b = 2

    [y]
    # in y
    z = 1
    """
    data, comments = parse_with_comments(str)
    @test comments.items[("x",)].above == [" above first"]
    @test comments.items[("y", "z")].above == [" in y"]
    @test length(comments.items) == 2
    @test isempty(comments.floating)
    out = sprint_with_comments(data, comments)
    data2, comments2 = parse_with_comments(out)
    @test data2 == data
    @test comments2 == comments
end

@testset "floating comments in tables and at EOF" begin
    str = """
    [a]
    x = 1

    # floats in a
    """
    data, comments = parse_with_comments(str)
    @test comments.floating[("a",)] == [" floats in a"]
    out = sprint_with_comments(data, comments)
    data2, comments2 = parse_with_comments(out)
    @test comments2.floating[("a",)] == [" floats in a"]

    # a comment block at EOF without a blank line before it also floats
    data, comments = parse_with_comments("x = 1\n# tail")
    @test comments.floating[()] == [" tail"]

    # only-comments and empty documents
    data, comments = parse_with_comments("# just a comment\n")
    @test isempty(data)
    @test comments.floating[()] == [" just a comment"]
    data, comments = parse_with_comments("")
    @test isempty(data)
    @test isempty(comments)
end

@testset "comment without preceding whitespace" begin
    data, comments = parse_with_comments("a = 1# c\n[b]# d\nx = 2\n")
    @test comments.items[("a",)].inline == " c"
    @test comments.items[("b",)].inline == " d"
end

@testset "crlf line endings" begin
    str = "# above\r\na = 1\r\n\r\n# floating\r\n\r\nb = 2\r\n"
    data, comments = parse_with_comments(str)
    @test comments.items[("a",)].above == [" above"]
    @test comments.floating[()] == [" floating"]
    @test !haskey(comments.items, ("b",))
end

@testset "comments removed with their item" begin
    str = """
    # above a
    a = 1
    # above b
    b = 2
    """
    data, comments = parse_with_comments(str)
    delete!(data, "a")
    out = sprint_with_comments(data, comments)
    @test !occursin("above a", out)
    @test occursin("above b", out)
end

@testset "programmatic comments are sanitized" begin
    data = Dict{String, Any}("a" => 1)
    comments = TOML.Comments()
    comments.items[("a",)] = TOML.Internals.CommentBlock(["evil\nb = 2"], "inline\nevil = 3")
    out = sprint_with_comments(data, comments)
    data2, comments2 = parse_with_comments(out)
    @test data2 == data
    @test comments2.items[("a",)].above == [" evil", " b = 2"]
end

@testset "comments in nested and empty tables" begin
    str = """
    [a]
    # above nested header
    [a.b] # inline nested header
    x = 1
    # above empty table
    [c]
    """
    data, comments = parse_with_comments(str)
    @test comments.items[("a", "b")].above == [" above nested header"]
    @test comments.items[("a", "b")].inline == " inline nested header"
    @test comments.items[("c",)].above == [" above empty table"]
    out = sprint_with_comments(data, comments)
    data2, comments2 = parse_with_comments(out)
    @test data2 == data
    @test comments2 == comments
end

@testset "comments force elided table headers" begin
    # `[a]` would normally be elided since `a` only contains the table `b`;
    # a comment associated with `a` must keep its anchor
    str = """
    # keep me
    [a]
    [a.b]
    x = 1
    """
    data, comments = parse_with_comments(str)
    @test comments.items[("a",)].above == [" keep me"]
    out = sprint_with_comments(data, comments)
    @test occursin("[a]", out)
    data2, comments2 = parse_with_comments(out)
    @test data2 == data
    @test comments2 == comments
    @test sprint_with_comments(data2, comments2) == out

    # same for a floating comment inside the elided table
    str = """
    [a]

    # floating in a

    [a.b]
    x = 1
    """
    data, comments = parse_with_comments(str)
    @test comments.floating[("a",)] == [" floating in a"]
    out = sprint_with_comments(data, comments)
    data2, comments2 = parse_with_comments(out)
    @test data2 == data
    @test comments2 == comments
    @test sprint_with_comments(data2, comments2) == out

    # without comments the header is still elided
    data, comments = parse_with_comments("[a.b]\nx = 1\n")
    @test !occursin(r"^\[a\]"m, sprint_with_comments(data, comments))
end

@testset "empty inline comments are preserved" begin
    data, comments = parse_with_comments("x = 1 #\ny = 2\n")
    @test comments.items[("x",)].inline == ""
    @test !haskey(comments.items, ("y",))
    out = sprint_with_comments(data, comments)
    @test occursin("x = 1 #\n", out)
    @test occursin("y = 2\n", out)
    data2, comments2 = parse_with_comments(out)
    @test data2 == data
    @test comments2 == comments
    @test sprint_with_comments(data2, comments2) == out
end

@testset "comments in tables printed inline are hoisted" begin
    # A table printed via `inline_tables` has no lines for its entries'
    # comments; they are hoisted above the entry that owns the inline table,
    # which is where a reparse of the output associates them
    str = """
    [deps]
    Example = "7876af07-990d-54b4-ab0e-23690620f79a"

    # above the sources entry
    [sources.Example]
    # local checkout
    path = "../Example" # trailing
    """
    data, comments = parse_with_comments(str)
    inline_tables = Base.IdSet{Dict{String, Any}}()
    push!(inline_tables, data["sources"]["Example"])
    out = sprint_with_comments(data, comments; inline_tables, sorted = true)
    @test occursin("# above the sources entry", out)
    @test occursin("# local checkout", out)
    @test occursin("# trailing", out)
    @test occursin("Example = {path = \"../Example\"}", out)
    data2, comments2 = parse_with_comments(out)
    @test data2 == data
    @test comments2.items[("sources", "Example")].above ==
        [" above the sources entry", " local checkout", " trailing"]
    @test !haskey(comments2.items, ("sources", "Example", "path"))
    # stable from the second print onwards
    inline_tables2 = Base.IdSet{Dict{String, Any}}()
    push!(inline_tables2, data2["sources"]["Example"])
    out2 = sprint_with_comments(data2, comments2; inline_tables = inline_tables2, sorted = true)
    @test out2 == out
end

@testset "Comments is emptied on reuse" begin
    comments = TOML.Comments()
    TOML.parse("# hi\na = 1"; comments)
    @test haskey(comments.items, ("a",))
    TOML.parse("b = 2"; comments)
    @test !haskey(comments.items, ("a",))
    @test isempty(comments)

    # reusing a Parser without comments does not touch a previous capture
    p = TOML.Parser()
    comments = TOML.Comments()
    TOML.parse(p, "# hi\na = 1"; comments)
    @test haskey(comments.items, ("a",))
    @test TOML.parse(p, "b = 2") == Dict("b" => 2)
    @test haskey(comments.items, ("a",))
end

@testset "parsefile/tryparsefile/tryparse with comments" begin
    mktemp() do path, io
        write(io, "# above\na = 1 # inline\n")
        close(io)
        for parsef in (TOML.parsefile, TOML.tryparsefile)
            comments = TOML.Comments()
            data = parsef(path; comments)
            @test data == Dict("a" => 1)
            @test comments.items[("a",)].above == [" above"]
            @test comments.items[("a",)].inline == " inline"
            comments = TOML.Comments()
            data = parsef(TOML.Parser(), path; comments)
            @test data == Dict("a" => 1)
            @test comments.items[("a",)].inline == " inline"
        end
        comments = TOML.Comments()
        @test TOML.tryparse("# c\na = 1"; comments) == Dict("a" => 1)
        @test comments.items[("a",)].above == [" c"]
        comments = TOML.Comments()
        @test open(io -> TOML.parse(io; comments), path) == Dict("a" => 1)
        @test comments.items[("a",)].inline == " inline"
    end
end

@testset "comments do not affect sorting or inline tables" begin
    str = """
    # above b
    b = 1
    a = { x = 1 }
    """
    data, comments = parse_with_comments(str)
    inline_tables = Base.IdSet{Dict{String, Any}}()
    push!(inline_tables, data["a"])
    out = sprint_with_comments(data, comments; sorted=true, inline_tables)
    @test occursin("a = {x = 1}", out) || occursin("a = { x = 1 }", out)
    lines = split(out, '\n')
    @test lines[findfirst(==("b = 1"), lines) - 1] == "# above b"
end

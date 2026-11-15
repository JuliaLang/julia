# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using REPL
using Markdown
using DocBrowser: DocBrowser, KeyEvent, MouseEvent, PasteEvent, read_event, Catalog,
    module_entries, search, resolve_ref, NameTarget, ModuleTarget, entry_page, module_page,
    Browser, frame, handle!, browse

@test isempty(Test.detect_closure_boxes(DocBrowser))

include("FakeTerminals.jl")
import .FakeTerminals.FakeTerminal

"""
Fixture

A module for testing the browser. See [`Fixture.documented`](@ref) and `helper`.
"""
module Fixture
export documented, Point, @mac, Inner
public undocumented_public, documented_public

"""
    documented(x)

Calls [`documented_public`](@ref). See also `helper` and [`Base.sum`](https://docs.julialang.org).
"""
documented(x) = x

"Public but not exported."
documented_public(x) = x
undocumented_public(x) = x

"Internal, but documented."
helper() = nothing
private_undocumented() = nothing

"A point."
struct Point
    x::Int
end

"A macro."
macro mac() end

"An inner module."
module Inner
    export inner_fn
    "Inside."
    inner_fn() = 1
end
end

module UndocumentedExt end

strip_ansi(s) = replace(s, r"\e\[[0-9;?]*[a-zA-Z]|\e\]8;[^\e]*\e\\\\" => "")

@testset "input decoding" begin
    ev(s) = read_event(IOBuffer(s))
    @test ev("q") == KeyEvent('q')
    @test ev("\r") == KeyEvent(:enter)
    @test ev("\x7f") == KeyEvent(:backspace)
    @test ev("\x03") == KeyEvent('c', DocBrowser.CTRL)
    @test ev("\e") == KeyEvent(:escape)
    @test ev("\e[A") == KeyEvent(:up)
    @test ev("\eOB") == KeyEvent(:down)
    @test ev("\e[1;3D") == KeyEvent(:left, DocBrowser.ALT)
    @test ev("\e[5~") == KeyEvent(:pageup)
    @test ev("\e[Z") == KeyEvent(:backtab, DocBrowser.SHIFT)
    @test ev("\eb") == KeyEvent('b', DocBrowser.ALT)
    @test ev("\e[<0;12;7M") == MouseEvent(:press, 1, 7, 12, 0)
    @test ev("\e[<0;12;7m") == MouseEvent(:release, 1, 7, 12, 0)
    @test ev("\e[<35;3;4M") == MouseEvent(:move, 4, 4, 3, 0)
    @test ev("\e[<65;3;4M") == MouseEvent(:wheeldown, 0, 4, 3, 0)
    @test ev("\e[<128;3;4M") == MouseEvent(:press, 8, 4, 3, 0)
    @test ev("\e[200~some text\e[201~") == PasteEvent("some text")
    # Replies to terminal queries are skipped.
    @test ev("\e]11;rgb:0000/0000/0000\e\\") === nothing
    @test ev("\e[?62;22c") === nothing
    # Alt with a key that can start a sequence, and an unterminated string, swallow nothing.
    @test ev("\e]") == KeyEvent(']', DocBrowser.ALT)
    @test ev("\eO") == KeyEvent('O', DocBrowser.ALT)
    io = IOBuffer("\e]11;rgb")
    @test read_event(io) === nothing && eof(io)
    # A sequence split across two reads, as over a slow connection, is still one key. The rest
    # is written once the reader waits for it; a fixed delay was flaky on Windows.
    p = Pipe(); Base.link_pipe!(p)
    write(p, "\e")
    Base.wait_readnb(p.out, 1)
    t = @async write(p, "[A")
    @test read_event(p.out) == KeyEvent(:up)
    wait(t); close(p)
    # After a click closes the browser, its release is read rather than left for the prompt.
    io = IOBuffer("\e[<35;3;4M\e[<0;79;26mabc")
    DocBrowser.await_release(io)
    @test read(io, String) == "abc"
end

@testset "catalog" begin
    cat = Catalog()
    @test any(p -> p.mod === Base && p.role === :julia, cat.pkgs)
    @test any(p -> p.mod === REPL, cat.pkgs)
    entries = Dict(e.name => e for e in module_entries(cat, Fixture))
    @test entries[:documented].vis === :exported
    @test entries[:documented].kind === :function
    @test entries[:documented].documented
    @test entries[:documented_public].vis === :public
    @test !entries[:undocumented_public].documented
    @test entries[:helper].vis === :internal
    @test entries[:Point].kind === :type
    @test entries[Symbol("@mac")].kind === :macro
    @test entries[:Inner].kind === :module
    @test !haskey(entries, :eval) && !haskey(entries, :include)
    # Results are grouped by kind: modules, types, functions, macros.
    kinds = [e.kind for e in module_entries(cat, Fixture)]
    @test issorted(kinds; by = k -> findfirst(==(k), DocBrowser.KINDS))

    @test resolve_ref("documented_public", Fixture, cat) == NameTarget(Fixture, :documented_public)
    @test resolve_ref("helper()", Fixture, cat) == NameTarget(Fixture, :helper)
    @test resolve_ref("Base.sum", Fixture, cat) == NameTarget(Base, :sum)
    @test resolve_ref("Fixture.Inner", @__MODULE__, cat) == ModuleTarget(Fixture.Inner)
    @test resolve_ref("private_undocumented", Fixture, cat) === nothing
    @test resolve_ref("x", Fixture, cat) === nothing
    @test resolve_ref("1 + 2", Fixture, cat) === nothing

    hits = search(cat, "sum")
    @test !isempty(hits)
    @test first(hits).name === :sum && first(hits).mod === Base
    @test all(e -> e.vis !== :internal, hits)
    @test any(e -> e.vis === :internal, search(cat, "sum"; internal=true))
    # A dotted query searches inside matching modules only.
    @test all(e -> startswith(join(fullname(e.mod), '.'), "Base.Iterators"), search(cat, "Iterators.fla"))
end

@testset "pages" begin
    cat = Catalog()
    e = only(filter(e -> e.name === :documented, module_entries(cat, Fixture)))
    p = entry_page(cat, e, 70)
    text = join(map(String, p.lines), '\n')
    @test occursin("Calls documented_public", text)
    targets = [s.target for s in p.spots]
    @test NameTarget(Fixture, :documented_public) in targets
    @test NameTarget(Fixture, :helper) in targets
    @test DocBrowser.URLTarget("https://docs.julialang.org") in targets
    @test p.source !== nothing && endswith(p.source.file, "runtests.jl")
    @test all(l -> textwidth(String(l)) <= 70 || occursin("julia", String(l)), p.lines)

    p = module_page(cat, Fixture, 70)
    text = join(map(String, p.lines), '\n')
    @test occursin("A module for testing the browser.", text)
    @test occursin("Submodules", text)
    @test ModuleTarget(Fixture.Inner) in [s.target for s in p.spots]
end

function fake_terminal(input::String; rows=30, cols=110)
    out = IOContext(IOBuffer(), :displaysize => (rows, cols), :color => true)
    return FakeTerminal(IOBuffer(input), out, IOBuffer())
end

@testset "drawing" begin
    for (rows, cols) in ((30, 120), (24, 80), (20, 50), (4, 10))
        b = Browser(fake_terminal(""; rows, cols))
        lines = frame(b)
        @test length(lines) == rows
        @test all(l -> textwidth(strip_ansi(l)) == cols, lines)
    end
    b = Browser(fake_terminal(""))
    lines = strip_ansi.(frame(b))
    @test occursin("Julia docs", lines[1])
    # The active project is named in the header, when there is one.
    label = DocBrowser.project_label()
    @test label === nothing || occursin("($label)", lines[1])
    @test occursin("Packages", lines[2])
    @test any(l -> occursin("Base", l), lines)
end

@testset "navigation" begin
    b = Browser(fake_terminal(""))
    frame(b)
    DocBrowser.follow!(b, NameTarget(Fixture, :documented))
    @test b.mod === Fixture
    @test DocBrowser.current_key(b).name === :documented
    @test b.focus === :doc
    frame(b)
    # Step through the links and follow the first one.
    handle!(b, KeyEvent(:tab))
    @test b.link == 1
    handle!(b, KeyEvent(:enter))
    @test DocBrowser.current_key(b).name === :documented_public
    handle!(b, KeyEvent(:backspace))
    @test DocBrowser.current_key(b).name === :documented
    handle!(b, KeyEvent(']'))
    @test DocBrowser.current_key(b).name === :documented_public

    # Internal names are hidden until asked for.
    DocBrowser.show_module!(b, Fixture)
    names_shown() = [r.entry.name for r in b.name_rows if r.entry !== nothing]
    @test !(:helper in names_shown())
    handle!(b, KeyEvent('i'))
    @test :helper in names_shown()

    # Entering a submodule from the names list, and stepping back out with ←.
    b.focus = :names
    b.names.cursor = findfirst(r -> r.entry !== nothing && r.entry.name === :Inner, b.name_rows)
    handle!(b, KeyEvent(:enter))
    @test b.mod === Fixture.Inner
    handle!(b, KeyEvent(:left))
    @test b.mod === Fixture

    # Search, then open the top result.
    handle!(b, KeyEvent('/'))
    for c in "documented_pub"
        handle!(b, KeyEvent(c))
    end
    @test b.typing
    @test first(b.results).name === :documented_public
    handle!(b, KeyEvent(:enter))
    @test !b.typing && b.query === nothing
    @test DocBrowser.current_key(b).name === :documented_public

    # Clicking the title of the names pane selects the module itself.
    lines = frame(b)
    pane = only(filter(p -> p[1] === :names, DocBrowser.layout(b)))
    row = findfirst(l -> occursin(r"\d+ names", strip_ansi(l)), lines)
    handle!(b, MouseEvent(:press, 1, row, pane[2] + 3, 0))
    @test b.focus === :names && b.names.cursor == 1
end

@testset "browse" begin
    # `y` closes the browser and returns the name that was shown.
    @test browse(fake_terminal("y"); query="sum") == "sum"
    @test browse(fake_terminal("y"); query="Base.Iterators") == "Iterators"
    # An unknown name starts a search; Enter opens the first result.
    @test browse(fake_terminal("\ry"); query="iterators.flatte") == "Base.Iterators.flatten"
    # `q` closes without picking anything, as does running out of input.
    @test browse(fake_terminal("jjlq")) === nothing
    @test browse(fake_terminal("\e[<0;10;5M\e[<0;10;5m/abc\e")) === nothing
    # The terminal is put back the way it was found.
    t = fake_terminal("q")
    browse(t)
    out = String(take!(t.out_stream.io))
    @test occursin("\e[?1049h", out) && endswith(out, DocBrowser.LEAVE_SCREEN)
end

@testset "loading a project dependency" begin
    # A project that depends on a standard library which is not loaded yet.
    candidates = ["SharedArrays", "DelimitedFiles", "CRC32c", "Mmap"]
    i = findfirst(n -> !any(m -> nameof(m) === Symbol(n), Base.loaded_modules_array()), candidates)
    if i === nothing
        @test_broken false
    else
        name = candidates[i]
        uuid = Base.identify_package(name).uuid
        mktempdir() do dir
            write(joinpath(dir, "Project.toml"), "[deps]\n$name = \"$uuid\"\n")
            old = Base.active_project()
            Base.set_active_project(joinpath(dir, "Project.toml"))
            try
                b = Browser(fake_terminal(""))
                id = Base.PkgId(uuid, name)
                @test id in b.cat.unloaded
                row = findfirst(r -> r.unloaded == id, b.side_rows)
                @test row !== nothing
                b.side.cursor = row - 1
                DocBrowser.move!(b, :side, 1)
                @test DocBrowser.current_key(b) == DocBrowser.LoadTarget(id)
                # The names of the module shown before are not listed under it.
                @test occursin("not loaded", String(DocBrowser.pane_title(b, :names, 1, 40)))
                listed = String.(DocBrowser.draw_list(b, :names, 1, 40))
                @test occursin("load it", listed[1]) && all(isempty ∘ strip, listed[2:end])
                handle!(b, KeyEvent(:enter))
                @test Base.root_module_exists(id)
                # The import is shown as if typed at a coloured prompt, followed by a blank line.
                shown = String(take!(b.out.io))
                @test occursin(Regex("\\e\\[[0-9;]*mjulia> (\\e\\[[0-9;]*m)+import $name\\n\\n"), shown)
                @test isempty(b.cat.unloaded)
                @test DocBrowser.current_key(b) == ModuleTarget(Base.root_module(id))
                @test (@invokelatest getglobal(Main, Symbol(name))) === Base.root_module(id)
            finally
                Base.set_active_project(old)
            end
        end
    end
end

@testset "repository links" begin
    @test DocBrowser.web_url("git@github.com:JuliaLang/Foo.jl.git") == "https://github.com/JuliaLang/Foo.jl"
    @test DocBrowser.web_url("https://gitlab.com/a/b.git/") == "https://gitlab.com/a/b"
    @test DocBrowser.web_url("/home/me/Foo.jl") === nothing
    @test DocBrowser.web_url("https://me:token@github.com/a/Private.jl.git") == "https://github.com/a/Private.jl"
    id = Base.PkgId(Base.UUID(UInt128(1)), "NotARealPackage")
    mktempdir() do dir
        # A developed package is found through its git remote.
        mkpath(joinpath(dir, ".git"))
        write(joinpath(dir, ".git", "config"), """
            [core]
            \tbare = false
            [remote "origin"]
            \turl = git@github.com:someone/NotARealPackage.jl.git
            """)
        @test DocBrowser.repo_url(id, dir) == "https://github.com/someone/NotARealPackage.jl"
    end
    # Without a repository, the page offers a web search for the package.
    p = DocBrowser.unloaded_page(id, 70)
    @test DocBrowser.URLTarget("https://duckduckgo.com/?q=NotARealPackage.jl") in [s.target for s in p.spots]
    @test any(l -> occursin("NotARealPackage.jl", String(l)), p.lines)
end

"""
    Shape

An abstract shape.

# Examples

```jldoctest
julia> 1 + 1
2
```

# Notes

Uses Example.jl.

# Extended help

More detail here.
"""
abstract type Shape end
"A square."
struct Square <: Shape
    side::Float64
end
area(s::Square) = s.side^2

@testset "pages: methods, types, examples, contents" begin
    cat = Catalog()
    entry(m, s) = only(filter(e -> e.name === s, module_entries(cat, m)))
    text(p) = join(map(String, p.lines), '\n')
    targets(p) = [s.target for s in p.spots]

    p = entry_page(cat, entry(Base, :sum), 90)
    @test occursin(r"Methods \(\d+\)", text(p))
    @test any(t -> t isa DocBrowser.SourceTarget, targets(p))

    p = entry_page(cat, entry(@__MODULE__, :Shape), 90)
    t = text(p)
    @test occursin("Subtypes (1)", t) && occursin("Square", t)
    @test !occursin("More detail here.", t) && occursin("Show the extended help", t)
    @test DocBrowser.ToggleTarget(:methodswith) in targets(p)
    # The example can be pasted, and package mentions are links.
    @test DocBrowser.CodeTarget("1 + 1") in targets(p)
    @test any(t -> t isa DocBrowser.URLTarget && occursin("Example.jl", t.url), targets(p))
    @test length(p.headings) == 2

    # With the extended help there are enough headings for a list of contents.
    p = entry_page(cat, entry(@__MODULE__, :Shape), 90, Set([:extended, :methodswith]))
    @test length(p.headings) == 3 && DocBrowser.AnchorTarget(3) in targets(p)
    @test occursin("More detail here.", text(p))
    @test occursin("Methods that take this type", text(p))

    p = entry_page(cat, entry(@__MODULE__, :Square), 90)
    @test occursin(r"side\s*::\s*Float64", text(p))
    @test occursin("Constructors", text(p))

    @test DocBrowser.paste_text(["  julia> x = 1", "  1", "", "  julia> f(x,", "             y)"]) ==
        "x = 1\nf(x,\n    y)"
    @test DocBrowser.paste_text(["  sum(x)"]) === nothing
    @test DocBrowser.licence(pkgdir(DocBrowser)) === nothing
end

@testset "text search, toggles and layout" begin
    cat = Catalog()
    hits = DocBrowser.search_text(cat, "eigenvalue decomposition")
    @test any(e -> e.name === :eigen, hits) || !isdefined(Main, :LinearAlgebra)
    @test any(e -> e.name === :sum, DocBrowser.search_text(cat, "sum of all elements"))

    b = Browser(fake_terminal(""))
    frame(b)
    handle!(b, KeyEvent('/'))
    handle!(b, KeyEvent('t', DocBrowser.CTRL))
    @test b.textsearch
    for c in "sum of"
        handle!(b, KeyEvent(c))
    end
    @test !isempty(b.results)
    # The search field is drawn with its own face while typing.
    @test occursin("sum of", strip_ansi(frame(b)[1]))
    handle!(b, KeyEvent(:escape))

    DocBrowser.follow!(b, NameTarget(@__MODULE__, :Shape))
    frame(b)
    handle!(b, KeyEvent('x'))
    @test :extended in DocBrowser.page_flags(b, DocBrowser.current_key(b))
    handle!(b, KeyEvent('}'))
    @test b.doc_scroll > 0

    handle!(b, KeyEvent('w'))
    @test b.layout === :three
    handle!(b, KeyEvent('w'))
    @test length(DocBrowser.layout(b)) == 2
    handle!(b, KeyEvent('w'))
    @test length(DocBrowser.layout(b)) == 1
    handle!(b, KeyEvent('w'))
    @test b.layout === :auto

    # A paste link returns its code.
    DocBrowser.follow!(b, DocBrowser.CodeTarget("1 + 1"))
    @test b.quit && b.result == "1 + 1"
end

@testset "browse objects, reopening, no colour" begin
    @test browse(fake_terminal("y"), sum) == "sum"
    @test browse(fake_terminal("y"), Base.Iterators) == "Iterators"
    @test browse(fake_terminal("y"), 1.5) == "Float64"
    # With nothing to go to, the browser opens where it was last closed.
    @test browse(fake_terminal("y")) == "Float64"

    out = IOContext(IOBuffer(), :displaysize => (30, 110), :color => false)
    b = Browser(FakeTerminal(IOBuffer(""), out, IOBuffer(), false))
    @test !b.color
    lines = frame(b)
    @test !any(l -> occursin('\e', l), lines)
    @test any(l -> occursin('▶', l), lines)
end

@testset "finding the source" begin
    b = Browser(fake_terminal(""))
    frame(b)
    src(x) = DocBrowser.source_of(b, x)
    entry(m, s) = only(filter(e -> e.name === s, module_entries(b.cat, m)))
    # A documented name goes to its docstring, an undocumented one to its first method.
    @test endswith(src(entry(Fixture, :documented)).file, "runtests.jl")
    @test src(entry(Fixture, :undocumented_public)) ==
        DocBrowser.method_target(only(methods(Fixture.undocumented_public)))
    @test src(NameTarget(Base, :sum)) isa DocBrowser.SourceTarget
    @test endswith(src(ModuleTarget(DocBrowser)).file, "DocBrowser.jl")
    @test src(DocBrowser.URLTarget("https://julialang.org")) === nothing
    m = first(methods(sum))
    @test src(DocBrowser.method_target(m)) == DocBrowser.method_target(m)
    # Rows of the lists are found from where they are drawn.
    row = findfirst(r -> r.pkg !== nothing, b.side_rows)
    @test DocBrowser.hit_subject(b, (:side, row)) === b.side_rows[row].pkg
    # Right-clicking where nothing has a source says so.
    handle!(b, MouseEvent(:press, 3, 1, 1, 0))
    @test b.status == "No source location for that"
end

@testset "discoverability" begin
    DocBrowser.FIRST_OPEN[] = true
    b = Browser(fake_terminal(""; cols=120))
    footer() = strip_ansi(frame(b)[end])
    # The first open shows a tip, until the first key.
    @test occursin("right-click opens its source", footer())
    handle!(b, KeyEvent('j'))
    @test !occursin("Tip:", footer())
    @test occursin("? help", footer()) && occursin("q quit", footer())

    # Hovering or selecting a link says where it goes.
    DocBrowser.follow!(b, NameTarget(Fixture, :documented))
    lines = frame(b)
    p = DocBrowser.page(b, DocBrowser.current_key(b), DocBrowser.doc_width(b))
    k = findfirst(s -> s.target isa DocBrowser.URLTarget, p.spots)
    b.hover = (:spot, k)
    @test occursin("in your web browser", footer())
    # Going to another page forgets what was under the pointer on the old one.
    DocBrowser.follow!(b, NameTarget(Base, :sum))
    frame(b)
    @test b.hover === nothing
    DocBrowser.follow!(b, NameTarget(Fixture, :documented))
    frame(b)
    handle!(b, KeyEvent(:tab))
    @test occursin("right-click: source", footer())
    @test DocBrowser.describe(b, (:load, Base.PkgId(Base.UUID(UInt128(1)), "Foo"))) ==
        "↓ runs import Foo, then shows its documentation"

    # Hints follow what the page offers.
    DocBrowser.follow!(b, NameTarget(@__MODULE__, :Shape))
    frame(b)
    @test occursin("x extended", footer()) && occursin("{ } headings", footer())
    handle!(b, KeyEvent('/'))
    @test occursin("^T text", footer())
    handle!(b, KeyEvent(:escape))

    # Narrow terminals keep `? help` and offer other layouts.
    b = Browser(fake_terminal(""; cols=70))
    handle!(b, KeyEvent('j'))
    @test occursin("? help", footer()) && occursin("w layout", footer())

    # Without colour the legend matches the letters in the list.
    out = IOContext(IOBuffer(), :displaysize => (30, 130), :color => false)
    b = Browser(FakeTerminal(IOBuffer(""), out, IOBuffer(), false))
    @test occursin("E exported", frame(b)[end])
end

@testset "JLL pages" begin
    cat = Catalog()
    m = Base.require(Main, :Zlib_jll)
    cat = Catalog()
    p = module_page(cat, m, 90)
    t = join(map(String, p.lines), '\n')
    @test occursin("binary wrapper", t)
    @test occursin("libz", t) && occursin("Libraries", t)
    @test DocBrowser.URLTarget("https://github.com/JuliaBinaryWrappers/Zlib_jll.jl") in [s.target for s in p.spots]
    @test !any(s -> s.target isa DocBrowser.URLTarget && occursin("docs.julialang.org", s.target.url), p.spots)
end

# Nothing a test does may launch a web browser or an editor.
const OPENED = String[]
const EDITED = Tuple{String,Int}[]
DocBrowser.URL_OPENER[] = url -> push!(OPENED, url)
DocBrowser.EDITOR[] = (file, line) -> push!(EDITED, (file, line))
const COPIED = String[]
DocBrowser.COPY[] = (b, text) -> (push!(COPIED, text); "to the clipboard")

function test_browser(; rows=30, cols=120)
    b = Browser(fake_terminal(""; rows, cols))
    empty!(b.cat.unloaded) # so no click loads a package
    DocBrowser.refresh_side!(b)
    frame(b)
    return b
end
press!(b, keys...) = for k in keys
    handle!(b, k isa KeyEvent ? k : KeyEvent(k))
    frame(b)
end

@testset "keys in the lists" begin
    b = test_browser()
    n = length(b.side_rows)
    press!(b, :end);      @test b.side.cursor == n
    press!(b, :home);     @test b.side.cursor == 1
    press!(b, 'G');       @test b.side.cursor == n
    press!(b, 'g');       @test b.side.cursor == 1
    press!(b, :pagedown); @test b.side.cursor == min(n, 1 + DocBrowser.content_rows(b))
    press!(b, :pageup);   @test b.side.cursor == 1
    # The first row is a section heading: Enter and → fold it, and fold it back.
    press!(b, :enter);    @test !(:direct in b.expanded) || length(b.side_rows) < n
    press!(b, :right)
    press!(b, 'a');       @test all(in(b.expanded), DocBrowser.SECTION_ORDER)
    press!(b, 'a');       @test !(:indirect in b.expanded)
    # On a package, → moves to its names, and ← back again.
    b.side.cursor = findfirst(r -> r.pkg !== nothing, b.side_rows)
    press!(b, :right);    @test b.focus === :names
    press!(b, :left);     @test b.focus === :side
    press!(b, KeyEvent('l', DocBrowser.CTRL))
    @test isempty(b.prev) || length(b.prev) == b.height
    press!(b, :tab);      @test b.focus === :names
    press!(b, :tab);      @test b.focus === :doc
    # In the documentation ⇧⇥ steps through links. ← goes back to the list that chose the page.
    press!(b, :left);     @test b.focus === :side
    press!(b, :backtab);  @test b.focus === :names
    # Group headings in the names pane fold too.
    b.focus = :names
    b.names.cursor = findfirst(r -> r.kind === :header, b.name_rows)
    group = b.name_rows[b.names.cursor].group
    press!(b, :enter);    @test group in b.collapsed
    press!(b, :enter);    @test !(group in b.collapsed)
end

@testset "selecting text" begin
    b = test_browser()
    DocBrowser.follow!(b, NameTarget(Fixture, :documented))
    frame(b)
    p = DocBrowser.page(b, DocBrowser.current_key(b), DocBrowser.doc_width(b))
    li = findfirst(l -> occursin("Calls", String(l)), p.lines)
    line = String(p.lines[li])
    x = only(l[2] for l in DocBrowser.layout(b) if l[1] === :doc)
    # The screen position of a column of the page line `l`.
    at(l, col) = (l - b.doc_scroll + 2, x + col - 1)
    calls = textwidth(line[1:prevind(line, findfirst("Calls", line)[1])]) + 1
    mouse(action, (row, col); button=1) = (handle!(b, MouseEvent(action, button, row, col, 0)); frame(b))
    # A drag over text in the page selects and copies it.
    mouse(:press, at(li, calls))
    mouse(:drag, at(li, calls + 4))
    @test DocBrowser.selected_cols(b, li) == calls:calls+4
    mouse(:release, at(li, calls + 4))
    @test COPIED[end] == "Calls" && occursin("Copied 5 characters", b.status)
    # Across lines, the indentation of the page is dropped.
    for start in (1, 2) # from the edge of the pane, or from inside the indentation
        mouse(:press, at(li - 2, start))
        mouse(:drag, at(li, calls + 4))
        mouse(:release, at(li, calls + 4))
        @test startswith(COPIED[end], "documented(x)") && endswith(COPIED[end], "\n\nCalls")
    end
    # The highlight leaves out the indentation, which is not copied either.
    w = only(l[3] for l in DocBrowser.layout(b) if l[1] === :doc)
    shown = DocBrowser.draw_doc(b, x, w)[li - b.doc_scroll]
    @test [first(a.region) for a in Base.annotations(shown) if a.value === :REPL_DocBrowser_selection] == [calls]
    # A click selects nothing, a key clears a selection, and Esc only clears it.
    mouse(:press, at(li, calls)); mouse(:release, at(li, calls))
    @test b.sel === nothing
    b.sel = (li, 1, li, 3)
    press!(b, :escape)
    @test b.sel === nothing && !b.quit
    # Two clicks select a word, and three the line.
    b.clicks = (0.0, 0.0, 0.0, 0.0)
    for _ in 1:2
        mouse(:press, at(li, calls + 1)); mouse(:release, at(li, calls + 1))
    end
    @test COPIED[end] == "Calls"
    mouse(:press, at(li, calls + 1)); mouse(:release, at(li, calls + 1))
    @test COPIED[end] == strip(line)
    # A double-click word takes in dots and paths, but not a full stop or spaces.
    @test DocBrowser.word_cols("see Base.sort!.", 6) == 5:14
    @test DocBrowser.word_cols("a  b", 2) === nothing
    @test DocBrowser.col_slice("αβγδ", 2, 3) == "βγ"
end

@testset "edge cases" begin
    b = test_browser()
    doc_text(b) = join(String.(DocBrowser.page(b, DocBrowser.current_key(b), DocBrowser.doc_width(b)).lines), '\n')
    # Types with no definite number of fields still have pages.
    for T in (:Tuple, :NamedTuple, :NTuple)
        DocBrowser.follow!(b, NameTarget(Core, T))
        @test !occursin("Could not show", doc_text(b))
    end
    # The help leaves the page under it as it was.
    DocBrowser.follow!(b, NameTarget(Base, :sort!)); frame(b)
    b.doc_scroll = 3
    press!(b, '?'); press!(b, :escape)
    @test b.doc_scroll == 3
    # A link to a name in a folded group unfolds the group, not internal names.
    DocBrowser.show_module!(b, Fixture)
    push!(b.collapsed, :function)
    DocBrowser.show_module!(b, Fixture; entry=:documented)
    @test !b.internal && DocBrowser.name_row(b).entry.name === :documented
    # A long name can only be clicked inside its own pane.
    DocBrowser.show_module!(b, Base); frame(b)
    _, x, w = only(l for l in DocBrowser.layout(b) if l[1] === :names)
    i = argmax(i -> b.name_rows[i].entry === nothing ? 0 : length(String(b.name_rows[i].entry.name)), eachindex(b.name_rows))
    b.names.cursor = i; DocBrowser.reveal!(b.names, DocBrowser.content_rows(b)); frame(b)
    @test all(h -> !(h.action isa Tuple && first(h.action) === :names) || last(h.cols) <= x + w - 1, b.hits)
    # A click ends typing in the search field.
    press!(b, '/', 's')
    h = first(h for h in b.hits if h.action isa Tuple && first(h.action) === :side)
    handle!(b, MouseEvent(:press, 1, h.row, first(h.cols), 0)); frame(b)
    @test !b.typing
    # On a section heading, Shift-Tab does not move into the empty names pane.
    DocBrowser.close_search!(b)
    b.side.cursor = 1
    DocBrowser.move!(b, :side, 0)
    b.focus = :side
    press!(b, :backtab)
    @test b.focus === :side
    # A double-click past the end of a line selects nothing.
    @test DocBrowser.word_cols("abc", 4) === nothing
end

@testset "keys in the documentation" begin
    b = test_browser()
    DocBrowser.follow!(b, NameTarget(Base, :sort!))
    frame(b)
    @test b.focus === :doc
    press!(b, :pagedown); @test b.doc_scroll > 0
    press!(b, :pageup);   @test b.doc_scroll == 0
    press!(b, ' ');       @test b.doc_scroll > 0
    press!(b, :home);     @test b.doc_scroll == 0
    press!(b, :end);      @test b.doc_scroll > 0
    press!(b, 'g', 'j');  @test b.doc_scroll == 1
    press!(b, 'k');       @test b.doc_scroll == 0
    press!(b, 'o');       @test occursin("web link", b.status)
    press!(b, :enter);    @test occursin("pick a link", b.status)
    press!(b, :backtab);  @test b.link > 0
    press!(b, :escape);   @test b.link == 0 && !b.quit
    # History with the keyboard: ⌫ [ ] and Alt with arrows or b and f.
    DocBrowser.follow!(b, NameTarget(Base, :sum))
    press!(b, KeyEvent(:left, DocBrowser.ALT))
    @test DocBrowser.current_key(b).name === :sort!
    press!(b, KeyEvent(:right, DocBrowser.ALT))
    @test DocBrowser.current_key(b).name === :sum
    press!(b, KeyEvent('b', DocBrowser.ALT), KeyEvent('f', DocBrowser.ALT))
    @test DocBrowser.current_key(b).name === :sum
    press!(b, ']');       @test occursin("Nothing to go forward", b.status)
    empty!(b.back)
    press!(b, '[');       @test occursin("Nothing to go back", b.status)
    # The help page scrolls, and any other key closes it.
    press!(b, '?');       @test b.help
    press!(b, :down, :pagedown, :up, ' ')
    @test b.help
    press!(b, 'z');       @test !b.help
    press!(b, :f1);       @test b.help
    press!(b, :escape);   @test !b.help
    press!(b, 'x');       @test :extended in DocBrowser.page_flags(b, DocBrowser.current_key(b)) ||
                                occursin("No extended help", b.status)
    # A web link opens with `o`, and `e` opens the source of the page.
    DocBrowser.follow!(b, NameTarget(Fixture, :documented))
    p = DocBrowser.page(b, DocBrowser.current_key(b), DocBrowser.doc_width(b))
    b.link = findfirst(s -> s.target isa DocBrowser.URLTarget, p.spots)
    press!(b, 'o');       @test OPENED[end] == "https://docs.julialang.org"
    b.link = 0
    press!(b, 'e');       @test endswith(EDITED[end][1], "runtests.jl")
    # Quitting.
    for k in ('q', KeyEvent('c', DocBrowser.CTRL), KeyEvent('d', DocBrowser.CTRL), :escape)
        b.quit = false
        press!(b, k)
        @test b.quit
    end
end

@testset "keys while searching" begin
    b = test_browser()
    press!(b, '/', :backspace); @test b.typing && b.query == ""
    press!(b, 's', 'u', 'm')
    @test !isempty(b.results)
    press!(b, KeyEvent('u', DocBrowser.CTRL)); @test b.query == ""
    press!(b, 'z', 'q', 'x', 'j', 'v')
    press!(b, :enter);    @test b.status == "No matches"
    press!(b, KeyEvent('u', DocBrowser.CTRL), 's', 'u', 'm')
    # How many names match depends on what earlier tests loaded.
    start = b.names.cursor
    press!(b, :down, KeyEvent('n', DocBrowser.CTRL)); @test b.names.cursor == start + 2
    press!(b, :up, KeyEvent('p', DocBrowser.CTRL));   @test b.names.cursor == start
    press!(b, :pagedown); @test b.names.cursor > start
    press!(b, :pageup, :pageup); @test b.names.cursor == 1
    press!(b, :tab);      @test !b.typing && b.query == "sum"
    press!(b, 'i');       @test b.internal && length(b.results) > 0
    press!(b, :escape);   @test b.query === nothing
    press!(b, '/', 'a', KeyEvent('g', DocBrowser.CTRL))
    @test b.query === nothing
    press!(b, KeyEvent('f', DocBrowser.CTRL), :right)
    @test b.query == "" && !b.typing
    handle!(b, PasteEvent("  sort \n "))
    @test b.query == "sort" && b.typing
    @test !handle!(b, PasteEvent("   "))
end

@testset "clicking and hovering everything" begin
    b = test_browser()
    DocBrowser.follow!(b, NameTarget(Fixture, :documented))
    frame(b)
    for h in copy(b.hits)
        # Every region says what clicking it does, or nothing, without failing.
        b.hover = h.action
        @test DocBrowser.footer_message(b) isa String
        handle!(b, MouseEvent(:move, 0, h.row, first(h.cols), 0))
        handle!(b, MouseEvent(:press, 1, h.row, first(h.cols), 0))
        handle!(b, MouseEvent(:release, 1, h.row, first(h.cols), 0))
        frame(b)
        b.quit = false
        b.help = false
    end
    # The scroll bar can be dragged.
    DocBrowser.follow!(b, NameTarget(Base, :sort!))
    frame(b)
    bar = findfirst(h -> first(h.action) === :scrollbar, b.hits)
    last_bar = findlast(h -> first(h.action) === :scrollbar, b.hits)
    handle!(b, MouseEvent(:drag, 1, b.hits[last_bar].row, first(b.hits[last_bar].cols), 0))
    @test b.doc_scroll > 0
    handle!(b, MouseEvent(:press, 1, b.hits[bar].row, first(b.hits[bar].cols), 0))
    @test b.doc_scroll == 0
    # The wheel scrolls the pane under the pointer, and nothing outside the panes.
    for (pane, x, w) in DocBrowser.layout(b)
        handle!(b, MouseEvent(:wheeldown, 0, 5, x + 1, 0))
        handle!(b, MouseEvent(:wheelup, 0, 5, x + 1, 0))
    end
    @test !handle!(b, MouseEvent(:wheeldown, 0, 1, 1, 0))
    @test !handle!(b, MouseEvent(:wheelside, 0, 5, 5, 0))
    # A click outside every region focuses the pane under it; elsewhere it closes help.
    b.help = true
    handle!(b, MouseEvent(:press, 1, b.height - 1, 3, 0))
    @test !b.help
    # The mouse's back and forward buttons.
    DocBrowser.follow!(b, NameTarget(Base, :sort!))
    DocBrowser.follow!(b, NameTarget(Base, :sum))
    handle!(b, MouseEvent(:press, 8, 5, 5, 0)); @test DocBrowser.current_key(b).name === :sort!
    handle!(b, MouseEvent(:press, 9, 5, 5, 0)); @test DocBrowser.current_key(b).name === :sum
    @test !handle!(b, MouseEvent(:press, 2, 5, 5, 0))
    # Double-clicking a package opens its names.
    frame(b)
    b.focus = :side
    i = findfirst(r -> r.pkg !== nothing, b.side_rows)
    h = b.hits[findfirst(h -> h.action == (:side, i), b.hits)]
    handle!(b, MouseEvent(:press, 1, h.row, first(h.cols), 0))
    handle!(b, MouseEvent(:press, 1, h.row, first(h.cols), 0))
    @test b.focus === :names
end

@testset "following every kind of link" begin
    b = test_browser()
    DocBrowser.follow!(b, DocBrowser.URLTarget("https://example.org"))
    @test OPENED[end] == "https://example.org" && occursin("Opened", b.status)
    DocBrowser.follow!(b, DocBrowser.SourceTarget(@__FILE__, 3))
    @test EDITED[end] == (@__FILE__, 3)
    DocBrowser.follow!(b, DocBrowser.SourceTarget("/no/such/file.jl", 1))
    @test occursin("not found", b.status)
    DocBrowser.follow!(b, DocBrowser.SectionTarget(:julia))
    @test b.focus === :side && DocBrowser.current_key(b) == DocBrowser.SectionTarget(:julia)
    DocBrowser.follow!(b, NameTarget(Fixture, :Inner))
    @test b.mod === Fixture.Inner
    DocBrowser.follow!(b, NameTarget(Fixture, :helper))
    @test b.internal && DocBrowser.current_key(b).name === :helper
    # Failures while away from the screen are reported, not thrown.
    DocBrowser.URL_OPENER[] = url -> error("no browser")
    DocBrowser.follow!(b, DocBrowser.URLTarget("https://example.org"))
    @test occursin("Could not open", b.status)
    DocBrowser.EDITOR[] = (f, l) -> error("no editor")
    DocBrowser.follow!(b, DocBrowser.SourceTarget(@__FILE__, 1))
    @test occursin("no editor", b.status)
    DocBrowser.URL_OPENER[] = url -> push!(OPENED, url)
    DocBrowser.EDITOR[] = (file, line) -> push!(EDITED, (file, line))
    # `e` on a selected row opens that name's source.
    DocBrowser.show_module!(b, Fixture)
    frame(b)
    b.focus = :names
    b.names.cursor = findfirst(r -> r.entry !== nothing && r.entry.name === :documented, b.name_rows)
    press!(b, 'e')
    @test endswith(EDITED[end][1], "runtests.jl")
end

@testset "package pages" begin
    cat = Catalog()
    for role in DocBrowser.SECTION_ORDER
        p = DocBrowser.section_page(cat, role, 80)
        @test occursin(getfield(DocBrowser.SECTION_TITLES, role), String(p.lines[2]))
    end
    # How each kind of package describes its place in the environment.
    id = Base.PkgId(Base.UUID(UInt128(2)), "Fixture")
    roles = Dict(:project => "the active project", :direct => "direct dependency",
                 :stdlib => "standard library", :indirect => "indirect dependency",
                 :extension => "package extension")
    for (role, words) in roles
        cat.bymod[Fixture] = DocBrowser.PkgEntry(Fixture, id, v"1.2.3", role, ["Base", "Core"], "Base")
        text = join(first.(DocBrowser.role_line(cat, Fixture)))
        @test occursin(words, text)
        role in (:indirect, :stdlib) && @test occursin("loaded by Base → Core", text)
    end
    @test occursin("submodule of", join(first.(DocBrowser.role_line(cat, Fixture.Inner))))
    delete!(cat.bymod, Fixture)
    # An extension shows neither the README nor the links of the package it extends.
    cat.bymod[UndocumentedExt] = DocBrowser.PkgEntry(UndocumentedExt, id, nothing, :extension, String[], "Base")
    text = join(String.(module_page(cat, UndocumentedExt, 80).lines), '\n')
    @test occursin("No docstring found", text)
    @test !occursin("search the web", text)
    delete!(cat.bymod, UndocumentedExt)
    # Standard libraries shipped with Julia show no version.
    @test cat.bymod[Markdown].version === nothing
    @test !DocBrowser.is_bundled(Fixture)
    @test occursin("workspace", join(first.(DocBrowser.role_line(cat, Main))))
    # Licences are recognised from the start of the licence file.
    mktempdir() do dir
        @test DocBrowser.licence(dir) === nothing
        write(joinpath(dir, "LICENSE.md"), "MIT License\n\nCopyright (c) 2020")
        @test DocBrowser.licence(dir) == "MIT"
        write(joinpath(dir, "LICENSE.md"), "                 Apache License\n           Version 2.0")
        @test DocBrowser.licence(dir) == "Apache-2.0"
        write(joinpath(dir, "LICENSE.md"), "All rights reserved by nobody in particular.")
        @test DocBrowser.licence(dir) == "see LICENSE.md"
        write(joinpath(dir, "COPYING"), "SPDX-License-Identifier: BSD-3-Clause WITH some-exception")
        write(joinpath(dir, "GPL-3.0+"), "")
        @test DocBrowser.licence(dir) == "BSD-3-Clause, GPL-3.0+"
        rm(joinpath(dir, "COPYING")); rm(joinpath(dir, "GPL-3.0+"))
        # The docs link comes from a README badge, preferring the stable docs.
        @test DocBrowser.docs_url(dir) === nothing
        write(joinpath(dir, "README.md"), """
            [![CI](https://x.org/ci.svg)](https://github.com/o/P.jl/actions)
            [![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://o.github.io/P.jl/dev)
            [![][docs-stable-img]][docs-stable-url]

            [docs-stable-img]: https://img.shields.io/badge/docs-stable-blue.svg?style=flat
            [docs-stable-url]: https://o.github.io/P.jl/stable
            """)
        @test DocBrowser.docs_url(dir) == "https://o.github.io/P.jl/stable"
        pb = DocBrowser.PageBuilder(80)
        DocBrowser.add_links!(pb, id, dir, :indirect)
        @test occursin("docs o.github.io/P.jl/stable", String(pb.lines[end]))
        write(joinpath(dir, "README.md"), "<a href=\"https://o.github.io/P.jl/dev\"><img src=\"docs-dev.svg\"></a>")
        @test DocBrowser.docs_url(dir) == "https://o.github.io/P.jl/dev"
    end
    # A standard library in Julia's repository has Julia's licence, and a JLL that of its library.
    pb = DocBrowser.PageBuilder(200)
    DocBrowser.add_links!(pb, Base.PkgId(Markdown), pkgdir(Markdown), :stdlib)
    @test occursin("licence MIT", String(pb.lines[end]))
    libs = DocBrowser.jll_licence_dir(Base.PkgId(nothing, "OpenBLAS_jll"))
    isdir(libs) && @test DocBrowser.licence(libs) !== nothing
    # Julia's manual is linked at the address deployed for each kind of build.
    for (v, ver) in (v"1.13.4" => "v1.13", v"1.15.0-DEV.1234" => "v1.15-dev",
                     v"1.13.5-DEV.12" => "v1.13-dev", v"1.14.0-beta1.5" => "v1.14-dev")
        @test DocBrowser.julia_docs("stdlib/REPL/", v) == "https://docs.julialang.org/en/$ver/stdlib/REPL/"
    end
    # README badges and HTML are dropped.
    md = DocBrowser.strip_badges!(Markdown.parse("""
        [![CI](https://x/ci.svg)](https://x/ci) ![logo](logo.png)

        <p align="center">hello</p>

        Some text.
        """))
    @test length(md.content) == 1
end

@testset "repositories from a registry or a manifest" begin
    uuid = Base.UUID("7876af07-990d-54b4-ab0e-23690620f79a")
    id = Base.PkgId(uuid, "NotRegisteredAnywhere")
    mktempdir() do depot
        reg = joinpath(depot, "registries", "Test")
        mkpath(joinpath(reg, "N", "NotRegisteredAnywhere"))
        write(joinpath(reg, "Registry.toml"), """
            name = "Test"
            [packages]
            $uuid = { name = "NotRegisteredAnywhere", path = "N/NotRegisteredAnywhere" }
            """)
        write(joinpath(reg, "N", "NotRegisteredAnywhere", "Package.toml"),
              "repo = \"https://github.com/someone/NotRegisteredAnywhere.jl.git\"\n")
        pushfirst!(DEPOT_PATH, depot)
        try
            @test DocBrowser.repo_url(id, nothing) == "https://github.com/someone/NotRegisteredAnywhere.jl"
        finally
            popfirst!(DEPOT_PATH)
        end
    end
    mktempdir() do dir
        write(joinpath(dir, "Project.toml"), "[deps]\nNotRegisteredAnywhere = \"$uuid\"\n")
        write(joinpath(dir, "Manifest.toml"), """
            manifest_format = "2.0"
            [[deps.NotRegisteredAnywhere]]
            uuid = "$uuid"
            repo-url = "git@gitlab.com:someone/NotRegisteredAnywhere.jl.git"
            """)
        old = Base.active_project()
        Base.set_active_project(joinpath(dir, "Project.toml"))
        try
            @test DocBrowser.repo_url(id, nothing) == "https://gitlab.com/someone/NotRegisteredAnywhere.jl"
        finally
            Base.set_active_project(old)
        end
    end
end

# An output whose size can change, like a terminal window being resized.
mutable struct ResizableIO <: IO
    buf::IOBuffer
    size::Tuple{Int,Int}
end
Base.displaysize(io::ResizableIO) = io.size
Base.unsafe_write(io::ResizableIO, p::Ptr{UInt8}, n::UInt) = unsafe_write(io.buf, p, n)
Base.write(io::ResizableIO, x::UInt8) = write(io.buf, x)
Base.flush(io::ResizableIO) = nothing

@testset "resizing" begin
    out = ResizableIO(IOBuffer(), (30, 120))
    b = Browser(FakeTerminal(IOBuffer(""), out, IOBuffer()))
    DocBrowser.draw(b)
    @test !DocBrowser.check_size!(b)
    out.size = (20, 70)
    @test DocBrowser.check_size!(b)
    @test (b.height, b.width) == (20, 70) && isempty(b.prev)
    DocBrowser.draw(b)
    @test length(b.prev) == 20
end

@testset "search engine" begin
    @test DocBrowser.web_search("Foo").url == "https://duckduckgo.com/?q=Foo.jl"
    withenv("JULIA_DOCBROWSER_SEARCH_URL" => "https://example.org/find?q=%s&lang=en") do
        @test DocBrowser.web_search("Foo").url == "https://example.org/find?q=Foo.jl&lang=en"
        @test DocBrowser.web_search("Ünï code!").url ==
            "https://example.org/find?q=%C3%9Cn%C3%AF%20code%21.jl&lang=en"
    end
    # Without a place for the query the setting is ignored.
    withenv("JULIA_DOCBROWSER_SEARCH_URL" => "https://example.org") do
        @test DocBrowser.web_search("Foo").url == "https://duckduckgo.com/?q=Foo.jl"
    end
end

@testset "headings at every level" begin
    md = Markdown.parse("# One\n\ntext\n\n## Two\n\ntext\n\n### Three\n\ntext\n\n#### Four\n\ntext\n")
    pb = DocBrowser.PageBuilder(60)
    DocBrowser.add_markdown!(pb, md, Main, Catalog(); contents=true)
    p = DocBrowser.Page(pb)
    @test [strip(String(p.lines[i])) for i in p.headings] == ["One", "Two", "Three", "Four"]
end

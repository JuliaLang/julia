# This file is a part of Julia. License is MIT: https://julialang.org/license

# The start page: where the browser opens in a fresh session.

# The sections of the start page, in order. The names pane lists them.
const START_SECTIONS = ("New to Julia?", "Finding things", "Your project", "Release notes")

# Tips for the start page, with code in backticks, and the section of the docs that says more:
# a page and heading of a standard library or of the manual (`:manual`), or nothing.
const TIPS = (
    ("Made a change in `pkg>` you didn't mean? `undo` takes back the last change to the project, and `redo` puts it back.", nothing),
    ("`pkg> activate --temp` starts an empty environment that is deleted when Julia exits: a place to try a package without adding it to your project.",
     ("Pkg", "environments.md", "Temporary environments")),
    ("`pkg> why Foo` shows which of your packages bring in `Foo`.",
     ("Pkg", "managing-packages.md", "Why is a package not at its latest version?")),
    ("`pkg> status --outdated` shows which packages have newer versions, and what holds each one back.",
     ("Pkg", "managing-packages.md", "Why is a package not at its latest version?")),
    ("A string in help mode searches the text of every docstring: try `help?> \"sort by\"`.", ("REPL", "index.md", "Help mode")),
    ("You can paste a whole REPL session, `julia>` prompts and output included: the REPL runs only the code.",
     ("REPL", "index.md", "The Julian mode")),
    ("`^R` searches everything you have typed at the REPL before, in this session and earlier ones.",
     ("REPL", "index.md", "History searching")),
    ("Type `\\alpha` and press Tab to get `α`. To find out how to type a character you see, ask help mode: `help?> α`.",
     ("REPL", "index.md", "Tab completion")),
    ("`@edit sum([1, 2])` opens the method that this call runs in your editor, and `@which` says where it is. Set `JULIA_EDITOR` to choose the editor.", nothing),
    ("`julia --project` with no directory uses the project of the current directory, or of the nearest folder above it with a `Project.toml`.", nothing),
    ("Alt-Enter starts a new line at the prompt without running what you have typed so far.", ("REPL", "index.md", "Key bindings")),
    ("After an error, type the number of a frame in the stacktrace and press `^Q` to open your editor at that line. The error itself is in `err`.",
     ("REPL", "index.md", "Key bindings")),
    ("Type the start of an earlier line and press Up: the REPL steps back through only the lines that begin that way.",
     ("REPL", "index.md", "Key bindings")),
    ("In `^R` history search, Tab selects lines and `^S` saves them to the clipboard or a file: a quick way to turn what you tried into a script.",
     ("REPL", "index.md", "History searching")),
    ("Not sure which function you need? Type `?(\"data.csv\")` and press Tab to list the methods that take a string, or `Base.?(\"data.csv\")` for those in `Base` only.",
     ("REPL", "index.md", "Tab completion")),
    ("Type a module's name and press Alt-m, and what you type next runs inside that module. Alt-m at an empty prompt switches back.",
     ("REPL", "index.md", "Changing the contextual module which is active at the REPL")),
    ("`help?> ??foo` shows all of a docstring, including the extended help that `?foo` leaves out.", ("REPL", "index.md", "Help mode")),
    ("Shell mode runs commands without a system shell: `shell> ls \$dir` puts in the value of the Julia variable `dir`, and `cd` changes Julia's own working directory.",
     ("REPL", "index.md", "Shell mode")),
    ("Precompiling is taking a while? Press `d` to get the prompt back while it carries on in the background, and `pkg> precompile --monitor` to watch it again.",
     ("Pkg", "environments.md", "Keyboard Controls and Background Precompilation")),
    ("`ENV[\"JULIA_DEBUG\"] = \"Foo\"` shows the `@debug` messages of module `Foo`. Start Julia with `JULIA_DEBUG=loading` to see why a package is precompiled again.",
     ("Logging", "index.md", "Environment variables")),
    ("The first `@time` of a call includes compiling it, and says how much of the time that was. Run it again to time the call alone.",
     (:manual, "performance-tips.md", "Measure performance with @time and pay attention to memory allocation")),
    ("`@code_warntype f(x)` marks in red the values whose type Julia can't work out before running, a common cause of slow code.",
     (:manual, "performance-tips.md", "man-code-warntype")),
    ("A function that uses a global variable is slow, since the variable could change type at any time. Pass it in as an argument, declare it `const`, or give it a type: `x::Float64 = 0.0`.",
     (:manual, "performance-tips.md", "Avoid untyped global variables")),
    ("Slicing copies: `A[:, 1]` is a new array. `@views` in front of an expression makes its slices views into `A` instead.",
     (:manual, "performance-tips.md", "man-performance-views")),
    ("Arrays are stored column by column, so a loop over a matrix runs fastest with the first index in the innermost loop.",
     (:manual, "performance-tips.md", "man-performance-column-major")),
    ("After `using Profile`, `@profile f(x)` records where the time goes, and `Profile.print(format=:flat)` lists how much of it each line took.",
     ("Profile", "index.md", "Via @profile")),
    ("With Revise.jl loaded, changes you save to your code take effect in the running session, without a restart. Put `using Revise` in `~/.julia/config/startup.jl` to always have it.",
     (:manual, "workflow-tips.md", "Revise-based workflows")),
    ("Define `function (@main)(args)` in a script, and `julia script.jl a b` calls it with `[\"a\", \"b\"]`. `julia -m Foo` runs the `@main` of package `Foo`.",
     (:manual, "command-line-interface.md", "The Main.main entry point")),
    ("Your code runs on one thread unless you start Julia with `-t auto`, which gives it one per core. `Threads.@spawn` runs work on them.",
     (:manual, "multi-threading.md", "Starting Julia with multiple threads")),
    ("`varinfo()` lists your global variables and their sizes: a quick way to see what holds on to memory.", nothing),
    ("`methodswith(Dict)` lists the functions with a method for `Dict`, and `supertypes(Int)` shows where a type sits among the others.", nothing),
    # Windows has no signal to start a profile peek.
    (Sys.iswindows() ? () :
     ((string("Wondering what a long-running command is doing? ",
              Sys.isbsd() ? "Press `^T` while it runs" : "Run `kill -USR1 <pid>` from another terminal",
              " to see where it is, and a profile of the next second."),
       ("Profile", "index.md", "Triggered During Execution")),))...,
)

# The page that a tip links to, if it is installed.
function tip_link(tip)
    link = tip[2]
    link === nothing && return nothing
    path = link[1] === :manual ? joinpath(manual_dir(), "manual", link[2]) :
        joinpath(Sys.STDLIB, link[1], "docs", "src", link[2])
    return isfile(path) ? ManualTarget(path, link[3]) : nothing
end

# The words of a tip as segments, so that the paragraph can wrap, with code kept whole.
function tip_segments(text::String)
    segs = Any[]
    for (i, part) in enumerate(split(text, '`'))
        if iseven(i)
            push!(segs, (part, :markdown_code))
        else
            words = [m.match for m in eachmatch(r"\S+\s*", part)]
            # What follows code stays on its line: a space, so that no line starts with one,
            # or punctuation.
            if !isempty(segs)
                glue = startswith(part, ' ') ? " " : isempty(words) ? "" : popfirst!(words)
                segs[end] = Any[segs[end], glue]
            end
            append!(segs, words)
        end
    end
    return segs
end

# Chapters of the manual to read first, by file.
const FIRST_CHAPTERS = ("getting-started", "variables", "functions", "control-flow", "types",
                        "methods", "modules", "arrays", "performance-tips", "workflow-tips")

const START_KEYS = (("/", "search names, and the pages of the manual"),
                    ("^T", "search the text of docstrings and the manual"),
                    ("click", "anything to open it; right-click opens its source"),
                    ("?", "every key, and what the marks mean"))

"""
    version_block!(pb)

The version of Julia and what kind of build it is, beside the Julia logo.
"""
function version_block!(pb::PageBuilder)
    git = Base.GIT_VERSION_INFO
    kind = isempty(VERSION.prerelease) ? "release" :
        "DEV" in VERSION.prerelease ? "development build" : "prerelease"
    date = isempty(git.date_string) ? "" : string(", ", first(split(git.date_string)))
    # The Julia logo's three dots, beside the version.
    addline!(pb, "   ", ("●", :REPL_DocBrowser_logo_green), "    ",
             ("Julia ", :REPL_DocBrowser_pane_title), (string(Base.version_with_build_variant()), :REPL_DocBrowser_title))
    addline!(pb, "  ", ("●", :REPL_DocBrowser_logo_red), " ", ("●", :REPL_DocBrowser_logo_purple), "   ",
             (kind * date, :REPL_DocBrowser_dim))
end

# The release notes of this version of Julia: installed with the manual, or in the source tree.
function release_notes()
    for path in (joinpath(manual_dir(), "NEWS.md"), normpath(Sys.BINDIR, "..", "..", "NEWS.md"))
        isfile(path) && return ManualTarget(path, "")
    end
    return URLTarget(julia_docs("NEWS/"))
end

"""
    start_page(cat, width, tip=1, pos=tip) -> Page

The start page: this Julia, and where to begin, how to find things, the packages of the
active project, the release notes, and tip number `tip`, shown as the `pos`th.
"""
function start_page(cat::Catalog, width::Int, tip::Int=1, pos::Int=tip)
    pb = PageBuilder(width)
    addline!(pb)
    version_block!(pb)

    section!(pb, START_SECTIONS[1])
    chapters = ManualPage[]
    for name in FIRST_CHAPTERS
        i = findfirst(p -> p.role === :manual && splitext(basename(p.path))[1] == name, cat.manual)
        i === nothing || push!(chapters, cat.manual[i])
    end
    if isempty(chapters)
        addflow!(pb, "  ", Any[("The manual is at ", nothing), (link_label(julia_docs()), nothing, URLTarget(julia_docs()))])
    else
        addline!(pb, "  ", "Start with ", (chapters[1].title, nothing, ManualTarget(chapters[1].path, "")),
                 ", then read on:")
        segs = Any[]
        for p in chapters[2:end]
            isempty(segs) || push!(segs, ("  ", nothing))
            push!(segs, (p.title, nothing, ManualTarget(p.path, "")))
        end
        addflow!(pb, "    ", segs)
    end

    section!(pb, START_SECTIONS[2])
    for (key, what) in START_KEYS
        addline!(pb, "    ", (rpad(key, 7), :REPL_DocBrowser_key), what)
    end

    section!(pb, START_SECTIONS[3])
    proj = Base.active_project()
    label = project_label()
    label === nothing || addline!(pb, "  ", (label, :REPL_DocBrowser_pane_title))
    proj === nothing || addline!(pb, "  ", (Base.contractuser(proj), :REPL_DocBrowser_dim))
    segs = Any[]
    for p in cat.pkgs
        p.role in (:project, :direct) || continue
        isempty(segs) || push!(segs, ("   ", nothing))
        version = p.version === nothing ? "" : string(" ", p.version)
        push!(segs, Any[(p.id.name, nothing, ModuleTarget(p.mod)), (version, :REPL_DocBrowser_dim)])
    end
    for id in cat.unloaded
        isempty(segs) || push!(segs, ("   ", nothing))
        push!(segs, Any[(id.name * " ", nothing), ("↓ load", :REPL_DocBrowser_key, LoadTarget(id))])
    end
    isempty(segs) ? addline!(pb, "  ", ("Nothing is loaded from it yet.", :REPL_DocBrowser_dim)) : addflow!(pb, "    ", segs)

    section!(pb, START_SECTIONS[4])
    notes = release_notes()
    addline!(pb, "  ", "What is new in Julia $(VERSION.major).$(VERSION.minor): ",
             (notes isa ManualTarget ? "NEWS.md" : link_label(notes.url), nothing, notes))
    addline!(pb)
    addline!(pb)
    tip_box!(pb, tip, pos)
    return Page(pb)
end

# A tip in a box of its own, so that it stands out from the rest of the page.
function tip_box!(pb::PageBuilder, tip::Int, pos::Int)
    t = TIPS[mod1(tip, length(TIPS))]
    w = min(pb.width - 4, 80) # the width of the box
    inner = PageBuilder(w - 6)
    addline!(inner)
    addflow!(inner, "", tip_segments(t[1]))
    addline!(inner)
    segs = Any[Any[("◂ previous", nothing, ToggleTarget(:tip_back)), "   "],
               Any[("next ▸", nothing, ToggleTarget(:tip)), "   "],
               Any[(string(mod1(pos, length(TIPS)), " of ", length(TIPS)), :REPL_DocBrowser_dim), "     "]]
    link = tip_link(t)
    link === nothing || push!(segs, ("more about this", nothing, link))
    addflow!(inner, "", segs)
    addline!(inner)
    label = " Did you know? "
    addline!(pb, "  ", ("╭─", :REPL_DocBrowser_tip), (label, :REPL_DocBrowser_tip_title),
             ("─"^max(0, w - 3 - textwidth(label)) * "╮", :REPL_DocBrowser_tip))
    for (k, line) in enumerate(inner.lines)
        row = length(pb.lines) + 1
        pad = " "^max(0, w - 6 - textwidth(String(line)))
        push!(pb.lines, annotatedstring("  ", styled("│  ", :REPL_DocBrowser_tip), line, pad,
                                        styled("  │", :REPL_DocBrowser_tip)))
        for s in inner.spots
            s.line == k && push!(pb.spots, Spot(row, s.cols .+ 5, s.target))
        end
    end
    addline!(pb, "  ", ("╰" * "─"^(w - 2) * "╯", :REPL_DocBrowser_tip))
end

# A section of the start page, as a heading that `{ }`, the names pane and links can find.
function section!(pb::PageBuilder, title::String)
    addline!(pb)
    addline!(pb, "  ", (title, :markdown_h2))
    addline!(pb, "  ", ("─"^textwidth(title), :markdown_h2))
end

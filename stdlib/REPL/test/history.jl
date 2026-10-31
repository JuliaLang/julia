# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using REPL
using Dates
using Base64: base64encode

using REPL.History
using REPL.History: HistoryFile, HistEntry, update!,
    ConditionSet, FilterSpec, filterchunkrev!, ismorestrict, matchregions,
    SelectorState, componentrows, countlines_selected, hoveridx, ishover, gethover,
    candidates, movehover, toggleselection, fullselection, addcache!, sync_selection!,
    boxedcontent, highlightcand

const HISTORY_SAMPLE_FORMAT_1 = """
# time: 2020-10-31 05:16:39 AWST
# mode: julia
\tcos
# time: 2020-10-31 05:16:40 AWST
# mode: help
\tcos
# time: 2021-03-12 09:03:06 AWST
# mode: julia
\tfunction is_leap_year(year)
\t    if year % 4 == 0 && (! year % 100 == 0 || year % 400 == 0)
\t        return true
\t    else
\t        return false
\t    end
\tend
# time: 2021-03-23 16:48:55 AWST
# mode: julia
\tL²norm(x -> x^2, ℐ)
# time: 2021-03-23 16:49:06 AWST
# mode: julia
\tL²norm(x -> 9x, ℐ)
"""

const HISTORY_SAMPLE_FORMAT_2 = """
# time: 2025-10-18 18:21:03Z
# mode: julia
\tIterators.partition([1,2,3,4,5,6,7], 2) |> eltype
# time: 2025-10-19 06:27:10Z
# mode: julia
\tusing Chairmarks
# time: 2025-10-19 06:27:18Z
# mode: julia
\t@b REPL.History.HistoryFile("/home/tec/.julia/logs/repl_history.jl") REPL.History.update!
"""

const HISTORY_SAMPLE_MALFORMED = """
time: 2025-10-18 18:20:59Z
mode: julia
"""

const HISTORY_SAMPLE_BAD_SPACES = """
# time: 2025-10-18 18:20:59Z
# mode: julia
    "Spaces instead of tabs :("
"""

const HISTORY_SAMPLE_INCOMPLETE = """
# time: 2025-05-10 12:34:56Z
# mode: julia
\tfoo()
# time: 2025-05-10 12:40:00Z
# mode: julia
"""

@testset "Histfile" begin
    hpath = tempname()
    mkpath(dirname(hpath))
    @testset "History reading" begin
        @testset "Create empty HistoryFile" begin
            hist = HistoryFile(hpath)
            @test isempty(hist)
            @test length(hist) == 0
            close(hist)
            @test read(hpath, String) == ""
        end
        @testset "Format 1" begin
            write(hpath, HISTORY_SAMPLE_FORMAT_1)
            hist = HistoryFile(hpath)
            update!(hist)
            @test length(hist) == 5
            @test hist[1] == HistEntry(:julia, DateTime("2020-10-31T05:16:39"), "cos", 1)
            @test hist[2] == HistEntry(:help, DateTime("2020-10-31T05:16:40"), "cos", 2)
            funccontent = """
        function is_leap_year(year)
            if year % 4 == 0 && (! year % 100 == 0 || year % 400 == 0)
                return true
            else
                return false
            end
        end"""
            @test hist[3] == HistEntry(:julia, DateTime("2021-03-12T09:03:06"), funccontent, 3)
            @test hist[4] == HistEntry(:julia, DateTime("2021-03-23T16:48:55"), "L²norm(x -> x^2, ℐ)", 4)
            @test hist[5] == HistEntry(:julia, DateTime("2021-03-23T16:49:06"), "L²norm(x -> 9x, ℐ)", 5)
            close(hist)
        end
        @testset "Format 2" begin
            write(hpath, HISTORY_SAMPLE_FORMAT_2)
            hist = HistoryFile(hpath)
            update!(hist)
            @test length(hist) == 3
            @test hist[1] == HistEntry(:julia, DateTime("2025-10-18T18:21:03"), "Iterators.partition([1,2,3,4,5,6,7], 2) |> eltype", 1)
            @test hist[2] == HistEntry(:julia, DateTime("2025-10-19T06:27:10"), "using Chairmarks", 2)
            @test hist[3] == HistEntry(:julia, DateTime("2025-10-19T06:27:18"), "@b REPL.History.HistoryFile(\"/home/tec/.julia/logs/repl_history.jl\") REPL.History.update!", 3)
            close(hist)
        end
        @testset "Malformed" begin
            write(hpath, HISTORY_SAMPLE_MALFORMED)
            hist = HistoryFile(hpath)
            @test_warn "Malformed history entry" update!(hist)
            @test length(hist) == 0
            close(hist)
        end
        @testset "Spaces instead of tabs" begin
            write(hpath, HISTORY_SAMPLE_BAD_SPACES)
            hist = HistoryFile(hpath)
            @test_warn "Malformed history content" update!(hist)
            @test length(hist) == 0
            close(hist)
        end
        @testset "Incomplete entry" begin
            write(hpath, HISTORY_SAMPLE_INCOMPLETE)
            hist = HistoryFile(hpath)
            @test_nowarn update!(hist)
            @test length(hist) == 1
            @test hist[1] == HistEntry(:julia, DateTime("2025-05-10T12:34:56"), "foo()", 1)
            close(hist)
        end
    end

    @testset "History round trip" begin
        write(hpath, "")
        hist = HistoryFile(hpath)
        entries = [
            HistEntry(:julia, DateTime("2024-06-01T10:00:00"), "println(\"Hello, World!\")", 0),
            HistEntry(:shell, DateTime("2024-06-01T10:05:00"), "ls -la", 0),
            HistEntry(:help, DateTime("2024-06-01T10:10:00"), "? println", 0),
        ]
        for entry in entries
            push!(hist, entry)
        end
        close(hist)
        hist = HistoryFile(hpath)
        update!(hist)
        @test length(hist) == length(entries)
        for (i, entry) in enumerate(entries)
            @test hist[i].mode == entry.mode
            @test hist[i].date == entry.date
            @test hist[i].content == entry.content
            @test hist[i].index == i
        end
        close(hist)
    end

    @testset "Incremental updating" begin
        write(hpath, HISTORY_SAMPLE_FORMAT_1)
        hist_a = HistoryFile(hpath)
        hist_b = HistoryFile(hpath)
        update!(hist_a)
        update!(hist_b)
        @test length(hist_b) == 5
        push!(hist_a, HistEntry(:julia, now(UTC), "2 + 2", 0))
        @test length(hist_a) == 6
        update!(hist_b)
        @test length(hist_b) == 6
        @test hist_b[end] == hist_a[end]
        push!(hist_b, HistEntry(:shell, now(UTC), "echo 'Hello'", 0))
        @test length(hist_b) == 7
        update!(hist_a)
        @test length(hist_a) == 7
        @test hist_a[end] == hist_b[end]
        close(hist_a)
        close(hist_b)
    end
end

@testset "Filtering" begin
    @testset "ConditionSet" begin
        @testset "Parsing" begin
            @testset "Basic" begin
                cset = ConditionSet("hello world")
                @test cset.words == [SubString("hello world")]
                @test isempty(cset.exacts)
                @test isempty(cset.negatives)
                @test isempty(cset.initialisms)
                @test isempty(cset.fuzzy)
                @test isempty(cset.regexps)
                @test isempty(cset.modes)
            end
            @testset "Exact match" begin
                cset = ConditionSet("=exact")
                @test cset.exacts == [SubString("exact")]
            end
            @testset "Negative match" begin
                cset = ConditionSet("!exclude")
                @test cset.negatives == [SubString("exclude")]
            end
            @testset "Initialism" begin
                cset = ConditionSet("`im")
                @test cset.initialisms == [SubString("im")]
            end
            @testset "Regexp" begin
                cset = ConditionSet("/foo.*bar")
                @test cset.regexps == [SubString("foo.*bar")]
            end
            @testset "Mode" begin
                cset = ConditionSet("shell>")
                @test cset.modes == [SubString("shell")]
            end
            @testset "Fuzzy" begin
                cset = ConditionSet("~fuzzy")
                @test cset.fuzzy == [SubString("fuzzy")]
            end
            @testset "Space trimming" begin
                cset = ConditionSet("  word with spaces  ")
                @test cset.words == [SubString("word with spaces")]
            end
            @testset "Escaped prefix" begin
                cset = ConditionSet("\\=not exact")
                @test cset.words == [SubString("=not exact")]
            end
            @testset "Multiple conditions" begin
                cset = ConditionSet("word;=exact;!neg")
                @test cset.words == [SubString("word")]
                @test cset.exacts == [SubString("exact")]
                @test cset.negatives == [SubString("neg")]
            end
            @testset "Escaped separator" begin
                cset = ConditionSet("hello\\;world;=exact")
                @test cset.words == [SubString("hello;world")]
                @test cset.exacts == [SubString("exact")]
                cset = ConditionSet("1 \\; 2")
                @test cset.words == [SubString("1 ; 2")]
            end
            @testset "Complex query" begin
                cset = ConditionSet("some = words ;; !error ; julia> ;/^def.*;")
                @test cset.words == [SubString("some = words")]
                @test cset.negatives == [SubString("error")]
                @test cset.modes == [SubString("julia")]
                @test cset.regexps == [SubString("^def.*")]
            end
        end
    end
    @testset "FilterSpec" begin
        @testset "Construction" begin
            @testset "Words" begin
                cset = ConditionSet("bag of words")
                spec = FilterSpec(cset)
                @test isempty(spec.exacts)
                @test spec.regexps == [r"\Qbag\E"i, r"\Qof\E"i, r"\Qwords\E"i]
                cset2 = ConditionSet("Bag of Words")
                spec2 = FilterSpec(cset2)
                @test spec2.exacts == ["Bag", "of", "Words"]
                @test isempty(spec2.regexps)
            end
            @testset "Complex query" begin
                cset = ConditionSet("=exact;!neg;/foo.*bar;julia>")
                spec = FilterSpec(cset)
                @test spec.exacts == ["exact"]
                @test spec.negatives == ["neg"]
                @test spec.regexps == [r"foo.*bar"]
                @test spec.modes == [:julia]
            end
        end
        @testset "Matching" begin
            entries = [
                HistEntry(:julia, now(UTC), "println(\"hello world\")", 1),
                HistEntry(:julia, now(UTC), "log2(1234.5)", 1),
                HistEntry(:julia, now(UTC), "test case", 1),
                HistEntry(:help, now(UTC), "cos", 1),
                HistEntry(:julia, now(UTC), "cos(2π)", 1),
                HistEntry(:julia, now(UTC), "case of tests", 1),
                HistEntry(:shell, now(UTC), "echo 'Hello World'", 4),
                HistEntry(:julia, now(UTC), "foo_bar(2, 7)", 5),
                HistEntry(:julia, now(UTC), "test_fun()", 5),
            ]
            results = HistEntry[]
            @testset "Words" begin
                empty!(results)
                cset = ConditionSet("hello")
                spec = FilterSpec(cset)
                @test filterchunkrev!(results, entries, spec) == 0
                @test results == [entries[1], entries[7]]
                empty!(results)
                cset2 = ConditionSet("world")
                spec2 = FilterSpec(cset2)
                @test filterchunkrev!(results, entries, spec2) == 0
                @test results == [entries[1], entries[7]]
                empty!(results)
                cset3 = ConditionSet("World")
                spec3 = FilterSpec(cset3)
                @test filterchunkrev!(results, entries, spec3) == 0
                @test results == [entries[7]]
            end
            @testset "Exact" begin
                empty!(results)
                cset = ConditionSet("=test")
                spec = FilterSpec(cset)
                @test filterchunkrev!(results, entries, spec; maxresults = 2) == 5
                @test results == [entries[6], entries[9]]
                empty!(results)
                cset2 = ConditionSet("=test case")
                spec2 = FilterSpec(cset2)
                @test filterchunkrev!(results, entries, spec2) == 0
                @test results == [entries[3]]
            end
            @testset "Negative" begin
                empty!(results)
                cset = ConditionSet("!hello ; !test;! cos")
                spec = FilterSpec(cset)
                @test filterchunkrev!(results, entries, spec) == 0
                @test results == [entries[2], entries[7], entries[8]]
            end
            @testset "Initialism" begin
                empty!(results)
                cset = ConditionSet("`tc")
                spec = FilterSpec(cset)
                @test filterchunkrev!(results, entries, spec) == 0
                @test results == [entries[3]]
                empty!(results)
                cset2 = ConditionSet("`fb")
                spec2 = FilterSpec(cset2)
                @test filterchunkrev!(results, entries, spec2) == 0
                @test results == [entries[8]]
                # Initials start a word, follow underscores, or begin a camelCase hump
                isinitialism(q, s) = all(rx -> occursin(rx, s), FilterSpec(ConditionSet("`" * q)).regexps)
                for s in ("foo bar", "foo_bar", "_foo_bar", "fooBar", "getF(bar)", "(foo).bar")
                    @test isinitialism("fb", s)
                end
                for s in ("afoo bar", "f bar", "foo b", "bar foo")
                    @test !isinitialism("fb", s)
                end
                @test isinitialism("fb", "false):iBar")
                @test isinitialism("λf", "λx fy")
            end
            @testset "Regexp" begin
                empty!(results)
                cset = ConditionSet("/^c.s\\b")
                spec = FilterSpec(cset)
                @test filterchunkrev!(results, entries, spec) == 0
                @test results == [entries[4], entries[5]]
            end
            @testset "Mode" begin
                empty!(results)
                cset = ConditionSet("shell>")
                spec = FilterSpec(cset)
                @test filterchunkrev!(results, entries, spec) == 0
                @test results == [entries[7]]
            end
            @testset "Fuzzy" begin
                empty!(results)
                cset = ConditionSet("~cs")
                spec = FilterSpec(cset)
                @test filterchunkrev!(results, entries, spec) == 0
                @test results == entries[3:6]
            end
            @testset "Resumption" begin
                many = [HistEntry(:julia, now(UTC), "foo $i", i) for i in 1:500]
                spec = FilterSpec(ConditionSet("foo"))
                function filterresumed(; maxresults = length(many), maxtime = Inf)
                    found = HistEntry[]
                    idx = filterchunkrev!(found, many, spec; maxresults, maxtime)
                    while idx != 0
                        idx = filterchunkrev!(found, many, spec, idx; maxtime)
                    end
                    found
                end
                # Stopping early, by result count or by time, must neither skip
                # nor repeat entries, wherever that falls relative to batch boundaries.
                for maxresults in (1, 7, 10, 43, 499, 500, 1000)
                    @test filterresumed(; maxresults) == many
                end
                @test filterresumed(; maxtime = 0.0) == many
            end
        end
        @testset "matchregions with multibyte characters" begin
            # Handle search for multi-byte characters (issue 61653)
            spec_ab = FilterSpec(ConditionSet("=αβ"))
            @test matchregions(spec_ab, "αβ") == [1:3]
            # Two exact matches separated by a single space should merge, even when
            # the boundary characters are multibyte
            spec_two = FilterSpec(ConditionSet("=αβ;=γδ"))
            @test matchregions(spec_two, "αβ γδ") == [1:8]
            # Adjacent multibyte matches (no space) must not merge
            @test matchregions(spec_two, "αβγδ") == [1:3, 5:7]
            # Multi-character gap must not merge
            @test matchregions(spec_two, "αβ  γδ") == [1:3, 7:9]
            # ASCII sanity: single-space-separated matches still merge
            spec_ascii = FilterSpec(ConditionSet("=foo;=bar"))
            @test matchregions(spec_ascii, "foo bar") == [1:7]
        end
        @testset "matchregions at end of string" begin
            # Duplicate terms matching at the end of the string (issue 62341)
            spec_dup = FilterSpec(ConditionSet("test\\; test\\;"))
            @test matchregions(spec_dup, "test test;") == [6:10, 6:10]
            # Overlapping matches extending to the end of the string
            spec_overlap = FilterSpec(ConditionSet("=st;=test"))
            @test matchregions(spec_overlap, "test") == [1:4, 3:4]
        end
        @testset "Strictness comparison" begin
            c1 = ConditionSet("hello world")
            c2 = ConditionSet("hello world more")
            c3 = ConditionSet("hello world more;!exclude")
            @test ismorestrict(c2, c1)
            @test !ismorestrict(c1, c2)
            @test ismorestrict(c3, c2)
            @test !ismorestrict(c2, c3)
            @test ismorestrict(c3, c1)
            @test !ismorestrict(c1, c3)
        end
    end
end

@testset "Display calculations" begin
    entries = [HistEntry(:julia, now(UTC), "test_$i", i) for i in 1:20]
    @testset "componentrows" begin
        @testset "Standard terminal" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries)
            @test componentrows(state) == (candidates = 13, preview = 6)
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = [1, 3], gathered = HistEntry[]), 1)
            @test componentrows(state) == (candidates = 13, preview = 6)
            gathered = [HistEntry(:julia, now(UTC), "old", i) for i in 21:22]
            state = SelectorState((30, 80), "", FilterSpec(), entries, gathered)
            @test componentrows(state) == (candidates = 13, preview = 6)
        end
        @testset "Terminal size variations" begin
            @test componentrows(SelectorState((10, 80), "", FilterSpec(), entries)) == (candidates = 6, preview = 0)
            @test componentrows(SelectorState((5, 40), "", FilterSpec(), entries)) == (candidates = 2, preview = 0)
            @test componentrows(SelectorState((1, 80), "", FilterSpec(), entries)) == (candidates = 0, preview = 0)
            @test componentrows(SelectorState((100, 200), "", FilterSpec(), entries)) == (candidates = 44, preview = 22)
        end
        @testset "Preview clamping" begin
            multiline = join(["line$i" for i in 1:20], '\n')
            state = SelectorState((30, 80), "", FilterSpec(), [HistEntry(:julia, now(UTC), multiline, 1)], 0, (active = [1], gathered = HistEntry[]), 1)
            @test componentrows(state) == (candidates = 7, preview = 12)
        end
    end
    @testset "countlines_selected" begin
        @testset "Basic counting" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries)
            @test countlines_selected(state) == 0
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = [1], gathered = HistEntry[]), 1)
            @test countlines_selected(state) == 1
        end
        @testset "Multi-line entries" begin
            code = "begin\n    x = 10\n    y = 20\n    x + y\nend"
            state = SelectorState((30, 80), "", FilterSpec(), [HistEntry(:julia, now(UTC), code, 1)], 0, (active = [1], gathered = HistEntry[]), 1)
            @test countlines_selected(state) == 5
            huge = join(["line" for _ in 1:1000], '\n')
            state = SelectorState((30, 80), "", FilterSpec(), [HistEntry(:julia, now(UTC), huge, 1)], 0, (active = [1], gathered = HistEntry[]), 1)
            @test countlines_selected(state) == 1000
        end
        @testset "With gathered entries" begin
            gathered = [HistEntry(:julia, now(UTC), "old", i) for i in 21:22]
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = [1], gathered), 1)
            @test countlines_selected(state) == 4
        end
    end
    @testset "gethover" begin
        @testset "Basic retrieval" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries)
            @test gethover(state) == entries[20]
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 3)
            @test gethover(state) == entries[18]
        end
        @testset "With gathered entries" begin
            gathered = [HistEntry(:julia, now(UTC), "old_$i", i) for i in 21:22]
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered), -2)
            @test gethover(state) == gathered[2]
        end
        @testset "Invalid hover positions" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 0)
            @test gethover(state) === nothing
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 999)
            @test gethover(state) === nothing
        end
    end
    @testset "candidates" begin
        @testset "Basic windowing" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries)
            cands = candidates(state, 10)
            @test cands.active.rows == 10
            @test cands.active.width == 80
            @test cands.active.entries == entries[11:20]
            @test cands.active.selected == Int[]
            @test cands.gathered.rows == 0
        end
        @testset "With selections" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = [5, 15, 18], gathered = HistEntry[]), 1)
            cands = candidates(state, 10)
            @test cands.active.selected == [-5, 5, 8]
        end
        @testset "With gathered entries" begin
            gathered = [HistEntry(:julia, now(UTC), "gathered_$i", 20+i) for i in 1:2]
            state = SelectorState((30, 80), "", FilterSpec(), entries, gathered)
            state = SelectorState(state.area, state.query, state.filter, state.candidates, -2, state.selection, 1)
            cands = candidates(state, 10)
            @test cands.gathered.rows == 2
            @test cands.gathered.entries == gathered
            @test cands.gathered.selected == [1, 2]
        end
        @testset "Scrolling" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 6)
            state = SelectorState(state.area, state.query, state.filter, state.candidates, 5, state.selection, 6)
            cands = candidates(state, 10)
            @test cands.active.entries[1] == entries[6]
            @test cands.active.entries[end] == entries[15]
        end
        @testset "Edge cases" begin
            state = SelectorState((30, 80), "", FilterSpec(), HistEntry[])
            cands = candidates(state, 10)
            @test isempty(cands.active.entries)
            @test cands.active.rows == 10
            gathered = [HistEntry(:julia, now(UTC), "old_$i", 20+i) for i in 1:15]
            state = SelectorState((30, 80), "", FilterSpec(), entries, gathered)
            state = SelectorState(state.area, state.query, state.filter, state.candidates, -10, state.selection, -1)
            cands = candidates(state, 8)
            @test cands.gathered.rows == 7
            @test cands.active.rows == 0
            few = [HistEntry(:julia, now(UTC), "entry_$i", i) for i in 1:3]
            state = SelectorState((30, 80), "", FilterSpec(), few)
            cands = candidates(state, 20)
            @test cands.active.entries == few
        end
    end
    @testset "Rendering" begin
        # Tabs are zero-width to `textwidth` but not to terminals, so must not be displayed
        for mode in (:julia, :shell)
            @test '\t' ∉ String(highlightcand(HistEntry(mode, now(UTC), "a\tb", 1)))
        end
        # Every boxed line must exactly fill the box
        for content in (" "^200 * "\nx", "x\n" * " "^100 * "\n" * "y"^100, "x\n" * " "^100 * "y")
            io = IOBuffer()
            boxedcontent(io, Base.AnnotatedString(content), 40, 10)
            @test all(==(40) ∘ textwidth, eachsplit(chomp(String(take!(io))), '\n'))
        end
    end
end

@testset "Search state manipulation" begin
    entries = [HistEntry(:julia, now(UTC), "test_$i", i) for i in 1:20]
    @testset "movehover" begin
        @testset "Single step moves" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 5)
            @test movehover(state, false, false).hover == 4
            @test movehover(state, true, false).hover == 6
        end
        @testset "Page moves" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 5)
            @test movehover(state, false, true).hover == 1
            @test movehover(state, true, true).hover == 17
        end
        @testset "Boundary clamping" begin
            top = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 20)
            @test movehover(top, true, false).hover == 20
            bottom = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 1)
            @test movehover(bottom, false, false).hover == 1
        end
        @testset "With gathered entries" begin
            gathered = [HistEntry(:julia, now(UTC), "old_cmd", 21)]
            state = SelectorState((30, 80), "", FilterSpec(), entries, gathered)
            state = SelectorState(state.area, state.query, state.filter, state.candidates, -1, state.selection, 1)
            @test movehover(state, false, false).hover == -1
            state = SelectorState(state.area, state.query, state.filter, state.candidates, -1, state.selection, 1)
            down = movehover(state, false, false)
            @test down.hover == -1
            up = movehover(down, true, false)
            @test up.hover == 1
        end
        @testset "Empty candidates" begin
            state = SelectorState((30, 80), "", FilterSpec(), HistEntry[])
            @test movehover(state, true, false).hover == 1
            @test movehover(state, false, false).hover == 1
            gathered = [HistEntry(:julia, now(UTC), "old_cmd", 1)]
            state = SelectorState((30, 80), "", FilterSpec(), HistEntry[], gathered)
            state = SelectorState(state.area, state.query, state.filter, state.candidates, -1, state.selection, -1)
            @test movehover(state, true, false).hover == 1
            @test movehover(state, false, false).hover == -1
        end
        @testset "Single candidate" begin
            one = [HistEntry(:julia, now(UTC), "only", 1)]
            state = SelectorState((30, 80), "", FilterSpec(), one)
            @test movehover(state, true, false).hover == 1
            @test movehover(state, false, false).hover == 1
        end
    end
    @testset "toggleselection" begin
        @testset "Basic toggle" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries)
            state = toggleselection(state)
            @test state.selection.active == [20]
            state = toggleselection(state)
            @test state.selection.active == Int[]
        end
        @testset "Multiple selections" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries)
            state = toggleselection(state)
            state = movehover(state, true, false)
            state = movehover(state, true, false)
            state = toggleselection(state)
            @test state.selection.active == [18, 20]
        end
        @testset "Gathered entries" begin
            gathered = [HistEntry(:julia, now(UTC), "old_$i", 20+i) for i in 1:2]
            state = SelectorState((30, 80), "", FilterSpec(), entries, -1, (active = Int[], gathered), -1)
            @test toggleselection(state).selection.gathered == [gathered[2]]
        end
        @testset "Edge cases" begin
            invalid = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 0)
            @test toggleselection(invalid) === invalid
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20], gathered = HistEntry[]), 1)
            result = toggleselection(state)
            @test 20 ∉ result.selection.active
            state = SelectorState((30, 80), "", FilterSpec(), HistEntry[])
            @test toggleselection(state) === state
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 100)
            @test toggleselection(state) === state
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 20)
            @test 1 in toggleselection(state).selection.active
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 1)
            @test 20 in toggleselection(state).selection.active
        end
    end
    @testset "fullselection" begin
        entries = [
            HistEntry(:julia, now(UTC), "using DataFrames", 1),
            HistEntry(:julia, now(UTC), "df = load_data()", 2),
            HistEntry(:shell, now(UTC), "cat data.csv", 3),
            HistEntry(:julia, now(UTC), "describe(df)", 4),
        ]
        @testset "No selection" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries)
            @test fullselection(state) == (mode = :julia, text = "describe(df)")
        end
        @testset "Single selection" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = [2], gathered = HistEntry[]), 1)
            @test fullselection(state) == (mode = :julia, text = "df = load_data()")
        end
        @testset "Multiple selections" begin
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = [4, 1, 3], gathered = HistEntry[]), 1)
            @test fullselection(state) == (mode = :julia, text = "using DataFrames\ncat data.csv\ndescribe(df)")
        end
        @testset "With gathered entries" begin
            gathered = [HistEntry(:julia, now(UTC), "ENV[\"COLUMNS\"] = 120", 0)]
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = [2], gathered), 1)
            @test fullselection(state) == (mode = :julia, text = "ENV[\"COLUMNS\"] = 120\ndf = load_data()")
        end
        @testset "Edge cases" begin
            state = SelectorState((30, 80), "", FilterSpec(), HistEntry[], 0, (active = Int[], gathered = HistEntry[]), 1)
            @test fullselection(state) == (mode = nothing, text = "")
            state = SelectorState((30, 80), "", FilterSpec(), entries, 0, (active = Int[], gathered = HistEntry[]), 100)
            @test fullselection(state) == (mode = nothing, text = "")
            state = SelectorState((30, 80), "", FilterSpec(), HistEntry[], 0, (active = Int[], gathered = HistEntry[]), -1)
            @test fullselection(state) == (mode = nothing, text = "")
            gathered = [HistEntry(:julia, now(UTC), "old_1", 1)]
            state = SelectorState((30, 80), "", FilterSpec(), HistEntry[], 0, (active = Int[], gathered), -1)
            @test fullselection(state) == (mode = :julia, text = "old_1")
        end
    end
    @testset "Selection as candidates arrive" begin
        many = [HistEntry(:julia, now(UTC), "foo $i", i) for i in 1:100]
        function filterstate(gathered = HistEntry[])
            state = SelectorState((30, 80), "foo", FilterSpec(ConditionSet("foo")), HistEntry[], gathered)
            filterchunkrev!(state, many; maxresults = 10)
        end
        # Selections made while filtering is incomplete keep their entries
        state, idx = filterstate()
        state = toggleselection(state)
        while idx != 0
            state, idx = filterchunkrev!(state, many, idx)
        end
        @test fullselection(state).text == "foo 100"
        # Carried-over entries become active once filtering reaches them
        state, idx = filterstate([many[50]])
        while idx != 0
            state, idx = filterchunkrev!(state, many, idx)
        end
        @test isempty(state.selection.gathered)
        @test fullselection(state).text == "foo 50"
        # Carried-over entries become active in an unfiltered list too, so they can be deselected
        state = SelectorState((30, 80), "", FilterSpec(), HistEntry[], [many[100]])
        append!(state.candidates, many)
        state = sync_selection!(state, length(many))
        @test state.selection == (active = [100], gathered = HistEntry[])
        @test state.scroll == 0
        @test isempty(toggleselection(state).selection.active)
    end
    @testset "addcache!" begin
        cache, state = Int[], zero(UInt8)
        for i in 1:128
            state = addcache!(cache, state, i)
        end
        @test cache == [1, 65, 97, 113, 121, 125, 127, 128]
    end
end

# Run an interactive search in a fake terminal. The caller is given one function
# to type keys and one to wait until some text has been displayed.
function searchhistory(interact::Function, entries::Vector{HistEntry};
                       size = (30, 80), props = REPL.LineEdit.TerminalProperties())
    input, output = Pipe(), Pipe()
    Base.link_pipe!(input, reader_supports_async=true, writer_supports_async=true)
    Base.link_pipe!(output, reader_supports_async=true, writer_supports_async=true)
    term = REPL.Terminals.TTYTerminal("dumb", input.out, IOContext(output.in, :displaysize => size), devnull)
    hist = HistoryFile()
    append!(hist.records, entries)
    search = @async runsearch(hist, term, "", props)
    shown = IOBuffer()
    function waitfor(text)
        got = readuntil(output.out, text, keep = true)
        write(shown, got)
        endswith(got, text) || error("output ended before $(repr(text)) was displayed")
    end
    interact(keys -> write(input.in, keys), waitfor)
    result = fetch(search)
    close(output.in)
    close(input.in)
    write(shown, read(output.out))
    result, String(take!(shown))
end
searchhistory(entries::Vector{HistEntry}, keys::String; kwargs...) =
    searchhistory((type, _) -> type(keys), entries; kwargs...)

@testset "Interactive search" begin
    entries = [HistEntry(:julia, now(UTC), "foo $i", i) for i in 1:40]
    push!(entries, HistEntry(:shell, now(UTC), "ls bar", 41))
    nothingselected = (mode = nothing, text = "")
    @testset "Navigation" begin
        @test searchhistory(entries, "\r")[1] == (mode = :shell, text = "ls bar")
        @test searchhistory(entries, "\e[A\e[A\r")[1].text == "foo 39"
        @test searchhistory(entries, "\x10\x10\x0e\r")[1].text == "foo 40"
        @test searchhistory(entries, "\e[5~\r")[1].text == "foo 29"
        @test searchhistory(entries, "\e[5~\e[6~\r")[1].text == "ls bar"
        @test searchhistory(entries, "\e<\r")[1].text == "foo 1"
        @test searchhistory(entries, "\e<\e>\r")[1].text == "ls bar"
        result, shown = searchhistory(entries, "\r")
        @test occursin("[41/41]", shown)
        @test occursin("try ? for help", shown)
        @test occursin("shell> ls bar", shown)
        @test occursin("Ctrl+S to save", shown)
        result, shown = searchhistory(entries, "\f\r")
        @test occursin("\e[H\e[2J", shown)
        @test result.text == "ls bar"
        # Everything is drawn on the search terminal, not on the process output
        mktemp() do path, io
            redirect_stdout(io) do
                searchhistory(entries, "\r")
                searchhistory(entries, "\x19")
            end
            close(io)
            @test read(path, String) == ""
        end
    end
    @testset "Aborting" begin
        for keys in ("\x03", "\x04", "\a", "\e\e", "\e[A\t\x03")
            @test searchhistory(entries, keys)[1] == nothingselected
        end
    end
    @testset "Filtering" begin
        result, shown = searchhistory(entries, "bar\r")
        @test result == (mode = :shell, text = "ls bar")
        @test occursin("[1/1]", shown)
        @test occursin("words", shown)
        @test searchhistory(entries, "7 foo\r")[1].text == "foo 37"
        @test searchhistory(entries, "shell>\r")[1].text == "ls bar"
        @test searchhistory(entries, "nomatch\r")[1] == nothingselected
        for (query, hint) in ("=foo" => "exact", "!bar" => "negated", "`f" => "initialism",
                              "/fo+" => "regexp", "~fo" => "fuzzy", "foo;" => "separator",
                              "foo;=4" => "exact", "bar;!foo" => "negated")
            @test occursin(hint, searchhistory(entries, query * "\r")[2])
        end
        # Confirming moves the cursor to the end, so wait for the hint first
        result = searchhistory(entries) do type, waitfor
            type("julia>\e[D")
            waitfor("mode [")
            type("\r")
        end[1]
        @test result.text == "foo 40"
        # Removing characters widens the search again
        @test searchhistory(entries, "bar\x7f\x7f\x7f\r")[1].text == "ls bar"
        # Narrowing a finished search reuses its results
        result = searchhistory(entries) do type, waitfor
            type("foo")
            waitfor("words [40/40]")
            type(" 1\r")
        end[1]
        @test result.text == "foo 31"
    end
    @testset "Help" begin
        for (query, help) in ("?" => "Enter a search term", "??" => "Search your REPL history")
            result = searchhistory(entries) do type, waitfor
                type(query)
                waitfor(help)
                type("\x7f"^length(query) * "\r")
            end[1]
            @test result.text == "ls bar"
        end
    end
    @testset "Selection" begin
        result, shown = searchhistory(entries, "\e[A\e[A\t\t\r")
        @test result == (mode = :julia, text = "foo 39\nfoo 40")
        @test occursin("│ foo 39", shown) && occursin("│ foo 40", shown)
        @test searchhistory(entries, "\t\t\r")[1].text == "ls bar"
        # Selected entries are carried over into a new search
        result, shown = searchhistory(entries, "\e[A\t\e[A\e[A\tbar\t\r")
        @test result == (mode = :julia, text = "foo 39\nfoo 40\nls bar")
        @test occursin("carried over", shown)
        @test occursin("words [•/1]", shown)
    end
    @testset "Long entries" begin
        long = [HistEntry(:julia, now(UTC), "x = " * "a"^100 * " + needle", 1),
                HistEntry(:julia, now(UTC), join(("line$i" for i in 1:30), '\n'), 2),
                HistEntry(:shell, now(UTC), "echo " * "b"^100 * " haystack " * "c"^100, 3)]
        result, shown = searchhistory(long, "needle\r")
        @test result.text == long[1].content
        @test occursin("…", shown) && occursin("needle", shown)
        result, shown = searchhistory(long, "haystack\r")
        @test result.text == long[3].content
        @test occursin("…bbb", shown) && occursin("haystack ccc", shown)
        result, shown = searchhistory(long, "\t\e[A\t\e[A\e[A\t\r"; size = (30, 16))
        @test result == (mode = :julia, text = join((e.content for e in long), '\n'))
        result, shown = searchhistory(long, "\e[A\e[A\t\t\r")
        @test result.text == long[1].content * '\n' * long[2].content
        @test occursin("lines hidden", shown)
    end
    @testset "Incremental filtering" begin
        # An old selected entry is only found after the first batch of results is shown
        many = [HistEntry(:julia, now(UTC), "foo $i", i) for i in 1:2000]
        result = searchhistory(many) do type, waitfor
            type("\e<\tf")
            waitfor("words [2000/2000]")
            type("\r")
        end[1]
        @test result.text == "foo 1"
    end
    @testset "Saving" begin
        osc52 = REPL.LineEdit.TerminalProperties()
        osc52.da1 = [62, 52]
        result, shown = searchhistory(entries, "\x19"; props = osc52)
        @test result == nothingselected
        @test occursin("\e]52;c;$(base64encode("ls bar"))\e\\", shown)
        @test occursin("Copied 1 line to clipboard", shown)
        result, shown = searchhistory(entries, "\e[A\e[A\t\t\x13\r"; props = osc52)
        @test result == nothingselected
        @test occursin("save to", shown)
        @test occursin("\e]52;c;$(base64encode("foo 39\nfoo 40"))\e\\", shown)
        @test occursin("Copied 2 lines to clipboard", shown)
        # Escape sequences in the save menu do not change the choice
        result, shown = searchhistory(entries, "\x13\e[A\e]11;rgb:0/0/0\a\r"; props = osc52)
        @test occursin("\e]52;c;$(base64encode("ls bar"))\e\\", shown)
        # The clipboard can be offered once the terminal reports it
        props = REPL.LineEdit.TerminalProperties()
        result, shown = searchhistory(entries, "\x13\e[?62;52c\r"; props)
        @test props.da1 == [62, 52]
        @test occursin("\e]52;c;$(base64encode("ls bar"))\e\\", shown)
        mktempdir() do dir
            path = joinpath(dir, "selection.jl")
            result, shown = searchhistory(entries, "\e[A\t\x13\t\r$path\n"; props = osc52)
            @test result == nothingselected
            @test read(path, String) == "foo 40\n"
            @test occursin("Wrote 1 selected line to", shown)
            result, shown = searchhistory(entries, "\x13\t\r\n"; props = osc52)
            @test occursin("History selection aborted", shown)
            @test read(path, String) == "foo 40\n"
            # Saving to an existing file appends, starting on a new line
            searchhistory(entries, "\x13\t\r$path\n"; props = osc52)
            @test read(path, String) == "foo 40\nls bar\n"
            write(path, "x = 1")
            searchhistory(entries, "\x13\t\r$path\n"; props = osc52)
            @test read(path, String) == "x = 1\nls bar\n"
        end
        # Leaving the save menu returns to the search
        for keys in ("\x13\x03\r", "\x13\e\e\r", "\x13\ex\x03\r")
            @test searchhistory(entries, keys; props = osc52)[1].text == "ls bar"
        end
    end
end

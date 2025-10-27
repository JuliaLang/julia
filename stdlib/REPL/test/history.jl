# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using REPL
using Dates

using REPL.History
using REPL.History: HistoryFile, HistEntry, update!,
    ConditionSet, FilterSpec, filterchunkrev!, ismorestrict,
    SelectorState, componentrows, countlines_selected, hoveridx, ishover, gethover,
    candidates, movehover, toggleselection, fullselection, addcache!

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
                cset = ConditionSet(">shell")
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
            end
            @testset "Complex query" begin
                cset = ConditionSet("some = words ;; !error ;> julia;/^def.*;")
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
                cset = ConditionSet("=exact;!neg;/foo.*bar;>julia")
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
                @test filterchunkrev!(results, entries, spec, maxresults = 2) == 5
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
                cset = ConditionSet(">shell")
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
    @testset "addcache!" begin
        cache, state = Int[], zero(UInt8)
        for i in 1:128
            state = addcache!(cache, state, i)
        end
        @test cache == [1, 65, 97, 113, 121, 125, 127, 128]
    end
end

# TODO: Prompt handling/events, terminal rendering, and end-to-end integration tests

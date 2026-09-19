#!/usr/bin/env julia
#
# Compare one Buildkite build's job durations against the last N days of Julia CI history.
#
# Usage (from the julia repo root):
#   julia --project=contrib/ci-timing contrib/ci-timing <PR-number>
#     [--days 7] [--include-failed] [--min-seconds 0]
#   julia --project=contrib/ci-timing contrib/ci-timing --build julia-pr/853 [...]
#
# Prints a markdown table (one row per job, sorted slowest first) plus a TOTAL row.
# No Buildkite token needed: job data comes from the build page's public JSON
# endpoint, history from https://perf.julialang.org/data/timing_summary.json.gz
# (the JuliaCI/julia-ci-timing dataset, cached in the temp dir for 6h).
# Resolving a PR number to a build needs the `gh` CLI; --build does not.

using Pkg
Pkg.instantiate()

using JSON, Downloads, Dates, Statistics, Printf

const HISTORY_URL = "https://perf.julialang.org/data/timing_summary.json.gz"
const CACHE = joinpath(tempdir(), "julia_timing_summary.json.gz")
const CACHE_MAX_AGE = 6 * 3600

getjson(url) = JSON.parse(sprint(io -> Downloads.download(url, io;
                                headers = ["Accept" => "application/json"])))

"Find the newest Buildkite build on a JuliaLang/julia PR, plus its title and sha."
function build_from_pr(pr)
    out = read(`gh pr view $pr --repo JuliaLang/julia --json statusCheckRollup,title,headRefOid`, String)
    data = JSON.parse(out)
    builds = Set{Tuple{String, Int}}()
    for c in data["statusCheckRollup"]
        m = match(r"buildkite\.com/julialang/([\w-]+)/builds/(\d+)", something(get(c, "targetUrl", nothing), ""))
        m === nothing || push!(builds, (m[1], parse(Int, m[2])))
    end
    isempty(builds) && error("no Buildkite build found on PR #$pr")
    pipeline, number = last(sort!(collect(builds); by = last))
    return pipeline, number, data["title"], data["headRefOid"][1:9]
end

parsetime(s) = DateTime(s, dateformat"yyyy-mm-dd\THH:MM:SS.sssZ")

"(name, duration, failed) for every finished job of a build, newest attempt only."
function fetch_jobs(pipeline, number)
    records = getjson("https://buildkite.com/julialang/$pipeline/builds/$number/data/jobs")["records"]
    jobs = Tuple{String, Float64, Bool}[]
    for j in records
        # skip unnamed steps, broken (never ran) and running jobs, and superseded retries
        (get(j, "name", nothing) isa String && j["state"] == "finished" &&
            j["started_at"] isa String && j["finished_at"] isa String &&
            get(j, "retried_at", nothing) === nothing) || continue
        dur = Dates.value(parsetime(j["finished_at"]) - parsetime(j["started_at"])) / 1000
        push!(jobs, (j["name"], dur, get(j, "exit_status", 1) != 0))
    end
    return jobs
end

function history()
    if !isfile(CACHE) || time() - mtime(CACHE) > CACHE_MAX_AGE
        Downloads.download(HISTORY_URL, CACHE)
    end
    return JSON.parse(read(pipeline(`gzip -dc $CACHE`), String))["jobs"]
end

fmt(sec) = (m = round(Int, sec); @sprintf("%d:%02d", m ÷ 60, m % 60))

function main(args)
    pr = nothing; build = nothing; days = 7; include_failed = false; min_seconds = 0.0
    i = 1
    while i <= length(args)
        a = args[i]
        if a == "--build"; build = args[i += 1]
        elseif a == "--days"; days = parse(Int, args[i += 1])
        elseif a == "--min-seconds"; min_seconds = parse(Float64, args[i += 1])
        elseif a == "--include-failed"; include_failed = true
        elseif !startswith(a, "-"); pr = parse(Int, a)
        else error("unknown argument $a")
        end
        i += 1
    end

    title = sha = nothing
    if build !== nothing
        p, n = split(build, '/')
        pipeline, number = String(p), parse(Int, n)
    elseif pr !== nothing
        pipeline, number, title, sha = build_from_pr(pr)
    else
        error("give a PR number or --build <pipeline>/<number>")
    end

    jobs = fetch_jobs(pipeline, number)
    hist = history()
    latest = maximum(DateTime(r["date"], dateformat"yyyy-mm-dd HH:MM")
                     for v in values(hist) for r in v["recent"])
    cut = latest - Day(days)

    rows = NamedTuple[]; failed_out = String[]; nobase = String[]
    for (name, dur, failed) in jobs
        if failed && !include_failed
            push!(failed_out, name); continue
        end
        vals = Float64[r["duration"] for r in get(get(hist, name, Dict()), "recent", [])
                       if r["state"] == "passed" && r["duration"] !== nothing &&
                          DateTime(r["date"], dateformat"yyyy-mm-dd HH:MM") >= cut]
        if isempty(vals)
            push!(nobase, name); continue
        end
        m = mean(vals)
        m < min_seconds && continue
        push!(rows, (; name = name * (failed ? " *(failed)*" : ""), dur,
                       mean = m, mn = minimum(vals), mx = maximum(vals), n = length(vals)))
    end
    sort!(rows; by = r -> -r.dur)

    label = pr === nothing ? "$pipeline#$number" : "PR #$pr"
    title === nothing || println("<!-- $title ($sha), $pipeline#$number; baseline ",
        Dates.format(cut, "yyyy-mm-dd HH:MM"), "..", Dates.format(latest, "yyyy-mm-dd HH:MM"),
        " UTC, passed runs only -->")
    println("| Job | $label | Mean | Min | Max | N | Δ vs mean |")
    println("|---|---:|---:|---:|---:|---:|---:|")
    for r in rows
        flag = r.dur > r.mx ? " ⚠️" : r.dur < r.mn ? " ✅" : ""
        @printf("| %s | %s | %s | %s | %s | %d | %+.1f%%%s |\n", r.name, fmt(r.dur),
                fmt(r.mean), fmt(r.mn), fmt(r.mx), r.n, 100 * (r.dur - r.mean) / r.mean, flag)
    end
    tp = sum(r -> r.dur, rows); tm = sum(r -> r.mean, rows)
    @printf("| **TOTAL (%d jobs)** | **%s** | **%s** | — | — | — | **%+.1f%%** |\n",
            length(rows), fmt(tp), fmt(tm), 100 * (tp - tm) / tm)
    isempty(failed_out) || println("<!-- failed, excluded: ", join(failed_out, ", "), " -->")
    isempty(nobase) || println("<!-- no baseline: ", join(nobase, ", "), " -->")
end

main(ARGS)

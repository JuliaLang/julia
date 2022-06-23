# Write in a format compatible to BuildKite
# https://buildkite.com/docs/test-analytics/importing-json#json-test-results-data-reference

# TODO: Capture more information about Pass, TestSet
# TestSet:
#  - location
# Better error output


escape(string) = "\"$string\""

struct History
    start_at::Float64
    end_at::Float64
    duration::Float64
end

function write_json(io, indent, hist::History)
    println(io, " "^indent, "{")
    indent += 2
    println(io, " "^indent, escape("start_at"), ": $(hist.start_at)")
    println(io, " "^indent, escape("end_at"), ": $(hist.end_at)")
    println(io, " "^indent, escape("duration"), ": $(hist.duration)")
    indent -= 2
    print(io, " "^indent, "}")
end

struct TestResult
    uuid::Base.UUID
    scope::String
    id::String # required
    location::String # file:line
    result::String # passed, failed, skipped or unknown
    failure_reason::String
    expanded::Dict{String, Any}
    history::History
end

function write_json(io, indent, tr::TestResult)
    println(io, " "^indent, "{")
    indent += 2
    println(io, " "^indent, escape("uuid"), ": $(tr.uuid)")
    println(io, " "^indent, escape("scope"), ": ", tr.scope)
    println(io, " "^indent, escape("id"), ": ", tr.id)
    println(io, " "^indent, escape("location"), ": ", tr.location)
    println(io, " "^indent, escape("result"), ": ", tr.result)
    println(io, " "^indent, escape("failure_reason"), ": ", tr.failure_reason)
    # expanded
    println(io, " "^indent, escape("history"), ": ")
    write_json(io, indent, tr.history)
    println(io)
    indent -= 2
    print(io, " "^indent, "}")
end

function report!(results, ts::Test.DefaultTestSet, path="")
    scope = join((path, ts.description), "/")
    history = History(ts.time_start, ts.time_end, ts.time_end-ts.time_start)
    failed_tests = 0
    for (i, result) in enumerate(ts.results)
        if result isa Test.DefaultTestSet
            report!(results, result, scope)
        elseif result isa Test.Fail || result isa Test.Pass
            report!(results, result, scope, history)
        end
        if result isa Test.DefaultTestSet || result isa Test.Pass
            continue
        end
        failed_tests += 1
    end
    result = TestResult(
        Base.UUID(rand(UInt128)),
        scope,
        scope,
        "",
        failed_tests > 0 ? "failed" : "passed",
        "Passed: $(ts.n_passed)",
        Dict{String, Any}(),
        history
    )
    push!(results, result)
end

function report!(results, p::Test.Pass, scope, history)
    location = string(p.source)
    pass = TestResult(
        Base.UUID(rand(UInt128)),
        scope,
        location,
        location,
        "passed",
        "",
        Dict{String, Any}(),
        history
    )
    push!(results, pass)
    nothing
end

function report!(results, f::Test.Fail, scope, history)
    location = string(f.source)
    fail = TestResult(
        Base.UUID(rand(UInt128)),
        scope,
        location,
        location,
        "failed",
        "",
        Dict{String, Any}(),
        history
    )
    push!(results, fail)
    nothing
end

function write_testset(ts::Test.DefaultTestSet)
    results = TestResult[]
    report!(results, ts)
    open("results.json", "w") do io
        println(io, "[")
        for result in results
            write_json(io, 2, result)
            println(",")
        end
        println(io, "]")
    end
end

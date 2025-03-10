# This file is a part of Julia. License is MIT: https://julialang.org/license

# Convert test(set) results to a Buildkite-compatible JSON representation.
# Based on <https://buildkite.com/docs/test-analytics/importing-json#json-test-results-data-reference>.

module BuildkiteTestJSON

using Test
using Dates

export write_testset_json_files

# Bootleg JSON writer

"""
    json_repr(io::IO, value; kwargs...) -> Nothing

Obtain a JSON representation of `value`, and print it to `io`.

This may not be the best, most feature-complete, or fastest implementation.
However, it works for its intended purpose.
"""
function json_repr end

function json_repr(io::IO, val::String; indent::Int=0)
    print(io, '"')
    escape_string(io, val, ('"',))
    print(io, '"')
end
json_repr(io::IO, val::Integer; indent::Int=0) = print(io, val)
json_repr(io::IO, val::Float64; indent::Int=0) = print(io, val)
function json_repr(io::IO, val::AbstractVector; indent::Int=0)
    print(io, '[')
    for i in eachindex(val)
        print(io, '\n', ' '^(indent + 2))
        json_repr(io, val[i]; indent=indent + 2)
        i == lastindex(val) || print(io, ',')
    end
    print(io, '\n', ' '^indent, ']')
end
function json_repr(io::IO, val::Dict; indent::Int=0)
    print(io, '{')
    len = length(val)
    for (i, (k, v)) in enumerate(pairs(val))
        print(io, '\n', ' '^(indent + 2))
        json_repr(io, string(k))
        print(io, ": ")
        json_repr(io, v; indent=indent + 2)
        i == len || print(io, ',')
    end
    print(io, '\n', ' '^indent, '}')
end
json_repr(io::IO, val::Any; indent::Int=0) = json_repr(io, string(val))

# Test result processing

function result_dict(testset::Test.DefaultTestSet, prefix::String="")
    scope = if isempty(prefix)
        testset.description == "Overall" ? "" : testset.description
    else
        join((prefix, testset.description), '/')
    end
    data = Dict{String,Any}(
        "id" => Base.UUID(rand(UInt128)),
        "scope" => scope,
        "tags" => Dict{String,String}(
            "job_label" => get(ENV, "BUILDKITE_LABEL", "unknown"),
            "job_id" => get(ENV, "BUILDKITE_JOB_ID", "unknown"),
            "job_group" => get(ENV, "BUILDKITE_GROUP_LABEL", "unknown"),
            "os" => string(Sys.KERNEL),
            "arch" => string(Sys.ARCH),
            "julia_version" => string(VERSION),
            "testset" => testset.description,
        ),
        # note we drop some of this from common_data before merging into individual results
        "history" => if !isnothing(testset.time_end)
            Dict{String,Any}(
                "start_at" => testset.time_start,
                "end_at" => testset.time_end,
                "duration" => testset.time_end - testset.time_start)
        else
            Dict{String,Any}("start_at" => testset.time_start, "duration" => 0.0)
        end)
    return data
end

# Test paths on runners are often in deep directories, so just make them contain enough information
# to be able to identify the file. Also convert Windows-style paths to Unix-style paths so tests can
# be grouped by file.
const generalize_file_paths_cache = Dict{AbstractString,AbstractString}()
const norm_build_root_path = normpath(Sys.BUILD_ROOT_PATH)
const bindir_dir = dirname(Sys.BINDIR)
const pathsep = Sys.iswindows() ? '\\' : '/'
function generalize_file_paths(path::AbstractString)
    return get!(generalize_file_paths_cache, path) do
        path = replace(path,
            string(Sys.STDLIB, pathsep) => "",
            string(norm_build_root_path, pathsep) => "",
            string(bindir_dir, pathsep) => ""
        )
        @static if Sys.iswindows()
            return replace(path, "\\" => "/")
        else
            return path
        end
    end
end

# passed, failed, skipped, or unknown
function get_status(result)
    if result isa Test.Pass && result.test_type === :skipped
        "skipped"
    elseif result isa Test.Broken
        "skipped" # buildkite don't have a "broken" status
    elseif result isa Test.Pass
        "passed"
    elseif result isa Test.Fail || result isa Test.Error
        "failed"
    else
        "unknown"
    end
end

# An attempt to reconstruct the test call.
# Note we can't know if broken or skip was via the broken/skip macros or kwargs.
function get_test_call_str(result)
    tt = result.test_type
    if tt in (:test, :test_nonbool, :test_error, :test_interrupted)
        "@test $(result.orig_expr)"
    elseif tt === :test_unbroken
        "@test_broken $(result.orig_expr)"
    elseif tt === :skipped
        "@test_skip $(result.orig_expr)"
    elseif tt === (:test_throws, :test_throws_wrong, :test_throws_nothing)
        expected = t.data
        "@test_throws $expected $(result.orig_expr)"
    elseif tt === :nontest_error
        "Non-test error"
    end
end

function result_dict(result::Test.Result)
    file, line = if !hasproperty(result, :source) || isnothing(result.source)
        "unknown", 0
    else
        something(result.source.file, "unknown"), result.source.line
    end
    file = generalize_file_paths(string(file))

    status = get_status(result)
    test_call = get_test_call_str(result)

    data = Dict{String,Any}(
        "name" => test_call,
        "location" => string(file, ':', line),
        "file_name" => file,
        "result" => status)

    job_label = replace(get(ENV, "BUILDKITE_LABEL", "job label not found"), r":\w+:\s*" => "")
    if result isa Test.Fail || result isa Test.Error
        result_show = sprint(show, result; context=:color => false)
        firstline = split(result_show, '\n')[1]
        data["failure_reason"] = generalize_file_paths(firstline) * " | $job_label"
        err_trace = split(result_show, "\nStacktrace:\n", limit=2)
        if length(err_trace) == 2
            err, trace = err_trace
            data["failure_expanded"] = [Dict{String,Any}("expanded" => split(err, '\n'), "backtrace" => split(trace, '\n'))]
        else
            data["failure_expanded"] = [Dict{String,Any}("expanded" => split(result_show, '\n'), "backtrace" => [])]
        end
    end
    return data
end

function collect_results!(results::Vector{Dict{String,Any}}, testset::Test.DefaultTestSet, prefix::String="")
    common_data = result_dict(testset, prefix)
    # testset duration is not relevant for individual test results
    common_data["history"]["duration"] = 0.0 # required field
    delete!(common_data["history"], "end_at")
    result_offset = length(results) + 1
    result_counts = Dict{Tuple{String,String,UInt64},Int}()
    get_rid(rdata) = (rdata["location"], rdata["result"], haskey(rdata, "failure_expanded") ? hash(rdata["failure_expanded"]) : UInt64(0))
    for (i, result) in enumerate(testset.results)
        if result isa Test.Result
            rdata = result_dict(result)
            rid = get_rid(rdata)
            if haskey(result_counts, rid)
                result_counts[rid] += 1
            else
                result_counts[rid] = 1
                push!(results, merge(common_data, rdata))
            end
        elseif result isa Test.DefaultTestSet
            collect_results!(results, result, common_data["scope"])
        end
    end
    # Add a tag for count of each result
    for result in results[result_offset:end]
        rid = get_rid(result)
        result["tags"]["count"] = string(get(result_counts, rid, 1))
    end
    return results
end

function write_testset_json_files(dir::String, testset::Test.DefaultTestSet)
    data = Dict{String,Any}[]
    collect_results!(data, testset)
    files = String[]
    # Buildkite is limited to 5000 results per file https://buildkite.com/docs/test-analytics/importing-json
    for (i, chunk) in enumerate(Iterators.partition(data, 5000))
        res_file = joinpath(dir, "results_$i.json")
        open(io -> json_repr(io, chunk), res_file, "w")
        push!(files, res_file)
        @info "Saved $(basename(res_file)) ($(Base.format_bytes(filesize(res_file))))"
    end
    return files
end

end

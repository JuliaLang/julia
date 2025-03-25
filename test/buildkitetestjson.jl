# This file is a part of Julia. License is MIT: https://julialang.org/license

# Convert test(set) results to a Buildkite-compatible JSON representation.
# Based on <https://buildkite.com/docs/test-analytics/importing-json#json-test-results-data-reference>.

module BuildkiteTestJSON

using Test
using Dates
using Serialization

export serialize_testset_result_file, write_testset_json_files

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
            Sys.STDLIB => "stdlib",
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

# raw_file_path,line => file,location
const location_cache = Dict{Tuple{Symbol,Int},Tuple{String,String}}()
function get_location(file::Symbol, line::Int)
    return get!(location_cache, (file, line)) do
        _file = generalize_file_paths(string(file))
        _location = string(_file, ":", line)
        return _file, _location
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
const TEST_TYPE_MAP = Dict(
    :test => "@test",
    :test_nonbool => "@test",
    :test_error => "@test",
    :test_interrupted => "@test",
    :test_unbroken => "@test_broken",
    :skipped => "@test_skip",
    :test_throws => "@test_throws",
    :test_throws_wrong => "@test_throws",
    :test_throws_nothing => "@test_throws"
)
function get_test_call_str(result)
    prefix = get(TEST_TYPE_MAP, result.test_type, nothing)
    prefix === nothing && return error("Unknown test type $(repr(result.test_type))")
    return prefix == "@test_throws" ? "@test_throws $(result.data) $(result.orig_expr)" : "$prefix $(result.orig_expr)"
end

get_rid(rdata) = (rdata["location"], rdata["result"], haskey(rdata, "failure_expanded") ? hash(rdata["failure_expanded"]) : UInt64(0))

const ResultCountDict = Dict{Tuple{String,String,UInt64},Int}

function is_duplicate_pass(result::Test.Pass, location, status, result_counts::ResultCountDict)
    rid = (location, status, UInt64(0))
    count = get(result_counts, rid, nothing)
    if count !== nothing
        result_counts[rid] = count + 1
        return true
    end
    return false
end
is_duplicate_pass(result::Test.Result, location, status, result_counts::ResultCountDict) = false

function result_dict(result::Test.Result, result_counts::ResultCountDict)
    file, line = if !hasproperty(result, :source) || isnothing(result.source)
        :unknown, 0
    else
        something(result.source.file, :unknown), result.source.line
    end
    file, location = get_location(file, line)
    status = get_status(result)

    # Early exit for passed tests before more expensive operations
    if is_duplicate_pass(result, location, status, result_counts)
        return nothing
    end

    data = Dict{String,Any}(
        "location" => location,
        "result" => status,
        "name" => get_test_call_str(result),
        "file_name" => file)

    if result isa Test.Fail || result isa Test.Error
        job_label = replace(get(ENV, "BUILDKITE_LABEL", "job label not found"), r":\w+:\s*" => "")
        result_show = sprint(show, result; context=:color => false)
        firstline = split(result_show, '\n')[1]
        # put the job label at the end here because of the way buildkite UI is laid out
        data["failure_reason"] = generalize_file_paths(firstline) * " | $job_label"
        err_trace = split(result_show, "\nStacktrace:\n", limit=2)
        if length(err_trace) == 2
            err, trace = err_trace
            data["failure_expanded"] = [Dict{String,Any}("expanded" => split(err, '\n'), "backtrace" => split(trace, '\n'))]
        else
            data["failure_expanded"] = [Dict{String,Any}("expanded" => split(result_show, '\n'), "backtrace" => [])]
        end
    end

    rid = get_rid(data)
    duplicate = haskey(result_counts, rid)

    if duplicate
        result_counts[rid] += 1
        return nothing
    else
        result_counts[rid] = 1
        return data
    end
end

function collect_results!(results::Vector{Dict{String,Any}}, result::Test.Result, common_data::Dict{String,Any}, result_counts::ResultCountDict)
    rdata = result_dict(result, result_counts)
    if rdata !== nothing # nothing if it's a duplicate that's been counted
        push!(results, merge(common_data, rdata))
    end
end
function collect_results!(results::Vector{Dict{String,Any}}, result::Test.DefaultTestSet, common_data::Dict{String,Any}, result_counts::ResultCountDict)
    collect_results!(results, result, common_data["scope"])
end
function collect_results!(results::Vector{Dict{String,Any}}, result, common_data::Dict{String,Any}, result_counts::ResultCountDict)
    return nothing
end

function collect_results!(results::Vector{Dict{String,Any}}, testset::Test.DefaultTestSet, prefix::String="")
    common_data = result_dict(testset, prefix)
    # testset duration is not relevant for individual test results
    common_data["history"]["duration"] = 0.0 # required field
    delete!(common_data["history"], "end_at")
    result_offset = length(results) + 1
    result_counts = ResultCountDict()

    for result in testset.results
        collect_results!(results, result, common_data, result_counts)
    end
    # Add a tag for count of each result
    for result in results[result_offset:end]
        rid = get_rid(result)
        result["tags"]["count"] = string(get(result_counts, rid, 1))
    end
    return results
end

function serialize_testset_result_file(dir::String, testset::Test.DefaultTestSet)
    data = Dict{String,Any}[]
    t = @elapsed collect_results!(data, testset)
    if t > 20 # most are << 5s
        @warn "Collating test result data was slow: $t seconds" collated_results=length(data)
    end
    name = replace(testset.description, r"[^a-zA-Z0-9]" => "_")
    res_file = joinpath(dir, "results_$(name).dat")
    t = @elapsed Serialization.serialize(res_file, data)
    if t > 10
        @warn "Serializing test result data was slow: $t seconds" file = res_file size = Base.format_bytes(filesize(res_file))
    end
    return res_file
end

# deserilalizes the results files and writes them to collated JSON files of 5000 max results
function write_testset_json_files(dir::String)
    data = Dict{String,Any}[]
    read_files = String[]
    for res_dat in filter!(x -> occursin(r"^results.*\.dat$", x), readdir(dir))
        res_file = joinpath(dir, res_dat)
        append!(data, Serialization.deserialize(res_file))
        @debug "Loaded $(basename(res_file)) ($(Base.format_bytes(filesize(res_file))))"
        push!(read_files, res_file)
    end
    files = String[]
    # Buildkite is limited to 5000 results per file https://buildkite.com/docs/test-analytics/importing-json
    for (i, chunk) in enumerate(Iterators.partition(data, 5000))
        res_file = joinpath(dir, "results_$(lpad(i, 3, '0')).json")
        open(io -> json_repr(io, chunk), res_file, "w")
        push!(files, res_file)
        @debug "Saved $(basename(res_file)) ($(length(chunk)) results, $(Base.format_bytes(filesize(res_file))))"
    end
    for res_file in read_files
        rm(res_file)
        @debug "Deleted $(basename(res_file))"
    end
    return files
end

end

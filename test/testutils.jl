module TestUtils

import Test

function write_failed_testsets_if_requested(ts::Test.DefaultTestSet)
    failed_testset_file = get_failed_testset_filename()

    if failed_testset_file !== nothing
        touch(failed_testset_file)
        print_failed_testsets(failed_testset_file, ts)
        @info "Wrote the list of failed test sets to file" failed_testset_file

        @info "" read(failed_testset_file, String) # TODO: delete this line
        flush(stdout) # TODO: delete this line
        flush(stderr) # TODO: delete this line
        println("### BEGIN contents of the failed_testset_file") # TODO: delete this line
        println(read(failed_testset_file, String)) # TODO: delete this line
        println("### END contents of the failed_testset_file") # TODO: delete this line
        flush(stdout) # TODO: delete this line
        flush(stderr) # TODO: delete this line
    end

    return failed_testset_file
end

function get_failed_testset_filename()
    environment_variable_name = "JULIA_TEST_FAILED_TESTSET_FILE"
    environment_variable_value = strip(get(ENV, environment_variable_name, ""))
    if isempty(environment_variable_value)
        return nothing
    end
    filename = convert(String, environment_variable_value)::String
    return filename
end

function print_failed_testsets(filename::String, ts::Test.DefaultTestSet)
    open(filename, "w") do io
        print_failed_testsets(io, ts)
    end
    return nothing
end

function print_failed_testsets(io::IO, ts::Test.DefaultTestSet)
    failed_testsets = get_failed_testsets(ts)
    for name in failed_testsets
        println(io, name)
    end
    return nothing
end

function get_failed_testsets(ts::Test.DefaultTestSet)::Vector{String}
    if ts.description != "Overall"
        msg = "Expected the testset name to be \"Overall\", but got \"$(ts.description)\" instead"
        throw(ErrorException(msg))
    end

    if isempty(ts.results)
        msg = "Did not find any testset results"
        throw(ErrorException(msg))
    end

    failed_testsets = String[]

    for t in ts.results
        t::Test.DefaultTestSet
        if t.anynonpass
            push!(failed_testsets, strip(t.description))
        end
    end

    if length(failed_testsets) != length(unique(failed_testsets))
        msg = "Duplicate testset names"
        throw(ErrorException(msg))
    end

    return failed_testsets
end

end # module

# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestDownload

using Test

mktempdir() do temp_dir
    candidate1 = strip(get(ENV, "JULIA_TEST_HTTPBINGO_SERVER", ""))
    candidate2 = "https://httpbingo.julialang.org"
    candidate3 = "https://httpbin.julialang.org"
    # httpbingo and httpbin do not have the same API in general, but for the purposes of this specific testset, either will work.
    url = nothing
    # We try candidate1 and candidate2, to see if either work
    # We use the first one that works
    for candidate in [candidate1, candidate2]
        if !isempty(candidate)
            did_succeed = try
                download(candidate)
            catch ex
                bt = catch_backtrace()
                @warn "Encountered error with server candidate" candidate exception=(ex,bt)
            end
            if did_succeed
                url = candidate
            end
        end
    end
    if url === nothing
        # If we have to fall back to candidate3, we don't bother trying it, we just use it
        url = candidate3
    end
    @debug "Selected server: $url"

    # Download a file
    file = joinpath(temp_dir, "ip")
    @test download("$url/ip", file) == file
    @test isfile(file)
    @test !isempty(read(file))
    ip = read(file, String)

    # Download an empty file
    empty_file = joinpath(temp_dir, "empty")
    @test download("$url/status/200", empty_file) == empty_file
    @test isfile(empty_file)
    @test isempty(read(empty_file))

    # Make sure that failed downloads do not leave files around
    missing_file = joinpath(temp_dir, "missing")
    @test_throws Exception download("$url/status/404", missing_file)
    @test !isfile(missing_file)

    # Use a TEST-NET (192.0.2.0/24) address which shouldn't be bound
    invalid_host_file = joinpath(temp_dir, "invalid_host")
    @test_throws Exception download("http://192.0.2.1", invalid_host_file)
    @test !isfile(invalid_host_file)
end

end # module

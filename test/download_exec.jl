# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestDownload

using Test

mktempdir() do temp_dir
    # Download a file
    file = joinpath(temp_dir, "ip")
    @test download("https://httpbin.julialang.org/ip", file) == file
    @test isfile(file)
    @test !isempty(read(file))
    ip = read(file, String)

    # Download an empty file
    empty_file = joinpath(temp_dir, "empty")
    @test download("https://httpbin.julialang.org/status/200", empty_file) == empty_file
    @test isfile(empty_file)
    @test isempty(read(empty_file))

    # Make sure that failed downloads do not leave files around
    missing_file = joinpath(temp_dir, "missing")
    @test_throws Exception download("https://httpbin.julialang.org/status/404", missing_file)
    @test !isfile(missing_file)

    # Use a TEST-NET (192.0.2.0/24) address which shouldn't be bound
    invalid_host_file = joinpath(temp_dir, "invalid_host")
    @test_throws Exception download("http://192.0.2.1", invalid_host_file)
    @test !isfile(invalid_host_file)
end

end # module
